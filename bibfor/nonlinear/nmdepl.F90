subroutine nmdepl(modele, numedd , mate      , carele , comref     ,&
                  compor, lischa , fonact    , sdstat , ds_algopara,&
                  carcri, noma   , numins    , iterat , solveu     ,&
                  matass, sddisc , sddyna    , sdnume , sdpilo     ,&
                  sdtime, sderro , ds_contact, valinc , solalg     ,&
                  veelem, veasse , eta       , ds_conv, lerrit)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cldual_maj.h"
#include "asterfort/dbgcha.h"
#include "asterfort/diinst.h"
#include "asterfort/GetResi.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcoun.h"
#include "asterfort/nmcret.h"
#include "asterfort/nmfext.h"
#include "asterfort/nmltev.h"
#include "asterfort/nmmajc.h"
#include "asterfort/nmpich.h"
#include "asterfort/nmpild.h"
#include "asterfort/nmreli.h"
#include "asterfort/nmrepl.h"
#include "asterfort/nmsolm.h"
#include "asterfort/nmsolu.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! aslint: disable=W1504
!
    integer :: fonact(*)
    integer :: iterat, numins
    real(kind=8) :: eta
    character(len=8) :: noma
    type(NL_DS_Conv), intent(inout) :: ds_conv
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=19) :: sddisc, sdnume, sddyna, sdpilo
    character(len=19) :: lischa, matass, solveu
    character(len=24) :: modele, numedd, mate, carele, comref, compor
    character(len=24) :: carcri, sdtime, sderro, sdstat
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    type(NL_DS_Contact), intent(in) :: ds_contact
    aster_logical :: lerrit
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DE L'INCREMENT DE DEPLACEMENT A PARTIR DE(S) DIRECTION(S)
! DE DESCENTE
! PRISE EN COMPTE DU PILOTAGE ET DE LA RECHERCHE LINEAIRE
!
! --------------------------------------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! In  ds_algopara      : datastructure for algorithm parameters
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  NOMA   : NOM DU MAILLAGE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  NUMINS : NUMERO D'INSTANT
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  SOLVEU : NOM DU SOLVEUR
! IN  SDNUME : SD NUMEROTATION
! IN  SDDISC : SD DISCRETISATION
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDPILO : SD PILOTAGE
! IN  SDERRO : SD GESTION DES ERREURS
! In  ds_contact       : datastructure for contact management
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IO  ds_conv          : datastructure for convergence management
! I/O ETA    : PARAMETRE DE PILOTAGE
! OUT LERRIT : .TRUE. SI ERREUR PENDANT L'ITERATION
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: etan, offset, rho
    real(kind=8) :: instam, instap, deltat, resi_glob_rela
    aster_logical :: lpilo, lreli, lctcd, lunil, l_diri_undead
    character(len=19) :: cnfext, depplu
    integer :: ctccvg, ldccvg, pilcvg
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CORRECTION INCR. DEPL.'
    endif
!
! --- INITIALISATIONS CODES RETOURS
!
    ldccvg = -1
    ctccvg = -1
    pilcvg = -1
!
! - Active functionnalites
!
    lpilo         = isfonc(fonact,'PILOTAGE')
    lreli         = isfonc(fonact,'RECH_LINE')
    lunil         = isfonc(fonact,'LIAISON_UNILATER')
    lctcd         = isfonc(fonact,'CONT_DISCRET')
    l_diri_undead = isfonc(fonact,'DIRI_UNDEAD')
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
!
! --- INITIALISATIONS
!
    instam = diinst(sddisc,numins-1)
    instap = diinst(sddisc,numins)
    deltat = instap - instam
    etan = eta
    rho = 1.d0
    ds_conv%line_sear_coef = 1.d0
    offset = 0.d0
    eta = 0.d0
    call GetResi(ds_conv, type = 'RESI_GLOB_RELA' , vale_calc_ = resi_glob_rela)
!
! --- CALCUL DE LA RESULTANTE DES EFFORTS EXTERIEURS
!
    call nmchex(veasse, 'VEASSE', 'CNFEXT', cnfext)
    call nmfext(etan, fonact, sddyna, veasse, cnfext)
!
! --- CONVERSION RESULTAT dU VENANT DE K.dU = F SUIVANT SCHEMAS
!
    call nmsolu(sddyna, solalg)
!
! --- PAS DE RECHERCHE LINEAIRE (EN PARTICULIER SUITE A LA PREDICTION)
!
    if (.not.lreli .or. iterat .eq. 0) then
        if (lpilo) then
            call nmpich(modele, numedd, mate  , carele, comref,&
                        compor, lischa, carcri, fonact, sdstat,&
                        ds_contact, sdpilo, iterat, sdnume,&
                        deltat, valinc, solalg, veelem, veasse,&
                        sdtime, sddisc, eta   , rho   , offset,&
                        ldccvg, pilcvg, matass)
            ds_conv%line_sear_coef = 1.d0
            ds_conv%line_sear_iter = 0
        endif
    else
!
! --- RECHERCHE LINEAIRE
!
        if (lpilo) then
            call nmrepl(modele , numedd, mate       , carele, comref,&
                        compor , lischa, ds_algopara, carcri, fonact,&
                        iterat , sdstat, sdpilo     , sdnume, sddyna,&
                        ds_contact, deltat  , valinc, solalg,&
                        veelem , veasse, sdtime     , sddisc, etan  ,&
                        ds_conv, eta   , offset     , ldccvg, pilcvg,&
                        matass )
        else
            call nmreli(modele , numedd, mate  , carele     , comref,&
                        compor , lischa, carcri, fonact     , iterat,&
                        sdstat , sdnume, sddyna, ds_algopara, ds_contact,&
                        valinc , solalg, veelem, veasse     , sdtime,&
                        ds_conv, ldccvg)
        endif
    endif
!
! --- SI ERREUR PENDANT L'INTEGRATION OU LE PILOTAGE -> ON SORT DIRECT
!
    if ((ldccvg .eq. 1) .or. (pilcvg .eq. 1)) then
        goto 999
    endif
!
! --- AJUSTEMENT DE LA DIRECTION DE DESCENTE (AVEC ETA, RHO ET OFFSET)
!
    rho = ds_conv%line_sear_coef
    call nmpild(numedd, sddyna, solalg, eta, rho,&
                offset)
!
! --- MODIFICATIONS DEPLACEMENTS SI CONTACT DISCRET OU LIAISON_UNILA
!
    if (lunil .or. lctcd) then
        call nmcoun(noma  , fonact, solveu, numedd, matass,&
                    ds_contact%sdcont_defi, ds_contact%sdcont_solv,&
                    ds_contact%sdunil_defi, ds_contact%sdunil_solv, iterat,&
                    valinc, solalg, veasse, instap, resi_glob_rela,&
                    sdtime, sdstat, ctccvg)
        if (ctccvg .eq. 0) then
            call nmsolm(sddyna, solalg)
        else
            goto 999
        endif
    endif
!
! --- ACTUALISATION DES CHAMPS SOLUTIONS
!
    call nmmajc(fonact, sddyna, sdnume, deltat, numedd,&
                valinc, solalg)
!
! - Update dualized relations for non-linear Dirichlet boundary conditions (undead)
!
    if (l_diri_undead) then
        call cldual_maj(lischa, depplu)
    endif
!
999 continue
!
! --- TRANSFORMATION DES CODES RETOURS EN EVENEMENTS
!
    call nmcret(sderro, 'LDC', ldccvg)
    call nmcret(sderro, 'PIL', pilcvg)
    call nmcret(sderro, 'CTC', ctccvg)
!
! --- EVENEMENT ERREUR ACTIVE ?
!
    call nmltev(sderro, 'ERRI', 'NEWT', lerrit)
!
! --- IMPRESSION D'UN CHAMP POUR DEBUG
!
    call dbgcha(valinc, instap, iterat)
!
end subroutine
