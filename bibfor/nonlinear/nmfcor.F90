subroutine nmfcor(modele, numedd  , mate  , carele     , comref,&
                  compor, lischa  , fonact, ds_algopara, carcri,&
                  numins, iterat  , sdstat, sdtime     , sddisc,&
                  sddyna, sdnume  , sderro, ds_contact,&
                  ds_inout, valinc, solalg     , veelem,&
                  veasse, meelem  , measse, matass     , lerrit)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmadir.h"
#include "asterfort/nmaint.h"
#include "asterfort/nmbudi.h"
#include "asterfort/nmchar.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmchfi.h"
#include "asterfort/nmcret.h"
#include "asterfort/nmctcd.h"
#include "asterfort/nmdiri.h"
#include "asterfort/nmfint.h"
#include "asterfort/nmfocc.h"
#include "asterfort/nmltev.h"
#include "asterfort/nmrigi.h"
#include "asterfort/nmtime.h"
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
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=24) :: sdstat, sdtime
    character(len=19) :: sddisc, sddyna, sdnume
    character(len=19) :: lischa, matass
    character(len=24) :: modele, numedd, mate, carele, comref, compor
    character(len=24) :: carcri, sderro
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=19) :: meelem(*), veelem(*), measse(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    type(NL_DS_Contact), intent(in) :: ds_contact
    aster_logical :: lerrit
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MISE A JOUR DES EFFORTS APRES CALCUL DE LA CORRECTION DES CHAMPS
! DEPLACEMENTS/VITESSES ACCELERATIONS
!
! ----------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENTC IN  LISCHA : LISTE DES CHARGES
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SDTIME : SD TIMER
! In  ds_algopara      : datastructure for algorithm parameters
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  NUMINS : NUMERO D'INSTANT
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : GESTION DES ERREURS
! In  ds_contact       : datastructure for contact management
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  SDNUME : SD NUMEROTATION
! OUT LERRIT : .TRUE. SI ERREUR PENDANT CORRECTION
!
! ----------------------------------------------------------------------
!
    aster_logical :: lcfint, lcrigi, lcdiri, lcbudi
    character(len=24) :: codere
    character(len=19) :: vefint, vediri, vebudi, cnfint, cndiri, cnbudi
    character(len=19) :: depplu, vitplu, accplu
    character(len=16) :: option
    aster_logical :: lctcd, lunil, leltc
    integer :: ldccvg
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CORRECTION DES FORCES'
    endif
!
! --- INITIALISATIONS CODES RETOURS
!
    ldccvg = -1
    codere = '&&NMFCOR.CODERE'
!
! --- FONCTIONNALITES ACTIVEES
!
    lunil = isfonc(fonact,'LIAISON_UNILATER')
    lctcd = isfonc(fonact,'CONT_DISCRET')
    leltc = isfonc(fonact,'ELT_CONTACT')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call nmchex(veelem, 'VEELEM', 'CNDIRI', vediri)
    call nmchex(veelem, 'VEELEM', 'CNBUDI', vebudi)
    call nmchex(veelem, 'VEELEM', 'CNFINT', vefint)
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veasse, 'VEASSE', 'CNBUDI', cnbudi)
!
! --- CALCUL DES CHARGEMENTS VARIABLES AU COURS DU PAS DE TEMPS
!
    call nmchar('VARI'  , 'CORRECTION', modele, numedd, mate,&
                carele  , compor, lischa, numins, sdtime,&
                sddisc  , fonact, comref,&
                ds_inout, valinc, solalg, veelem, measse,&
                veasse  , sddyna)
!
! --- CALCUL DU SECOND MEMBRE POUR CONTACT/XFEM
!
    if (leltc) then
        call nmfocc('CONVERGENC', modele, mate, numedd, fonact,&
                    ds_contact, sdstat, sdtime, solalg,&
                    valinc, veelem, veasse)
    endif
!
! --- OPTION POUR MERIMO
!
    call nmchfi(ds_algopara, fonact, sddisc, sddyna, numins,&
                iterat     , ds_contact, lcfint, lcdiri, lcbudi,&
                lcrigi     , option)
!
! --- CALCUL DES FORCES INTERNES ET DE LA RIGIDITE SI NECESSAIRE
!
    if (lcfint) then
        if (lcrigi) then
            call nmrigi(modele, mate, carele, compor, carcri,&
                        sddyna, sdstat, sdtime, fonact, iterat,&
                        valinc, solalg, comref, meelem, veelem,&
                        option, ldccvg, codere)
        else
            call nmfint(modele, mate, carele, comref, compor,&
                        carcri, fonact, iterat, sddyna, sdstat,&
                        sdtime, valinc, solalg, ldccvg, codere,&
                        vefint)
        endif
    endif
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if (ldccvg .eq. 1) goto 999
!
! - Compute vectors for DISCRETE contact
!
    if (lctcd .or. lunil) then
        call nmctcd(fonact, ds_contact, numedd, veasse)
    endif
!
! --- ASSEMBLAGE DES FORCES INTERIEURES
!
    call nmtime(sdtime, 'INI', 'SECO_MEMB')
    call nmtime(sdtime, 'RUN', 'SECO_MEMB')
    if (lcfint) then
        call nmaint(numedd, fonact, ds_contact, veasse, vefint,&
                    cnfint, sdnume)
    endif
!
! --- CALCUL ET ASSEMBLAGE DES REACTIONS D'APPUI BT.LAMBDA
!
    if (lcdiri) then
        call nmdiri(modele, mate, carele, lischa, sddyna,&
                    depplu, vitplu, accplu, vediri)
        call nmadir(numedd, fonact, ds_contact, veasse, vediri,&
                    cndiri)
    endif
!
! --- CALCUL ET ASSEMBLAGE DE B.U
!
    call nmbudi(modele, numedd, lischa, depplu, vebudi,&
                cnbudi, matass)
!
    call nmtime(sdtime, 'END', 'SECO_MEMB')
!
999 continue
!
! --- TRANSFORMATION DES CODES RETOURS EN EVENEMENTS
!
    call nmcret(sderro, 'LDC', ldccvg)
!
! --- EVENEMENT ERREUR ACTIVE ?
!
    call nmltev(sderro, 'ERRI', 'NEWT', lerrit)
!
end subroutine
