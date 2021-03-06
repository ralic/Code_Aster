subroutine ndxpre(modele  , numedd         , numfix    , mate       , carele,&
                  comref  , ds_constitutive, lischa    , ds_algopara, solveu,&
                  fonact  , sddisc         , ds_measure, numins     , valinc,&
                  solalg  , matass         , maprec    , sddyna     , sderro,&
                  ds_inout, meelem         , measse    , veelem     , veasse,&
                  lerrit)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/ndxprm.h"
#include "asterfort/nmassx.h"
#include "asterfort/nmchar.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcret.h"
#include "asterfort/nmltev.h"
#include "asterfort/nmresd.h"
#include "asterfort/vtzero.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: numins
    type(NL_DS_InOut), intent(in) :: ds_inout
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=19) :: matass, maprec
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19) :: lischa, solveu, sddisc, sddyna
    character(len=24) :: modele, mate, carele, comref
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=24) :: numedd, numfix
    character(len=24) :: sderro
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: measse(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    aster_logical :: lerrit
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! PHASE DE PREDICTION - CAS EXPLICITE
!
! --------------------------------------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARIABLES DE COMMANDE DE REFERENCE
! In  ds_constitutive  : datastructure for constitutive laws management
! In  ds_inout         : datastructure for input/output management
! In  ds_algopara      : datastructure for algorithm parameters
! IN  SOLVEU : SOLVEUR
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  LISCHA : SD LISTE DES CHARGES
! IO  ds_measure       : datastructure for measure and statistics management
! IN  SDDISC : SD DISCRETISATION
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDDYNA : SD DYNAMIQUE
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  MAPREC : NOM DE LA MATRICE DE PRECONDITIONNEMENT (GCPC)
! OUT MATASS : MATRICE DE RESOLUTION ASSEMBLEE
! OUT MAPREC : MATRICE DE RESOLUTION ASSEMBLEE - PRECONDITIONNEMENT
! OUT LERRIT  : .TRUE. SI ERREUR PENDANT PREDICTION
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: instap
    character(len=19) :: cncine, cndonn, cnzero
    integer :: ldccvg, faccvg, rescvg
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CALCUL DE PREDICTION'
    endif
!
! --- INITIALISATIONS
!
    instap = diinst(sddisc,numins)
    cndonn = '&&CNCHAR.DONN'
    cnzero = '&&CNPART.ZERO'
    call vtzero(cndonn)
!
! --- INITIALISATION CODES RETOURS
!
    faccvg = -1
    rescvg = -1
    ldccvg = -1
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(veasse, 'VEASSE', 'CNCINE', cncine)
!
! --- CALCUL DE LA MATRICE GLOBALE
!
    call ndxprm(modele, mate  , carele    , ds_constitutive, ds_algopara,&
                lischa, numedd, numfix    , solveu         , comref     ,&
                sddisc, sddyna, ds_measure, numins         , fonact     ,&
                valinc, solalg, veelem    , meelem         , measse     ,&
                maprec, matass, faccvg    , ldccvg)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if ((faccvg.eq.1) .or. (faccvg.eq.2)) goto 999
    if (ldccvg .eq. 1) goto 999
!
! --- CALCUL DES CHARGEMENTS VARIABLES AU COURS DU PAS DE TEMPS
!
    call nmchar('VARI', 'PREDICTION'   , modele, numedd  , mate      ,&
                carele, ds_constitutive, lischa, numins  , ds_measure,&
                sddisc, fonact         , comref, ds_inout, valinc    ,&
                solalg, veelem         , measse, veasse  , sddyna)
!
! --- CALCUL DU SECOND MEMBRE
!
    call nmassx(modele         , numedd, mate  , carele    , comref,&
                ds_constitutive, lischa, fonact, ds_measure, sddyna,&
                valinc         , solalg, veelem, veasse    , ldccvg,&
                cndonn)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if (ldccvg .eq. 1) goto 999
!
! --- RESOLUTION
!
    call nmresd(fonact, sddyna, ds_measure, solveu, numedd,&
                instap, maprec, matass    , cndonn, cnzero,&
                cncine, solalg, rescvg)
!
999 continue
!
! --- TRANSFORMATION DES CODES RETOURS EN EVENEMENTS
!
    call nmcret(sderro, 'LDC', ldccvg)
    call nmcret(sderro, 'FAC', faccvg)
    call nmcret(sderro, 'RES', rescvg)
!
! --- EVENEMENT ERREUR ACTIVE ?
!
    call nmltev(sderro, 'ERRI', 'NEWT', lerrit)
!
end subroutine
