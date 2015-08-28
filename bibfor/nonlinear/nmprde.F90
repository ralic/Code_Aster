subroutine nmprde(modele, numedd, numfix  , mate       , carele,&
                  comref, compor, lischa  , ds_algopara, solveu,&
                  fonact, carcri, ds_print, sdstat     , sdtime,&
                  sddisc, numins, valinc  , solalg     , matass,&
                  maprec, defico, resoco  , sddyna     , meelem,&
                  measse, veelem, veasse  , ldccvg     , faccvg,&
                  rescvg, codere)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmprca.h"
#include "asterfort/nmprdc.h"
#include "asterfort/nmprex.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtzero.h"
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
    integer :: numins, ldccvg, faccvg, rescvg
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=19) :: maprec, matass
    character(len=24) :: sdtime, sdstat
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=19) :: lischa, solveu, sddisc, sddyna
    character(len=24) :: numedd, numfix
    character(len=24) :: modele, mate, carele, comref, compor
    character(len=24) :: carcri
    character(len=24) :: defico, resoco
    character(len=24) :: codere
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: meelem(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION)
!
! PREDICTION PAR DEPLACEMENT DONNE (EXTRAPOL/DEPL_CALCULE)
!
! ----------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  MAPREC : MATRICE DE PRECONDITIONNEMENT (GCPC)
! IN  MATASS : MATRICE ASSEMBLEE
! IN  SOLVEU : SOLVEUR
! In  ds_algopara      : datastructure for algorithm parameters
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IO  ds_print         : datastructure for printing parameters
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  DEFICO : SD DEFINITION CONTACT
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT FACCVG : CODE RETOUR FACTORISATION MATRICE GLOBALE
!                -1 : PAS DE FACTORISATION
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : MATRICE SINGULIERE
!                 2 : ERREUR LORS DE LA FACTORISATION
!                 3 : ON NE SAIT PAS SI SINGULIERE
! OUT RESCVG : CODE RETOUR RESOLUTION SYSTEME LINEAIRE
!                -1 : PAS DE RESOLUTION
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXIMUM D'ITERATIONS ATTEINT
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! OUT CODERE : CHAM_ELEM CODE RETOUR INTEGRATION LDC
!
!
!
!
    character(len=19) :: incest, depest, depmoi
    character(len=19) :: depso1, depso2
    aster_logical :: lproj
    integer :: iret
!
! ----------------------------------------------------------------------
!
    depest = '&&CNPART.CHP1'
    incest = '&&CNPART.CHP2'
    call vtzero(depest)
    call vtzero(incest)
    call nmchex(solalg, 'SOLALG', 'DEPSO1', depso1)
    call nmchex(solalg, 'SOLALG', 'DEPSO2', depso2)
    call vtzero(depso1)
    call vtzero(depso2)
    lproj = .true.
    faccvg = -1
    rescvg = -1
    ldccvg = -1
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
!
! --- VALEUR DU DEPLACEMENT -> DEPEST
! --- VALEUR DE L'INCREMENT DE DEPLACEMENT -> INCEST
!
    if (ds_algopara%matrix_pred .eq. 'EXTRAPOLE') then
        call nmprex(numedd, depmoi, solalg, sddisc, numins,&
                    incest, depest)
    else if (ds_algopara%matrix_pred .eq. 'DEPL_CALCULE') then
        call nmprdc(ds_algopara, numedd, depmoi, sddisc, numins,&
                    incest     , depest)
    else
        ASSERT(.false.)
    endif
!
! --- RECOPIE DE LA SOLUTION
!
    if (numins .eq. 1) then
        call vtcopy(incest, depso1, 'F', iret)
    else
        call copisd('CHAMP_GD', 'V', incest, depso1)
    endif
!
! --- PROJECTION POUR AVOIR UN CHAMP DE DEPLACEMENT
! --- CINEMATIQUEMENT ADMISSIBLE
!
    if (lproj) then
        call nmprca(modele, numedd, numfix  , mate       , carele,&
                    comref, compor, lischa  , ds_algopara, solveu,&
                    fonact, carcri, ds_print, sdstat     , sddisc,&
                    sdtime, numins, valinc  , solalg     , matass,&
                    maprec, defico, resoco  , sddyna     , meelem,&
                    measse, veelem, veasse  , depest     , ldccvg,&
                    faccvg, rescvg, codere)
    endif
!
end subroutine
