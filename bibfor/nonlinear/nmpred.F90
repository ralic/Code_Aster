subroutine nmpred(modele  , numedd, numfix  , mate       , carele,&
                  comref  , compor, lischa  , ds_algopara, solveu,&
                  fonact  , carcri, ds_print, sdstat     , sdtime,&
                  sddisc  , sdnume, sderro  , numins     , valinc,&
                  solalg  , matass, maprec  , ds_contact , sddyna,&
                  ds_inout, meelem, measse  , veelem     , veasse,&
                  lerrit)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/nmcret.h"
#include "asterfort/nmltev.h"
#include "asterfort/nmprde.h"
#include "asterfort/nmprta.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! aslint: disable=W1504
!
    integer :: fonact(*)
    integer :: numins
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=19) :: matass, maprec
    character(len=24) :: sdtime, sdstat
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=19) :: lischa, solveu, sddisc, sddyna, sdnume
    character(len=24) :: modele, mate, carele, comref, compor
    character(len=24) :: numedd, numfix
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24) :: carcri, sderro
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: measse(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    aster_logical :: lerrit
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! PHASE DE PREDICTION
!
! ----------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARIABLES DE COMMANDE DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  SOLVEU : SOLVEUR
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! In  ds_algopara      : datastructure for algorithm parameters
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IO  ds_print         : datastructure for printing parameters
! In  ds_inout         : datastructure for input/output management
! In  ds_contact       : datastructure for contact management
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : GESTION DES ERREURS
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IO  ds_print         : datastructure for printing parameters
! IN  SDDYNA : SD DYNAMIQUE
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  MAPREC : NOM DE LA MATRICE DE PRECONDITIONNEMENT (GCPC)
! IN  SDNUME : SD NUMEROTATION
! OUT LERRIT  : .TRUE. SI ERREUR PENDANT PREDICTION
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: faccvg, rescvg, ldccvg
    character(len=24) :: codere
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CALCUL DE PREDICTION'
    endif
!
! --- INITIALISATION CODES RETOURS
!
    codere = '&&NMPRED.CODERE'
    faccvg = -1
    rescvg = -1
    ldccvg = -1
!
! --- PREDICTION PAR LINEARISATION DU SYSTEME
!
    if ((ds_algopara%matrix_pred .eq. 'ELASTIQUE').or.&
        (ds_algopara%matrix_pred .eq. 'TANGENTE')) then
        call nmprta(modele  , numedd, numfix  , mate       , carele,&
                    comref  , compor, lischa  , ds_algopara, solveu,&
                    fonact  , carcri, ds_print, sdstat     , sdtime,&
                    sddisc  , numins, valinc  , solalg     , matass,&
                    maprec  , ds_contact, sddyna,&
                    meelem  , measse, veelem  , veasse     , sdnume,&
                    ds_inout, ldccvg, faccvg  , rescvg     , codere)
!
! --- PREDICTION PAR EXTRAPOLATION DU PAS PRECEDENT OU PAR DEPLACEMENT
! --- CALCULE
!
    elseif ((ds_algopara%matrix_pred .eq. 'EXTRAPOLE').or.&
            (ds_algopara%matrix_pred .eq.'DEPL_CALCULE')) then
        call nmprde(modele, numedd, numfix  , mate       , carele,&
                    comref, compor, lischa  , ds_algopara, solveu,&
                    fonact, carcri, ds_print, sdstat     , sdtime,&
                    sddisc, numins, valinc  , solalg     , matass,&
                    maprec, ds_contact, sddyna     , meelem,&
                    measse, veelem, veasse  , ldccvg     , faccvg,&
                    rescvg, codere)
    else
        ASSERT(.false.)
    endif
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
