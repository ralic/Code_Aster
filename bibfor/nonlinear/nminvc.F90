subroutine nminvc(modelz, mate  , carele, compor, sdtime  ,&
                  sddisc, sddyna, valinc, solalg, lischa  ,&
                  comref, numedd, ds_inout,&
                  veelem, veasse, measse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infdbg.h"
#include "asterfort/nmcvec.h"
#include "asterfort/nmxvec.h"
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
!
    character(len=*) :: modelz
    character(len=24) :: mate, carele
    character(len=24) :: compor
    character(len=19) :: sddisc, sddyna, lischa
    character(len=24) :: comref, numedd, sdtime
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=19) :: veelem(*), veasse(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL ET ASSEMBLAGE DES VECT_ELEM CONSTANTS AU COURS DU CALCUL
!
! ----------------------------------------------------------------------
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  MODELE : NOM DU MODELE
! IN  SOLVEU : SOLVEUR
! IN  NUMEDD : NUME_DDL
! IN  LISCHA : LISTE DES CHARGEMENTS
! In  ds_inout         : datastructure for input/output management
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  SDDISC : SD DISCRETISATION
! IN  SDTIME : SD TIMER
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT MEELEM : MATRICES ELEMENTAIRES
! OUT MEASSE : MATRICES ASSEMBLEES
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: numins
    integer :: nbvect
    character(len=6) :: ltypve(20)
    character(len=16) :: loptve(20)
    aster_logical :: lcalve(20), lassve(20)
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> PRECALCUL DES VECT_ELEM CONSTANTES'
    endif
!
! --- INITIALISATIONS
!
    numins = 1
    call nmcvec('INIT', ' ', ' ', .false._1, .false._1,&
                nbvect, ltypve, loptve, lcalve, lassve)
!
! --- CREATION DU VECT_ELEM POUR FORCE DE REFERENCE LIEE
! --- AUX VAR. COMMANDES EN T-
!
    call nmcvec('AJOU', 'CNVCF1', ' ', .true._1, .true._1,&
                nbvect, ltypve, loptve, lcalve, lassve)
!
! --- CALCUL DES VECT_ELEM DE LA LISTE
!
    if (nbvect .gt. 0) then
        call nmxvec(modelz, mate  , carele, compor, sdtime,&
                    sddisc, sddyna, numins, valinc, solalg,&
                    lischa, comref, numedd,&
                    ds_inout, veelem, veasse, measse, nbvect,&
                    ltypve  , lcalve, loptve, lassve)
    endif
!
end subroutine
