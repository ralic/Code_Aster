subroutine nmlect(result, model, mate, cara_elem, list_load, solver_)
!
implicit none
!
#include "asterc/getres.h"
#include "asterfort/cresol.h"
#include "asterfort/medomm.h"
#include "asterfort/nmdoch.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(out) :: result
    character(len=*), intent(out) :: model
    character(len=*), intent(out) :: mate
    character(len=*), intent(out) :: cara_elem
    character(len=*), intent(out) :: list_load
    character(len=*), optional, intent(out) :: solver_
!
! --------------------------------------------------------------------------------------------------
!
! Mechanics - Initializations
!
! Get parameters from command file
!
! --------------------------------------------------------------------------------------------------
!
! Out result           : name of results datastructure
! Out model            : name of model
! Out mate             : name of material characteristics (field)
! Out cara_elem        : name of elementary characteristics (field)
! Out list_load        : name of datastructure for list of loads
! Out solver           : name of datastructure for solver
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: k16dummy
!
! --------------------------------------------------------------------------------------------------
!
    list_load = '&&OP00XX.LIST_LOAD'
!
! - Get results
!
    call getres(result, k16dummy, k16dummy)
!
! - Get parameters from command file
!
    call medomm(model, mate, cara_elem)
!
! - Get loads information and create datastructure
!
    call nmdoch(list_load, l_load_user = .true._1)
!
! - Get parameters for solver
!
    if (present(solver_)) then
        solver_   = '&&OP00XX.SOLVER'
        call cresol(solver_)
    endif
!
end subroutine
