!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine nxlect(l_ther_nonl, list_load  , solver    , ther_para_i, ther_para_r,&
                      ther_crit_i, ther_crit_r, result_dry, matcst     , coecst     ,&
                      result     , model      , mate      , cara_elem  , compor     )
        aster_logical, intent(in) :: l_ther_nonl
        character(len=19), intent(inout) :: list_load
        character(len=19), intent(in) :: solver
        integer, intent(inout) :: ther_para_i(*)
        integer, intent(inout) :: ther_crit_i(*)
        real(kind=8), intent(inout) :: ther_para_r(*)
        real(kind=8), intent(inout) :: ther_crit_r(*)
        character(len=8), intent(out) :: result_dry
        aster_logical, intent(out) :: matcst
        aster_logical, intent(out) :: coecst
        character(len=24), intent(out) :: result
        character(len=24), intent(out) :: model
        character(len=24), intent(out) :: mate
        character(len=24), intent(out) :: cara_elem
        character(len=24), intent(out) :: compor
    end subroutine nxlect
end interface
