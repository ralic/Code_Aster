subroutine GetResiRefe(ds_conv   , type_ ,&
                       user_para_, cmp_name_, l_refe_test_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
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
    type(NL_DS_Conv), intent(in) :: ds_conv
    character(len=*), optional, intent(in) :: type_
    character(len=*), optional, intent(out) :: cmp_name_
    real(kind=8), optional, intent(out) :: user_para_
    aster_logical, optional, intent(out) :: l_refe_test_
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Convergence management
!
! Get values for reference residual (by name)
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_conv          : datastructure for convergence management
! In  type             : type of residual
!                        If .not. present => all residuals
! Out user_para        : user parameter for residual
! Out cmp_name         : name of component
! Out l_refe_test      : .true. to test this residual to evaluate convergence
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_refe, nb_refe, i_type
!
! --------------------------------------------------------------------------------------------------
!
    i_type  = 0
    nb_refe = ds_conv%nb_refe
!
! - Find residual
!
    do i_refe = 1, nb_refe
        if (ds_conv%list_refe(i_refe)%type .eq. type_) then
            ASSERT(i_type.eq.0)
            i_type = i_refe
        endif
    end do
    ASSERT(i_type.ne.0)
!
! - Get parameters
!
    if (present(user_para_)) then
        user_para_   = ds_conv%list_refe(i_type)%user_para
    endif
    if (present(cmp_name_)) then
        cmp_name_    = ds_conv%list_refe(i_type)%cmp_name
    endif
    if (present(l_refe_test_)) then
        l_refe_test_ = ds_conv%l_refe_test(i_type)
    endif
!
end subroutine

