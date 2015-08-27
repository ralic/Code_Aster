subroutine GetResi(ds_conv   , type  ,&
                   row_name_ , row_name_locus_, vale_calc_  , locus_calc_, user_para_,&
                   l_conv_   , event_type_    , l_resi_test_)
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
    character(len=*), intent(in) :: type
    character(len=16), optional, intent(out) :: row_name_
    character(len=16), optional, intent(out) :: row_name_locus_
    real(kind=8), optional, intent(out) :: vale_calc_
    character(len=16), optional, intent(out) :: locus_calc_
    real(kind=8), optional, intent(out) :: user_para_
    aster_logical, optional, intent(out) :: l_conv_
    character(len=16), optional, intent(out)  :: event_type_
    aster_logical, optional, intent(out) :: l_resi_test_
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Convergence management
!
! Get values for residual (by type)
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_conv          : datastructure for convergence management
! In  type             : type of residual
! Out row_name         : name of row in convergence table for value
! Out row_name_locus   : name of row in convergence table for locus
! Out vale_calc        : result of maximum norm of residual
! Out locus_calc       : locus where is maximum norm of residual
! Out user_para        : user parameter for residual
! Out l_conv           : .true. if residual has converged
! Out event_type       : type of event
! Out l_resi_test      : .true. to test this resiudal to evaluate convergence
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_resi, nb_resi, i_type
!
! --------------------------------------------------------------------------------------------------
!
    i_type  = 0
    nb_resi = ds_conv%nb_resi
!
! - Find residual
!
    do i_resi = 1, nb_resi
        if (ds_conv%list_resi(i_resi)%type .eq. type) then
            ASSERT(i_type.eq.0)
            i_type = i_resi
        endif
    end do
    ASSERT(i_type.ne.0)
!
! - Get parameters
!
    if (present(vale_calc_)) then
        vale_calc_      = ds_conv%list_resi(i_type)%vale_calc
    endif
    if (present(locus_calc_)) then
        locus_calc_     = ds_conv%list_resi(i_type)%locus_calc
    endif
    if (present(user_para_)) then
        user_para_      = ds_conv%list_resi(i_type)%user_para
    endif
    if (present(l_conv_)) then
        l_conv_         = ds_conv%list_resi(i_type)%l_conv
    endif
    if (present(row_name_)) then
        row_name_       = ds_conv%list_resi(i_type)%row_name
    endif
    if (present(row_name_locus_)) then
        row_name_locus_ = ds_conv%list_resi(i_type)%row_name_locus
    endif
    if (present(event_type_)) then
        event_type_     = ds_conv%list_resi(i_type)%event_type
    endif
    if (present(l_resi_test_)) then
        l_resi_test_    = ds_conv%l_resi_test(i_type)
    endif
 
!
end subroutine
