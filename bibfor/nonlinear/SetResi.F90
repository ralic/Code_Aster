subroutine SetResi(ds_conv   , type_ ,&
                   col_name_ , col_name_locus_, vale_calc_  , locus_calc_, user_para_,&
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
    type(NL_DS_Conv), intent(inout) :: ds_conv
    character(len=*), optional, intent(in) :: type_
    character(len=16), optional, intent(in) :: col_name_
    character(len=16), optional, intent(in) :: col_name_locus_
    real(kind=8), optional, intent(in) :: vale_calc_
    character(len=*), optional, intent(in) :: locus_calc_
    real(kind=8), optional, intent(in) :: user_para_
    aster_logical, optional, intent(in) :: l_conv_
    character(len=16), optional, intent(in)  :: event_type_
    aster_logical, optional, intent(in) :: l_resi_test_
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Convergence management
!
! Set values for residual (by type)
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_conv          : datastructure for convergence management
! In  type             : type of residual
!                        If .not. present => all residuals
! In  col_name         : name of column in convergence table
! In  col_name_locus   : name of column in convergence table for locus
! In  vale_calc        : result of maximum norm of residual
! In  locus_calc       : locus where is maximum norm of residual
! In  user_para        : user parameter for residual
! In  l_conv           : .true. if residual has converged
! In  event_type       : type of event
! In  l_resi_test      : .true. to test this residual to evaluate convergence
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
! - On all residuals
!
    if (.not.present(type_)) then
        do i_resi = 1, nb_resi
            if (present(vale_calc_)) then
                ds_conv%list_resi(i_resi)%vale_calc      = vale_calc_
            endif
            if (present(locus_calc_)) then
                ds_conv%list_resi(i_resi)%locus_calc     = locus_calc_
            endif
            if (present(user_para_)) then
                ds_conv%list_resi(i_resi)%user_para      = user_para_
            endif
            if (present(l_conv_)) then
                ds_conv%list_resi(i_resi)%l_conv         = l_conv_
            endif
            if (present(col_name_)) then
                ds_conv%list_resi(i_resi)%col_name       = col_name_
            endif
            if (present(col_name_locus_)) then
                ds_conv%list_resi(i_resi)%col_name_locus = col_name_locus_
            endif
            if (present(event_type_)) then
                ds_conv%list_resi(i_resi)%event_type     = event_type_
            endif
            if (present(l_resi_test_)) then
                ds_conv%l_resi_test(i_resi)              = l_resi_test_
            endif
        end do
    endif
!
! - On one residual
!
    if (present(type_)) then
        do i_resi = 1, nb_resi
            if (ds_conv%list_resi(i_resi)%type .eq. type_) then
                ASSERT(i_type.eq.0)
                i_type = i_resi
            endif
        end do
        ASSERT(i_type.ne.0)
        if (present(vale_calc_)) then
            ds_conv%list_resi(i_type)%vale_calc  = vale_calc_
        endif
        if (present(locus_calc_)) then
            ds_conv%list_resi(i_type)%locus_calc = locus_calc_
        endif
        if (present(user_para_)) then
            ds_conv%list_resi(i_type)%user_para  = user_para_
        endif
        if (present(l_conv_)) then
            ds_conv%list_resi(i_type)%l_conv     = l_conv_
        endif
        if (present(col_name_)) then
            ds_conv%list_resi(i_type)%col_name   = col_name_
        endif
        if (present(event_type_)) then
            ds_conv%list_resi(i_type)%event_type = event_type_
        endif
        if (present(l_resi_test_)) then
            ds_conv%l_resi_test(i_type) = l_resi_test_
        endif
    endif
!
end subroutine

