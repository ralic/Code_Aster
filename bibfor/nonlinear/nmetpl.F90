subroutine nmetpl(ds_inout, sd_suiv, sd_obsv)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
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
    type(NL_DS_InOut), intent(inout) :: ds_inout
    character(len=24), intent(in) :: sd_suiv
    character(len=19), intent(in) :: sd_obsv
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Update name of field in algorithm
!
! This utiliy is required for "hat" variables
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_inout         : datastructure for input/output management
! In  sd_suiv          : datastructure for dof monitor parameters
! In  sd_obsv          : datastructure for observation parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_field, i_field
    integer :: i_keyw_fact, nb_keyw_fact
    character(len=24) :: algo_name_old, algo_name_new
    character(len=6) :: hat_type, hat_vari
    character(len=24) :: field_type
    character(len=14) :: sdextr_suiv, sdextr_obsv
    character(len=24) :: extr_info
    integer, pointer :: v_extr_info(:) => null()
    character(len=24) :: extr_field
    character(len=24), pointer :: v_extr_field(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_field = ds_inout%nb_field
!
! - For storing
!
    do i_field = 1, nb_field
        algo_name_old = ds_inout%field(i_field)%algo_name
        if (algo_name_old(1:3) .eq. '#H#') then
            hat_type = algo_name_old(4:9)
            hat_vari = algo_name_old(11:16)
            if (hat_type .eq. 'VALINC') then
                if (hat_vari .eq. 'TEMP') then
                    ASSERT(.false.)
                endif
                algo_name_new = algo_name_old
                algo_name_new(14:16) = 'PLU'
                ds_inout%field(i_field)%algo_name = algo_name_new
            endif
        endif
    end do
!
! - For DOF monitoring
!
    sdextr_suiv   = sd_suiv(1:14)
    extr_info     = sdextr_suiv(1:14)//'     .INFO'
    call jeveuo(extr_info, 'L', vi = v_extr_info)
    nb_keyw_fact  = v_extr_info(1)
    nb_field      = v_extr_info(6)
    extr_field    = sdextr_suiv(1:14)//'     .CHAM'
    if (nb_keyw_fact .ne. 0) then
        call jeveuo(extr_field, 'E', vk24 = v_extr_field)
    endif
    do i_keyw_fact = 1, nb_keyw_fact
        i_field     = v_extr_info(7+7*(i_keyw_fact-1)+7)
        field_type  = v_extr_field(4*(i_field-1)+1)
        if (field_type .ne. 'NONE') then
            algo_name_old = v_extr_field(4*(i_field-1)+4)(1:19)
            if (algo_name_old(13:15) .eq. 'MOI') then
                algo_name_new = algo_name_old
                algo_name_new(13:15) = 'PLU'
                v_extr_field(4*(i_field-1)+4) = algo_name_new
            endif
        endif
    end do
!
! - For observation
!
    sdextr_obsv   = sd_obsv(1:14)
    extr_info     = sdextr_obsv(1:14)//'     .INFO'
    call jeveuo(extr_info, 'L', vi = v_extr_info)
    nb_keyw_fact  = v_extr_info(1)
    nb_field      = v_extr_info(6)
    extr_field    = sdextr_obsv(1:14)//'     .CHAM'
    if (nb_keyw_fact .ne. 0) then
        call jeveuo(extr_field, 'E', vk24 = v_extr_field)
    endif
    do i_keyw_fact = 1, nb_keyw_fact
        i_field     = v_extr_info(7+7*(i_keyw_fact-1)+7)
        field_type  = v_extr_field(4*(i_field-1)+1)
        if (field_type .ne. 'NONE') then
            algo_name_old = v_extr_field(4*(i_field-1)+4)(1:19)
            if (algo_name_old(13:15) .eq. 'MOI') then
                algo_name_new = algo_name_old
                algo_name_new(13:15) = 'PLU'
                v_extr_field(4*(i_field-1)+4) = algo_name_new
            endif
        endif
    end do
!
end subroutine
