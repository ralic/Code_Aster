subroutine romMultiParaCreateSyst(ds_multipara, syst_matr, syst_2mbr_type, syst_2mbr)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/mtcmbl.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_MultiPara), intent(in) :: ds_multipara
    character(len=19), intent(in) :: syst_matr
    character(len=1), intent(in) :: syst_2mbr_type
    character(len=19), intent(in) :: syst_2mbr
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Create system for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_multipara     : datastructure for multiparametric problems
! In  syst_matr        : name of system's matrix
! In  syst_2mbr_type   : type of system's second member (R or C)
! In  syst_2mbr        : name of system's second member
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!    integer, parameter :: nb_matr_maxi = 8
!    character(len=1) :: type_comb(nb_matr_maxi)
!    real(kind=8) :: coef_comb(2*nb_matr_maxi)
!    character(len=24) :: matr_comb(nb_matr_maxi)
!    integer :: i_coef, i_matr, i_equa, nb_matr, nb_equa
!    aster_logical :: l_coefm_cplx
!    character(len=8) :: vect_name
!    character(len=1) :: vect_type
!    real(kind=8), pointer :: v_syst_vect_r(:) => null()
!    complex(kind=8), pointer :: v_syst_vect_c(:) => null()
!    real(kind=8), pointer :: v_vect_r(:) => null()
!    complex(kind=8), pointer :: v_vect_c(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_30')
    endif
!!
!! - Get parameters
!!
!    nb_matr        = ds_multipara%nb_matr
!    vect_name      = ds_multipara%vect_name
!    vect_type      = ds_multipara%vect_type
!    ASSERT(nb_matr .le. nb_matr_maxi)
!!
!! - Compute matrix
!!
!    i_coef = 0
!    do i_matr = 1, nb_matr
!        matr_comb(i_matr) = ds_multipara%matr_name(i_matr)
!        l_coefm_cplx      = ds_multipara%l_coefm_cplx(i_matr)
!        if (l_coefm_cplx) then
!            type_comb(i_matr) = 'C'
!            i_coef = i_coef +1
!            coef_comb(i_coef) = real(ds_multipara%coefm_cplx(i_matr))
!            i_coef = i_coef +1
!            coef_comb(i_coef) = aimag(ds_multipara%coefm_cplx(i_matr))
!        else
!            type_comb(i_matr) = 'R'
!            i_coef = i_coef +1
!            coef_comb(i_coef) = ds_multipara%coefm_real(i_matr)
!        endif
!    end do
!    call mtcmbl(nb_matr, type_comb, coef_comb, matr_comb, syst_matr,&
!                ' ', ' ', 'ELIM=')
!!
!! - Compute second member
!!
!    call dismoi('NB_EQUA', vect_name, 'CHAM_NO' , repi = nb_equa)
!    if (vect_type .eq. 'R') then
!        call jeveuo(vect_name(1:8)//'           .VALE', 'L', vr = v_vect_r)
!        if (syst_2mbr_type .eq. 'R') then
!            call jeveuo(syst_2mbr(1:19)//'.VALE', 'E', vr = v_syst_vect_r)
!            ASSERT(ds_multipara%l_coefv_real)
!            do i_equa = 1, nb_equa
!                v_syst_vect_r(i_equa) = ds_multipara%coefv_real * v_vect_r(i_equa) 
!            end do
!        else
!            call jeveuo(syst_2mbr(1:19)//'.VALE', 'E', vc = v_syst_vect_c)
!            do i_equa = 1, nb_equa
!                if (ds_multipara%l_coefv_real) then
!                    v_syst_vect_c(i_equa) = ds_multipara%coefv_real * v_vect_r(i_equa)
!                else
!                    v_syst_vect_c(i_equa) = ds_multipara%coefv_cplx * v_vect_r(i_equa)
!                endif
!            end do
!        endif
!    else
!        call jeveuo(vect_name(1:8)//'           .VALE', 'L', vc = v_vect_c)
!        if (syst_2mbr_type .eq. 'R') then
!            ASSERT(.false.)
!        else
!            call jeveuo(syst_2mbr(1:19)//'.VALE', 'E', vc = v_syst_vect_c)
!            do i_equa = 1, nb_equa
!                if (ds_multipara%l_coefv_real) then
!                    v_syst_vect_c(i_equa) = ds_multipara%coefv_real * v_vect_r(i_equa)
!                else
!                    v_syst_vect_c(i_equa) = ds_multipara%coefv_cplx * v_vect_r(i_equa)
!                endif
!            end do
!        endif
!    endif
!
end subroutine
