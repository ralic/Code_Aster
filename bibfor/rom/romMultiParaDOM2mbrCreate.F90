subroutine romMultiParaDOM2mbrCreate(ds_multipara, i_coef, syst_2mbr_type, syst_2mbr)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/infniv.h"
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
    integer, intent(in) :: i_coef
    character(len=1), intent(in) :: syst_2mbr_type
    character(len=19), intent(in) :: syst_2mbr
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Create second member for multiparametric problems (complete problem)
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_multipara     : datastructure for multiparametric problems
! In  i_coef           : index of coefficient
! In  syst_2mbr_type   : type of second member
! In  syst_2mbr        : name of second member
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer, parameter :: nb_matr_maxi = 8
    integer :: i_equa, nb_matr, nb_equa
    character(len=8) :: vect_name
    character(len=1) :: vect_type
    real(kind=8), pointer :: v_syst_vect_r(:) => null()
    complex(kind=8), pointer :: v_syst_vect_c(:) => null()
    real(kind=8), pointer :: v_vect_r(:) => null()
    complex(kind=8), pointer :: v_vect_c(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_62')
    endif
!
! - Initializations
!
    nb_matr        = ds_multipara%nb_matr
    vect_name      = ds_multipara%vect_name
    vect_type      = ds_multipara%vect_type
    ASSERT(nb_matr .le. nb_matr_maxi)
!
! - Compute second member
!
    call dismoi('NB_EQUA', vect_name, 'CHAM_NO' , repi = nb_equa)
    if (vect_type .eq. 'R') then
        call jeveuo(vect_name(1:8)//'           .VALE', 'L', vr = v_vect_r)
        if (syst_2mbr_type .eq. 'R') then
            call jeveuo(syst_2mbr(1:19)//'.VALE', 'E', vr = v_syst_vect_r)
            ASSERT(ds_multipara%vect_coef%l_real)
            do i_equa = 1, nb_equa
                v_syst_vect_r(i_equa) = ds_multipara%vect_coef%coef_real(i_coef) *&
                                        v_vect_r(i_equa) 
            end do
        elseif (syst_2mbr_type .eq. 'C') then
            call jeveuo(syst_2mbr(1:19)//'.VALE', 'E', vc = v_syst_vect_c)
            do i_equa = 1, nb_equa
                if (ds_multipara%vect_coef%l_real) then
                    v_syst_vect_c(i_equa) = ds_multipara%vect_coef%coef_real(i_coef) *&
                                            v_vect_r(i_equa)
                else
                    v_syst_vect_c(i_equa) = ds_multipara%vect_coef%coef_cplx(i_coef) *&
                                            v_vect_r(i_equa)
                endif
            end do
        else
            ASSERT(.false.)
        endif
    else
        call jeveuo(vect_name(1:8)//'           .VALE', 'L', vc = v_vect_c)
        if (syst_2mbr_type .eq. 'R') then
            ASSERT(.false.)
        elseif (syst_2mbr_type .eq. 'C') then
            call jeveuo(syst_2mbr(1:19)//'.VALE', 'E', vc = v_syst_vect_c)
            do i_equa = 1, nb_equa
                if (ds_multipara%vect_coef%l_real) then
                    v_syst_vect_c(i_equa) = ds_multipara%vect_coef%coef_real(i_coef) *&
                                            v_vect_r(i_equa)
                else
                    v_syst_vect_c(i_equa) = ds_multipara%vect_coef%coef_cplx(i_coef) *&
                                            v_vect_r(i_equa)
                endif
            end do
        else
            ASSERT(.false.)
        endif
    endif
!
end subroutine
