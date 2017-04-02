subroutine romMultiCoefInit(nb_vari_coef, ds_multicoef)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/as_allocate.h"
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
    integer, intent(in) :: nb_vari_coef
    type(ROM_DS_MultiCoef), intent(inout) :: ds_multicoef
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Initializations
!
! Allocate list of coefficients and set them if constant
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_vari_coef     : nombre de fois où les coefficients vont varier
! IO  ds_multicoef     : datastructure for multiparametric problems - Coefficients
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cplx, l_real, l_cste
    integer :: i_para_vale
!
! --------------------------------------------------------------------------------------------------
!
    l_real = ds_multicoef%l_real
    l_cplx = ds_multicoef%l_cplx
    l_cste = ds_multicoef%l_cste
!
! - Allocate list of coefficients (include initial value)
!
    if (l_real) then
        AS_ALLOCATE(vr = ds_multicoef%coef_real, size = nb_vari_coef+1)
    elseif (l_cplx) then
        AS_ALLOCATE(vc = ds_multicoef%coef_cplx, size = nb_vari_coef+1)
    else
        ASSERT(.false.)
    endif
!
! - Set list of coefficients if it's constant
!
    if (l_cste) then
        do i_para_vale = 1, nb_vari_coef+1
            if (l_real) then
                ds_multicoef%coef_real(i_para_vale) = ds_multicoef%coef_cste_real
            elseif (l_cplx) then
                ds_multicoef%coef_cplx(i_para_vale) = ds_multicoef%coef_cste_cplx
            else
                ASSERT(.false.)
            endif
        end do
    endif
!
end subroutine
