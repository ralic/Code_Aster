subroutine romCoefInfo(object_type, object_name_, i_coef, ds_multicoef)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
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
    character(len=1), intent(in)       :: object_type
    character(len=8), intent(in)       :: object_name_
    integer, intent(in)                :: i_coef
    type(ROM_DS_MultiCoef), intent(in) :: ds_multicoef
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Initializations
!
! Information about coefficients
!
! --------------------------------------------------------------------------------------------------
!
! In  object_type      : type of object (VECT or MATR)
! In  object_name      : name of object
! In  i_coef           : index of coefficient
! In  ds_multicoef     : datastructure for multiparametric problems - Coefficients
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_real, l_cplx
    real(kind=8) :: cplx_real, cplx_imag, real_real, valr(2)
    character(len=8) :: object_name
!
! --------------------------------------------------------------------------------------------------
!
    l_cplx    = ds_multicoef%l_cplx
    l_real    = ds_multicoef%l_real

    if (object_name_ .eq. ' ') then
        object_name = '<NoName>'
    else
        object_name = object_name_
    endif

    if (l_cplx) then
        cplx_real = real(ds_multicoef%coef_cplx(i_coef))
        cplx_imag = dimag(ds_multicoef%coef_cplx(i_coef))
        valr(1) = cplx_real
        valr(2) = cplx_imag
    else
        real_real = ds_multicoef%coef_real(i_coef)
    endif
!
! - Print
!
    if (object_type .eq. 'V') then
        if (l_real) then
            call utmess('I', 'ROM5_45', sk = object_name, sr = real_real)
        else
            call utmess('I', 'ROM5_46', sk = object_name, nr = 2, valr = valr)
        endif
    elseif (object_type .eq. 'M') then
        if (l_real) then
            call utmess('I', 'ROM5_47', sk = object_name, sr = real_real)
        else
            call utmess('I', 'ROM5_48', sk = object_name, nr = 2, valr = valr)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
