subroutine romMultiCoefInfo(ds_multicoef)
!
use Rom_Datastructure_type
!
implicit none
!
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
    type(ROM_DS_MultiCoef), intent(in) :: ds_multicoef
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Informations about multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_multicoef     : datastructure for multiparametric problems - Coefficients
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: valr(2)
!
! --------------------------------------------------------------------------------------------------
!
    if (ds_multicoef%l_cplx) then
        if (ds_multicoef%l_cste) then
            valr(1) = real(ds_multicoef%coef_cste_cplx)
            valr(2) = dimag(ds_multicoef%coef_cste_cplx)
            call utmess('I', 'ROM3_41', nr = 2, valr = valr)
        elseif (ds_multicoef%l_func) then
            call utmess('I', 'ROM3_43', sk = ds_multicoef%func_name)
        else
            ASSERT(.false.)
        endif
    elseif (ds_multicoef%l_real) then
        if (ds_multicoef%l_cste) then
            call utmess('I', 'ROM3_42', sr = ds_multicoef%coef_cste_real)
        elseif (ds_multicoef%l_func) then
            call utmess('I', 'ROM3_44', sk = ds_multicoef%func_name)
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
