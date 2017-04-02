subroutine romMultiCoefDSInit(object_type, ds_multicoef)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
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
    type(ROM_DS_MultiCoef), intent(out) :: ds_multicoef
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Initializations
!
! Initialisation of datastructure for multiparametric problems - Coefficients
!
! --------------------------------------------------------------------------------------------------
!
! In  object_type      : type of object (VECT or MATR)
! Out ds_multicoef     : datastructure for multiparametric problems - Coefficients
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        if (object_type .eq. 'V') then
            call utmess('I', 'ROM5_90')
        elseif (object_type .eq. 'M') then
            call utmess('I', 'ROM5_91')
        else
            ASSERT(.false.)
        endif
    endif
!
    ds_multicoef%l_func         = .false.
    ds_multicoef%l_cste         = .false.
    ds_multicoef%l_cplx         = .false.
    ds_multicoef%l_real         = .false.
    ds_multicoef%func_name      = ' '
    ds_multicoef%coef_cste_cplx = dcmplx(0.d0, 0.d0)
    ds_multicoef%coef_cste_real = 0.d0
    ds_multicoef%coef_cplx      => null()
    ds_multicoef%coef_real      => null()
!
end subroutine
