subroutine romMultiCoefRead(ds_multicoef, keywfact, iocc)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
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
    type(ROM_DS_MultiCoef), intent(inout) :: ds_multicoef
    character(len=16), intent(in) :: keywfact
    integer, intent(in) :: iocc
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Read coefficients for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_multicoef     : datastructure for multiparametric problems - Coefficients
! In  keywfact         : name of factor keyword
! In  iocc             : index of factor keyword
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cplx, l_real, l_func, l_cste
    integer :: nocc_coef_r, nocc_coef_c, nocc_func_r, nocc_func_c
!
! --------------------------------------------------------------------------------------------------
!
    nocc_func_r = 0
    nocc_func_c = 0
    if (getexm(keywfact, 'FONC_R') .eq. 1) then
        call getvid(keywfact, 'FONC_R', iocc=iocc, nbret=nocc_func_r)
    endif
    if (getexm(keywfact, 'FONC_C') .eq. 1) then
        call getvid(keywfact, 'FONC_C', iocc=iocc, nbret=nocc_func_c)
    endif
!
! - Get type of coefficient
!
    call getvr8(keywfact, 'COEF_R', iocc=iocc, nbret=nocc_coef_r)
    call getvc8(keywfact, 'COEF_C', iocc=iocc, nbret=nocc_coef_c)
    l_cste = nocc_coef_r .ne. 0 .or. nocc_coef_c .ne. 0
    l_func = nocc_func_r .ne. 0 .or. nocc_func_c .ne. 0
    l_cplx = nocc_coef_c .ne. 0 .or. nocc_func_c .ne. 0
    l_real = nocc_coef_r .ne. 0 .or. nocc_func_r .ne. 0
!
! - Read informations
!
    if (l_func) then
        if (l_real) then
            call getvid(keywfact, 'FONC_R', iocc=iocc,&
                        scal = ds_multicoef%func_name)
        elseif (l_cplx) then
            call getvid(keywfact, 'FONC_C', iocc=iocc,&
                        scal = ds_multicoef%func_name)
        else
            ASSERT(.false.)
        endif
    elseif (l_cste) then
        if (l_real) then
            call getvr8(keywfact, 'COEF_R', iocc=iocc,&
                        scal = ds_multicoef%coef_cste_real)
        elseif (l_cplx) then
            call getvc8(keywfact, 'COEF_C', iocc=iocc,&
                        scal = ds_multicoef%coef_cste_cplx)
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
! - Save informations
!
    ds_multicoef%l_cste  = l_cste
    ds_multicoef%l_func  = l_func
    ds_multicoef%l_cplx  = l_cplx
    ds_multicoef%l_real  = l_real
!
end subroutine
