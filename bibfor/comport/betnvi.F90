subroutine betnvi(elem_model, ndt_, ndi_, nr_, nvi_)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: elem_model
    integer, optional, intent(out) :: ndt_
    integer, optional, intent(out) :: ndi_
    integer, optional, intent(out) :: nr_
    integer, optional, intent(out) :: nvi_
!
! --------------------------------------------------------------------------------------------------
!
! Comportment BETON_DOUBLE_DP
!
! Size of tensors and number of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_model : type of model on element
! Out ndt        : number of terms in tensors
! Out ndi        : number of "indirect" (?) terms in tensors
! Out nr         : number of non-linear system
! Out nvi        : number of internal variables
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ndt, ndi, nr, nvi
!
! --------------------------------------------------------------------------------------------------
!
    nvi = 4
    if (elem_model(1:2) .eq. '3D') then
        ndt = 6
        ndi = 3
        nr = ndt+2
    else if (elem_model(1:6).eq.'D_PLAN'.or.elem_model(1:4).eq.'AXIS') then
        ndt = 4
        ndi = 3
        nr = ndt+2
    else if (elem_model(1:6).eq.'C_PLAN') then
        call utmess('F', 'COMPOR5_51')
    else if (elem_model(1:2).eq.'1D') then
        ndt = 3
        ndi = 3
        nr = ndt+2
    else
        ASSERT(.false.)
    endif
!
    if (present(ndt_)) then
        ndt_ = ndt
    endif
    if (present(ndi_)) then
        ndi_ = ndi
    endif
    if (present(nr_)) then
        nr_  = nr
    endif
    if (present(nvi_)) then
        nvi_ = nvi
    endif
!
end subroutine
