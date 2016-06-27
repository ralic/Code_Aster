subroutine apelem_getcenter(elem_code, ksi1_cent, ksi2_cent)
!
implicit none
!
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    character(len=8), intent(in) :: elem_code
    real(kind=8), intent(out) :: ksi1_cent
    real(kind=8), intent(out) :: ksi2_cent
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Get center of element in parametric space
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_code        : code of current element
! Out ksi1_cent        : first parametric coordinate for center of element
! Out ksi2_cent        : second parametric coordinate for center of element
!
! --------------------------------------------------------------------------------------------------
!
    ksi1_cent = 0.d0
    ksi2_cent = 0.d0
!    
    if (elem_code .eq. 'SE2'.or.&
        elem_code .eq. 'SE3') then
        ksi1_cent   = 0.d0 
    elseif (elem_code .eq. 'TR3'.or.&
            elem_code .eq. 'TR6') then
        ksi1_cent   = 1.d0/3.d0
        ksi2_cent   = 1.d0/3.d0
    elseif (elem_code .eq. 'QU4' .or.&
            elem_code .eq. 'QU8' .or.&
            elem_code .eq. 'QU9') then
        ksi1_cent   = 0.d0
        ksi2_cent   = 0.d0
    else
        ASSERT(.false.) 
    end if
!
end subroutine
