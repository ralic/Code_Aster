subroutine routhc(hr, hi, pr, a0, dr,&
                  ior)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
#include "jeveux.h"
    integer :: ior
    real(kind=8) :: hr, hi, a0(*), dr(*)
    complex(kind=8) :: pr
!
    complex(kind=8) :: h, z
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    h = dcmplx ( 1.0d0, 0.0d0 )
    do 1 i = 1, ior
        z = a0(i)/(pr+dr(i))
        h = h - pr*z
 1  end do
    hr = dble(h)
    hi = dimag(h)
!
end subroutine
