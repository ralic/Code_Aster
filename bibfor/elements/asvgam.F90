subroutine asvgam(tpelt, xyzno, bj, gam)
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
    implicit none
!
! ASSUMED STRAIN VECTOR GAMMA
! EVALUATION OF GAMMA VECTORS
!
! IN  tpelt    element type: 1 for SHB6, 2 for HEXS8 or SHB8
! IN  xyzno    element node coordinates
! IN  bj       obtained from shape function derivatives at (0,0,0)
! OUT gam      gamma vectors
!
! Gamma vectors are required within the frame of the assumed strain method,
! implemented for under-integrated HEXS8, SHB8 & SHB6 elements.
!
! For HEXS8 & SHB8, gamma vectors read:
! $\gamma_{\alpha}=\frac{1}{8}\left[h_{\alpha}-\overset{3}{\underset{j=1}
!{\sum}}\left(h_{\alpha}^{T}\cdot x_{j}\right)b_{j}\right]$
! $\alpha=1...4$
! with
! $h_{1}^{T}=\left(1,1,-1,-1,-1,-1,1,1\right)$
! $h_{2}^{T}=\left(1,-1,-1,1,-1,1,1,-1\right)$
! $h_{3}^{T}=\left(1,-1,1,-1,1,-1,1,-1\right)$
! $h_{4}^{T}=\left(-1,1,-1,1,1,-1,1,-1\right)$
!
! For SHB6, gamma vectors read:
! $\gamma_{\alpha}=\frac{1}{2}\left[h_{\alpha}-\overset{3}{\underset{j=1}
!{\sum}}\left(h_{\alpha}^{T}\cdot x_{j}\right)b_{j}\right]$
! $\alpha=1...2$
! with
! $h_{1}^{T}=\left(-1,0,0,1,0,0\right)$
! $h_{2}^{T}=\left(0,-1,0,0,1,0\right)$
!
! $b_{j}$ terms are obtained from the shape functions derivatives evaluated at
! parametric coordinates (0,0,0). They can be expressed in  Hallquist form
! (derivatives) or Flanagan-Belytschko (integer of derivatives).
!
!
!

#include "asterfort/r8inir.h"
!
    integer, intent(in) :: tpelt
    real(kind=8), intent(in) :: xyzno(3,8)
    real(kind=8), intent(in) :: bj(3,*)
    real(kind=8), intent(out) :: gam(4,8)
!
    integer :: i, k, j, nno, nvc
    real(kind=8) :: hx(3,4), factor, s
    real(kind=8), pointer :: h(:,:) => null()
    real(kind=8), target :: hh(8,4), hp(6,2)
!
    data hp/ -1.d0, 0.d0, 0.d0, 1.d0, 0.d0, 0.d0,&
     &        0.d0,-1.d0, 0.d0, 0.d0, 1.d0, 0.d0/
!
    data hh/ 1.d0, 1.d0,-1.d0,-1.d0,-1.d0,-1.d0, 1.d0, 1.d0,&
     &       1.d0,-1.d0,-1.d0, 1.d0,-1.d0, 1.d0, 1.d0,-1.d0,&
     &       1.d0,-1.d0, 1.d0,-1.d0, 1.d0,-1.d0, 1.d0,-1.d0,&
     &      -1.d0, 1.d0,-1.d0, 1.d0, 1.d0,-1.d0, 1.d0,-1.d0/
!
! ......................................................................
!
    if(tpelt.eq.1) then
!      for prisme (SHB6)
        nno = 6
        nvc = 2
        factor = 0.5d0
        h => hp
    elseif(tpelt.eq.2) then
!      for hexa (HEXS8 or SHB8)
        nno = 8
        nvc = 4
        factor = 0.125d0
        h => hh
    endif
!
    call r8inir(12, 0.d0, hx, 1)
!
! ......................................................................
!
    do 1 i = 1, nvc
        do 2 k = 1, 3
            hx(k,i)=0.d0
            do 3 j = 1, nno
                hx(k,i) = hx(k,i) + h(j,i) * xyzno(k,j)
 3          continue
 2      continue
 1  continue
!
    do 10 i = 1, nvc
        do 11 j = 1, nno
            s = 0.d0
            do 12 k = 1, 3
                s = s + hx(k,i) * bj(k,j)
12          continue
            gam(i,j) = factor * (h(j,i) - s)
11      continue
10  continue
!
end subroutine
