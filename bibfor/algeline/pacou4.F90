subroutine pacou4(a, n, c, d, sing)
    implicit none
! ---------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ARGUMENTS
! ---------
    include 'jeveux.h'
    integer :: n
    real(kind=8) :: a(n, *), c(*), d(*)
    logical :: sing
!
! --------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j, k
    real(kind=8) :: scale, sigma, sum, tau
!-----------------------------------------------------------------------
    sing = .false.
    scale = 0.0d0
!
    do 17 k = 1, n-1
!
        do 11 i = k, n
            scale = max(scale,abs(a(i,k)))
11      continue
!
        if (abs(scale) .le. 1.0d-30) then
            sing = .true.
            c(k) = 0.0d0
            d(k) = 0.0d0
!
        else
            do 12 i = k, n
                a(i,k) = a(i,k)/scale
12          continue
!
            sum = 0.0d0
            do 13 i = k, n
                sum = sum + a(i,k)**2
13          continue
!
            sigma = sign ( sqrt(sum), a(k,k) )
            a(k,k) = a(k,k) + sigma
            c(k) = sigma*a(k,k)
            d(k) = -scale*sigma
!
            do 16 j = k+1, n
!
                sum = 0.0d0
                do 14 i = k, n
                    sum = sum + a(i,k)*a(i,j)
14              continue
!
                tau = sum/c(k)
                do 15 i = k, n
                    a(i,j) = a(i,j) - tau*a(i,k)
15              continue
16          continue
!
        endif
17  end do
!
    d(n) = a(n,n)
    if (abs(d(n)) .le. 1.0d-30) sing = .true.
!
end subroutine
