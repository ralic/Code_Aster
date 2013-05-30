subroutine pacou7(a, n, d, b)
    implicit none
!-----------------------------------------------------------------------
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
    real(kind=8) :: a(n, *), b(*), d(*)
! ---------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: sum
!-----------------------------------------------------------------------
    b(n) = b(n) / d(n)
!
    do 12 i = n-1, 1, -1
!
        sum = 0.0d0
        do 11 j = i+1, n
            sum = sum + a(i,j)*b(j)
11      continue
!
        b(i) = (b(i)-sum) / d(i)
!
12  end do
!
end subroutine
