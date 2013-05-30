subroutine pacou5(r, qt, n, u, v)
    implicit none
! --------------------------------------------------------------------
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
    include 'asterfort/pacou6.h'
    integer :: n
    real(kind=8) :: r(n, *), qt(n, *), u(*), v(*)
    integer :: i, j, k
! ---------------------------------------------------------------------
!
    do 11 k = n, 1, -1
        if (abs(u(k)) .gt. 1.0d-30) goto 1
11  end do
    k = 1
!
 1  continue
!
    do 12 i = k-1, 1, -1
!
        call pacou6(r, qt, n, i, u(i),&
                    -u(i+1))
!
        if (abs(u(i)) .le. 1.0d-30) then
            u(i) = abs(u(i+1))
!
        else if (abs(u(i)) .gt. abs(u(i+1))) then
            u(i) = abs(u(i)) * sqrt(1.0d0+(u(i+1)/u(i))**2)
!
        else
            u(i) = abs(u(i+1)) * sqrt(1.0d0+(u(i)/u(i+1))**2)
!
        endif
12  end do
!
    do 13 j = 1, n
        r(1,j) = r(1,j) + u(1)*v(j)
13  end do
!
    do 14 i = 1, k-1
!
        call pacou6(r, qt, n, i, r(i, i),&
                    -r(i+1, i))
14  end do
!
end subroutine
