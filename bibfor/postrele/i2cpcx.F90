subroutine i2cpcx(epsi, t1, t2, c1, c2,&
                  nt, nc)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    integer :: nc, nt, c1(*), c2(*)
    real(kind=8) :: epsi, t1(*), t2(*)
!
    integer :: a, d, i
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    a = 0
    d = 1
    i = 1
    nc = 0
!
    do 10, i = 1, nt-1, 1
!
    if (abs(t1(i+1) - t2(i)) .ge. epsi) then
!
        nc = nc + 1
        a = i
        c1(nc) = d
        c2(nc) = a
        d = a + 1
!
    endif
!
    10 end do
!
    nc = nc + 1
    c1(nc) = d
    c2(nc) = nt
!
end subroutine
