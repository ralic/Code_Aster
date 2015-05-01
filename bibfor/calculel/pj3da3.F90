subroutine pj3da3(m, a, b, c, ok,&
                  la, lb, lc, d2)
    implicit none
#include "asterf_types.h"
    real(kind=8) :: m(3), a(3), b(3), c(3), d2, la, lb, lc
    aster_logical :: ok
! ----------------------------------------------------------------------
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
! BUT :
!   TROUVER LES COORDONNEES BARYCENTRIQUES (LA,LB,LC) DE LA PROJECTION P
!   D'UN POINT M SUR UN TRIANGLE (A,B,C) .
!
!  IN   M(3)    R : COORDONNEES DE M
!  IN   A(3)    R : COORDONNEES DE A
!  IN   B(3)    R : COORDONNEES DE B
!  IN   C(3)    R : COORDONNEES DE C
!
!  OUT  OK         L  :/.TRUE.   : P EST INTERIEUR AU TRIANGLE ABC
!                      /.FALSE.  : P EST EXTERIEUR AU TRIANGLE
!                       (SI .FALSE.   D2 N'EST PAS CALCULE)
!  OUT  D2         R  : CARRE DE LA DISTANCE ENTRE M ET P
!  OUT  LA,LB,LC   R  : COORDONNEES BARYCENTRIQUES DE P DANS ABC
!
!
! ----------------------------------------------------------------------
    integer :: k
    real(kind=8) :: delta, p(3)
    real(kind=8) :: ab(3), ac(3), am(3), a11, a22, a12, b1, b2
! DEB ------------------------------------------------------------------
    do 1 k = 1, 3
        ab(k)=b(k)-a(k)
        ac(k)=c(k)-a(k)
        am(k)=m(k)-a(k)
  1 end do
!
    a11=ab(1)*ab(1)+ab(2)*ab(2)+ab(3)*ab(3)
    a22=ac(1)*ac(1)+ac(2)*ac(2)+ac(3)*ac(3)
    a12=ab(1)*ac(1)+ab(2)*ac(2)+ab(3)*ac(3)
!
    b1=ab(1)*am(1)+ab(2)*am(2)+ab(3)*am(3)
    b2=ac(1)*am(1)+ac(2)*am(2)+ac(3)*am(3)
!
    delta=a11*a22-a12*a12
    if (delta .eq. 0.d0) then
        ok=.false.
        goto 9999
    endif
    lb=(a22*b1-a12*b2)/delta
    lc=(a11*b2-a12*b1)/delta
    la=1.d0-lb-lc
!
    if ((la.ge.0.d0) .and. (la.le.1.d0) .and. (lb.ge.0.d0) .and. (lb.le.1.d0) .and.&
        (lc.ge.0.d0) .and. (lc.le.1.d0)) then
        ok=.true.
        do 2 k = 1, 3
            p(k)=la*a(k)+lb*b(k)+lc*c(k)
            p(k)=m(k)-p(k)
  2     continue
        d2=p(1)*p(1)+p(2)*p(2)+p(3)*p(3)
    else
        ok=.false.
    endif
!
9999 continue
end subroutine
