subroutine pj3da4(m, a, b, la, lb,&
                  d2)
    implicit none
    real(kind=8) :: m(3), a(3), b(3), d2, la, lb
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   TROUVER LES COORDONNEES BARYCENTRIQUES (LA,LB) DU POINT P
!   LE PLUS PROCHE DE M SUR UN SEGMENT (A,B) .
!
!  IN   M(3)    R : COORDONNEES DE M
!  IN   A(3)    R : COORDONNEES DE A
!  IN   B(3)    R : COORDONNEES DE B
!
!  OUT  D2      R  : CARRE DE LA DISTANCE ENTRE M ET P
!  OUT  LA,LB   R  : COORDONNEES BARYCENTRIQUES DE P SUR AB
!
!
! ----------------------------------------------------------------------
    integer :: k
    real(kind=8) :: p(3), a1, a2
    real(kind=8) :: ab(3), am(3)
! DEB ------------------------------------------------------------------
    do 1,k=1,3
    ab(k)=b(k)-a(k)
    am(k)=m(k)-a(k)
    1 end do
!
    a1= am(1)*ab(1)+am(2)*ab(2)+am(3)*ab(3)
    a2= ab(1)*ab(1)+ab(2)*ab(2)+ab(3)*ab(3)
!
!     -- CAS DU SEGMENT DE LONGUEUR NULLE :
    if (a2 .eq. 0.d0) then
        lb=0.5d0
    else
        lb=a1/a2
    endif
!
!
    if (lb .lt. 0.d0) lb=0.d0
    if (lb .gt. 1.d0) lb=1.d0
!
    la=1.d0-lb
    do 2,k=1,3
    p(k)=la*a(k)+lb*b(k)
    p(k)=m(k)-p(k)
    2 end do
    d2=p(1)*p(1)+p(2)*p(2)+p(3)*p(3)
end subroutine
