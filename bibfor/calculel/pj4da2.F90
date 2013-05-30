subroutine pj4da2(ino2, geom2, i, geom1, tria3,&
                  cobary, d2, surf)
    implicit none
    include 'asterc/r8maem.h'
    include 'asterfort/pj3da3.h'
    include 'asterfort/pj3da4.h'
    real(kind=8) :: cobary(3), geom1(*), geom2(*), d2, surf
    integer :: ino2, i, tria3(*)
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
!     BUT :
!       DETERMINER LA DISTANCE D ENTRE LE NOEUD INO2 ET LE TRIA3 I.
!       DETERMINER LES COORDONNEES BARYCENTRIQUES
!       DU POINT DE I LE PLUS PROCHE DE INO2.
!
!  IN   INO2       I  : NUMERO DU NOEUD DE M2 CHERCHE
!  IN   GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
!  IN   GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
!  IN   I          I  : NUMERO DU TRIA3 CANDIDAT
!  IN   TRIA3(*)   I  : OBJET '&&PJXXCO.TRIA3'
!  OUT  COBARY(3)  R  : COORDONNEES BARYCENTRIQUES DE INO2 PROJETE SUR I
!  OUT  D2         R  : CARRE DE LA DISTANCE ENTRE I ET INO2
!  OUT  SURF       R  : SURFACE DU TRIA3 I
!
!
! ----------------------------------------------------------------------
    integer :: k
    logical :: ok
    real(kind=8) :: dp, l1, l2, l3, la, lb, lc
    real(kind=8) :: a(3), b(3), c(3), m(3), ab(3), ac(3), v(3)
! DEB ------------------------------------------------------------------
!
    do 1,k=1,3
    m(k)=geom2(3*(ino2-1)+k)
    a(k)=geom1(3*(tria3(1+4*(i-1)+1)-1)+k)
    b(k)=geom1(3*(tria3(1+4*(i-1)+2)-1)+k)
    c(k)=geom1(3*(tria3(1+4*(i-1)+3)-1)+k)
    ab(k)=b(k)-a(k)
    ac(k)=c(k)-a(k)
    1 end do
!
    d2=r8maem()
    dp=r8maem()
!
!
!     1. ON CHERCHE LE POINT LE PLUS PROCHE A L'INTERIEUR DU TRIA3
!     -------------------------------------------------------------
    call pj3da3(m, a, b, c, ok,&
                l1, l2, l3, dp)
    if ((ok) .and. (dp.lt.d2)) then
        d2=dp
        la=l1
        lb=l2
        lc=l3
    endif
!
!
!     2. ON BOUCLE SUR LES 3 ARRETES DU TRIA3 :
!     -----------------------------------------
    call pj3da4(m, a, b, l1, l2,&
                dp)
    if (dp .lt. d2) then
        d2=dp
        la=l1
        lb=l2
        lc=0.d0
    endif
!
    call pj3da4(m, b, c, l1, l2,&
                dp)
    if (dp .lt. d2) then
        d2=dp
        lb=l1
        lc=l2
        la=0.d0
    endif
!
    call pj3da4(m, a, c, l1, l2,&
                dp)
    if (dp .lt. d2) then
        d2=dp
        la=l1
        lc=l2
        lb=0.d0
    endif
!
!
!     3. ON CALCULE SURF :
!     --------------------
    v(1)=ab(2)*ac(3)-ab(3)*ac(2)
    v(2)=ab(3)*ac(1)-ab(1)*ac(3)
    v(3)=ab(1)*ac(2)-ab(2)*ac(1)
    surf=sqrt(v(1)*v(1)+v(2)*v(2)+v(3)*v(3))
    surf=surf/2.d0
!
!
    cobary(1)=la
    cobary(2)=lb
    cobary(3)=lc
!
end subroutine
