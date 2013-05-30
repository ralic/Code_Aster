subroutine pj2da2(ino2, geom2, i, geom1, tria3,&
                  cobary, d2, surf)
    implicit none
    include 'asterc/r8maem.h'
    real(kind=8) :: cobary(3), geom1(*), geom2(*), d2, surf
    integer :: ino2, i, tria3(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT :
!       DETERMINER LA DISTANCE D2 ENTRE LE NOEUD INO2 ET LE TRIA3 I.
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
    real(kind=8) :: x1, y1, x2, y2, x3, y3, xp, yp, xm, ym
    real(kind=8) :: ksi, dist
    real(kind=8) :: v1(2), v2(2), v3(2), m(2), xc
! DEB ------------------------------------------------------------------
    xm=geom2(3*(ino2-1)+1)
    ym=geom2(3*(ino2-1)+2)
!
    x1=geom1(3*(tria3(1+4*(i-1)+1)-1)+1)
    y1=geom1(3*(tria3(1+4*(i-1)+1)-1)+2)
    x2=geom1(3*(tria3(1+4*(i-1)+2)-1)+1)
    y2=geom1(3*(tria3(1+4*(i-1)+2)-1)+2)
    x3=geom1(3*(tria3(1+4*(i-1)+3)-1)+1)
    y3=geom1(3*(tria3(1+4*(i-1)+3)-1)+2)
!
!
    v1(1)=x3-x2
    v1(2)=y3-y2
    v2(1)=x1-x3
    v2(2)=y1-y3
    v3(1)=x2-x1
    v3(2)=y2-y1
!
    surf=abs(v2(1)*v3(2)-v2(2)*v3(1))
    surf=surf/2.d0
    d2=r8maem()
!
!     COTE 1 (2->3):
!     --------------
    m(1)= xm-x2
    m(2)= ym-y2
    xc=v1(1)*v1(1)+v1(2)*v1(2)
    if (xc .eq. 0) then
        ksi=0.5d0
    else
        ksi=(m(1)*v1(1)+m(2)*v1(2))/xc
    endif
    if (ksi .ge. 1.d0) then
        ksi=1.d0
    else if (ksi.le.0.d0) then
        ksi=0.d0
    endif
    xp=ksi*x3+(1.d0-ksi)*x2
    yp=ksi*y3+(1.d0-ksi)*y2
    dist=(xp-xm)*(xp-xm)+(yp-ym)*(yp-ym)
    if (dist .lt. d2) then
        cobary(1)=0.d0
        cobary(2)=1.d0-ksi
        cobary(3)=ksi
        d2=dist
    endif
!
!
!     COTE 2 (3->1):
!     --------------
    m(1)= xm-x3
    m(2)= ym-y3
    xc=v2(1)*v2(1)+v2(2)*v2(2)
    if (xc .eq. 0) then
        ksi=0.5d0
    else
        ksi=(m(1)*v2(1)+m(2)*v2(2))/xc
    endif
    if (ksi .ge. 1.d0) then
        ksi=1.d0
    else if (ksi.le.0.d0) then
        ksi=0.d0
    endif
    xp=ksi*x1+(1.d0-ksi)*x3
    yp=ksi*y1+(1.d0-ksi)*y3
    dist=(xp-xm)*(xp-xm)+(yp-ym)*(yp-ym)
    if (dist .lt. d2) then
        cobary(2)=0.d0
        cobary(3)=1.d0-ksi
        cobary(1)=ksi
        d2=dist
    endif
!
!
!     COTE 3 (1->2):
!     --------------
    m(1)= xm-x1
    m(2)= ym-y1
    xc=v3(1)*v3(1)+v3(2)*v3(2)
    if (xc .eq. 0) then
        ksi=0.5d0
    else
        ksi=(m(1)*v3(1)+m(2)*v3(2))/xc
    endif
    if (ksi .ge. 1.d0) then
        ksi=1.d0
    else if (ksi.le.0.d0) then
        ksi=0.d0
    endif
    xp=ksi*x2+(1.d0-ksi)*x1
    yp=ksi*y2+(1.d0-ksi)*y1
    dist=(xp-xm)*(xp-xm)+(yp-ym)*(yp-ym)
    if (dist .lt. d2) then
        cobary(3)=0.d0
        cobary(1)=1.d0-ksi
        cobary(2)=ksi
        d2=dist
    endif
!
end subroutine
