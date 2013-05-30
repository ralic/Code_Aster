subroutine pj2dgb(ino2, geom2, geom1, tria3, btdi,&
                  btvr, btnb, btlc, btco, p1,&
                  q1, p2, q2)
    implicit none
    include 'asterfort/assert.h'
    real(kind=8) :: geom1(*), geom2(*), btvr(*)
    integer :: ino2, p1, q1, p2, q2
    integer :: btdi(*), btnb(*), btlc(*), btco(*), tria3(*)
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
!       TROUVER LA "GROSSE BOITE" (P1,Q1,P2,Q2) DANS LA QUELLE
!       ON EST SUR DE TROUVER LE TRIA3 LE PLUS PROCHE DE INO2
!
!  IN   INO2       I  : NUMERO DU NOEUD DE M2 CHERCHE
!  IN   GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
!  IN   GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
!  IN   TRIA3(*)   I  : OBJET '&&PJXXCO.TRIA3'
!  IN   BTDI(*)    I  : OBJET .BT2DDI DE LA SD BOITE_2D
!  IN   BTVR(*)    R  : OBJET .BT2DVR DE LA SD BOITE_2D
!  IN   BTNB(*)    I  : OBJET .BT2DNB DE LA SD BOITE_2D
!  IN   BTLC(*)    I  : OBJET .BT2DLC DE LA SD BOITE_2D
!  IN   BTCO(*)    I  : OBJET .BT2DCO DE LA SD BOITE_2D
!  OUT  P1         I  : ABSCISSE DU COIN BAS/GAUCHE DE LA GROSSE BOITE
!  OUT  Q1         I  : ORDONNEE DU COIN BAS/GAUCHE DE LA GROSSE BOITE
!  OUT  P2         I  : ABSCISSE DU COIN HAUT/DROIT DE LA GROSSE BOITE
!  OUT  Q2         I  : ORDONNEE DU COIN HAUT/DROIT DE LA GROSSE BOITE
! ----------------------------------------------------------------------
    real(kind=8) :: d, x1, y1, x2, y2, xmin, ymin, dx, dy
    integer :: p0, q0, nx, ny, k, p, q, itr, ntrbt, iposi, ino1
!
! DEB ------------------------------------------------------------------
!
    nx = btdi(1)
    ny = btdi(2)
    dx = btvr(5)
    dy = btvr(6)
    xmin = btvr(1)
    ymin = btvr(3)
!
!
!      1. ON CHERCHE UNE BOITE NON VIDE AUTOUR DE INO2
!     -------------------------------------------------------
    p0 = int((geom2(3* (ino2-1)+1)-xmin)/dx) + 1
    q0 = int((geom2(3* (ino2-1)+2)-ymin)/dy) + 1
!
    do 30,k = 0,max(nx,ny) - 1
    do 20,p = max(p0-k,1),min(p0+k,nx)
    do 10,q = max(q0-k,1),min(q0+k,ny)
    ntrbt = btnb((q-1)*nx+p)
!           -- SI LA BOITE EST NON VIDE :
    if (ntrbt .gt. 0) then
!             -- ON CHOISIT LE 1ER NOEUD DU 1ER TRIA3 DE LA BOITE:INO1
        iposi = btlc((q-1)*nx+p)
        itr = btco(iposi+1)
        ino1 = tria3(1+4* (itr-1)+1)
        goto 40
    endif
10  continue
20  continue
    30 end do
    call assert(.false.)
!
!
40  continue
!     2. ON CALCULE LA DISTANCE ENTRE INO2 ET INO1
!     -------------------------------------------------------
    x1 = geom1(3* (ino1-1)+1)
    y1 = geom1(3* (ino1-1)+2)
    x2 = geom2(3* (ino2-1)+1)
    y2 = geom2(3* (ino2-1)+2)
    d = sqrt((x2-x1)**2+ (y2-y1)**2)
!
!
!     3. ON DETERMINE LA GROSSE BOITE CONTENANT :
!        INO2 - D*VECTEUR_I - D*VECTEUR_J
!     ET INO2 + D*VECTEUR_I + D*VECTEUR_J
!     -------------------------------------------------------
    p1 = int((x2-d-xmin)/dx) + 1
    q1 = int((y2-d-ymin)/dy) + 1
    p1 = max(1,p1)
    q1 = max(1,q1)
!
    p2 = int((x2+d-xmin)/dx) + 1
    q2 = int((y2+d-ymin)/dy) + 1
    p2 = min(nx,p2)
    q2 = min(ny,q2)
!
end subroutine
