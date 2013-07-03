subroutine pj3dap(ino2, geom2, ma2, geom1, tetr4,&
                  cobary, itr3, nbtrou, btdi, btvr,&
                  btnb, btlc, btco, ifm, niv,&
                  ldmax, distma, loin, dmin)
    implicit none
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterfort/pj3da1.h"
#include "asterfort/pj3da2.h"
#include "asterfort/pj3dgb.h"
    real(kind=8) :: cobary(4), geom1(*), geom2(*), btvr(*)
    integer :: itr3, nbtrou, btdi(*), btnb(*), btlc(*), btco(*)
    integer :: tetr4(*), ifm, niv
    character(len=8) :: ma2
! ----------------------------------------------------------------------
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
!     BUT :
!       TROUVER LE TETR4 QUI SERVIRA A INTERPOLER LE NOEUD INO2
!       AINSI QUE LES COORDONNEES BARYCENTRIQUES DE INO2 DANS CE TETR4
!
!  IN   INO2       I  : NUMERO DU NOEUD DE M2 CHERCHE
!  IN   MA2        K8 : NOM DU MAILLAGE M2
!  IN   GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
!  IN   GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
!  IN   TETR4(*)   I  : OBJET '&&PJXXCO.TETR4'
!  IN   BTDI(*)    I  : OBJET .BT3DDI DE LA SD BOITE_3D
!  IN   BTVR(*)    R  : OBJET .BT3DVR DE LA SD BOITE_3D
!  IN   BTNB(*)    I  : OBJET .BT3DNB DE LA SD BOITE_3D
!  IN   BTLC(*)    I  : OBJET .BT3DLC DE LA SD BOITE_3D
!  IN   BTCO(*)    I  : OBJET .BT3DCO DE LA SD BOITE_3D
!  IN   IFM        I  : NUMERO LOGIQUE DU FICHIER MESSAGE
!  IN   NIV        I  : NIVEAU D'IMPRESSION POUR LES "INFO"
!  IN   LDMAX      L  : .TRUE. : IL FAUT PRENDRE DISTMA EN COMPTE
!  IN   DISTMA     R  : DISTANCE AU DELA DE LAQUELLE LE NOEUD INO2
!                       NE SERA PAS PROJETE.
!  OUT  NBTROU     I  : 2 -> ON A TROUVE 1 TETR4 QUI CONTIENT INO2
!                     : 1 -> ON A TROUVE 1 TETR4 ASSEZ PROCHE DE INO2
!                     : 0 -> ON N'A PAS TROUVE DE TETR4 ASSEZ PROCHE
!  OUT  ITR3       I  : NUMERO DU TETR4 SOLUTION
!  OUT  COBARY(4)  R  : COORDONNEES BARYCENTRIQUES DE INO2 DANS ITR3
!  OUT  DMIN       R  : DISTANCE DE INO2 AU BORD DE ITR3 SI INO2 EST
!                       EXTERIEUR A ITR3.
!  OUT  LOIN       L  : .TRUE. SI DMIN > 10% DIAMETRE(ITR3)
!
!  REMARQUE :
!    SI NBTROU=0, INO2 NE SERA PAS PROJETE CAR IL EST AU DELA DE DISTMA
!    ALORS : DMIN=0, LOIN=.FALSE.
! ----------------------------------------------------------------------
!
!
    real(kind=8) :: cobar2(4), dmin, d2, dx, dy, dz, xmin, ymin, zmin, volu
    real(kind=8) :: rtr3
    integer :: p, q, r, p1, q1, p2, q2, r1, r2, ino2, i, k, iposi, nx, ny, ntrbt
    logical :: ok
!
    logical :: ldmax, loin
    real(kind=8) :: distma
! DEB ------------------------------------------------------------------
    nbtrou=0
    loin=.false.
    dmin=0.d0
!
    nx=btdi(1)
    ny=btdi(2)
    dx=btvr(7)
    dy=btvr(8)
    dz=btvr(9)
    xmin=btvr(1)
    ymin=btvr(3)
    zmin=btvr(5)
!
!
!     -- 1. : ON CHERCHE UN TETR4 ITR3 QUI CONTIENNE INO2 :
!     -------------------------------------------------------
!     -- PARCOURS DES MAILLES CANDIDATES :
!     DO 1,I=1,NTR3
    p=int((geom2(3*(ino2-1)+1)-xmin)/dx)+1
    q=int((geom2(3*(ino2-1)+2)-ymin)/dy)+1
    r=int((geom2(3*(ino2-1)+3)-zmin)/dz)+1
    ntrbt=btnb((r-1)*nx*ny+(q-1)*nx+p)
    iposi=btlc((r-1)*nx*ny+(q-1)*nx+p)
    do 10,k=1,ntrbt
    i=btco(iposi+k)
    call pj3da1(ino2, geom2, i, geom1, tetr4,&
                cobar2, ok)
    if (ok) then
        itr3=i
        nbtrou=2
        cobary(1)=cobar2(1)
        cobary(2)=cobar2(2)
        cobary(3)=cobar2(3)
        cobary(4)=cobar2(4)
        goto 9999
!
    endif
    10 end do
!
!
!
!     -- 2. : SI ECHEC DE LA RECHERCHE PRECEDENTE, ON
!        CHERCHE LE TETR4 ITR3 LE PLUS PROCHE DE INO2 :
!     -------------------------------------------------------
    if (ldmax) then
        dmin=distma
    else
        dmin=r8maem()
    endif
!
!
!     -- ON RECHERCHE LA GROSSE BOITE CANDIDATE :
    call pj3dgb(ino2, geom2, geom1, tetr4, 6,&
                btdi, btvr, btnb, btlc, btco,&
                p1, q1, r1, p2, q2,&
                r2)
    do 60,p=p1,p2
    do 50,q=q1,q2
    do 40,r=r1,r2
    ntrbt=btnb((r-1)*nx*ny+(q-1)*nx+p)
    iposi=btlc((r-1)*nx*ny+(q-1)*nx+p)
    do 30,k=1,ntrbt
    i=btco(iposi+k)
    call pj3da2(ino2, geom2, i, geom1, tetr4,&
                cobar2, d2, volu)
    if (sqrt(d2) .lt. dmin) then
        rtr3=volu
        itr3=i
        dmin=sqrt(d2)
        nbtrou=1
        cobary(1)=cobar2(1)
        cobary(2)=cobar2(2)
        cobary(3)=cobar2(3)
        cobary(4)=cobar2(4)
    endif
30  continue
40  continue
50  continue
    60 end do
!
!
    if (nbtrou .eq. 1) then
        if (rtr3 .eq. 0) then
            loin=.true.
        else
            rtr3 = rtr3** (1.d0/3.d0)
            if (dmin/rtr3 .gt. 1.d-1) loin=.true.
        endif
    else
        dmin=0.d0
    endif
!
9999  continue
!
end subroutine
