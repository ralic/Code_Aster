subroutine pj2da1(ino2, geom2, i, geom1, tria3,&
                  cobar2, ok)
    implicit none
    real(kind=8) :: cobar2(3), geom1(*), geom2(*)
    integer :: i, tria3(*), ino2
    logical :: ok
! ----------------------------------------------------------------------
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
!     BUT :
!       DETERMINER SI LE TRIA3 I CONTIENT LE NOEUD INO2
!       SI OUI :
!       DETERMINER LES COORDONNEES BARYCENTRIQUES DE INO2 DANS CE TRIA3
!
!  IN   INO2       I  : NUMERO DU NOEUD DE M2 CHERCHE
!  IN   GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
!  IN   GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
!  IN   I          I  : NUMERO DU TRIA3 CANDIDAT
!  IN   TRIA3(*)   I  : OBJET '&&PJXXCO.TRIA3'
!  OUT  COBAR2(3)  R  : COORDONNEES BARYCENTRIQUES DE INO2 DANS I
!  OUT  OK         L  : .TRUE. : INO2 APPARTIENT AU TRIA3 I
!
!
! ----------------------------------------------------------------------
    real(kind=8) :: x1, y1, x2, y2, x3, y3, xp, yp
    real(kind=8) :: l1, l2, l3, s
    real(kind=8) :: v2(2), v3(2), p(2), epsi
! DEB ------------------------------------------------------------------
    xp=geom2(3*(ino2-1)+1)
    yp=geom2(3*(ino2-1)+2)
!
    x1=geom1(3*(tria3(1+4*(i-1)+1)-1)+1)
    y1=geom1(3*(tria3(1+4*(i-1)+1)-1)+2)
    x2=geom1(3*(tria3(1+4*(i-1)+2)-1)+1)
    y2=geom1(3*(tria3(1+4*(i-1)+2)-1)+2)
    x3=geom1(3*(tria3(1+4*(i-1)+3)-1)+1)
    y3=geom1(3*(tria3(1+4*(i-1)+3)-1)+2)
!
    v2(1)=x2-x1
    v2(2)=y2-y1
    v3(1)=x3-x1
    v3(2)=y3-y1
    s=v2(1)*v3(2)-v2(2)*v3(1)
    if (s .eq. 0) then
        ok=.false.
        goto 9999
    endif
!
    p(1)=xp-x1
    p(2)=yp-y1
    l3=(v2(1)*p(2)-v2(2)*p(1))/s
    l2=(p(1)*v3(2)-p(2)*v3(1))/s
    l1=1.d0-l2-l3
!
!     -- TOLERANCE EPSI POUR EVITER DES DIFFERENCES ENTRE
!        LES VERSIONS DEBUG ET NODEBUG
    epsi=1.d-10
    if ((l1.ge.-epsi) .and. (l1.le.1.d0+epsi ) .and. (l2.ge.-epsi) .and. (l2.le.1.d0+epsi )&
        .and. (l3.ge.-epsi) .and. (l3.le.1.d0+epsi )) then
        ok=.true.
        cobar2(1)=l1
        cobar2(2)=l2
        cobar2(3)=l3
    else
        ok=.false.
    endif
!
!
9999  continue
end subroutine
