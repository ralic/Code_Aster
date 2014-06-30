subroutine pj3da1(ino2, geom2, i, geom1, tetr4,&
                  cobar2, ok)
    implicit none
    real(kind=8) :: cobar2(4), geom1(*), geom2(*), epsi
    integer :: i, tetr4(*), ino2
    logical(kind=1) :: ok
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
!       DETERMINER SI LE TETR4 I CONTIENT LE NOEUD INO2
!       SI OUI :
!       DETERMINER LES COORDONNEES BARYCENTRIQUES DE INO2 DANS CE TETR4
!
!  IN   INO2       I  : NUMERO DU NOEUD DE M2 CHERCHE
!  IN   GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
!  IN   GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
!  IN   I          I  : NUMERO DU TETR4 CANDIDAT
!  IN   TETR4(*)   I  : OBJET '&&PJXXCO.TETR4'
!  OUT  COBAR2(4)  R  : COORDONNEES BARYCENTRIQUES DE INO2 DANS I
!  OUT  OK         L  : .TRUE. : INO2 APPARTIENT AU TETR4 I
!
!
! ----------------------------------------------------------------------
    integer :: perm(4), lino(4), k, p
    real(kind=8) :: p1(3), p2(3), p3(3), p4(3), pp(3), n(3), v12(3), v13(3)
    real(kind=8) :: v14(3)
    real(kind=8) :: vol, volp, v1p(3)
    data perm/2,3,4,1/
! DEB ------------------------------------------------------------------
    pp(1)=geom2(3*(ino2-1)+1)
    pp(2)=geom2(3*(ino2-1)+2)
    pp(3)=geom2(3*(ino2-1)+3)
!
    lino(1)=4
    lino(2)=1
    lino(3)=2
    lino(4)=3
!
    do 10, p=1,4
!       -- ON PERMUTE LES 4 NOEUDS DU TETRAEDRE :
    do 11, k=1,4
    lino(k)=perm(lino(k))
11  continue
!
    do 1, k=1,3
    p1(k)= geom1(3*(tetr4(1+6*(i-1)+lino(1))-1)+k)
    p2(k)= geom1(3*(tetr4(1+6*(i-1)+lino(2))-1)+k)
    p3(k)= geom1(3*(tetr4(1+6*(i-1)+lino(3))-1)+k)
    p4(k)= geom1(3*(tetr4(1+6*(i-1)+lino(4))-1)+k)
 1  continue
!
    do 2, k=1,3
    v12(k)= p2(k)-p1(k)
    v13(k)= p3(k)-p1(k)
    v14(k)= p4(k)-p1(k)
    v1p(k)= pp(k)-p1(k)
 2  continue
!
    n(1)= v12(2)*v13(3)-v12(3)*v13(2)
    n(2)= v12(3)*v13(1)-v12(1)*v13(3)
    n(3)= v12(1)*v13(2)-v12(2)*v13(1)
!
    vol =n(1)*v14(1)+n(2)*v14(2)+n(3)*v14(3)
    if (vol .eq. 0.d0) then
        ok=.false.
        goto 9999
    endif
    volp=n(1)*v1p(1)+n(2)*v1p(2)+n(3)*v1p(3)
    cobar2(lino(4))=volp/vol
    10 end do
!
!
    ok =.true.
!
!     -- TOLERANCE EPSI POUR EVITER DES DIFFERENCES ENTRE
!        LES VERSIONS DEBUG ET NODEBUG
    epsi=1.d-10
    do 30,k=1,4
    if ((cobar2(k).lt.-epsi) .or. (cobar2(k).gt.1.d0+epsi)) ok= .false.
    30 end do
!
9999  continue
end subroutine
