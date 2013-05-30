subroutine pj6da2(ino2, geom2, i, geom1, seg2,&
                  cobary, d2, long)
    implicit none
    include 'asterc/r8maem.h'
    include 'asterfort/pj3da4.h'
    real(kind=8) :: cobary(2), geom1(*), geom2(*), d2, long
    integer :: ino2, i, seg2(*)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
!     BUT :
!       DETERMINER LA DISTANCE D ENTRE LE NOEUD INO2 ET LE SEG2 I.
!       DETERMINER LES COORDONNEES BARYCENTRIQUES
!       DU POINT DE I LE PLUS PROCHE DE INO2.
!
!  IN   INO2       I  : NUMERO DU NOEUD DE M2 CHERCHE
!  IN   GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
!  IN   GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
!  IN   I          I  : NUMERO DU SEG2 CANDIDAT
!  IN   SEG2(*)    I  : OBJET '&&PJXXCO.SEG2'
!  OUT  COBARY(2)  R  : COORDONNEES BARYCENTRIQUES DE INO2 PROJETE SUR I
!  OUT  D2         R  : CARRE DE LA DISTANCE ENTRE I ET INO2
!  OUT  LONG       R  : LONGUEUR DU SEG2 I
!
!
! ----------------------------------------------------------------------
    integer :: k
    real(kind=8) :: dp, l1, l2, la, lb
    real(kind=8) :: a(3), b(3), m(3), ab(3)
! DEB ------------------------------------------------------------------
!
    do 1,k=1,3
    m(k)=geom2(3*(ino2-1)+k)
    a(k)=geom1(3*(seg2(1+3*(i-1)+1)-1)+k)
    b(k)=geom1(3*(seg2(1+3*(i-1)+2)-1)+k)
    ab(k)=b(k)-a(k)
    1 end do
!
    d2=r8maem()
    dp=r8maem()
!
!
!     1. ON CHERCHE LE POINT LE PLUS PROCHE DE AB :
!     -------------------------------------------------------------
    call pj3da4(m, a, b, l1, l2,&
                dp)
    if (dp .lt. d2) then
        d2=dp
        la=l1
        lb=l2
    endif
!
!
!     2. ON CALCULE LONG :
!     --------------------
    long=sqrt(ab(1)**2+ab(2)**2+ab(3)**2)
!
    cobary(1)=la
    cobary(2)=lb
!
end subroutine
