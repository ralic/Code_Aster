subroutine pmfk01(cars, gxjx, xl, sk)
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
    implicit none
!    -------------------------------------------------------------------
!    * CE SOUS PROGRAMME CALCULE LA MATRICE DE RAIDEUR DE L'ELEMENT DE
!    POUTRE MULTIFIBRE DROITE A SECTION CONSTANTE.
!
!    * DESCRIPTION DE L'ELEMENT:
!      C'EST UN ELEMENT A DEUX NOEUDS ET A SIX DEGRES DE LIBERTES PAR
!      NOEUDS (3 DEPLACEMENTS ET 3 ROTATIONS).
!
!    * REMARQUE :
!      LA MATRICE EST STOCKEE TRIANGULAIRE INFERIEURE DANS UN TABLEAU
!      UNICOLONNE
!    -------------------------------------------------------------------
!  DONNEES NON MODIFIEES
!
! IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
! IN -------------------------------------------------------------------
! IN R*8  ! CARS   !     6   ! CARACTERISTIQUES INTEGREES DE LA SECTION
!              INT(VAR DS) = INTEGRALE DE VAR SUR LA SECTION
!              E : MODULE D'UNE FIBRE
!              Y,Z : COORDONNEES DE LA FIBRE
!
! IN R*8  ! CARS(1)!     -   ! KS11   = INT(E DS)
! IN R*8  ! CARS(2)!     -   ! -KS13  = INT(E.Y DS)
! IN R*8  ! CARS(3)!     -   ! KS12  = INT(E.Z DS)
! IN R*8  ! CARS(4)!     -   ! KS33  = INT(E.Y.Y DS)
! IN R*8  ! CARS(5)!     -   ! KS22  = INT(E.Z.Z DS)
! IN R*8  ! CARS(6)!     -   ! -KS23 = INT(E.Y.Z DS)
! IN R*8  ! GXJX   !     -   ! G FOIS LA CONSTANTE DE TORSION
!
!
! OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
! OUT ------------------------------------------------------------------
! OUT R*8 !  SK   ! (78)    ! MATRICE ELEMENTAIRE UNICOLONNE
!
!
! LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
! LOC ------------------------------------------------------------------
! LOC I   ! IP     !   12    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
!     ------------------------------------------------------------------
    integer :: i, ip(12)
    real(kind=8) :: xl, sk(*), cars(6), gxjx
    real(kind=8) :: ks11, ks12, ks13, ks22, ks33, ks23
    real(kind=8) :: zero, un, deux, quatre, six, douze, co1, co2, co4, co6, co12
    parameter (zero=0.0d+0,un=1.d+0,deux=2.d+0,quatre=4.d+0,six=6.d+0,&
     &          douze=12.d+0)
    data ip/0,1,3,6,10,15,21,28,36,45,55,66/
!
    do 10,i = 1,78
    sk(i) = zero
    10 end do
!
    co1 = un/xl
    co2 = deux/xl
    co4 = quatre/xl
    co6 = six/ (xl*xl)
    co12 = douze/ (xl*xl*xl)
!
! --- POUR ETRE PLUS PARLANT
! --- ATTENTION : SIGNE MOINS POUR KS13 ET KS23
    ks11 = cars(1)
    ks13 = -cars(2)
    ks12 = cars(3)
    ks33 = cars(4)
    ks22 = cars(5)
    ks23 = -cars(6)
!
!     1/ TRACTION - COMPRESSION
    sk(1) = ks11*co1
    sk(ip(7)+1) = -sk(1)
    sk(ip(7)+7) = sk(1)
!
!     2/ FLEXION
!        FLEXION DANS LE PLAN XOY
    sk(ip(2)+2) = ks33*co12
    sk(ip(6)+2) = ks33*co6
    sk(ip(8)+2) = -sk(ip(2)+2)
    sk(ip(12)+2) = sk(ip(6)+2)
    sk(ip(6)+6) = ks33*co4
    sk(ip(8)+6) = -sk(ip(6)+2)
    sk(ip(12)+6) = ks33*co2
    sk(ip(8)+8) = sk(ip(2)+2)
    sk(ip(12)+8) = -sk(ip(6)+2)
    sk(ip(12)+12) = sk(ip(6)+6)
!
!     3/ FLEXION DANS LE PLAN XOZ
    sk(ip(3)+3) = ks22*co12
    sk(ip(5)+3) = -ks22*co6
    sk(ip(9)+3) = -sk(ip(3)+3)
    sk(ip(11)+3) = sk(ip(5)+3)
    sk(ip(5)+5) = ks22*co4
    sk(ip(9)+5) = -sk(ip(5)+3)
    sk(ip(11)+5) = ks22*co2
    sk(ip(9)+9) = sk(ip(3)+3)
    sk(ip(11)+9) = -sk(ip(5)+3)
    sk(ip(11)+11) = sk(ip(5)+5)
!
!     4/ TORSION
    sk(ip(4)+4) = gxjx/xl
    sk(ip(10)+4) = -sk(ip(4)+4)
    sk(ip(10)+10) = sk(ip(4)+4)
!
!     5/ COUPLAGE AXIAL-FLEXIONS
    sk(ip(6)+1) = ks13*co1
    sk(ip(12)+1) = -sk(ip(6)+1)
    sk(ip(12)+7) = sk(ip(6)+1)
    sk(ip(7)+6) = -sk(ip(6)+1)
    sk(ip(5)+1) = ks12*co1
    sk(ip(11)+1) = -sk(ip(5)+1)
    sk(ip(11)+7) = sk(ip(5)+1)
    sk(ip(7)+5) = -sk(ip(5)+1)
!
!     5/ COUPLAGE FLEXIONS XOZ - XOY
    sk(ip(3)+2) = -ks23*co12
    sk(ip(9)+2) = -sk(ip(3)+2)
    sk(ip(8)+3) = -sk(ip(3)+2)
    sk(ip(9)+8) = sk(ip(3)+2)
!
    sk(ip(5)+2) = ks23*co6
    sk(ip(6)+3) = -sk(ip(5)+2)
    sk(ip(11)+2) = sk(ip(5)+2)
    sk(ip(12)+3) = -sk(ip(5)+2)
    sk(ip(8)+5) = -sk(ip(5)+2)
    sk(ip(9)+6) = sk(ip(5)+2)
    sk(ip(11)+8) = -sk(ip(5)+2)
    sk(ip(12)+9) = sk(ip(5)+2)
!
    sk(ip(6)+5) = ks23*co4
    sk(ip(12)+11) = sk(ip(6)+5)
!
    sk(ip(12)+5) = ks23*co2
    sk(ip(11)+6) = sk(ip(12)+5)
!
! LES 28 AUTRES TERMES SONT NULS ...
!(SAUF SI ON MET DU COUPLAGE TORSION-FLEXION)
!
end subroutine
