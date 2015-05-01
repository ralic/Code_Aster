subroutine pmfbkb(cars, b, wi, gxjx, sk)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!    -------------------------------------------------------------------
!     CALCUL DE : WI BT KS B
!     KS = MATRICE DE SECTION (ISSUE DE L'INTEGRATION SUR LES FIBRES)
!     WI = POIDS DU PT DE GAUSS EN COURS
!     B = MATRICE B A CE POINT DE GAUSS ET BT SA TRANSPOSEE
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
! IN R*8  B(4) : LES QUATRES VALEURS NON NULLE ET DIFFERENTES DE B
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
    integer :: ip(12)
    real(kind=8) :: sk(78), cars(6), gxjx, b(4), wi
    real(kind=8) :: ks11, ks12, ks13, ks22, ks33, ks23
    real(kind=8) :: b1b1, b1b2, b1b3, b1b4, b2b2, b2b3, b2b4, b3b3, b3b4, b4b4
    data ip/0,1,3,6,10,15,21,28,36,45,55,66/
!
! SK MIS A ZERO AU DEBUT DE TE0535
! LES 20 TERMES DE COUPLAGE TORSION-QUELQUECHOSE SONT NULS
! ET DOIVENT ETRE INITIALISES UNE FOIS
! ET PUIS CA ARRANGE LE SEGMENTATION VIOLATION ...
!
    b1b1=b(1)*b(1)
    b1b2=b(1)*b(2)
    b1b3=b(1)*b(3)
    b1b4=b(1)*b(4)
    b2b2=b(2)*b(2)
    b2b3=b(2)*b(3)
    b2b4=b(2)*b(4)
    b3b3=b(3)*b(3)
    b3b4=b(3)*b(4)
    b4b4=b(4)*b(4)
!
! --- POUR ETRE PLUS PARLANT
! --- ATTENTION : SIGNE MOINS POUR KS13 ET KS23
! --- MULTIPLICATION PAR LE POIDS

    ks11=cars(1)*wi
    ks13=-cars(2)*wi
    ks12=cars(3)*wi
    ks33=cars(4)*wi
    ks22=cars(5)*wi
    ks23=-cars(6)*wi
!
!     1/ TRACTION - COMPRESSION
    sk(1) = ks11*b1b1
    sk(ip(7)+1) = -sk(1)
    sk(ip(7)+7) = sk(1)
!
!     2/ FLEXION
!        FLEXION DANS LE PLAN XOY
    sk(ip(2)+2) = ks33*b2b2
    sk(ip(6)+2) = ks33*b2b3
    sk(ip(8)+2) = -sk(ip(2)+2)
    sk(ip(12)+2) = ks33*b2b4
    sk(ip(6)+6) = ks33*b3b3
    sk(ip(8)+6) = -sk(ip(6)+2)
    sk(ip(12)+6) = ks33*b3b4
    sk(ip(8)+8) = sk(ip(2)+2)
    sk(ip(12)+8) = -sk(ip(12)+2)
    sk(ip(12)+12) = ks33*b4b4
!
!     3/ FLEXION DANS LE PLAN XOZ
    sk(ip(3)+3) = ks22*b2b2
    sk(ip(5)+3) = -ks22*b2b3
    sk(ip(9)+3) = -sk(ip(3)+3)
    sk(ip(11)+3) = -ks22*b2b4
    sk(ip(5)+5) = ks22*b3b3
    sk(ip(9)+5) = -sk(ip(5)+3)
    sk(ip(11)+5) = ks22*b3b4
    sk(ip(9)+9) = sk(ip(3)+3)
    sk(ip(11)+9) = -sk(ip(11)+3)
    sk(ip(11)+11) = ks22*b4b4
!
!     4/ TORSION
    sk(ip(4)+4) = gxjx*wi*b1b1
    sk(ip(10)+4) = -sk(ip(4)+4)
    sk(ip(10)+10) = sk(ip(4)+4)
!
!     5/ COUPLAGE AXIAL-FLEXIONS
    sk(ip(6)+1)=-ks13*b1b3
    sk(ip(12)+1)=-ks13*b1b4
    sk(ip(12)+7)=-sk(ip(12)+1)
    sk(ip(7)+6)=-sk(ip(6)+1)
    sk(ip(5)+1)=-ks12*b1b3
    sk(ip(11)+1)=-ks12*b1b4
    sk(ip(11)+7)=-sk(ip(11)+1)
    sk(ip(7)+5)=-sk(ip(5)+1)
!
!     5BIS/ COUPLAGE TRANCHANT NORMAL
    sk(ip(2)+1)=-ks13*b1b2
    sk(ip(8)+1)=-sk(ip(2)+1)
    sk(ip(7)+2)=-sk(ip(2)+1)
    sk(ip(8)+7)=sk(ip(2)+1)
    sk(ip(3)+1)=ks12*b1b2
    sk(ip(9)+1)=-sk(ip(3)+1)
    sk(ip(7)+3)=-sk(ip(3)+1)
    sk(ip(9)+7)=sk(ip(3)+1)
!
!
!
!     6/ COUPLAGE FLEXIONS XOZ - XOY
    sk(ip(3)+2)=-ks23*b2b2
    sk(ip(9)+2)=-sk(ip(3)+2)
    sk(ip(8)+3)=-sk(ip(3)+2)
    sk(ip(9)+8)=sk(ip(3)+2)
!
    sk(ip(5)+2)=ks23*b2b3
    sk(ip(6)+3)=-sk(ip(5)+2)
    sk(ip(11)+2)=ks23*b2b4
    sk(ip(12)+3)=-sk(ip(11)+2)
    sk(ip(8)+5)=-sk(ip(5)+2)
    sk(ip(9)+6)=sk(ip(5)+2)
    sk(ip(11)+8)=-sk(ip(11)+2)
    sk(ip(12)+9)=sk(ip(11)+2)
!
    sk(ip(6)+5)=ks23*b3b3
    sk(ip(12)+11)=ks23*b4b4
!
    sk(ip(12)+5)=ks23*b3b4
    sk(ip(11)+6)=sk(ip(12)+5)
!
end subroutine
