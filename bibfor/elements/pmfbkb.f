      SUBROUTINE PMFBKB(CARS,B,WI,GXJX,SK)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/05/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT NONE
C    -------------------------------------------------------------------
C     CALCUL DE : WI BT KS B
C     KS = MATRICE DE SECTION (ISSUE DE L'INTEGRATION SUR LES FIBRES)
C     WI = POIDS DU PT DE GAUSS EN COURS
C     B = MATRICE B A CE POINT DE GAUSS ET BT SA TRANSPOSEE
C
C    * REMARQUE :
C      LA MATRICE EST STOCKEE TRIANGULAIRE INFERIEURE DANS UN TABLEAU
C      UNICOLONNE
C    -------------------------------------------------------------------
C  DONNEES NON MODIFIEES
C
C IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
C IN -------------------------------------------------------------------
C IN R*8  ! CARS   !     6   ! CARACTERISTIQUES INTEGREES DE LA SECTION
C              INT(VAR DS) = INTEGRALE DE VAR SUR LA SECTION
C              E : MODULE D'UNE FIBRE
C              Y,Z : COORDONNEES DE LA FIBRE
C
C IN R*8  ! CARS(1)!     -   ! KS11   = INT(E DS)
C IN R*8  ! CARS(2)!     -   ! -KS13  = INT(E.Y DS)
C IN R*8  ! CARS(3)!     -   ! KS12  = INT(E.Z DS)
C IN R*8  ! CARS(4)!     -   ! KS33  = INT(E.Y.Y DS)
C IN R*8  ! CARS(5)!     -   ! KS22  = INT(E.Z.Z DS)
C IN R*8  ! CARS(6)!     -   ! -KS23 = INT(E.Y.Z DS)
C IN R*8  ! GXJX   !     -   ! G FOIS LA CONSTANTE DE TORSION
C
C IN R*8  B(4) : LES QUATRES VALEURS NON NULLE ET DIFFERENTES DE B

C OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
C OUT ------------------------------------------------------------------
C OUT R*8 !  SK   ! (78)    ! MATRICE ELEMENTAIRE UNICOLONNE
C
C
C LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
C LOC ------------------------------------------------------------------
C LOC I   ! IP     !   12    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
C     ------------------------------------------------------------------
      INTEGER IP(12),I
      REAL*8 SK(78),CARS(6),GXJX,B(4),WI
      REAL*8 KS11,KS12,KS13,KS22,KS33,KS23
      REAL*8 B1B1,B1B2,B1B3,B1B4,B2B2,B2B3,B2B4,B3B3,B3B4,B4B4
      DATA IP/0,1,3,6,10,15,21,28,36,45,55,66/

C SK MIS A ZERO AU DEBUT DE TE0535
C LES 20 TERMES DE COUPLAGE TORSION-QUELQUECHOSE SONT NULS
C ET DOIVENT ETRE INITIALISES UNE FOIS
C ET PUIS CA ARRANGE LE SEGMENTATION VIOLATION ...

      B1B1=B(1)*B(1)
      B1B2=B(1)*B(2)
      B1B3=B(1)*B(3)
      B1B4=B(1)*B(4)
      B2B2=B(2)*B(2)
      B2B3=B(2)*B(3)
      B2B4=B(2)*B(4)
      B3B3=B(3)*B(3)
      B3B4=B(3)*B(4)
      B4B4=B(4)*B(4)

C --- POUR ETRE PLUS PARLANT
C --- ATTENTION : SIGNE MOINS POUR KS13 ET KS23
C --- MULTIPLICATION PAR LE POIDS
      KS11=CARS(1)*WI
      KS13=-CARS(2)*WI
      KS12=CARS(3)*WI
      KS33=CARS(4)*WI
      KS22=CARS(5)*WI
      KS23=-CARS(6)*WI

C     1/ TRACTION - COMPRESSION
      SK(1) = KS11*B1B1
      SK(IP(7)+1) = -SK(1)
      SK(IP(7)+7) = SK(1)
C
C     2/ FLEXION
C        FLEXION DANS LE PLAN XOY
      SK(IP(2)+2) = KS33*B2B2
      SK(IP(6)+2) = KS33*B2B3
      SK(IP(8)+2) = -SK(IP(2)+2)
      SK(IP(12)+2) = KS33*B2B4
      SK(IP(6)+6) = KS33*B3B3
      SK(IP(8)+6) = -SK(IP(6)+2)
      SK(IP(12)+6) = KS33*B3B4
      SK(IP(8)+8) = SK(IP(2)+2)
      SK(IP(12)+8) = -SK(IP(12)+2)
      SK(IP(12)+12) = KS33*B4B4
C
C     3/ FLEXION DANS LE PLAN XOZ
      SK(IP(3)+3) = KS22*B2B2
      SK(IP(5)+3) = -KS22*B2B3
      SK(IP(9)+3) = -SK(IP(3)+3)
      SK(IP(11)+3) = -KS22*B2B4
      SK(IP(5)+5) = KS22*B3B3
      SK(IP(9)+5) = -SK(IP(5)+3)
      SK(IP(11)+5) = KS22*B3B4
      SK(IP(9)+9) = SK(IP(3)+3)
      SK(IP(11)+9) = -SK(IP(11)+3)
      SK(IP(11)+11) = KS22*B4B4
C
C     4/ TORSION
      SK(IP(4)+4) = GXJX*WI*B1B1
      SK(IP(10)+4) = -SK(IP(4)+4)
      SK(IP(10)+10) = SK(IP(4)+4)
C
C     5/ COUPLAGE AXIAL-FLEXIONS
      SK(IP(6)+1)=-KS13*B1B3
      SK(IP(12)+1)=-KS13*B1B4
      SK(IP(12)+7)=-SK(IP(12)+1)
      SK(IP(7)+6)=-SK(IP(6)+1)
      SK(IP(5)+1)=-KS12*B1B3
      SK(IP(11)+1)=-KS12*B1B4
      SK(IP(11)+7)=-SK(IP(11)+1)
      SK(IP(7)+5)=-SK(IP(5)+1)

C     5BIS/ COUPLAGE TRANCHANT NORMAL
      SK(IP(2)+1)=-KS13*B1B2
      SK(IP(8)+1)=-SK(IP(2)+1)
      SK(IP(7)+2)=-SK(IP(2)+1)
      SK(IP(8)+7)=SK(IP(2)+1)
      SK(IP(3)+1)=KS12*B1B2
      SK(IP(9)+1)=-SK(IP(3)+1)
      SK(IP(7)+3)=-SK(IP(3)+1)
      SK(IP(9)+7)=SK(IP(3)+1)



C     6/ COUPLAGE FLEXIONS XOZ - XOY
      SK(IP(3)+2)=-KS23*B2B2
      SK(IP(9)+2)=-SK(IP(3)+2)
      SK(IP(8)+3)=-SK(IP(3)+2)
      SK(IP(9)+8)=SK(IP(3)+2)

      SK(IP(5)+2)=KS23*B2B3
      SK(IP(6)+3)=-SK(IP(5)+2)
      SK(IP(11)+2)=KS23*B2B4
      SK(IP(12)+3)=-SK(IP(11)+2)
      SK(IP(8)+5)=-SK(IP(5)+2)
      SK(IP(9)+6)=SK(IP(5)+2)
      SK(IP(11)+8)=-SK(IP(11)+2)
      SK(IP(12)+9)=SK(IP(11)+2)

      SK(IP(6)+5)=KS23*B3B3
      SK(IP(12)+11)=KS23*B4B4

      SK(IP(12)+5)=KS23*B3B4
      SK(IP(11)+6)=SK(IP(12)+5)

C LES 20 TERMES DE COUPLAGE TORSION-QUELQUECHOSE SONT NULS

      END
