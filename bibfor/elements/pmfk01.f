      SUBROUTINE PMFK01(CARS,GXJX,XL,SK)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/11/2001   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT NONE
C    -------------------------------------------------------------------
C    * CE SOUS PROGRAMME CALCULE LA MATRICE DE RAIDEUR DE L'ELEMENT DE
C    POUTRE MULTIFIBRE DROITE A SECTION CONSTANTE.

C    * DESCRIPTION DE L'ELEMENT:
C      C'EST UN ELEMENT A DEUX NOEUDS ET A SIX DEGRES DE LIBERTES PAR
C      NOEUDS (3 DEPLACEMENTS ET 3 ROTATIONS).

C    * REMARQUE :
C      LA MATRICE EST STOCKEE TRIANGULAIRE INFERIEURE DANS UN TABLEAU
C      UNICOLONNE
C    -------------------------------------------------------------------
C  DONNEES NON MODIFIEES

C IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
C IN -------------------------------------------------------------------
C IN R*8  ! CARS   !     6   ! CARACTERISTIQUES INTEGREES DE LA SECTION
C              INT(VAR DS) = INTEGRALE DE VAR SUR LA SECTION
C              E : MODULE D'UNE FIBRE
C              Y,Z : COORDONNEES DE LA FIBRE

C IN R*8  ! CARS(1)!     -   ! KS11   = INT(E DS)
C IN R*8  ! CARS(2)!     -   ! -KS13  = INT(E.Y DS)
C IN R*8  ! CARS(3)!     -   ! KS12  = INT(E.Z DS)
C IN R*8  ! CARS(4)!     -   ! KS33  = INT(E.Y.Y DS)
C IN R*8  ! CARS(5)!     -   ! KS22  = INT(E.Z.Z DS)
C IN R*8  ! CARS(6)!     -   ! -KS23 = INT(E.Y.Z DS)
C IN R*8  ! GXJX   !     -   ! G FOIS LA CONSTANTE DE TORSION


C OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
C OUT ------------------------------------------------------------------
C OUT R*8 !  SK   ! (78)    ! MATRICE ELEMENTAIRE UNICOLONNE


C LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
C LOC ------------------------------------------------------------------
C LOC I   ! IP     !   12    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
C     ------------------------------------------------------------------
      INTEGER I,IP(12)
      REAL*8 XL,SK(*),CARS(6),GXJX
      REAL*8 KS11,KS12,KS13,KS22,KS33,KS23
      REAL*8 ZERO,UN,DEUX,QUATRE,SIX,DOUZE,CO1,CO2,CO4,CO6,CO12
      PARAMETER (ZERO=0.0D+0,UN=1.D+0,DEUX=2.D+0,QUATRE=4.D+0,SIX=6.D+0,
     &          DOUZE=12.D+0)
      DATA IP/0,1,3,6,10,15,21,28,36,45,55,66/

      DO 10,I = 1,78
        SK(I) = ZERO
   10 CONTINUE

      CO1 = UN/XL
      CO2 = DEUX/XL
      CO4 = QUATRE/XL
      CO6 = SIX/ (XL*XL)
      CO12 = DOUZE/ (XL*XL*XL)

C --- POUR ETRE PLUS PARLANT
C --- ATTENTION : SIGNE MOINS POUR KS13 ET KS23
      KS11 = CARS(1)
      KS13 = -CARS(2)
      KS12 = CARS(3)
      KS33 = CARS(4)
      KS22 = CARS(5)
      KS23 = -CARS(6)

C     1/ TRACTION - COMPRESSION
      SK(1) = KS11*CO1
      SK(IP(7)+1) = -SK(1)
      SK(IP(7)+7) = SK(1)

C     2/ FLEXION
C        FLEXION DANS LE PLAN XOY
      SK(IP(2)+2) = KS33*CO12
      SK(IP(6)+2) = KS33*CO6
      SK(IP(8)+2) = -SK(IP(2)+2)
      SK(IP(12)+2) = SK(IP(6)+2)
      SK(IP(6)+6) = KS33*CO4
      SK(IP(8)+6) = -SK(IP(6)+2)
      SK(IP(12)+6) = KS33*CO2
      SK(IP(8)+8) = SK(IP(2)+2)
      SK(IP(12)+8) = -SK(IP(6)+2)
      SK(IP(12)+12) = SK(IP(6)+6)

C     3/ FLEXION DANS LE PLAN XOZ
      SK(IP(3)+3) = KS22*CO12
      SK(IP(5)+3) = -KS22*CO6
      SK(IP(9)+3) = -SK(IP(3)+3)
      SK(IP(11)+3) = SK(IP(5)+3)
      SK(IP(5)+5) = KS22*CO4
      SK(IP(9)+5) = -SK(IP(5)+3)
      SK(IP(11)+5) = KS22*CO2
      SK(IP(9)+9) = SK(IP(3)+3)
      SK(IP(11)+9) = -SK(IP(5)+3)
      SK(IP(11)+11) = SK(IP(5)+5)

C     4/ TORSION
      SK(IP(4)+4) = GXJX/XL
      SK(IP(10)+4) = -SK(IP(4)+4)
      SK(IP(10)+10) = SK(IP(4)+4)

C     5/ COUPLAGE AXIAL-FLEXIONS
      SK(IP(6)+1) = KS13*CO1
      SK(IP(12)+1) = -SK(IP(6)+1)
      SK(IP(12)+7) = SK(IP(6)+1)
      SK(IP(7)+6) = -SK(IP(6)+1)
      SK(IP(5)+1) = KS12*CO1
      SK(IP(11)+1) = -SK(IP(5)+1)
      SK(IP(11)+7) = SK(IP(5)+1)
      SK(IP(7)+5) = -SK(IP(5)+1)

C     5/ COUPLAGE FLEXIONS XOZ - XOY
      SK(IP(3)+2) = -KS23*CO12
      SK(IP(9)+2) = -SK(IP(3)+2)
      SK(IP(8)+3) = -SK(IP(3)+2)
      SK(IP(9)+8) = SK(IP(3)+2)

      SK(IP(5)+2) = KS23*CO6
      SK(IP(6)+3) = -SK(IP(5)+2)
      SK(IP(11)+2) = SK(IP(5)+2)
      SK(IP(12)+3) = -SK(IP(5)+2)
      SK(IP(8)+5) = -SK(IP(5)+2)
      SK(IP(9)+6) = SK(IP(5)+2)
      SK(IP(11)+8) = -SK(IP(5)+2)
      SK(IP(12)+9) = SK(IP(5)+2)

      SK(IP(6)+5) = KS23*CO4
      SK(IP(12)+11) = SK(IP(6)+5)

      SK(IP(12)+5) = KS23*CO2
      SK(IP(11)+6) = SK(IP(12)+5)

C LES 28 AUTRES TERMES SONT NULS ...
C(SAUF SI ON MET DU COUPLAGE TORSION-FLEXION)

      END
