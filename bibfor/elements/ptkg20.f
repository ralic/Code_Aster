      SUBROUTINE PTKG20(SF,A,XIZ,XIY,IYR,IZR,L,EY,EZ,DSM)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8                  A,XIZ,XIY,EY,EZ,L,IYR,IZR,YRSIZ,ZRSIY
      REAL*8                  SF(*),                        DSM(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
C INSPI PTKG00
C     CALCUL DE LA MATRICE DE RAIDEUR GEOMETRIQUE  (POU_D_TG)
C ======================================================================
C     ------------------------------------------------------------------
C IN  SF     - (14) COMPOSANTES EFFORTS DANS LES ELEMENTS
C IN  A          - AIRE DE LA SECTION DROITE INITIALE
C IN  XIZ        - MOMENT D INERTIE / Z PRINCIPAL  SECTION INITIALE
C IN  XIY        - MOMENT D INERTIE / Y PRINCIPAL  SECTION INITIALE
C IN  EY         - COMPOSANTE SUIVANT Y PRINCIPAL DE GT.
C IN  EZ         - COMPOSANTE SUIVANT Z PRINCIPAL DE GT.
C IN  L          - LONGUEUR DE L ELEMENT
C OUT DSM    - (105) MATRICE DE RIGIDITE GEOMETRIQUE
C     ------------------------------------------------------------------
      REAL*8  FXB,MXB,MYA,MYB,MZA,MZB,KTILD,L2,UN2,USEZ,USEY
      REAL*8  YMAPB,ZMAPB
      INTEGER IP(14)
      DATA  IP/ 0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91 /
C     ------------------------------------------------------------------
C
C        FXA=FX(A), FXB=FX(B),... EFFORTS DANS LA POUTRE
C        (ORIENTEE DE A VERS B) EN A ET B
C
      FXB    =   SF(8 )
      MYA    =   SF(5 )
      MZA    =   SF(6 )
      MYB    =   SF(12)
      MZB    =   SF(13)
C
C     1/ AFFECTATION DES VALEURS :
C     ----------------------------
C     --- TRIANGULAIRE SUPERIEURE DE LA MATRICE ---
C
      UN2            = 1.20000D0
      ZP1            = 0.10000D0
       YRSIZ         =       -IYR/XIZ +2.D0*EY
       ZRSIY         =        IZR/XIY -2.D0*EZ
       USEZ          =  UN2*FXB*EZ/L
       USEY          =  UN2*FXB*EY/L
       YMAB          = 0.5000D0*(MYA - MYB)
       ZMAB          = 0.5000D0*(MZA - MZB)
       YMAPB         = (MYA + MYB)/L
       ZMAPB         = (MZA + MZB)/L
       KTILD         =   FXB*((XIY+XIZ)/A + EY*EY + EZ*EZ)
C
C
      DSM(2+IP(2 ))   =  UN2*FXB / L
      DSM(2+IP(4 ))   =  USEZ           +UN2*YMAB/L
     +                  +0.500D0*YMAPB
      DSM(2+IP(6 ))   =  FXB*ZP1
      DSM(2+IP(7 ))   =  FXB*EZ*ZP1     -ZP1*MYB
     +                  +ZP1*L*YMAPB
      DSM(2+IP(9 ))   = -UN2*FXB / L
      DSM(2+IP(11))   = -USEZ           -UN2*YMAB/L
     +                  +0.500D0*YMAPB
      DSM(2+IP(13))   =  FXB*ZP1
      DSM(2+IP(14))   =  FXB*EZ * ZP1   +ZP1*MYA
     +                  -ZP1*L*YMAPB
C
      DSM(3+IP(3 ))   =  UN2 * FXB / L
      DSM(3+IP(4 ))   = -USEY           +UN2*ZMAB/L
     +                  +0.500D0*ZMAPB
      DSM(3+IP(5 ))   = -FXB*ZP1
      DSM(3+IP(7 ))   = -EY*FXB*ZP1     -ZP1*MZB
     +                  +ZP1*L*ZMAPB
      DSM(3+IP(10))   = -UN2 * FXB / L
      DSM(3+IP(11))   =  USEY           -UN2*ZMAB/L
     +                  +0.500D0*ZMAPB
      DSM(3+IP(12))   = -FXB*ZP1
      DSM(3+IP(14))   = -EY*FXB*ZP1     +ZP1*MZA
     +                    -ZP1*L*ZMAPB
C
      DSM(4+IP(4 ))   =  UN2*KTILD/L
     &                                  -UN2*ZMAB*YRSIZ/L
     &                                  -UN2*YMAB*ZRSIY/L
     +                -EY*ZMAPB+EZ*YMAPB
     +                         +0.5D0*(-(IYR/XIZ)*ZMAPB+(IZR/XIY)*YMAPB)
      DSM(4+IP(5 ))   =  FXB*EY*ZP1     +ZP1*MZB
     +                  +ZP1*L*ZMAPB
     +                  -0.5D0*MZA
      DSM(4+IP(6 ))   =  FXB*EZ*ZP1     -ZP1*MYB
     +                  -ZP1*L*YMAPB
     +                  +0.5D0*MYA
      DSM(4+IP(7 ))   =  KTILD*ZP1      +ZP1*MZB*YRSIZ+0.1D0*MYB*ZRSIY
      DSM(4+IP(9 ))   = -USEZ           -UN2*YMAB/L
     +                  -0.500D0*YMAPB
      DSM(4+IP(10))   =  USEY           -UN2*ZMAB/L
     +                  -0.500D0*ZMAPB
      DSM(4+IP(11))   = -DSM(4+IP(4))
      DSM(4+IP(12))   =  FXB*EY*ZP1     -ZP1*MZA
     +                    -ZP1*L*ZMAPB
      DSM(4+IP(13))   =  FXB*EZ*ZP1     +ZP1*MYA
     +                  +ZP1*L*YMAPB
      DSM(4+IP(14))   =  KTILD*ZP1      -ZP1*MYA*ZRSIY-0.1D0*MZA*YRSIZ
C
      DSM(5+IP(5 ))   =  2.D0*FXB*L/15.D0
      DSM(5+IP(7 ))   =  2.D0*L*FXB*EY/15.D0 -(3.D0*MZA-MZB)*L/30.D0
      DSM(5+IP(10))   =  FXB*ZP1
      DSM(5+IP(11))   = -EY*FXB*ZP1      -ZP1*MZB
     +                    -ZP1*L*ZMAPB
      DSM(5+IP(12))   = -FXB*L/30.D0
      DSM(5+IP(14))   = -L*FXB*EY/30.D0    +ZMAB*L/30.D0
     +                    +L*L*ZMAPB/60.000D0
C
      DSM(6+IP(6 ))   =  2.D0*FXB*L/15.D0
      DSM(6+IP(7 ))   =  2.D0*L*FXB*EZ/15.D0 +(3.D0*MYA-MYB)*L/30.D0
      DSM(6+IP(9 ))   = -FXB*ZP1
      DSM(6+IP(11))   = -FXB*EZ*ZP1      +ZP1*MYB
     +                  +ZP1*L*YMAPB
      DSM(6+IP(13))   = -FXB*L/30.D0
      DSM(6+IP(14))   = -L*FXB*EZ/30.D0    -YMAB*L/30.D0
     +                    -L*L*YMAPB/60.000D0
C
      DSM(7+IP(7 ))   =  2.D0*KTILD*L/15.D0 -(3.D0*MZA-MZB)*L*YRSIZ/
     &                         30.D0 -(3.D0*MYA-MYB)*L*ZRSIY/30.D0
      DSM(7+IP(9 ))   = -FXB*EZ*ZP1     +ZP1*MYB
     +                  -ZP1*L*YMAPB
      DSM(7+IP(10 ))   =  FXB*EY*ZP1    +ZP1*MZB
     +                    -ZP1*L*ZMAPB
      DSM(7+IP(11 ))   = -KTILD*ZP1     -ZP1*(MYB*ZRSIY+MZB*YRSIZ)
      DSM(7+IP(12 ))   = -EY*L*FXB/30.D0  +ZMAB*L/30.D0
     +                    -L*L*ZMAPB/60.000D0
      DSM(7+IP(13 ))   = -EZ*L*FXB/30.D0  -YMAB*L/30.D0
     +                    +L*L*YMAPB/60.000D0
      DSM(7+IP(14 ))   = -KTILD*L/30.D0   +(ZMAB*YRSIZ+YMAB*ZRSIY)*
     &                    L/30.D0
C
C
      DSM(9+IP(9 ))    =  UN2 * FXB / L
      DSM(9+IP(11))    =  USEZ           +UN2*YMAB/L
     +                   -0.500D0*YMAPB
      DSM(9+IP(13))    = -FXB*ZP1
      DSM(9+IP(14))    = -EZ*FXB*ZP1    -ZP1*MYA
     +                   +ZP1*L*YMAPB
C
      DSM(10+IP(10))   =  UN2 * FXB / L
      DSM(10+IP(11))   = -USEY          +UN2*ZMAB/L
     +                  -0.500D0*ZMAPB
      DSM(10+IP(12))   =  FXB*ZP1
      DSM(10+IP(14))   =  FXB*EY*ZP1    -ZP1*MZA
     +                    +ZP1*L*ZMAPB
C
      DSM(11+IP(11))   =  UN2 * KTILD/L -UN2*(ZMAB*YRSIZ+YMAB*ZRSIY)/L
     +                +EY*ZMAPB-EZ*YMAPB
     +                        +0.5D0*( (IYR/XIZ)*ZMAPB-(IZR/XIY)*YMAPB)
      DSM(11+IP(12))   = -FXB*EY*ZP1    +ZP1*MZA
     +                    +ZP1*L*ZMAPB
     +                    -0.5D0*MZB
      DSM(11+IP(13))   = -FXB*EZ*ZP1    -ZP1*MYA
     +                    -ZP1*L*YMAPB
     +                    +0.5D0*MYB
      DSM(11+IP(14))   = -KTILD*ZP1     +ZP1*(MZA*YRSIZ+MYA*ZRSIY)
C
      DSM(12+IP(12))   =  2.D0*FXB*L/15.D0
      DSM(12+IP(14))   =  2.D0*L*FXB*EY/15.D0-(MZA-3.D0*MZB)*L/30.D0
C
      DSM(13+IP(13))   =  2.D0*FXB*L/15.D0
      DSM(13+IP(14))   =  2.D0*EZ*FXB*L/15.D0+(MYA-3.D0*MYB)*L/30.D0
C
      DSM(14+IP(14))   =  2.D0*KTILD*L/15.D0-(MZA - 3.D0*MZB)*YRSIZ
     &                       *L/30.D0 -(MYA - 3.D0*MYB)*ZRSIY*L/30.D0
C
C
C     2/ CHANGEMENT DE VARIABLES DY(T),DZ(T) --> DY(G),DZ(G) (EXCENTR.)
C     -----------------------------------------------------------------
C
         CALL POUEX7(DSM(1),EY,EZ)
C
C
 9999 CONTINUE
      END
