      SUBROUTINE CAL3FF(ALIAS,XI,YI,ZI,XIN,YIN,ZIN,TN,AJ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/01/95   AUTEUR ACBHHJA G.JACQUART 
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
C.......................................................................
C
C BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
C        AU POINT DE COORDONNEES XI,YI,ZI
C
C ENTREES  ---> NNO         : NOMBRE DE NOEUDS
C          ---> ALIAS       : NOM D'ALIAS DE L'ELEMENT
C          ---> XI,YI,ZI    : POINT DE CALCUL DES F FORMES ET DERIVEES
C          ---> XIN,YIN,ZIN : COORDONNEES INTRINSEQUES
C
C SORTIES  <--- TN  : FONCTIONS DE FORMES EN XI,YI,ZI
C          <--- AJ  : DERIVEES DES F FORMES EN XI,YI,ZI
C.......................................................................
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8  ALIAS
      REAL*8       TN(1),AJ(3,1),XIN(1),YIN(1),ZIN(1)
C_______________________________________________________________________
C
      IF ( ALIAS .EQ. 'HEXA8' ) THEN
C
         UNS8 = 1.D00 / 8.D00
         UN   = 1.D00
         DO 10 I=1,8
            X0=XI*XIN(I)
            Y0=YI*YIN(I)
            Z0=ZI*ZIN(I)
            TN(I)  = (UN+X0) * (UN+Y0) * (UN+Z0) * UNS8
            AJ(1,I) = XIN(I) * (UN+Y0) * (UN+Z0) * UNS8
            AJ(2,I) = YIN(I) * (UN+X0) * (UN+Z0) * UNS8
            AJ(3,I) = ZIN(I) * (UN+X0) * (UN+Y0) * UNS8
10       CONTINUE
C_______________________________________________________________________
C
      ELSE IF ( ALIAS .EQ. 'PENTA6' ) THEN
C
         AL1=XI
         AL2=YI
         AL3=(1.D00-YI-XI)
         DO 20 I=1,2
            II=3*(I-1)
            TN(II+1)   = AL1 * (1.D00 + ZI*ZIN(II+1)  )*0.5D00
            TN(II+2)   = AL2 * (1.D00 + ZI*ZIN(II+2)  )*0.5D00
            TN(II+3)   = AL3 * (1.D00 + ZI*ZIN(II+3)  )*0.5D00
            AJ(1,II+1) = (1.D00 + ZI*ZIN(II+1)) * 0.5D00
            AJ(2,II+1) = 0.D+00
            AJ(3,II+1) = AL1 * ZIN(II+1) * 0.5D00
            AJ(1,II+2) = 0.D+00
            AJ(2,II+2) = (1.D00 + ZI * ZIN(II+2)) * 0.5D00
            AJ(3,II+2) = AL2 * ZIN(II+2) * 0.5D00
            AJ(1,II+3) = -(1.D00 + ZI * ZIN(II+3)) * 0.5D00
            AJ(2,II+3) = -(1.D00 + ZI * ZIN(II+3)) * 0.5D00
            AJ(3,II+3) = AL3 * ZIN(II+3) * 0.5D00
20       CONTINUE
C_______________________________________________________________________
C
      ELSE IF ( ALIAS.EQ.'HEXA16' ) THEN
C
         UN   = 1.D00
         DE   = 2.D00
         UNS4 = UN*0.250D00
         UNS8 = UN*0.125D00
         DO 30 I = 1,4  
            X0  = XI*XIN(I)
            Y0  = YI*YIN(I)
            TNP = (X0+UN)* (Y0+UN)*(X0+Y0-UN) * UNS8
            AJ1 = XIN(I) * (Y0+UN)*(DE*X0+Y0) * UNS8
            AJ2 = YIN(I) * (X0+UN)*(DE*Y0+X0) * UNS8
            DO 31 K=0,1
               KK         = 4*K
               Z0         = ZI*ZIN(KK+I)
               TN(KK+I)   = TNP*(UN+Z0)
               AJ(1,KK+I) = AJ1*(UN+Z0)
               AJ(2,KK+I) = AJ2*(UN+Z0)
               AJ(3,KK+I) = TNP*ZIN(KK+I)
31          CONTINUE 
30       CONTINUE 
C
         UXXI =  UN-XI*XI
         UYYI =  UN-YI*YI
         DXI  = -DE*XI
         DYI  = -DE*YI
         DO 32 K=0,1
            KK        = 4*K
            TN( 9+KK) = UXXI*(UN+YI*YIN( 9+KK))*(UN+ZI*ZIN( 9+KK))*UNS4
            TN(10+KK) = UYYI*(UN+XI*XIN(10+KK))*(UN+ZI*ZIN(10+KK))*UNS4
            TN(11+KK) = UXXI*(UN+YI*YIN(11+KK))*(UN+ZI*ZIN(11+KK))*UNS4
            TN(12+KK) = UYYI*(UN+XI*XIN(12+KK))*(UN+ZI*ZIN(12+KK))*UNS4
 
            AJ(1, 9+KK) = DXI*(UN+YI*YIN( 9+KK))*(UN+ZI*ZIN( 9+KK))*UNS4
            AJ(2,10+KK) = DYI*(UN+XI*XIN(10+KK))*(UN+ZI*ZIN(10+KK))*UNS4
            AJ(1,11+KK) = DXI*(UN+YI*YIN(11+KK))*(UN+ZI*ZIN(11+KK))*UNS4
            AJ(2,12+KK) = DYI*(UN+XI*XIN(12+KK))*(UN+ZI*ZIN(12+KK))*UNS4
   
            AJ(2, 9+KK) = UXXI*YIN( 9+KK)*(UN+ZI*ZIN( 9+KK))*UNS4
            AJ(1,10+KK) = UYYI*XIN(10+KK)*(UN+ZI*ZIN(10+KK))*UNS4
            AJ(2,11+KK) = UXXI*YIN(11+KK)*(UN+ZI*ZIN(11+KK))*UNS4
            AJ(1,12+KK) = UYYI*XIN(12+KK)*(UN+ZI*ZIN(12+KK))*UNS4

            AJ(3, 9+KK) = UXXI*(UN+YI*YIN( 9+KK))*ZIN( 9+KK)*UNS4
            AJ(3,10+KK) = UYYI*(UN+XI*XIN(10+KK))*ZIN(10+KK)*UNS4
            AJ(3,11+KK) = UXXI*(UN+YI*YIN(11+KK))*ZIN(11+KK)*UNS4
            AJ(3,12+KK) = UYYI*(UN+XI*XIN(12+KK))*ZIN(12+KK)*UNS4
32       CONTINUE
C_______________________________________________________________________
C
      ELSE IF ( ALIAS.EQ.'PENTA12' ) THEN
C
         ZE   = 0.D00
         UN   = 1.D00
         DE   = 2.D00
         QU   = 4.D00
         UNS2 = UN*0.500D00
         A = UN-XI-YI
         DO 41 K=0,1
            KK         = 3*K

            TN(1+KK)   = -XI * (UN - DE*XI)  * (UN+ZI*ZIN(1+KK)) * UNS2
            TN(2+KK)   = -YI * (UN - DE*YI)  * (UN+ZI*ZIN(2+KK)) * UNS2
            TN(3+KK)   = -A  * (UN - DE*A )  * (UN+ZI*ZIN(3+KK)) * UNS2
            AJ(1,1+KK) = (-UN + QU * XI)     * (UN+ZI*ZIN(1+KK)) * UNS2
            AJ(1,2+KK) =   ZE              
            AJ(1,3+KK) = ( UN - QU * A )     * (UN+ZI*ZIN(3+KK)) * UNS2
            AJ(2,1+KK) =   ZE  
            AJ(2,2+KK) = (-UN + QU * YI)     * (UN+ZI*ZIN(2+KK)) * UNS2
            AJ(2,3+KK) = ( UN - QU * A )     * (UN+ZI*ZIN(3+KK)) * UNS2
            AJ(3,1+KK) =  -XI * (UN - DE*XI) * ZIN(1+KK)         * UNS2
            AJ(3,2+KK) =  -YI * (UN - DE*YI) * ZIN(2+KK)         * UNS2
            AJ(3,3+KK) =  -A  * (UN - DE*A ) * ZIN(3+KK)         * UNS2

            TN(7+KK)   =   DE * XI * YI      * (UN+ZI*ZIN(7+KK))
            TN(8+KK)   =   DE * YI * A       * (UN+ZI*ZIN(8+KK))
            TN(9+KK)   =   DE * XI * A       * (UN+ZI*ZIN(9+KK))
            AJ(1,7+KK) =   DE * YI           * (UN+ZI*ZIN(7+KK))
            AJ(1,8+KK) =  -DE * YI           * (UN+ZI*ZIN(8+KK))
            AJ(1,9+KK) =   DE * (A - XI)     * (UN+ZI*ZIN(9+KK))
            AJ(2,7+KK) =   DE * XI           * (UN+ZI*ZIN(7+KK))
            AJ(2,8+KK) =   DE * (A - YI)     * (UN+ZI*ZIN(8+KK))
            AJ(2,9+KK) =  -DE * XI           * (UN+ZI*ZIN(9+KK))
            AJ(3,7+KK) =   DE * XI * YI      * ZIN(7+KK)
            AJ(3,8+KK) =   DE * YI * A       * ZIN(8+KK)
            AJ(3,9+KK) =   DE * XI * A       * ZIN(9+KK)

41       CONTINUE

C_______________________________________________________________________
C
      ELSE
C_______________________________________________________________________
C
         CALL UTMESS('F','CAL3FF','L''ALIAS N''EST PAS DEFINI ')
C_______________________________________________________________________
C
      ENDIF
      END
