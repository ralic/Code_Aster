      SUBROUTINE CAL2FF(ALIAS,XI,YI,XIN,YIN,TN,AJ)
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
C        AU POINT DE COORDONNEES XI,YI
C
C ENTREES  ---> NNO         : NOMBRE DE NOEUDS
C          ---> ALIAS       : NOM D'ALIAS DE L'ELEMENT
C          ---> XI,YI       : POINT DE CALCUL DES F FORMES ET DERIVEES
C          ---> XIN,YIN     : COORDONNEES INTRINSEQUES
C
C SORTIES  <--- TN  : FONCTIONS DE FORMES EN XI,YI
C          <--- AJ  : DERIVEES DES F FORMES EN XI,YI
C.......................................................................
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8  ALIAS
      REAL*8       TN(1),AJ(2,1),XIN(1),YIN(1)
C_______________________________________________________________________
C
      IF ( ALIAS .EQ. 'QUAD4   ' ) THEN
C
        UNS4 = 1.D00 / 4.D00
        UN   = 1.D00
        DO 1 I=1,4
         X0=XI*XIN(I)
         Y0=YI*YIN(I)
           TN(I)  = (UN+X0) * (UN+Y0) * UNS4
           AJ(1,I) = XIN(I) * (UN+Y0) * UNS4
           AJ(2,I) = YIN(I) * (UN+X0) * UNS4
1       CONTINUE
C_______________________________________________________________________
 
      ELSE IF ( ALIAS.EQ.'QUAD6   ' ) THEN

        UNS4 = 1.D00 / 4.D00
        UNS2 = 1.D00 / 2.D00
        UN   = 1.D00
        DE   = 2.D00
C
C   NOEUDS SOMMETS
C
      DO 2 I=1,4
         X0=XI*XIN(I)
         Y0=YI*YIN(I)
         TN(I)   = (UN+X0) * X0 * (UN+Y0) * UNS4
         AJ(1,I) = (DE*X0+UN)   * (UN+Y0) * XIN(I) * UNS4
         AJ(2,I) = (UN+X0) * X0           * YIN(I) * UNS4
2     CONTINUE
C
C   NOEUDS MILIEUX
C
      DO 3 I=5,6
         Y0=YI*YIN(I)
         TN(I)   =  (UN-XI*XI) * (UN+Y0) * UNS2
         AJ(1,I) =     -DE*XI  * (UN+Y0) * UNS2
         AJ(2,I) =  (UN-XI*XI) *  YIN(I) * UNS2
3     CONTINUE
C_______________________________________________________________________


      ENDIF
      END
