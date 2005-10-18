      SUBROUTINE Q4GBC  ( QSI, ETA, JACOB, CARAQ4, BC )
      IMPLICIT  NONE
      REAL*8     QSI, ETA, JACOB(*), CARAQ4(*), BC(2,12)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2005   AUTEUR CIBHHLV L.VIVAN 
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
C     --------------------------------------------------------
C     MATRICE BC(2,12) AU POINT QSI ETA POUR L'ELEMENT Q4GAMMA
C     --------------------------------------------------------
      INTEGER  J
      REAL*8  VJ11, VJ12, VJ21, VJ22, PETA, META, PQSI, MQSI
      REAL*8  X5, X6, X7, X8 , Y5, Y6, Y7, Y8
      REAL*8  BQSI(12) , BETA(12)
C     ------------------------------------------------------------------
      VJ11 = JACOB(1)
      VJ12 = JACOB(2)
      VJ21 = JACOB(3)
      VJ22 = JACOB(4)
C
      X5   = CARAQ4(1)
      X6   = CARAQ4(2)
      X7   = CARAQ4(3)
      X8   = CARAQ4(4)
      Y5   = CARAQ4(5)
      Y6   = CARAQ4(6)
      Y7   = CARAQ4(7)
      Y8   = CARAQ4(8)
C
      PETA = (1.D0 + ETA) / 8.D0
      META = (1.D0 - ETA) / 8.D0
      PQSI = (1.D0 + QSI) / 8.D0
      MQSI = (1.D0 - QSI) / 8.D0
C     --------- CALCUL DE BQSI -----------------------
      BQSI( 1) = -2.D0 * META
      BQSI( 2) =   X5  * META
      BQSI( 3) =   Y5  * META
      BQSI( 4) =  2.D0 * META
      BQSI( 5) =   X5  * META
      BQSI( 6) =   Y5  * META
      BQSI( 7) =  2.D0 * PETA
      BQSI( 8) = - X7  * PETA
      BQSI( 9) = - Y7  * PETA
      BQSI(10) = -2.D0 * PETA
      BQSI(11) = - X7  * PETA
      BQSI(12) = - Y7  * PETA
      BETA( 1) = -2.D0 * MQSI
      BETA( 2) = - X8  * MQSI
      BETA( 3) = - Y8  * MQSI
      BETA( 4) = -2.D0 * PQSI
      BETA( 5) =   X6  * PQSI
      BETA( 6) =   Y6  * PQSI
      BETA( 7) =  2.D0 * PQSI
      BETA( 8) =   X6  * PQSI
      BETA( 9) =   Y6  * PQSI
      BETA(10) =  2.D0 * MQSI
      BETA(11) = - X8  * MQSI
      BETA(12) = - Y8  * MQSI
C     --------------------- CALCUL DE BC ------------------------------
      DO 100 J = 1, 12
        BC(1,J) = VJ11 * BQSI(J) + VJ12 * BETA(J)
        BC(2,J) = VJ21 * BQSI(J) + VJ22 * BETA(J)
C
  100 CONTINUE
      END
