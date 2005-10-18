      SUBROUTINE DKQNIW ( QSI, ETA, CARAQ4, WKQ )
      IMPLICIT  NONE
      REAL*8   QSI, ETA, CARAQ4(*), WKQ(12)
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
C     ------------------------------------------------------------------
C     FONCTIONS D'INTERPOLATION DE LA FLECHE POUR L'ELEMENT DKQ
C     ------------------------------------------------------------------
      REAL*8  PQSI,MQSI , PETA,META , QSIC,ETAC
      REAL*8  X5,X6,X7,X8 , Y5,Y6,Y7,Y8
      REAL*8  N(12)
C     ------------------------------------------------------------------
      X5  = CARAQ4(1)
      X6  = CARAQ4(2)
      X7  = CARAQ4(3)
      X8  = CARAQ4(4)
      Y5  = CARAQ4(5)
      Y6  = CARAQ4(6)
      Y7  = CARAQ4(7)
      Y8  = CARAQ4(8)
C
      PETA = 1.D0 + ETA
      META = 1.D0 - ETA
      PQSI = 1.D0 + QSI
      MQSI = 1.D0 - QSI
      ETAC = 1.D0 - ETA * ETA
      QSIC = 1.D0 - QSI * QSI
C
C ----- FONCTIONS D'INTERPOLATION DANS LE REPERE REDUIT ------------
      N(1)  =   MQSI * META / 8.D0 * (QSIC + ETAC - QSI - ETA)
      N(2)  =   MQSI * META / 8.D0 * QSIC
      N(3)  =   MQSI * META / 8.D0 * ETAC
      N(4)  =   PQSI * META / 8.D0 * (QSIC + ETAC + QSI - ETA)
      N(5)  = - PQSI * META / 8.D0 * QSIC
      N(6)  =   PQSI * META / 8.D0 * ETAC
      N(7)  =   PQSI * PETA / 8.D0 * (QSIC + ETAC + QSI + ETA)
      N(8)  = - PQSI * PETA / 8.D0 * QSIC
      N(9)  = - PQSI * PETA / 8.D0 * ETAC
      N(10) =   MQSI * PETA / 8.D0 * (QSIC + ETAC - QSI + ETA)
      N(11) =   MQSI * PETA / 8.D0 * QSIC
      N(12) = - MQSI * PETA / 8.D0 * ETAC
C ----- FONCTIONS D'INTERPOLATION DANS LE REPERE LOCAL -------------
      WKQ(1)  = N(1)
      WKQ(2)  = (- X5*N(2)  + X8*N(3))  / 2.D0
      WKQ(3)  = (- Y5*N(2)  + Y8*N(3))  / 2.D0
      WKQ(4)  = N(4)
      WKQ(5)  = (- X5*N(5)  - X6*N(6))  / 2.D0
      WKQ(6)  = (- Y5*N(5)  - Y6*N(6))  / 2.D0
      WKQ(7)  = N(7)
      WKQ(8)  = (  X7*N(8)  - X6*N(9))  / 2.D0
      WKQ(9)  = (  Y7*N(8)  - Y6*N(9))  / 2.D0
      WKQ(10) = N(10)
      WKQ(11) = (  X7*N(11) + X8*N(12)) / 2.D0
      WKQ(12) = (  Y7*N(11) + Y8*N(12)) / 2.D0
C
      END
