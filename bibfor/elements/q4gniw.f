      SUBROUTINE Q4GNIW ( INT , R , DBA , WSQ )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/09/98   AUTEUR ADBHHPM P.MASSIN 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER  INT
      REAL*8   DBA(2,12)
      REAL*8   R(*)
      REAL*8   WSQ(12)
C     ------------------------------------------------------------------
C     INTERPOLATION DE LA FLECHE AU POINTS D'INTEGRATION POUR LE Q4G
C     ------------------------------------------------------------------
      REAL*8  QSI,ETA , PQSI,MQSI , PETA,META , QSIC,ETAC
      REAL*8  X5,X6,X7,X8 , Y5,Y6,Y7,Y8
      REAL*8  N(12)
C
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC
               PARAMETER (NPG   = 4)
               PARAMETER (NNO   = 4)
               PARAMETER (NC    = 4)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI + NPG + NNO + 2*NC)
               PARAMETER (LWGT  = LETA + NPG + NNO + 2*NC)
               PARAMETER (LXYC  = LWGT + NPG)
C     ------------------------------------------------------------------
      QSI = R(LQSI+INT-1)
      ETA = R(LETA+INT-1)
      X5  = R(LXYC)
      X6  = R(LXYC+1)
      X7  = R(LXYC+2)
      X8  = R(LXYC+3)
      Y5  = R(LXYC+4)
      Y6  = R(LXYC+5)
      Y7  = R(LXYC+6)
      Y8  = R(LXYC+7)
C
      PETA = 1.D0 + ETA
      META = 1.D0 - ETA
      PQSI = 1.D0 + QSI
      MQSI = 1.D0 - QSI
      ETAC = 1.D0 - ETA * ETA
      QSIC = 1.D0 - QSI * QSI
C
C     --- FONCTIONS D'INTERPOLATION DU DKQ DANS LE REPERE REDUIT -------
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
C
C     --- RAPPEL DE LA DISTORSION ( DBA ) ---------------
C
C     --- FONCTIONS D'INTERPOLATION DU DSQ DANS LE REPERE LOCAL --------
      DO 140 J = 1, 12
      WSQ(J) = (- DBA(1,J)*X5 + DBA(2,J)*X8) * N(2)
     &       + (- DBA(1,J)*Y5 + DBA(2,J)*Y8) * N(2)
     &       + (- DBA(1,J)*X5 - DBA(2,J)*X6) * N(5)
     &       + (- DBA(1,J)*Y5 - DBA(2,J)*Y6) * N(5)
     &       + (  DBA(1,J)*X7 - DBA(2,J)*X6) * N(8)
     &       + (  DBA(1,J)*Y7 - DBA(2,J)*Y6) * N(8)
     &       + (  DBA(1,J)*X7 + DBA(2,J)*X8) * N(11)
     &       + (  DBA(1,J)*Y7 + DBA(2,J)*Y8) * N(11)
 140  CONTINUE
C
C ----- FONCTIONS D'INTERPOLATION DANS LE REPERE LOCAL -------------
      WSQ(1)  = WSQ(1)  + N(1)
      WSQ(2)  = WSQ(2)  + (- X5*N(2)  + X8*N(3) ) / 2.D0
      WSQ(3)  = WSQ(3)  + (- Y5*N(2)  + Y8*N(3) ) / 2.D0
      WSQ(4)  = WSQ(4)  + N(4)
      WSQ(5)  = WSQ(5)  + (- X5*N(5)  - X6*N(6) ) / 2.D0
      WSQ(6)  = WSQ(6)  + (- Y5*N(5)  - Y6*N(6) ) / 2.D0
      WSQ(7)  = WSQ(7)  + N(7)
      WSQ(8)  = WSQ(8)  + (  X7*N(8)  - X6*N(9) ) / 2.D0
      WSQ(9)  = WSQ(9)  + (  Y7*N(8)  - Y6*N(9) ) / 2.D0
      WSQ(10) = WSQ(10) + N(10)
      WSQ(11) = WSQ(11) + (  X7*N(11) + X8*N(12)) / 2.D0
      WSQ(12) = WSQ(12) + (  Y7*N(11) + Y8*N(12)) / 2.D0
      END
