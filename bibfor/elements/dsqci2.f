      SUBROUTINE DSQCI2 ( QSI,ETA,CARAQ4, HFT2, HMFT2, BCB,  BCA, BCM )
      IMPLICIT  NONE
      REAL*8    QSI, ETA, CARAQ4(*), HFT2(2,6), HMFT2(2,6)
      REAL*8    BCB(2,12), BCM(2,8), BCA(2,4)
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
C     -----------------------------------------------------------------
C     MATRICES BCB(2,12), BCA(2,4), BCM(2,8) AU POINT QSI, ETA POUR DSQ
C     -----------------------------------------------------------------
      INTEGER  J, K
      REAL*8   PETA , META , PQSI , MQSI , C(4) , S(4)
      REAL*8   TB(6,12) , TA(6,4) , TC(6,8)
C     ------------------------------------------------------------------
C
      C(1) = CARAQ4(13)
      C(2) = CARAQ4(14)
      C(3) = CARAQ4(15)
      C(4) = CARAQ4(16)
      S(1) = CARAQ4(17)
      S(2) = CARAQ4(18)
      S(3) = CARAQ4(19)
      S(4) = CARAQ4(20)
C
      PETA = 1.D0 + ETA
      META = 1.D0 - ETA
      PQSI = 1.D0 + QSI
      MQSI = 1.D0 - QSI
C
      DO 100 K = 1 , 72
         TB(K,1) = 0.D0
 100  CONTINUE
      TB(3,2)  =   0.25D0
      TB(3,5)  = - 0.25D0
      TB(3,8)  =   0.25D0
      TB(3,11) = - 0.25D0
      TB(6,3)  =   0.25D0
      TB(6,6)  = - 0.25D0
      TB(6,9)  =   0.25D0
      TB(6,12) = - 0.25D0
C
      DO 110 K = 1, 24
         TA(K,1) = 0.D0
 110  CONTINUE
      TA(1,1)  = - META * C(1)
      TA(1,3)  = - PETA * C(3)
      TA(2,2)  = - PQSI * C(2)
      TA(2,4)  = - MQSI * C(4)
      TA(3,1)  =    QSI * C(1)
      TA(3,2)  = -  ETA * C(2)
      TA(3,3)  = -  QSI * C(3)
      TA(3,4)  =    ETA * C(4)
      TA(4,1)  = - META * S(1)
      TA(4,3)  = - PETA * S(3)
      TA(5,2)  = - PQSI * S(2)
      TA(5,4)  = - MQSI * S(4)
      TA(6,1)  =    QSI * S(1)
      TA(6,2)  = -  ETA * S(2)
      TA(6,3)  = -  QSI * S(3)
      TA(6,4)  =    ETA * S(4)
C
      DO 120 K = 1 , 48
         TC(K,1) = 0.D0
 120  CONTINUE
      TC(3,1)  =   0.25D0
      TC(6,2)  =   0.25D0
      TC(3,3)  = - 0.25D0
      TC(6,4)  = - 0.25D0
      TC(3,5)  =   0.25D0
      TC(6,6)  =   0.25D0
      TC(3,7)  = - 0.25D0
      TC(6,8)  = - 0.25D0
C
C     -------------- BCB = HFT2.TB -----------------------------------
      DO 130 K = 1 , 24
         BCB(K,1) = 0.D0
 130  CONTINUE
      DO 140 J = 1, 12
         DO 140 K = 1, 6
            BCB(1,J) = BCB(1,J) + HFT2(1,K) * TB(K,J)
            BCB(2,J) = BCB(2,J) + HFT2(2,K) * TB(K,J)
 140  CONTINUE
C     -------------- BCA = HFT2.TA -----------------------------------
      DO 150 K = 1 , 8
         BCA(K,1) = 0.D0
 150  CONTINUE
      DO 160 J = 1, 4
         DO 160 K = 1, 6
            BCA(1,J) = BCA(1,J) + HFT2(1,K) * TA(K,J)
            BCA(2,J) = BCA(2,J) + HFT2(2,K) * TA(K,J)
 160  CONTINUE
C     -------------- BCM = HMFT2.TC ----------------------------------
      DO 170 K = 1 , 16
         BCM(K,1) = 0.D0
 170  CONTINUE
      DO 180 J = 1, 8
         DO 180 K = 1, 6
            BCM(1,J) = BCM(1,J) + HMFT2(1,K) * TC(K,J)
            BCM(2,J) = BCM(2,J) + HMFT2(2,K) * TC(K,J)
 180  CONTINUE
C
      END
