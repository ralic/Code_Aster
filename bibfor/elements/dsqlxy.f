      SUBROUTINE DSQLXY ( QSI, ETA, HLT2, AN, DEPF, CODI, LAMBDA )
      IMPLICIT  NONE
      REAL*8    QSI,ETA,CODI(*),HLT2(4,6),AN(4,12),DEPF(12),LAMBDA(4)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2011   AUTEUR DESOZA T.DESOZA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     'LAMBDA' DE L'ELEMENT DE PLAQUE DSQ
C     ------------------------------------------------------------------
      INTEGER  I, J, K
      REAL*8   PQSI , MQSI , PETA , META
      REAL*8   C(4) , S(4)
      REAL*8   TA(6,4),TB(6,12)
      REAL*8   BLB(4,12),BLA(4,4),BLN(4,12)
C     ------------------------------------------------------------------
C

      DO 200 K = 1,6
        DO 201 J = 1,12
          TB(K,J) = 0.D0
 201    CONTINUE
 200  CONTINUE
      TB(3,2) = 0.25D0
      TB(3,5) = -0.25D0
      TB(3,8) = 0.25D0
      TB(3,11) = -0.25D0
      TB(6,3) = 0.25D0
      TB(6,6) = -0.25D0
      TB(6,9) = 0.25D0
      TB(6,12) = -0.25D0
      C(1) = CODI(1)
      C(2) = CODI(2)
      C(3) = CODI(3)
      C(4) = CODI(4)
      S(1) = CODI(5)
      S(2) = CODI(6)
      S(3) = CODI(7)
      S(4) = CODI(8)

      PETA = 1.D0 + ETA
      META = 1.D0 - ETA
      PQSI = 1.D0 + QSI
      MQSI = 1.D0 - QSI
      DO 224 K = 1,6
        DO 225 J = 1,4
          TA(K,J) = 0.D0
 225    CONTINUE
 224  CONTINUE
      TA(1,1) = -META*C(1)
      TA(1,3) = -PETA*C(3)
      TA(2,2) = -PQSI*C(2)
      TA(2,4) = -MQSI*C(4)
      TA(3,1) = QSI*C(1)
      TA(3,2) = -ETA*C(2)
      TA(3,3) = -QSI*C(3)
      TA(3,4) = ETA*C(4)
      TA(4,1) = -META*S(1)
      TA(4,3) = -PETA*S(3)
      TA(5,2) = -PQSI*S(2)
      TA(5,4) = -MQSI*S(4)
      TA(6,1) = QSI*S(1)
      TA(6,2) = -ETA*S(2)
      TA(6,3) = -QSI*S(3)
      TA(6,4) = ETA*S(4)
C        -------------- BLA = HLT2.TA ----------------------------
      DO 228 I = 1,4
        DO 230 J = 1,4
        BLA(I,J) = 0.D0
          DO 232 K = 1,6
            BLA(I,J) = BLA(I,J) + HLT2(I,K)*TA(K,J)
 232      CONTINUE
 230    CONTINUE
 228  CONTINUE
C        -------------- BLB = HLT2.TB ----------------------------
      DO 236 I = 1,4
        DO 238 J = 1,12
          BLB(I,J) = 0.D0
          DO 240 K = 1,6
            BLB(I,J) = BLB(I,J) + HLT2(I,K)*TB(K,J)
 240     CONTINUE
 238    CONTINUE
 236  CONTINUE
C        -------- LAMBDA = (BLB + BLA.AN).DEPF ------------------
      DO 242 I = 1,4
        LAMBDA(I) = 0.D0
 242  CONTINUE
      DO 246 I = 1,4
        DO 248 J = 1,12
          BLN(I,J) = 0.D0
          DO 250 K = 1,4
            BLN(I,J) = BLN(I,J) + BLA(I,K)*AN(K,J)
 250      CONTINUE
          LAMBDA(I) = LAMBDA(I) + (BLB(I,J)+BLN(I,J))*DEPF(J)
 248   CONTINUE
 246  CONTINUE
C
      END
