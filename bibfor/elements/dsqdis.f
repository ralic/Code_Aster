      SUBROUTINE DSQDIS ( NOMTE, XYZL , DF , DCI , AN )
      IMPLICIT  NONE
      REAL*8   XYZL(3,*), DF(3,3), DCI(2,2), AN(4,12)
      CHARACTER*16  NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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
C     -------------------------------------------------------
C     MATRICE AN(4,12) DU CISAILLEMENT POUR LE DSQ
C     -------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER LZR, K, IC, INT, J, I, IRET
      REAL*8  QSI,ETA,PETA,META,PQSI,MQSI,DET
      REAL*8  L(4) , C(4) , S(4)
      REAL*8  X(4) , Y(4)
      REAL*8  HFT2(2,6)
      REAL*8  TB(6,12)
      REAL*8  TA(6,4)
      REAL*8  DT(2,6)
      REAL*8  DIB(2,12)
      REAL*8  DIA(2,4)
      REAL*8  AW(4,12)
      REAL*8  AA(4,4)
      REAL*8  AAI(4,4)
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
               PARAMETER (NPG   = 4)
               PARAMETER (NNO   = 4)
               PARAMETER (NC    = 4)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI + NPG + NNO + 2*NC)
               PARAMETER (LWGT  = LETA + NPG + NNO + 2*NC)
               PARAMETER (LXYC  = LWGT + NPG)
               PARAMETER (LCOTE = LXYC + 2*NC)
               PARAMETER (LCOS  = LCOTE + NC)
               PARAMETER (LSIN  = LCOS + NC)
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL JEVETE( '&INEL.'//NOMTE(1:8)//'.DESR' ,' ',LZR )
C
      CALL GQUAD4 (XYZL , ZR(LZR))
      C(1) = ZR(LZR-1+LCOS)
      C(2) = ZR(LZR-1+LCOS+1)
      C(3) = ZR(LZR-1+LCOS+2)
      C(4) = ZR(LZR-1+LCOS+3)
      S(1) = ZR(LZR-1+LSIN)
      S(2) = ZR(LZR-1+LSIN+1)
      S(3) = ZR(LZR-1+LSIN+2)
      S(4) = ZR(LZR-1+LSIN+3)
      L(1) = ZR(LZR-1+LCOTE)
      L(2) = ZR(LZR-1+LCOTE+1)
      L(3) = ZR(LZR-1+LCOTE+2)
      L(4) = ZR(LZR-1+LCOTE+3)
      X(1) = ZR(LZR-1+LXYC)
      X(2) = ZR(LZR-1+LXYC+1)
      X(3) = ZR(LZR-1+LXYC+2)
      X(4) = ZR(LZR-1+LXYC+3)
      Y(1) = ZR(LZR-1+LXYC+4)
      Y(2) = ZR(LZR-1+LXYC+5)
      Y(3) = ZR(LZR-1+LXYC+6)
      Y(4) = ZR(LZR-1+LXYC+7)
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
      DO 300 IC = 1, NC
C
         DO 105 K = 1 , 24
            DIB(K,1) = 0.D0
 105     CONTINUE
         DO 110 K = 1 , 8
            DIA(K,1) = 0.D0
 110     CONTINUE
C
         DO 250 INT = 1, 2
            CALL JQUAD4 (NPG+NNO+2*(IC-1)+INT , XYZL , ZR(LZR))
            QSI  = ZR(LZR-1+LQSI+NPG+NNO+2*(IC-1)+INT-1)
            ETA  = ZR(LZR-1+LETA+NPG+NNO+2*(IC-1)+INT-1)
            PETA = 1.D0 + ETA
            META = 1.D0 - ETA
            PQSI = 1.D0 + QSI
            MQSI = 1.D0 - QSI
C
            DO 120 K = 1, 24
               TA(K,1) = 0.D0
 120        CONTINUE
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
            CALL DSXHFT (DF , ZR(LZR) , HFT2)
C
C           -------- PRODUIT DCI.HFT2 ----------------------------------
            DO 130 K = 1 , 12
               DT(K,1) = 0.D0
 130        CONTINUE
            DO 140 J = 1, 6
               DT(1,J) = DT(1,J)+DCI(1,1)*HFT2(1,J)+DCI(1,2)*HFT2(2,J)
               DT(2,J) = DT(2,J)+DCI(2,1)*HFT2(1,J)+DCI(2,2)*HFT2(2,J)
 140        CONTINUE
C           -------- PRODUIT DT.TB -------------------------------------
            DO 160 J = 1, 12
               DO 160 K = 1, 6
                  DIB(1,J) = DIB(1,J) + DT(1,K) * TB(K,J)
                  DIB(2,J) = DIB(2,J) + DT(2,K) * TB(K,J)
 160        CONTINUE
C           -------- PRODUIT DT.TA -------------------------------------
            DO 180 J = 1, 4
               DO 180 K = 1, 6
                  DIA(1,J) = DIA(1,J) + DT(1,K) * TA(K,J)
                  DIA(2,J) = DIA(2,J) + DT(2,K) * TA(K,J)
 180        CONTINUE
 250     CONTINUE
         DO 260 J = 1, 12
            AW(IC,J) =   (C(IC)*DIB(1,J) + S(IC)*DIB(2,J)) * L(IC)/2.D0
 260     CONTINUE
         DO 270 J = 1, 4
            AA(IC,J) = - (C(IC)*DIA(1,J) + S(IC)*DIA(2,J)) * L(IC)/2.D0
 270     CONTINUE
         AA(IC,IC) =  AA(IC,IC) + 2.D0 / 3.D0 * L(IC)
 300  CONTINUE
C     -------------- INVERSION DE AA -----------------------------------
      DO 310 K = 1 , 16
         AAI(K,1) = 0.D0
 310  CONTINUE
      DO 320 I = 1, 4
         AAI(I,I) = 1.D0
 320  CONTINUE
      CALL MGAUSS ('NFVP',AA , AAI , 4 , 4, 4, DET, IRET )
C
      AW(1,1)  = AW(1,1)  + 1.D0
      AW(1,2)  = AW(1,2)  - X(1)/2.D0
      AW(1,3)  = AW(1,3)  - Y(1)/2.D0
      AW(1,4)  = AW(1,4)  - 1.D0
      AW(1,5)  = AW(1,5)  - X(1)/2.D0
      AW(1,6)  = AW(1,6)  - Y(1)/2.D0
      AW(2,4)  = AW(2,4)  + 1.D0
      AW(2,5)  = AW(2,5)  - X(2)/2.D0
      AW(2,6)  = AW(2,6)  - Y(2)/2.D0
      AW(2,7)  = AW(2,7)  - 1.D0
      AW(2,8)  = AW(2,8)  - X(2)/2.D0
      AW(2,9)  = AW(2,9)  - Y(2)/2.D0
      AW(3,7)  = AW(3,7)  + 1.D0
      AW(3,8)  = AW(3,8)  - X(3)/2.D0
      AW(3,9)  = AW(3,9)  - Y(3)/2.D0
      AW(3,10) = AW(3,10) - 1.D0
      AW(3,11) = AW(3,11) - X(3)/2.D0
      AW(3,12) = AW(3,12) - Y(3)/2.D0
      AW(4,1)  = AW(4,1)  - 1.D0
      AW(4,2)  = AW(4,2)  - X(4)/2.D0
      AW(4,3)  = AW(4,3)  - Y(4)/2.D0
      AW(4,10) = AW(4,10) + 1.D0
      AW(4,11) = AW(4,11) - X(4)/2.D0
      AW(4,12) = AW(4,12) - Y(4)/2.D0
C
C     -------------- AN = AAI.AW ---------------------------------------
      DO 410 K = 1 , 48
         AN(K,1) = 0.D0
 410  CONTINUE
      DO 420 I = 1 , 4
         DO 420 K = 1, 4
            DO 420 J = 1, 12
               AN(I,J) = AN(I,J) + AAI(I,K) * AW(K,J)
 420  CONTINUE
      CALL JEDEMA()
      END
