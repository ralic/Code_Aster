      SUBROUTINE DSTCIS ( DCI , R , HFT2 , BCA , AN )
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
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8   DCI(2,2)
      REAL*8   R(*)
      REAL*8   HFT2(2,6)
      REAL*8   BCA(2,3)
      REAL*8   AN(3,9)
C     --------------------------------------------------------
C     MATRICES BCA(2,3) ET AN(3,9) DU CISAILLEMENT POUR LE DST
C     --------------------------------------------------------
C
      REAL*8  L(3) , C(3) , S(3)
      REAL*8  X(3) , Y(3)
      REAL*8  TA(6,3)
      REAL*8  DB(2,3)
      REAL*8  AA(3,3)
      REAL*8  AAI(3,3)
      REAL*8  AW(3,9)
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
               PARAMETER (NPG   = 3)
               PARAMETER (NNO   = 3)
               PARAMETER (NC    = 3)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI  + NPG + NNO )
               PARAMETER (LWGT  = LETA  + NPG + NNO )
               PARAMETER (LXYC  = LWGT  + NPG)
               PARAMETER (LCOTE = LXYC  + 2*NC)
               PARAMETER (LCOS  = LCOTE + NC)
               PARAMETER (LSIN  = LCOS  + NC)
C     ------------------------------------------------------------------
      C(1) = R(LCOS)
      C(2) = R(LCOS+1)
      C(3) = R(LCOS+2)
      S(1) = R(LSIN)
      S(2) = R(LSIN+1)
      S(3) = R(LSIN+2)
      L(1) = R(LCOTE)
      L(2) = R(LCOTE+1)
      L(3) = R(LCOTE+2)
      X(1) = R(LXYC)
      X(2) = R(LXYC+1)
      X(3) = R(LXYC+2)
      Y(1) = R(LXYC+3)
      Y(2) = R(LXYC+4)
      Y(3) = R(LXYC+5)
C
      DO 100 K = 1 , 18
         TA(K,1) = 0.D0
 100  CONTINUE
      TA(1,1) = - 8.D0 * C(1)
      TA(2,3) = - 8.D0 * C(3)
      TA(3,1) = - 4.D0 * C(1)
      TA(3,2) =   4.D0 * C(2)
      TA(3,3) = - 4.D0 * C(3)
      TA(4,1) = - 8.D0 * S(1)
      TA(5,3) = - 8.D0 * S(3)
      TA(6,1) = - 4.D0 * S(1)
      TA(6,2) =   4.D0 * S(2)
      TA(6,3) = - 4.D0 * S(3)
C     -------------- PRODUIT HFT2.TA -----------------------------------
      DO 110 K = 1 , 6
         BCA(K,1) = 0.D0
 110  CONTINUE
      DO 120 J = 1, 3
         DO 120 K = 1, 6
            BCA(1,J) = BCA(1,J) + HFT2(1,K) * TA(K,J)
            BCA(2,J) = BCA(2,J) + HFT2(2,K) * TA(K,J)
 120  CONTINUE
C     -------------- PRODUIT DCI.BCA -----------------------------------
      DO 130 J = 1, 3
         DB(1,J) = DCI(1,1) * BCA(1,J) + DCI(1,2) * BCA(2,J)
         DB(2,J) = DCI(2,1) * BCA(1,J) + DCI(2,2) * BCA(2,J)
 130  CONTINUE
C     -------------- CALCUL DE AA --------------------------------------
      DO 150 I = 1 , 3
         DO 140 J = 1, 3
            AA(I,J) = - (X(I) * DB(1,J) + Y(I) * DB(2,J))
 140     CONTINUE
      AA(I,I) = AA(I,I) + 2.D0/3.D0 * L(I)
 150  CONTINUE
C     -------------- INVERSION DE AA -----------------------------------
      DO 155 K = 1 , 9
         AAI(K,1) = 0.D0
 155  CONTINUE
      DO 156 I = 1, 3
         AAI(I,I) = 1.D0
 156  CONTINUE
      CALL MGAUSS ( 'NFVP',AA , AAI , 3 , 3 , 3, DET, IRET )
C
C     -------------- CALCUL DE AW --------------------------------------
      DO 160 K = 1 , 27
         AW(K,1) = 0.D0
 160  CONTINUE
      AW(1,1) =   1.D0
      AW(1,2) = - X(1)/2.D0
      AW(1,3) = - Y(1)/2.D0
      AW(1,4) = - 1.D0
      AW(1,5) = - X(1)/2.D0
      AW(1,6) = - Y(1)/2.D0
      AW(2,4) =   1.D0
      AW(2,5) = - X(2)/2.D0
      AW(2,6) = - Y(2)/2.D0
      AW(2,7) = - 1.D0
      AW(2,8) = - X(2)/2.D0
      AW(2,9) = - Y(2)/2.D0
      AW(3,1) = - 1.D0
      AW(3,2) = - X(3)/2.D0
      AW(3,3) = - Y(3)/2.D0
      AW(3,7) =   1.D0
      AW(3,8) = - X(3)/2.D0
      AW(3,9) = - Y(3)/2.D0
C
C     -------------- PRODUIT AAI.AW ------------------------------------
      DO 170 K = 1 , 27
         AN(K,1) = 0.D0
 170  CONTINUE
      DO 180 I = 1, 3
         DO 180 K = 1, 3
            DO 180 J = 1, 9
               AN(I,J) = AN(I,J) + AAI(I,K) * AW(K,J)
 180  CONTINUE
C
      END
