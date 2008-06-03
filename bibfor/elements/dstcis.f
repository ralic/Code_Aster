      SUBROUTINE DSTCIS ( DCI , CARAT3 , HFT2 , BCA , AN )
      IMPLICIT  NONE
      REAL*8    DCI(2,2), CARAT3(*), HFT2(2,6), BCA(2,3), AN(3,9)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/06/2008   AUTEUR PELLET J.PELLET 
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
C     MATRICES BCA(2,3) ET AN(3,9) DU CISAILLEMENT POUR LE DST
C     --------------------------------------------------------
C
      INTEGER  I, J, K, IRET
      REAL*8  L(3) , C(3) , S(3) , X(3) , Y(3) , DET
      REAL*8  TA(6,3), DB(2,3), AA(3,3), AAI(3,3), AW(3,9)
C     ------------------------------------------------------------------
      C(1) = CARAT3(16)
      C(2) = CARAT3(17)
      C(3) = CARAT3(18)
      S(1) = CARAT3(19)
      S(2) = CARAT3(20)
      S(3) = CARAT3(21)
      L(1) = CARAT3(13)
      L(2) = CARAT3(14)
      L(3) = CARAT3(15)
      X(1) = CARAT3(1)
      X(2) = CARAT3(2)
      X(3) = CARAT3(3)
      Y(1) = CARAT3(4)
      Y(2) = CARAT3(5)
      Y(3) = CARAT3(6)
C
      DO 100 K = 1 , 6
        DO 101 J = 1 , 3
          TA(K,J) = 0.D0
 101    CONTINUE
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
