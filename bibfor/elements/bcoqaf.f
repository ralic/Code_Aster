      SUBROUTINE BCOQAF (BM, BF, BC, NBNO, BMAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      REAL*8   BM(3,1), BF(3,1), BC(2,1), BMAT(8,1)
C     ------------------------------------------------------------------
C --- AFFECTATION DES MATRICES B_MEMBRANE (NOTEE (BM)) ET B_FLEXION
C --- (NOTEE (BF)) DES ELEMENTS COQUES A FACETTES PLANES DST, DKT,
C --- DSQ, DKQ OU Q4G A LA MATRICE B COMPLETE (NOTEE BMAT).
C     ------------------------------------------------------------------
C     IN  BM(3,NBNO*2)  : MATRICE B_MEMBRANE
C     IN  BF(3,NBNO*3)  : MATRICE B_FLEXION
C     IN  BC(2,NBNO*3)  : MATRICE B_CISAILLEMENT
C     IN  NBNO          : NOMBRE DE NOEUDS DE L'ELEMENT
C     OUT BMAT(8,NBNO*6): MATRICE B COMPLETE : (BMAT) = !(BM)!
C                                                       !(BF)!
C                                                       !(BC)!
C                        !BM1 0   0    0    0    0 ....!
C                        !0   BM2 0    0    0    0 ....!
C                        !BM2 BM1 0    0    0    0 ....!
C                        !0   0   BF11 BF12 BF13 0 ....!
C PLUS EXACTEMENT BMAT = !0   0   BF21 BF22 BF23 0 ....!
C                        !0   0   BF31 BF32 BF33 0 ....!
C                        !0   0   BC11 BC12 BC13 0 ....!
C                        !0   0   BC21 BC22 BC23 0 ....!
C
C-----------------------------------------------------------------------
      INTEGER I ,J ,K ,NBNO
C-----------------------------------------------------------------------
C
C --- AFFECTATION DE (BM) A (BMAT)
C     ----------------------------
      DO 10 I = 1, NBNO
         DO 20 K = 1, 2
           DO 30 J = 1, 3
             BMAT(J,6*(I-1)+K) = BM(J,2*(I-1)+K)
  30       CONTINUE
  20     CONTINUE
  10  CONTINUE
C
C --- AFFECTATION DE (BF) A (BMAT)
C     ----------------------------
      DO 40 I = 1, NBNO
         DO 50 K = 1, 3
           DO 60 J = 1, 3
             BMAT(3+J,6*(I-1)+K+2) = BF(J,3*(I-1)+K)
  60       CONTINUE
  50     CONTINUE
  40  CONTINUE
C
C --- AFFECTATION DE (BC) A (BMAT)
C     ----------------------------
      DO 70 I = 1, NBNO
         DO 80 K = 1, 3
           DO 90 J = 1, 2
             BMAT(6+J,6*(I-1)+K+2) = BC(J,3*(I-1)+K)
  90       CONTINUE
  80     CONTINUE
  70  CONTINUE
C
      END
