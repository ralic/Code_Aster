      SUBROUTINE VPZBAL(MAT,NEQ,MXEQ,D,K,L)
      IMPLICIT NONE
      INTEGER               NEQ,MXEQ,  K,L
      REAL*8            MAT(MXEQ,1),D(1)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     REDUCTION DE LA NORME DE LA MATRICE PAR LA TRANSFORMATION DE
C     SIMILITUDE STOCKEE DANS "D"
C     ------------------------------------------------------------------
C     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
C        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
C        PAGE 320
C     ------------------------------------------------------------------
      INTEGER            L1,K1,J,I,LL,NOCONV
      REAL*8             B,B2, R,C,F,G,S
C     ------------------------------------------------------------------
C     --- RECUPERATION DE LA BASE DE NUMEROTATION DE LA MACHINE
C-----------------------------------------------------------------------
      REAL*8 R8BAEM 
C-----------------------------------------------------------------------
      B = R8BAEM()
      B2 = B * B
C
C     ---RECHERCHE DES VALEURS PROPRES ISOLEES (ON LES METS A LA FIN)---
C     --- CAS DES LIGNES ---
      L1 = 1
      K1 = NEQ
    5 CONTINUE
      DO 30 J=K1,1,-1
         R = -ABS(MAT(J,J))
         DO 10 I=1,K1
            R = R + ABS(MAT(J,I))
   10    CONTINUE
         IF (R.EQ.0.D0) THEN
            D(K1) = J
            IF (J.NE.K1) THEN
               DO 15 I=1,K1
                  F       = MAT(I,J)
                  MAT(I,J) = MAT(I,K1)
                  MAT(I,K1) = F
   15          CONTINUE
               DO 20 I=L1,NEQ
                  F      = MAT(J,I)
                 MAT(J,I) = MAT(K1,I)
                 MAT(K1,I) = F
   20          CONTINUE
            ENDIF
            K1 = K1-1
            GO TO 5
         ENDIF
   30 CONTINUE
C
C     ---RECHERCHE DES VALEURS PROPRES ISOLEES (ON LES METS A GAUCHE)---
C     --- CAS DES COLONNES -
   35 CONTINUE
      LL = L1
      DO 60 J=LL,K1
         C = -ABS(MAT(J,J))
         DO 40 I= L1, K1
            C = C + ABS(MAT(I,J))
   40    CONTINUE
         IF (C. EQ. 0.D0) THEN
            D(L1) = J
            IF (J.NE.L1) THEN
               DO 45 I=1,K1
                  F       = MAT(I,J)
                  MAT(I,J) = MAT(I,L1)
                  MAT(I,L1) = F
   45          CONTINUE
               DO 50  I = L1, NEQ
                  F       = MAT(J,I)
                  MAT(J,I) = MAT(L1,I)
                  MAT(L1,I) = F
   50          CONTINUE
            ENDIF
            L1 = L1+1
            GO TO 35
         ENDIF
   60 CONTINUE
C
C     EQUILIBRER LA SOUS-MATRICE DE LA LIGNES L1 A K1
      K = L1
      L = K1
      DO 70  I=L1,K1
         D(I) = 1.D0
   70 CONTINUE
   75 CONTINUE
      NOCONV = 0
      DO 115 I=L1,K1
         C = -ABS(MAT(I,I))
         R = C
         DO 80 J=L1,K1
            C = C + ABS(MAT(J,I))
            R = R + ABS(MAT(I,J))
   80    CONTINUE
         G = R/B
         F = 1.D0
         S = C+R
   85    CONTINUE
         IF (C.LT.G) THEN
            F = F*B
            C = C*B2
            GO TO 85
         ENDIF
         G = R*B
   95    CONTINUE
         IF (C.GE.G) THEN
            F = F/B
            C = C/B2
            GO TO 95
         ENDIF
C
C        --- EQUILIBRAGE ---
         IF ((C+R)/F.LT.0.95D0*S) THEN
            G = 1.D0/F
            D(I) = D(I)*F
            NOCONV = 1
            DO 105 J=L1,NEQ
               MAT(I,J) = MAT(I,J)*G
  105       CONTINUE
            DO 110 J=1,K1
               MAT(J,I) = MAT(J,I)*F
  110       CONTINUE
         ENDIF
  115 CONTINUE
      IF (NOCONV.EQ.1) GO TO 75
      END
