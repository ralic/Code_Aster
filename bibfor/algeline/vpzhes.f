      SUBROUTINE VPZHES (MAT,K,L,NEQ,MXEQ,D)
      IMPLICIT NONE
      INTEGER              K,L,NEQ,MXEQ
      REAL*8             MAT(MXEQ,NEQ),D(NEQ)
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
C     MISE SOUS FORME DE HESSENBERG (FORME SUPERIEURE)
C     ------------------------------------------------------------------
C     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
C        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
C        PAGE 342
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER            M,I,J
      REAL*8             F,G,H,SCALE,ZERO
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      ZERO = 0.D0
      DO 45 M = K+1, L-1
         H     = ZERO
         D(M)  = ZERO
         SCALE = ZERO
C
C        --- MISE A L'ECHELLE DE LA COLONNE ---
         DO 5 I = M, L
            SCALE = SCALE + ABS(MAT(I,M-1))
    5    CONTINUE
         IF (SCALE .EQ. ZERO ) GOTO 45
         DO 10 I=L,M,-1
            D(I) = MAT(I,M-1) / SCALE
            H = H + D(I) * D(I)
   10    CONTINUE
         G = -SIGN(SQRT(H),D(M))
         H = H - D(M) * G
         D(M) = D(M) - G
C
C        --- FORMATION DE  (I-(U*UT)/H) * MAT  ---
         DO 25 J = M,NEQ
            F = ZERO
            DO 15 I=L,M,-1
               F = F + D(I) * MAT(I,J)
   15       CONTINUE
            F = F / H
            DO 20 I = M, L
               MAT(I,J) = MAT(I,J) - F * D(I)
   20       CONTINUE
   25    CONTINUE
C
C        --- FORMATION DE (I-(U*UT)/H)*MAT*(I-(U*UT)/H)  ---
         DO 40 I = 1,L
            F = ZERO
            DO 30 J=L,M,-1
               F = F + D(J) * MAT(I,J)
   30       CONTINUE
            F = F / H
            DO 35 J = M, L
               MAT(I,J) = MAT(I,J) - F * D(J)
   35       CONTINUE
   40    CONTINUE
         D(M) = SCALE * D(M)
         MAT(M,M-1) = SCALE * G
   45 CONTINUE
      END
