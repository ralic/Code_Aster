      SUBROUTINE TRIDIA (N, A, LDA, D, E, TAU, W)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, LDA
      REAL *8 D(*), E(*)
      COMPLEX    *16 A(LDA,*), TAU(*), W(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C-----------------------------------------------------------------------
C      REDUCTION D'UNE MATRICE HERMITIENNE EN UNE MATRICE TRIDIAGONALE
C                   SYMETRIQUE (METHODE DE HOUSEHOLDER).
C-----------------------------------------------------------------------
C IN  : N    : DIMENSION DE LA MATRICE.
C     : A    : MATRICE HERMITIENNE.
C     : LDA  : DIMENSION DE A.
C OUT : D    : VECTEUR DE DIMENSION N CONTENANT LA DIAGONALE DE LA
C              MATRICE TRIDIAGONALE
C     : E    : VECTEUR DE DIMENSION N CONTENANT LA DIAGONALE
C              SUPERIEURE DE LA MATRICE TRIDIAGONALE DANS E(2:N),
C              E(1) = 0.
C     : TAU  : VECTEUR COMPLEXE DE DIMENSION N CONTENANT LA DIAGONALE
C              DE LA MATRICE UNITAIRE T
C     : W    : VECTEUR COMPLEXE DE DIMENSION N (VECTEUR DE TRAVAIL)
C-----------------------------------------------------------------------
      INTEGER    I, J, K
      REAL *8 BB, DELTA, DGAMMA, RATIO, RHO, ROOT, TOL, VR
      COMPLEX    *16 TEMP1
      COMPLEX    *16 CMPLX, CONJG
      REAL *8 R8PREM
      COMPLEX    *16 ZDOTC
C
      TOL = 0.0D0
      DO 10  I=1, N
         DO 10  J=1, I
            TOL = MAX(ABS(DBLE(A(I,J))),ABS(DIMAG(A(I,J))),TOL)
   10 CONTINUE
      DGAMMA = R8PREM()**2
C
C  --- REALISATION DE N-2 TRANSFORMATIONS SIMILAIRES ---
      DO 30  K=2, N - 1
         TAU(K) = 0.0D0
         VR = ZDOTC(N-K+1,A(K,K-1),1,A(K,K-1),1)
         IF (VR .LE. DGAMMA*TOL**2) GO TO 30
         IF (DBLE(A(K,K-1)).EQ.0.0D0 .AND. DIMAG(A(K,K-1)).EQ.0.0D0)
     &       THEN
            A(K,K-1) = SQRT(VR)
            DELTA = VR
            TAU(1) = -A(K,K-1)
         ELSE
            ROOT = ABS(A(K,K-1))*SQRT(VR)
            DELTA = VR + ROOT
            RATIO = VR/ROOT
            TAU(1) = -RATIO*DCONJG(A(K,K-1))
            A(K,K-1) = (RATIO+1.0D0)*A(K,K-1)
         END IF
C
C   --- TRANSFORMATIONS ---
         DO 20  J=K, N
            TAU(J) = A(J,K-1)/DELTA
   20    CONTINUE
C
         CALL ZMVPY ('LOWER', N-K+1, (1.0D0,0.0D0), A(K,K), LDA,
     &               A(K,K-1), 1, (0.0D0,0.0D0), W(K), 1)
C                                  RHO = U*NV
         TEMP1 = ZDOTC(N-K+1,W(K),1,TAU(K),1)
         RHO = DBLE(TEMP1)
         CALL ZADER2 ('LOWER', N-K+1, (-1.0D0,0.0D0), TAU(K), 1, W(K),
     &               1, A(K,K), LDA)
         CALL ZADDER ('LOWER', N-K+1, RHO*DELTA, TAU(K), 1, A(K,K), LDA)
         TAU(K) = TAU(1)
   30 CONTINUE
C
C  --- LA MATRICE A ETE REDUITE EN UNE MATRICE HERMITIENNE
C      TRIDIAGONALE. LA DIAGONALE SUPERIEURE EST TEMPORAIREMENT
C      STOCKEE DANS LE VECTEUR TAU. LA DIAGONALE EST STOCKEE DANS D
      DO 40  I=1, N
         D(I) = DBLE(A(I,I))
   40 CONTINUE
C
      TAU(1) = 1.0D0
      IF (N .GT. 1) TAU(N) = DCONJG(A(N,N-1))
      E(1) = 0.0D0
C
      DO 50  I=2, N
         BB = ABS(TAU(I))
         E(I) = BB
         A(I,I) = DCMPLX(DBLE(A(I,I)),BB)
         IF (BB .EQ. 0.0D0) THEN
            TAU(I) = 1.0D0
            BB = 1.0D0
         END IF
         TAU(I) = TAU(I)*TAU(I-1)/BB
   50 CONTINUE
C
 9999 CONTINUE
      END
