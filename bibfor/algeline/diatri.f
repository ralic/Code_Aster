      SUBROUTINE DIATRI (N, D, E, VECTOR, EVEC, LDEVEC)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    N, LDEVEC
      REAL *8 D(*), E(*), EVEC(LDEVEC,*)
      LOGICAL    VECTOR
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C   CALCUL DES VALEURS PROPRES ET VECTEURS PROPRES (OPTION => "VECTOR")
C      SUITE A LA TRANSFORMATION DE HOUSEHOLDER ( SUB. TRIDIA).
C-----------------------------------------------------------------------
C IN  : N    : DIMENSION DES MATRICES.
C I/O : D    : VECTEUR DE REELS DE LONGUEUR N.
C         IN : CONTIENT LA DIAGONALE DE LA MATRICE.
C        OUT : CONTIENT LES VALEURS PROPRES DANS L'ORDRE CROISSANT.
C     : E    : VECTEUR DE REELS DE LONGUEUR N.
C         IN : CONTIENT LES ELEMENTS DE LA DIAGONALE, E(1) ARBITRAIRE
C        OUT : VALEURS QUELCONQUES.
C IN  :VECTOR: VARIABLE LOGIQUE  .TRUE. SI LES VECTEURS PROPRES SONT
C              CALCULEES.
C I/O : EVEC : MATRICE REELLE D'ORDRE N.
C         IN : MATRICE TRANSFORMEE UTILISEE POUR TRANFORMER LA MATRICE
C              INITIALE EN UNE MATRICE TRIDIAGONALE.SI LES VECTEURS
C              PROPRES SONT CALCULES, ELLE CONTIENT LA MATRICE IDENTITE.
C        OUT : LE VECTEUR PROPRE ASSOCIE A EVAL(J) (J-TH VALEUR PROPRE
C              EST STOCKE DANS LA J-TH CLONNE.
C IN  :LDEVEC: DIMENSION EXACTE DE EVEC.
C-----------------------------------------------------------------------
      INTEGER    I, ITER, J, K, L, M
      REAL *8 B, C, F, G, P, R, S, SCALE, TINY, TOL
      INTEGER    IDAMAX
      REAL *8 R8MIEM,R8PREM, R8SQRT
C
      IF (N .EQ. 1) GO TO 9000
C
      CALL R8COPY (N-1, E(2), 1, E(1), 1)
      E(N) = 0.0D0
C
      TINY = 100.0D0*R8MIEM()
      TOL = R8PREM()
      ITER = 0
      DO 60  L=1, N
C    --- RECHERCHE DE LA PLUS PETITE VALEUR DE LA DIAGONALE SUPERIEURE.
   10    CONTINUE
         DO 20  M=L, N
            IF (M .EQ. N) GO TO 30
            IF (ABS(E(M)) .LE. MAX(TOL*(ABS(D(M))+ABS(D(M+1))),
     &          TINY)) GO TO 30
   20    CONTINUE
C
   30    CONTINUE
         P = D(L)
         IF (M .EQ. L) GO TO 60
         IF (ITER .EQ. 30*N) THEN
            WRITE(6,*)  'THE ITERATION FOR THE EIGENVALUES DID '//
     &                  'NOT CONVERGE.'
            GO TO 9000
         END IF
         ITER = ITER + 1
C       --- VALEUR DE SHIFT ---
         G = (D(L+1)-P)/(2.0D0*E(L))
         R = R8SQRT(G,1.0D0)
         G = D(M) - P + E(L)/(G+SIGN(R,G))
         S = 1.0D0
         C = 1.0D0
         P = 0.0D0
C
         DO 40  I=M - 1, L, -1
            F = S*E(I)
            B = C*E(I)
            CALL R8ROTG (G, F, C, S)
            E(I+1) = G
            IF (G .EQ. 0.0D0) GO TO 50
            G = D(I+1) - P
            R = (D(I)-G)*S + 2.0D0*C*B
            P = S*R
            D(I+1) = G + P
            G = C*R - B
C
            IF (VECTOR) CALL R8ROT (N, EVEC(1,I+1), 1, EVEC(1,I), 1, C,
     &          S)
C
   40    CONTINUE
C
         D(L) = D(L) - P
         E(L) = G
         E(M) = 0.0D0
         GO TO 10
C
   50    CONTINUE
         D(I+1) = D(I+1) - P
         E(M) = 0.0D0
         GO TO 10
   60 CONTINUE
C    --- POSITION DES VALEURS ET VECTERUS PROPRES ---
      DO 90  I=1, N - 1
         K = I
         P = D(I)
C
         DO 70  J=I + 1, N
            IF (D(J) .LT. P) THEN
               K = J
               P = D(J)
            END IF
   70    CONTINUE
C
         IF (K .NE. I) THEN
            D(K) = D(I)
            D(I) = P
            IF (VECTOR) CALL R8SWAP (N, EVEC(1,I), 1, EVEC(1,K), 1)
         END IF
C
   80    CONTINUE
C
   90 CONTINUE
C          --- NORMALISATION DES VECTEURS PROPRES ---
      IF (VECTOR) THEN
         DO 100  J=1, N
            I = IDAMAX(N,EVEC(1,J),1)
            SCALE = EVEC(I,J)
            CALL R8SCAL (N, 1.0D0/SCALE, EVEC(1,J), 1)
  100    CONTINUE
      END IF
C
 9000 CONTINUE
 9999 CONTINUE
      END
