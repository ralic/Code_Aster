      SUBROUTINE CALSVD(NM,M,N,A,W,MATU,U,MATV,V,IERR,RV1)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/05/98   AUTEUR H1BAXBG M.LAINET 
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
C
C DESCRIPTION :   CALCUL DE LA DECOMPOSITION AUX VALEURS SINGULIERES
C -----------                          T
C                             A = U S V
C
C                 D'UNE MATRICE REELLE RECTANGULAIRE (M,N)
C                 METHODE : REDUCTION A LA FORME BIDIAGONALE PAR
C                 TRANSFORMATIONS DE HOUSEHOLDER ET MISE EN OEUVRE
C                 D'UNE VARIANTE DE L'ALGORITHME QR
C
C IN     : NM   : INTEGER , SCALAIRE
C                 PREMIERE DIMENSION DES TABLEAUX A, U ET V, DECLAREE
C                 DANS L'APPELANT, NM >= MAX(M,N)
C IN     : M    : INTEGER , SCALAIRE
C                 NOMBRE DE LIGNES DES MATRICES A ET U
C IN     : N    : INTEGER , SCALAIRE
C                 NOMBRE DE COLONNES DES MATRICES A ET U
C                  = ORDRE DE LA MATRICE V
C IN     : A    : REAL*8 , TABLEAU DE DIMENSION(NM,N)
C                 CONTIENT LA MATRICE RECTANGULAIRE A DONT ON VEUT
C                 CALCULER LA DECOMPOSITION AUX VALEURS SINGULIERES
C                 LE CONTENU EST INCHANGE EN SORTIE : LE TABLEAU EST
C                 RECOPIE DANS U EN DEBUT DE CALCUL
C OUT    : W    : REAL*8 , VECTEUR DE DIMENSION N
C                 CONTIENT LES N VALEURS SINGULIERES DE A, NON
C                 NEGATIVES (ELEMENTS DIAGONAUX DE S)
C                 LES VALEURS SINGULIERES NE SONT PAS ORDONNEES
C IN     : MATU : LOGICAL , SCALAIRE
C                 MATU = .TRUE.  INDIQUE QUE LA MATRICE U EST DESIREE
C                 MATU = .FALSE. SINON
C OUT    : U    : REAL*8 , TABLEAU DE DIMENSION (NM,N)
C                 SI MATU = .TRUE. LE TABLEAU U CONTIENT LA MATRICE U
C                 (MATRICE (M,N) A COLONNES ORTHOGONALES)
C                 SI MATU = .FALSE. LE TABLEAU U SERT DE TABLEAU DE
C                 TRAVAIL POUR STOCKAGE TEMPORAIRE
C IN     : MATV : LOGICAL , SCALAIRE
C                 MATV = .TRUE.  INDIQUE QUE LA MATRICE V EST DESIREE
C                 MATV = .FALSE. SINON
C OUT    : V    : REAL*8 , TABLEAU DE DIMENSION (NM,N)
C                 SI MATV = .TRUE. LE TABLEAU V CONTIENT LA MATRICE V
C                 (MATRICE CARREE D'ORDRE N ORTHOGONALE)
C                 SI MATV = .FALSE. V EST INUTILE
C OUT    : IERR : INTEGER , SCALAIRE , CODE RETOUR
C                 IERR = 0  OK
C                 IERR = K  LA K-IEME VALEUR SINGULIERE N'A PAS ETE
C                           DETERMINEE APRES 30 ITERATIONS
C IN/OUT : RV1  : REAL*8 , VECTEUR DE DIMENSION N
C                 VECTEUR DE TRAVAIL POUR STOCKAGE TEMPORAIRE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER     NM, M, N, IERR
      REAL*8      A(NM,N), W(N), U(NM,N), V(NM,N), RV1(N)
      LOGICAL     MATU, MATV
C
C VARIABLES LOCALES
C -----------------
      INTEGER     I, J, K, L, I1, K1, L1, MN, ITS
      REAL*8      C, F, G, H, S, X, Y, Z, TST1, TST2, SCALE
C
C FONCTIONS EXTERNES
C ------------------
      REAL*8      R8SQRT
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IERR = 0
C
      DO 10 J = 1, N
         DO 11 I = 1, M
            U(I,J) = A(I,J)
  11     CONTINUE
  10  CONTINUE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  1. REDUCTION A LA FORME BIDIAGONALE PAR TRANSFORMATIONS
C     DE HOUSEHOLDER
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      G = 0.0D0
      SCALE = 0.0D0
      X = 0.0D0
C
      DO 100 I = 1, N
C
         L = I + 1
         RV1(I) = SCALE * G
         G = 0.0D0
         S = 0.0D0
         SCALE = 0.0D0
C
         IF (I .LE. M) THEN
            DO 110 K = I, M
               SCALE = SCALE + ABS(U(K,I))
 110        CONTINUE
            IF (SCALE .NE. 0.0D0) THEN
               DO 120 K = I, M
                  U(K,I) = U(K,I) / SCALE
                  S = S + U(K,I)**2
 120           CONTINUE
               F = U(I,I)
               G = -SIGN(SQRT(S),F)
               H = F * G - S
               U(I,I) = F - G
               IF (I .NE. N) THEN
                  DO 130 J = L, N
                     S = 0.0D0
                     DO 131 K = I, M
                        S = S + U(K,I) * U(K,J)
 131                 CONTINUE
                     F = S / H
                     DO 132 K = I, M
                        U(K,J) = U(K,J) + F * U(K,I)
 132                 CONTINUE
 130              CONTINUE
               ENDIF
               DO 140 K = I, M
                  U(K,I) = SCALE * U(K,I)
 140           CONTINUE
            ENDIF
         ENDIF
C
         W(I) = SCALE * G
         G = 0.0D0
         S = 0.0D0
         SCALE = 0.0D0
C
         IF (I .LE. M .AND. I .NE. N) THEN
            DO 150 K = L, N
               SCALE = SCALE + ABS(U(I,K))
 150        CONTINUE
            IF (SCALE .NE. 0.0D0) THEN
               DO 160 K = L, N
                  U(I,K) = U(I,K) / SCALE
                  S = S + U(I,K)**2
 160           CONTINUE
               F = U(I,L)
               G = -SIGN(SQRT(S),F)
               H = F * G - S
               U(I,L) = F - G
               DO 170 K = L, N
                  RV1(K) = U(I,K) / H
 170           CONTINUE
               IF (I .NE. M) THEN
                  DO 180 J = L, M
                     S = 0.0D0
                     DO 181 K = L, N
                        S = S + U(J,K) * U(I,K)
 181                 CONTINUE
                     DO 182 K = L, N
                        U(J,K) = U(J,K) + S * RV1(K)
 182                 CONTINUE
 180              CONTINUE
               ENDIF
               DO 190 K = L, N
                  U(I,K) = SCALE * U(I,K)
 190           CONTINUE
            ENDIF
         ENDIF
C
         X = MAX(X,ABS(W(I))+ABS(RV1(I)))
C
 100  CONTINUE
C
C --- ACCUMULATION DES TRANSFORMATIONS A DROITE
C
      IF ( MATV ) THEN
C
         DO 200 I = N, 1, -1
            IF (I .NE. N) THEN
               IF (G .NE. 0.0D0) THEN
                  DO 210 J = L, N
                     V(J,I) = (U(I,J) / U(I,L)) / G
 210              CONTINUE
                  DO 220 J = L, N
                     S = 0.0D0
                     DO 221 K = L, N
                        S = S + U(I,K) * V(K,J)
 221                 CONTINUE
                     DO 222 K = L, N
                        V(K,J) = V(K,J) + S * V(K,I)
 222                 CONTINUE
 220              CONTINUE
               ENDIF
               DO 230 J = L, N
                  V(I,J) = 0.0D0
                  V(J,I) = 0.0D0
 230           CONTINUE
            ENDIF
            V(I,I) = 1.0D0
            G = RV1(I)
            L = I
 200     CONTINUE
C
      ENDIF
C
C --- ACCUMULATION DES TRANSFORMATIONS A GAUCHE
C
      IF ( MATU ) THEN
C
         MN = N
         IF (M .LT. N) MN = M
C
         DO 300 I = MN, 1, -1
            L = I + 1
            G = W(I)
            IF (I .NE. N) THEN
               DO 310 J = L, N
                  U(I,J) = 0.0D0
 310           CONTINUE
            ENDIF
            IF (G .NE. 0.0D0) THEN
               IF (I .NE. MN) THEN
                  DO 320 J = L, N
                     S = 0.0D0
                     DO 321 K = L, M
                        S = S + U(K,I) * U(K,J)
 321                 CONTINUE
                     F = (S / U(I,I)) / G
                     DO 322 K = I, M
                        U(K,J) = U(K,J) + F * U(K,I)
 322                 CONTINUE
 320              CONTINUE
               ENDIF
               DO 330 J = I, M
                  U(J,I) = U(J,I) / G
 330           CONTINUE
            ELSE
               DO 340 J = I, M
                  U(J,I) = 0.0D0
 340           CONTINUE
            ENDIF
            U(I,I) = U(I,I) + 1.0D0
 300     CONTINUE
C
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  2. DIAGONALISATION DE LA FORME BIDIAGONALE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      TST1 = X
C
      DO 400 K = N, 1, -1
C
         K1 = K - 1
         ITS = 0
C
C ------ TEST POUR DECOUPAGE
C
 410     CONTINUE
         DO 420 L = K, 1, -1
            L1 = L - 1
            TST2 = TST1 + ABS(RV1(L))
            IF (TST2 .EQ. TST1) GO TO 460
            TST2 = TST1 + ABS(W(L1))
            IF (TST2 .EQ. TST1) GO TO 430
 420     CONTINUE
C
C ------ MISE A ZERO DE RV1(L) POUR L > 1
C
 430     CONTINUE
         C = 0.0D0
         S = 1.0D0
         DO 440 I = L, K
            F = S * RV1(I)
            RV1(I) = C * RV1(I)
            TST2 = TST1 + ABS(F)
            IF (TST2 .EQ. TST1) GO TO 460
            G = W(I)
            H = R8SQRT(F,G)
            W(I) = H
            C = G / H
            S = -F / H
            IF ( MATU ) THEN
               DO 450 J = 1, M
                  Y = U(J,L1)
                  Z = U(J,I)
                  U(J,L1) = Y * C + Z * S
                  U(J,I) = -Y * S + Z * C
 450           CONTINUE
            ENDIF
 440     CONTINUE
C
 460     CONTINUE
         Z = W(K)
C
C ------ TEST POUR ITERATION SUPPLEMENTAIRE
C
         IF (L .NE. K) THEN
C
C --------- SORTIE EN ERREUR SI NON CONVERGENCE VERS UNE VALEUR
C --------- SINGULIERE APRES 30 ITERATIONS
C
            IF (ITS .EQ. 30) THEN
               IERR = K
               GO TO 9999
            ENDIF
C
C --------- ITERATION SUPPLEMENTAIRE
C
            ITS = ITS + 1
C
C --------- SAUT POUR ACCELERATION DE LA CONVERGENCE
C
            X = W(L)
            Y = W(K1)
            G = RV1(K1)
            H = RV1(K)
            F = 0.5D0 * (((G + Z) / H) * ((G - Z) / Y) + Y / H - H / Y)
            G = R8SQRT(F,1.0D0)
            F = X - (Z / X) * Z + (H / X) * (Y / (F + SIGN(G,F)) - H)
C
C --------- FACTORISATION QR
C
            C = 1.0D0
            S = 1.0D0
            DO 470 I1 = L, K1
               I = I1 + 1
               G = RV1(I)
               Y = W(I)
               H = S * G
               G = C * G
               Z = R8SQRT(F,H)
               RV1(I1) = Z
               C = F / Z
               S = H / Z
               F = X * C + G * S
               G = -X * S + G * C
               H = Y * S
               Y = Y * C
               IF ( MATV ) THEN
                  DO 480 J = 1, N
                     X = V(J,I1)
                     Z = V(J,I)
                     V(J,I1) = X * C + Z * S
                     V(J,I) = -X * S + Z * C
 480              CONTINUE
               ENDIF
               Z = R8SQRT(F,H)
               W(I1) = Z
               IF (Z .NE. 0.0D0) THEN
                  C = F / Z
                  S = H / Z
               ENDIF
C ------------ N.B. LA ROTATION PEUT ETRE ARBITRAIRE SI Z EST NUL
               F = C * G + S * Y
               X = -S * G + C * Y
               IF ( MATU ) THEN
                  DO 490 J = 1, M
                     Y = U(J,I1)
                     Z = U(J,I)
                     U(J,I1) = Y * C + Z * S
                     U(J,I) = -Y * S + Z * C
 490              CONTINUE
               ENDIF
 470        CONTINUE
C
            RV1(L) = 0.0D0
            RV1(K) = F
            W(K) = X
            GO TO 410
C
         ENDIF
C
C ------ CONVERGENCE VERS UNE VALEUR SINGULIERE
C ------ ON IMPOSE W(K) NON NEGATIF
C
         IF (Z .LT. 0.0D0) THEN
            W(K) = -Z
            IF ( MATV ) THEN
               DO 495 J = 1, N
                  V(J,K) = -V(J,K)
 495           CONTINUE
            ENDIF
         ENDIF
C
 400  CONTINUE
C
9999  CONTINUE
C
C --- FIN DE CALSVD.
      END
