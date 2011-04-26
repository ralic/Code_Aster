      SUBROUTINE MTCROG(A, B, NMAX, N, NBSC, C, WKS, IER)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER       NMAX, N, NBSC, IER
      REAL*8        A(NMAX,N), B(NMAX,NBSC), C(NMAX,NBSC), WKS(NMAX)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------
C     RESOLUTION APPROCHEE D UN SYSTEME LINEAIRE AX = B PAR LA METHODE
C     DE CROUT, POUR UNE MATRICE A QUELCONQUE DE DIMENSION N*N
C     SI B EST DE DIMENSION N*1, IL S AGIT D UN SIMPLE SYSTEME
C     LINEAIRE. SI B EST DE DIMENSION N*N ET VAUT L IDENTITE, IL S AGIT
C     DE L INVERSION D UNE MATRICE
C ----------------------------------------------------------------------
C     OPERATEUR APPELANT: OP0144, FLUST3, MEFIST, MEFEIG, MEFREC, MEFCIR
C ----------------------------------------------------------------------
C IN  : NMAX   : DIMENSIONS DES TABLEAUX
C IN  : N      : DIMENSIONS EFFECTIVE DES MATRICES
C IN  : NBSC   : DIMENSIONS DU SYSTEM A RESOUDRE
C IN  : A      : MATRICE A DU SYSTEME LINEAIRE (MODIFIEE LORS DE LA
C                RESOLUTION)
C IN  : B      : MATRICE B DU SYSTEME LINEAIRE
C OUT : C      : SOLUTION DU SYSTEME LINEAIRE (SI IER = 0)
C                SI B EST L IDENTITE ET NBSC = N, C EST L INVERSE DE A
C --  : WKS    : TABLEAU DE TRAVAIL
C OUT : IER    : INDICE D ERREUR (0: RESOLUTION CORRECTE
C                                 1: PAS DE CONVERGENCE OU ERREUR)
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C ----------------------------------------------------------------------
      INTEGER      ID, I, J, K, L, IAPRO
      REAL*8       DET, PREC, X, Y
C ----------------------------------------------------------------------
C
C
C --- PREC EST LA PRECISION MACHINE.
C --- PREC   : LE PLUS PETIT REEL POSITIF, TEL QUE: 1.0 + PREC > 1.0
      PREC = .1110223024625156651D-15

      IER = 0
C
C --- TEST DE VERIFICATION DES DIMENSIONS
C
      IF(NMAX.LT.N) THEN
         CALL U2MESS('F','ALGELINE2_14')
      ENDIF

C
C --- SAUVEGARDE DE LA MATRICE B
C
      DO 10 I = 1, N
         DO 10 J = 1, NBSC
         C(I,J) = B(I,J)
  10  CONTINUE
C
C --- A EST DECOMPOSEE EN A = LU, OU L EST UNE MATRICE TRIANGULAIRE
C --- INFERIEURE, ET U, UNE MATRICE TRIANGULAIRE SUPERIEURE UNITAIRE.
C --- L ET U SONT ECRITES A LA PLACE DE A
C --- WKS EST LA TABLE DES PERMUTATIONS EFFECTUEES
C --- SI LA MATRICE A EST SINGULIERE OU PRESQUE SINGULIERE, IER = 1.
C --- IER = 0 SINON, ET DET EST LE DETERMINANT
C
C
      DO 20 I = 1, N
         WKS(I) = 0.0D0
  20  CONTINUE
C
      DO 60 J = 1, N
         DO 40 I = 1, N
            WKS(I) = WKS(I) + A(I,J)**2
  40     CONTINUE
  60  CONTINUE
C
      DO 80 I = 1, N
         IF (WKS(I).LE.0.0D0) GO TO 240
         WKS(I) = 1.0D0/SQRT(WKS(I))
  80  CONTINUE
C
      DET = 1.0D0
      ID = 0
      DO 220 K = 1, N
         L = K
         X = 0.0D0
         DO 100 I = K, N
            Y = ABS(A(I,K)*WKS(I))
            IF (Y.LE.X) GO TO 100
            X = Y
            L = I
 100     CONTINUE
         IF (L.NE.K) THEN
            DET = -DET
            DO 120 J = 1, N
               Y = A(K,J)
               A(K,J) = A(L,J)
               A(L,J) = Y
 120        CONTINUE
            WKS(L) = WKS(K)
         ENDIF
         WKS(K) = L
         DET = DET*A(K,K)
         IF (X.LT.8.0D0*PREC) THEN
             IER = 1
             GO TO 240
         ENDIF
 160     CONTINUE
         IF (ABS(DET).LT.1.0D0) GO TO 180
         DET = DET*0.0625D0
         ID = ID + 4
         GO TO 160
 180     CONTINUE
         IF (ABS(DET).GE.0.0625D0) GO TO 200
         DET = DET*16.0D0
         ID = ID - 4
         GO TO 180
 200  CONTINUE
         IF (K.LT.N) THEN
            CALL MTCRO1(K,A,NMAX,A(1,K+1))
            CALL MTCRO3(N-K,K,A(K+1,1),NMAX,A(1,K+1),
     &                 A(K+1,K+1))
         ENDIF
 220  CONTINUE
 240  CONTINUE
C
C
C --- RESOLUTION D UN SYSTEME LINEAIRE AX = B
C --- B UNE EST UNE MATRICE DE DIMENSION NMAX*NBSC
C --- A EST SOUS LA FORME A = LU, OU L EST UNE MATRICE TRIANGULAIRE
C --- INFERIEURE, ET U, UNE MATRICE TRIANGULAIRE SUPERIEURE UNITAIRE.
C --- L ET U SONT ECRITES A LA PLACE DE A
C --- P EST LA TABLE DES PERMUTATIONS EFFECTUEES AUPARAVANT.
C --- AX = B EST RESOLU EN TROIS ETAPES, PERMUTATION DES ELEMENTS DE B,
C --- CALCUL DE LY = B, ET CALCUL DE  UX = Y. LES MATRICES Y ET X SONT
C --- ECRITES A LA PLACE DE B
C
      IF (IER.EQ.0) THEN
C ---    PERMUTATION DES ELEMENTS DE B
C
         DO 340 I = 1, N
            IAPRO = INT(WKS(I) + 0.5D0)
            IF (IAPRO.EQ.I) GO TO 340
            DO 320 K = 1, NBSC
               X = B(I,K)
               B(I,K) = B(IAPRO,K)
               B(IAPRO,K) = X
 320        CONTINUE
 340     CONTINUE
         DO 360 K = 1, NBSC
C ---       RECHERCHE DE LA SOLUTION DE LY = B
            CALL MTCRO1(N,A,NMAX,B(1,K))
C ---       RECHERCHE DE LA SOLUTION DE UX = Y
            CALL MTCRO2(N,A,NMAX,B(1,K))
 360     CONTINUE
C
C --- RESTAURATION DE LA MATRICE B, ET DE LA SOLUTION
C
         DO 380 I = 1, N
            DO 380 J = 1, NBSC
            X = B(I,J)
            B(I,J) = C(I,J)
            C(I,J) = X
 380     CONTINUE
C
      ELSE
         IER = 1
      ENDIF
C
C
      END
