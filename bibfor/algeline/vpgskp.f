      SUBROUTINE VPGSKP
     &  (NBEQ, NCONV, VECT, ALPHA, LMATB, TYPEPS, VAUX, DDLEXC, DELTA)
C---------------------------------------------------------------------
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
C
C     SUBROUTINE ASTER ORTHONORMALISANT LES NCONV VECTEUR VECT(:,J)
C VIA L'ALGORITHME DE TYPE GRAM-SCHMIDT ITERATIF (IGS) SUIVANT LA
C VERSION DE KAHAN-PARLETT.
C---------------------------------------------------------------------
C L'ORTHONORMALISATION EST REALISEE AU SENS DU PRODUIT SCALAIRE:
C                             (MATRICE (B) DOIT ETRE SYMETRIQUE + ...)
C   SI TYPEPS=0 -> L2         (QUELCONQUE -> PRODUIT SCALAIRE EUCLIDIEN)
C   SI TYPEPS=1 -> LMATB*SIGN (INDEFINIE -> PSEUDO-PRODUIT SCALAIRE)
C   SI TYPEPS=2 -> LMATB  (SEMI DEFINIE POSITIVE -> B-PRODUIT SCALAIRE)
C   -------------------------------------------------------------------
C     PARTANT DE LA MATRICE DE VECTEUR (V1,V2..VI..VN)
C     ON CHERCHE A OBTENIR LA MATRICE ORTHOGONALE (Q1,Q2..QN)
C
C     (0) NORMALISATION DE V1: Q1 <- V1/||V1||
C     (I) BOUCLE I = 2..N
C     (IJ)  BOUCLE J = 1..I-1
C             CALCUL DE (VJ,VI)
C             CALCUL DE ||VI||
C             CALCUL DE VI+ <- VI - (VJ,VI)VJ
C             CALCUL DE ||VI+||
C             SI ||VI+|| >= ALPHA * ||VI|| ALORS (TEST 1)
C               QI <- VI+
C             SINON
C               CALCUL DE (VJ,VI+)
C               CALCUL DE VI++ <- VI+ - (VJ,VI+)VJ
C               CALCUL DE ||VI++||
C               SI ||VI++|| >= ALPHA * ||VI+|| ALORS (TEST 2)
C                 QI <- VI++
C               SINON
C                 QI <- 0
C               FIN TEST 2
C             FIN DE TEST 1
C           FIN DE BOUCLE J
C         FIN DE BOUCLE I
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C
C       MRMULT -> (SUBROUTINE ASTER)
C         PRODUIT MATRICE-VECTEUR.
C       R8MIEM -> (FONCTION ASTER)
C         LA VALEUR MINIMUM EVITANT L'OVERFLOW LORSQU'ON L'INVERSE.
C
C     FONCTIONS INTRINSEQUES:
C
C       MAX, SIGN, ABS, SQRT.
C   --------------------------------------------------------------------
C     PARAMETRES D'APPELS:
C
C IN  NBEQ   : IS : DIMENSION DES VECTEURS.
C IN  NCONV  : IS : NOMBRE DE VALEURS PROPRES CONVERGEES.
C IN  ALPHA  : R8 : PARAMETRE DE L'ALGORITHME DE KAHAN-PARLETT.
C IN  LMATB  : IS : DESCRIPTEUR MATRICE DE PRODUIT SCALAIRE.
C IN  TYPEPS : IS : TYPE DE PRODUIT SCALAIRE.
C IN  DDLEXC : IS : DDLEXC(1..NBEQ) VECTEUR POSITION DES DDLS BLOQUES.
C
C IN/OUT VECT  : R8 : VECT(1..NBEQ,1..NCONV) MATRICE DES VECTEURS A
C                     ORTHONORMALISER (IN),
C                     MATRICE ORTHOGONALE RESULTAT (OUT).
C IN/OUT VAUX  : R8 : VAUX(1..NBEQ) VECTEUR DE TRAVAIL.
C IN/OUT DELTA : R8 : DELTA(1..NCONV) VECTEUR DE STOCKAGE DES SIGN(PS).
C
C ASTER INFORMATION
C 11/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE DLAMCH PAR R8MIEM,
C            REMPLACEMENT DE DABS, DSQRT, DSIGN.
C--------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER NBEQ, NCONV, LMATB, TYPEPS, DDLEXC(NBEQ)
      REAL*8 VECT(NBEQ,NCONV), ALPHA, VAUX(NBEQ), DELTA(NCONV)

C----------------------------------------------------------------
C DECLARATION VARIABLES LOCALES

      INTEGER I, J, K, STEP
      REAL*8 EPS, RAUX, RAUOLD, DELTA1, R8MIEM
C----------------------------------------------------------------
C INITIALISATION DU PLUS PETIT REEL*8 EVITANT L'OVERFLOW
      EPS = R8MIEM()**(2.0D+0 / 3.0D+0)

C NORMALISATION DU PREMIER VECTEUR PROPRE
      IF (TYPEPS.NE.0) THEN
        CALL MRMULT('ZERO',LMATB,VECT(1,1),'R',VAUX,1)
      ELSE
        DO 5 I=1,NBEQ
          VAUX(I) = VECT(I,1)
    5   CONTINUE
      ENDIF
      RAUX = 0.D0
      DO 10 I=1,NBEQ
        RAUX = RAUX + VECT(I,1) * VAUX(I) * DDLEXC(I)
   10 CONTINUE
      IF (TYPEPS.EQ.1) THEN
        DELTA(1) = SIGN(1.D0,RAUX)
      ELSE
        DELTA(1) = 1.D0
      ENDIF
      RAUX = DELTA(1) / MAX(EPS,SQRT(ABS(RAUX)))
      DO 15 I=1,NBEQ
        VECT(I,1) = VECT(I,1) * RAUX * DDLEXC(I)
   15 CONTINUE

C BOUCLE 1 SUR LES VECTEURS PROPRES
      DO 70 I=2,NCONV

C BOUCLE 2 SUR LES VECTEURS PROPRES
        DO 65 J=1,I-1

C CALCUL (VJ,VI) ET ||VI|| (STEP 1)/ (VJ,VI+) (STEP 2 SI NECESSAIRE)
          STEP = 0
   20     CONTINUE
          STEP = STEP + 1

          IF (TYPEPS.NE.0) THEN
            CALL MRMULT('ZERO',LMATB,VECT(1,I),'R',VAUX,1)
          ELSE
            DO 25 K=1,NBEQ
              VAUX(K) = VECT(K,I)
   25       CONTINUE
          ENDIF
          RAUX = 0.D0
          DO 30 K=1,NBEQ
            RAUX = RAUX + VECT(K,J) * VAUX(K) * DDLEXC(K)
   30     CONTINUE
          IF (STEP.EQ.1) THEN
            RAUOLD = 0.D0
            DO 35 K=1,NBEQ
              RAUOLD = RAUOLD + VECT(K,I) * VAUX(K) * DDLEXC(K)
   35       CONTINUE
          ENDIF

C CALCUL VI+ <- VI - (VJ,VI)VJ (STEP 1)
C CALCUL VI++ <- VI+ - (VJ,VI+)VJ (STEP 2)
          DELTA1 = RAUX * DELTA(J)
          DO 40 K=1,NBEQ
            VECT(K,I) = (VECT(K,I) - DELTA1 * VECT(K,J)) * DDLEXC(K)
   40     CONTINUE

C CALCUL DE ||VI+|| (STEP 1) ET ||VI++|| (STEP 2)
          IF (TYPEPS.NE.0) THEN
            CALL MRMULT('ZERO',LMATB,VECT(1,I),'R',VAUX,1)
          ELSE IF (TYPEPS.EQ.0) THEN
            DO 45 K=1,NBEQ
              VAUX(K) = VECT(K,I)
   45       CONTINUE
          ENDIF
          RAUX = 0.D0
          DO 50 K=1,NBEQ
            RAUX = RAUX + VECT(K,I) * VAUX(K) * DDLEXC(K)
   50     CONTINUE

C PREMIER TEST
          IF ((SQRT(ABS(RAUX)).GT.(ALPHA*SQRT(ABS(RAUOLD))
     &      -EPS)).AND.(STEP.LE.2)) THEN
            GOTO 60
          ELSE IF (STEP.EQ.1) THEN
            RAUOLD = RAUX
            GOTO 20
          ELSE IF (STEP.EQ.2) THEN
            DO 55 K=1,NBEQ
              VECT(K,I) = 0.D0
   55       CONTINUE
          ENDIF

   60     CONTINUE
          IF (TYPEPS.EQ.1) THEN
            DELTA(I) = SIGN(1.D0,RAUX)
          ELSE
            DELTA(I) = 1.D0
          ENDIF
          RAUX = DELTA(I)/MAX(EPS,SQRT(ABS(RAUX)))
          DO 65 K=1,NBEQ
            VECT(K,I) = VECT(K,I) * DDLEXC(K) * RAUX
   65     CONTINUE

   70   CONTINUE

C FIN ROUTINE VPGSKP
      END
