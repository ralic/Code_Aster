      SUBROUTINE WPGSKP
     &  (NBEQ, NCONV, VECT, ALPHA, LMASSE, LAMOR,
     &   TYPEPS, VAUX, DDLEXC, DELTA)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 18/04/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
C
C     SUBROUTINE ASTER ORTHONORMALISANT LES NCONV VECTEURS COMPLEXES
C VECT(:,J) VIA L'ALGORITHME DE TYPE GRAM-SCHMIDT ITERATIF (IGS)
C SUIVANT LA VERSION DE KAHAN-PARLETT.
C---------------------------------------------------------------------
C L'ORTHONORMALISATION EST REALISEE AU SENS DU PRODUIT SCALAIRE:
C                             (MATRICE (B) DOIT ETRE SYMETRIQUE + ...)
C   SI TYPEPS=0 -> L2         (QUELCONQUE -> PRODUIT SCALAIRE EUCLIDIEN)
C   SI TYPEPS=1 -> LMASSE*SIGN (INDEFINIE -> PSEUDO-PRODUIT SCALAIRE)
C   SI TYPEPS=2 -> LMASSE  (SEMI DEFINIE POSITIVE -> B-PRODUIT SCALAIRE)
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
C       MCMULT -> (SUBROUTINE ASTER)
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
C IN  LMASSE  : IS : DESCRIPTEUR MATRICE DE PRODUIT SCALAIRE.
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
      INTEGER NBEQ, NCONV, LMASSE, LAMOR, TYPEPS,
     &        DDLEXC(*)
      REAL*8  ALPHA, DELTA(*)
      COMPLEX*16      VAUX(2*NBEQ), VECT(2*NBEQ,*)

C----------- DECLARATION DES COMMUNS NORMALISES JEVEUX -----------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8,KBID
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------- FIN DES COMMUNS NORMALISES JEVEUX -------------------
C----------------------------------------------------------------
C DECLARATION VARIABLES LOCALES

      INTEGER I, J, K, STEP, AU1, AU2, AU3
      REAL*8 EPS, R8MIEM
      COMPLEX*16  DELTA1, RAUXC, RAUOLC
C----------------------------------------------------------------
      CALL JEMARQ()
C INITIALISATION DU PLUS PETIT REEL*8 EVITANT L'OVERFLOW
      EPS = R8MIEM()**(2.0D+0 / 3.0D+0)
C     ---- ALLOCATION DES ZONES DE TRAVAIL ---
      CALL WKVECT('&&WPGSKP.VECT.AUX.U1C','V V C',NBEQ,AU1)
      CALL WKVECT('&&WPGSKP.VECT.AUX.U2C','V V C',NBEQ,AU2)
      CALL WKVECT('&&WPGSKP.VECT.AUX.U3C','V V C',NBEQ,AU3)

C NORMALISATION DU PREMIER VECTEUR PROPRE

      IF (TYPEPS.NE.0) THEN
          CALL MCMULT('ZERO',LAMOR,VECT(1,1),'C',ZC(AU1),1)
          CALL MCMULT('ZERO',LMASSE,VECT(1+NBEQ,1),'C',ZC(AU2),1)
          CALL MCMULT('ZERO',LMASSE,VECT(1,1),'C',ZC(AU3),1)
         DO 443 K=1,NBEQ
              VAUX(K) = -(ZC(AU1 + K-1) + ZC(AU2 + K-1))
              VAUX(K+NBEQ) = -ZC(AU3 + K-1)
 443       CONTINUE
      ELSE
        DO 5 I=1,2*NBEQ
          VAUX(I) = VECT(I,1)
    5   CONTINUE
      ENDIF
      RAUXC = DCMPLX(0.D0,0.D0)
      DO 10 I=1,NBEQ
            RAUXC = RAUXC + VECT(I,1) * VAUX(I) * DDLEXC(I)
            RAUXC = RAUXC + VECT(I+NBEQ,1) * VAUX(I+NBEQ)
     +             * DDLEXC(I)
   10 CONTINUE
      IF (TYPEPS.EQ.1) THEN
        DELTA(1) = SIGN(1.D0,DBLE(RAUXC))
      ELSE
        DELTA(1) = 1.D0
      ENDIF
      RAUXC = DELTA(1) / SQRT(RAUXC)
      DO 15 I=1,NBEQ
        VECT(I,1) = VECT(I,1) * RAUXC * DDLEXC(I)
        VECT(I+NBEQ,1) = VECT(I+NBEQ,1) * RAUXC * DDLEXC(I)
   15 CONTINUE

C BOUCLE 1 SUR LES VECTEURS PROPRES
      DO 75 I=2,NCONV
C BOUCLE 2 SUR LES VECTEURS PROPRES
        DO 70 J=1,I-1
C CALCUL (VJ,VI) ET ||VI|| (STEP 1)/ (VJ,VI+) (STEP 2 SI NECESSAIRE)
          STEP = 0
   20     CONTINUE
          STEP = STEP + 1

          IF (TYPEPS.NE.0) THEN
            CALL MCMULT('ZERO',LAMOR,VECT(1,I),'C',ZC(AU1),1)
            CALL MCMULT('ZERO',LMASSE,VECT(NBEQ+1,I),'C',ZC(AU2),1)
            CALL MCMULT('ZERO',LMASSE,VECT(1,I),'C',ZC(AU3),1)
            DO 444 K=1,NBEQ
              VAUX(K) = -(ZC(AU1 + K-1) + ZC(AU2 + K-1))
              VAUX(K+NBEQ) =  -ZC(AU3 + K-1)
 444       CONTINUE
          ELSE
            DO 25 K=1,2*NBEQ
              VAUX(K) = VECT(K,I)
   25       CONTINUE
          ENDIF
          RAUXC = DCMPLX(0.D0,0.D0)
          DO 30 K=1,NBEQ
              RAUXC = RAUXC + VECT(K,J) *VAUX(K)
     +                * DDLEXC(K)
              RAUXC = RAUXC + VECT(K+NBEQ,J) *
     +                VAUX(K+NBEQ) * DDLEXC(K)
   30     CONTINUE
          IF (STEP.EQ.1) THEN
            RAUOLC = DCMPLX(0.D0,0.D0)
            DO 35 K=1,NBEQ
               RAUOLC = RAUOLC + VECT(K,I) * VAUX(K)
     +                  * DDLEXC(K)
               RAUOLC = RAUOLC + VECT(K+NBEQ,I)*
     +                  VAUX(K+NBEQ) * DDLEXC(K)
   35       CONTINUE
          ENDIF
C CALCUL VI+ <- VI - (VJ,VI)VJ (STEP 1)
C CALCUL VI++ <- VI+ - (VJ,VI+)VJ (STEP 2)
          DELTA1 = RAUXC * DELTA(J)
          DO 40 K=1,NBEQ
            VECT(K,I) = (VECT(K,I) - VECT(K,J) * DELTA1)
     +       * DDLEXC(K)
            VECT(K+NBEQ,I) = (VECT(K+NBEQ,I) - VECT(K+NBEQ,J) *
     +        DELTA1)* DDLEXC(K)
   40    CONTINUE
C CALCUL DE ||VI+|| (STEP 1) ET ||VI++|| (STEP 2)
         IF (TYPEPS.NE.0) THEN
           CALL MCMULT('ZERO',LAMOR,VECT(1,I),'C',ZC(AU1),1)
           CALL MCMULT('ZERO',LMASSE,VECT(NBEQ+1,I),'C',ZC(AU2),1)
           CALL MCMULT('ZERO',LMASSE,VECT(1,I),'C',ZC(AU3),1)
           DO 445 K=1,NBEQ
              VAUX(K) = -(ZC(AU1 + K-1) + ZC(AU2 + K-1))
              VAUX(K+NBEQ) =  -ZC(AU3 + K-1)
 445       CONTINUE
          ELSE IF (TYPEPS.EQ.0) THEN
            DO 45 K=1,2*NBEQ
              VAUX(K) = VECT(K,I)
   45       CONTINUE
          ENDIF
          RAUXC =DCMPLX(0.D0,0.D0)
          DO 50 K=1,NBEQ
            RAUXC = RAUXC + VECT(K,I) * VAUX(K) * DDLEXC(K)
            RAUXC = RAUXC + VECT(K+NBEQ,I)*VAUX(K+NBEQ)
     +             * DDLEXC(K)
   50     CONTINUE
C PREMIER TEST
          IF ((SQRT(ABS(RAUXC)).GT.(ALPHA*SQRT(ABS(RAUOLC))
     &      -EPS)).AND.(STEP.LE.2))
     &      THEN
            GOTO 60
          ELSE IF (STEP.GE.1) THEN
            RAUOLC = RAUXC
            GOTO 20
          ELSE IF (STEP.EQ.2) THEN
            DO 55 K=1,2*NBEQ
              VECT(K,I) = DCMPLX(0.D0,0.D0)
   55       CONTINUE
          ENDIF
   60     CONTINUE

          IF (TYPEPS.EQ.1) THEN
            DELTA(I) = SIGN(1.D0,DBLE(RAUXC))
          ELSE
            DELTA(I) = 1.D0
          ENDIF
          RAUXC = DELTA(I) / SQRT(RAUXC)
          DO 65 K=1,NBEQ
             VECT(K,I) = VECT(K,I) * RAUXC  * DDLEXC(K)
             VECT(K+NBEQ,I) = VECT(K+NBEQ,I) * RAUXC  * DDLEXC(K)
   65     CONTINUE
   70   CONTINUE
   75 CONTINUE
C     --- DESTRUCTION DES OJB TEMPORAIRES
      CALL JEDETC('V','&&WPGSKP',1)
      CALL JEDEMA()

C FIN ROUTINE WPGSKP
      END
