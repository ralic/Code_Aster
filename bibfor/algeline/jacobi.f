      SUBROUTINE JACOBI(NBVEC,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,
     &                  VALAUX,NITJAC,TYPE,IORDRE)
      IMPLICIT NONE
      INTEGER  NBVEC, NPERM, NITJAC, TYPE, IORDRE
      REAL *8  AR(*), BR(*), VECPRO(NBVEC,NBVEC), VALPRO(NBVEC),
     +         VALAUX(NBVEC),TOL, TOLDYN
C
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
C     ------------------------------------------------------------------
C BUT : RESOLUTION DU PROBLEME REDUIT AUX VALEURS PROPRES PAR
C       LA DECOMPOSITION DE JACOBI GENERALISEE
C
C     IN  : NBVEC  : NOMBRE DE VECTEURS
C     IN  : NPERM  : NOMBRE MAX D'ITERATIONS DE LA METHODE DE JACOBI
C     IN  : TOL    : PRECISION DE CONVERGENCE
C     IN  : TOLDYN : PRECISION DE PETITESSE DYNAMIQUE
C     IN / OUT  : AR : MATRICE DE RAIDEUR PROJETEE
C     IN / OUT  : BR : MATRICE DE MASSE PROJETEE
C                      CES DEUX MATRICES SONT SYMETRIQUES ET ON A
C                      DONC STOCKE SOUS FORME D'UN VECTEUR SEULEMENT
C                      LA MOITIE : A11 A12  A1N A22  A2N
C         ATTENTION : CES DEUX MATRICES SONT MODIFIEES ET DONC
C                     INUTILISABLES EN SORTIE.
C     OUT : VECPRO : VECTEURS PROPRES DE L ITERATION
C     OUT : VALPRO : VALEURS PROPRES DE L ITERATION
C     OUT : NITJAC : NOMBRE D'ITERATIONS DE JACOBI
C     IN  : TYPE  : TYPE DU TRI SUR LES VALEURS PROPRES (ET VECT. PRO.)
C        * SI TYPE = 0  TRI EN VALEUR RELATIVE
C        * SI TYPE = 1  TRI EN VALEUR ABSOLUE
C        * SI TYPE = 2  PAS DE TRI
C IN  IORDRE : IS : ORDRE DU TRI SUR LES VALEURS.
C        * SI IORDRE = 0  TRI PAR ORDRE CROISSANT
C        * SI IORDRE = 1  TRI PAR ORDRE DECROISSANT
C        * SI IORDRE = 2  PAS DE TRI
C ----------------------------------------------------------------------
      LOGICAL ICONV
C ----------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     ------- INITIALISATION DES VALEURS ET VECTEURS PROPRES -----------
C     ------------------------------------------------------------------
C
C     ---       INITIALISATION DES VALEURS PROPRES      ---
C     --- TERME DIAGONAL RAIDEUR / TERME DIAGONAL MASSE ---
C
C-----------------------------------------------------------------------
      INTEGER I ,II ,IJ ,IK ,IM1 ,J ,JI 
      INTEGER JJ ,JK ,JM1 ,JP1 ,K ,KI ,KK 
      INTEGER KM1 ,KP1 ,LJI ,LJK ,LKI ,NITER 
      REAL*8 AB ,AJ ,AJJ ,AK ,AKK ,BB ,BJ 
      REAL*8 BK ,CA ,CG ,COMPA ,COMPB ,D1 ,D2 
      REAL*8 DEN ,DIF ,EPCOMA ,EPCOMB ,EPS ,EPSA ,EPSB 
      REAL*8 EPTOLA ,EPTOLB ,RACI ,RTOL ,VERIF ,XJ ,XK 

C-----------------------------------------------------------------------
      II = 1
      DO 10 I = 1, NBVEC
         IF (BR(II) .EQ. 0.0D0) THEN
            CALL U2MESS('F', 'ALGELINE4_19')
         ENDIF
         VALAUX(I) = AR(II) / BR(II)
         VALPRO(I) = VALAUX(I)
         II = II + NBVEC + 1 - I
 10   CONTINUE
C
C     --- INITIALISATION DES VECTEURS PROPRES (MATRICE IDENTITE) ---
C

      CALL MATINI(NBVEC,NBVEC,0.D0,VECPRO)

      DO 20 I = 1, NBVEC
         VECPRO(I,I) = 1.0D0
 20   CONTINUE
C
C     ------------------------------------------------------------------
C     ------------------- ALGORITHME DE JACOBI -------------------------
C     ------------------------------------------------------------------
C
      NITER = 0
      NITJAC = 0
C
 30   CONTINUE
C
      NITER = NITER + 1
      EPS = (TOLDYN**NITER)**2
C
C     --- BOUCLE SUR LES LIGNES ---
      DO 40 J = 1, NBVEC - 1
         JP1 = J + 1
         JM1 = J - 1
         LJK = JM1 * NBVEC - JM1 * J / 2
         JJ = LJK + J
C        ---- BOUCLE SUR LES COLONNES ---
         DO 41 K = JP1, NBVEC
            KP1 = K + 1
            KM1 = K - 1
            JK = LJK + K
            KK = KM1 * NBVEC - KM1 * K / 2 + K
C           --- CALCUL DES COEFFICIENTS DE LA ROTATION DE GIVENS ---
            EPTOLA = ABS( (AR(JK)*AR(JK)) )
            EPCOMA = ABS(AR(JJ)*AR(KK))*EPS
            EPTOLB = ABS( (BR(JK)*BR(JK)) )
            EPCOMB = ABS(BR(JJ)*BR(KK))*EPS
            IF((EPTOLA.EQ.0.D0).AND.(EPTOLB.EQ.0.D0)) GOTO 41
            IF((EPTOLA.LE.EPCOMA).AND.(EPTOLB.LE.EPCOMB)) GOTO 41
            AKK = AR(KK)*BR(JK) - BR(KK)*AR(JK)
            AJJ = AR(JJ)*BR(JK) - BR(JJ)*AR(JK)
            AB  = AR(JJ)*BR(KK) - AR(KK)*BR(JJ)
            VERIF = (AB * AB + 4.0D0 * AKK * AJJ)/4.0D0
            IF ( VERIF .GE. 0.0D0 ) THEN
               RACI = SQRT(VERIF)
               D1 = AB/2.0D0 + RACI
               D2 = AB/2.0D0 - RACI
            ELSE
               GOTO 41
            ENDIF
            DEN = D1
            IF (ABS(D2).GT.ABS(D1)) DEN = D2
            IF (DEN .EQ. 0.0D0) THEN
               CA = 0.D0
               CG = - AR(JK)/AR(KK)
            ELSE
               CA = AKK / DEN
               CG = -AJJ/DEN
            ENDIF
C           --- TRANSFORMATION DES MATRICES DE RAIDEUR ET DE MASSE ---
            IF (NBVEC-2 .NE. 0) THEN
               IF (JM1-1 .GE. 0) THEN
                  DO 51 I = 1, JM1
                     IM1 = I - 1
                     IJ = IM1 * NBVEC - IM1 * I / 2 + J
                     IK = IM1 * NBVEC - IM1 * I / 2 + K
                     AJ = AR(IJ)
                     BJ = BR(IJ)
                     AK = AR(IK)
                     BK = BR(IK)
                     AR(IJ) = AJ + CG * AK
                     BR(IJ) = BJ + CG * BK
                     AR(IK) = AK + CA * AJ
                     BR(IK) = BK + CA * BJ
 51               CONTINUE
               ENDIF
               IF (KP1-NBVEC .LE. 0) THEN
                  LJI = JM1 * NBVEC - JM1 * J / 2
                  LKI = KM1 * NBVEC - KM1 * K / 2
                  DO 52 I = KP1, NBVEC
                     JI = LJI + I
                     KI = LKI + I
                     AJ = AR(JI)
                     BJ = BR(JI)
                     AK = AR(KI)
                     BK = BR(KI)
                     AR(JI) = AJ + CG * AK
                     BR(JI) = BJ + CG * BK
                     AR(KI) = AK + CA * AJ
                     BR(KI) = BK + CA * BJ
 52               CONTINUE
               ENDIF
               IF (JP1-KM1 .LE. 0) THEN
                  LJI = JM1 * NBVEC - JM1 * J /2
                  DO 53 I = JP1, KM1
                     JI = LJI + I
                     IM1 = I - 1
                     IK = IM1 * NBVEC - IM1 * I / 2 + K
                     AJ = AR(JI)
                     BJ = BR(JI)
                     AK = AR(IK)
                     BK = BR(IK)
                     AR(JI) = AJ + CG * AK
                     BR(JI) = BJ + CG * BK
                     AR(IK) = AK + CA * AJ
                     BR(IK) = BK + CA * BJ
 53               CONTINUE
               ENDIF
            ENDIF
            AK = AR(KK)
            BK = BR(KK)
            AR(KK) = AK + 2.0D0 * CA * AR(JK) + CA * CA * AR(JJ)
            BR(KK) = BK + 2.0D0 * CA * BR(JK) + CA * CA * BR(JJ)
            AR(JJ) = AR(JJ) + 2.0D0 * CG * AR(JK) + CG * CG * AK
            BR(JJ) = BR(JJ) + 2.0D0 * CG * BR(JK) + CG * CG * BK
            AR(JK) = 0.0D0
            BR(JK) = 0.0D0
C           --- TRANSFORMATION DES VECTEURS PROPRES ---
            DO 54 I =1, NBVEC
               XJ = VECPRO(I,J)
               XK = VECPRO(I,K)
               VECPRO(I,J) = XJ + CG * XK
               VECPRO(I,K) = XK + CA * XJ
 54         CONTINUE
C
 41      CONTINUE
 40   CONTINUE
C
C     --- CALCUL DES NOUVELLES VALEURS PROPRES ---
C
      II = 1
      DO 60 I = 1, NBVEC
         IF (BR(II) .EQ. 0.0D0) THEN
            CALL U2MESS('F', 'ALGELINE4_19')
         ENDIF
         VALPRO(I) = AR(II) / BR(II)
         II = II + NBVEC + 1 - I
 60   CONTINUE
C
C     --- TEST DE CONVERGENCE SUR LES VALEURS PROPRES ---
C
      ICONV = .TRUE.
      DO 70 I = 1, NBVEC
          RTOL = TOL * VALAUX(I)
          DIF = ABS(VALPRO(I) - VALAUX(I))
          IF (DIF .GT. ABS(RTOL)) THEN
             ICONV = .FALSE.
             GOTO 9998
          ENDIF
 70   CONTINUE
C
C     ---    CALCUL DES FACTEURS DE COUPLAGE   ---
C     --- TEST DE CONVERGENCE SUR CES FACTEURS ---
C
      EPS = TOL**2
      DO 80 J = 1, NBVEC - 1
         JM1 = J - 1
         JP1 = J + 1
         LJK = JM1 * NBVEC - JM1 * J /2
         JJ = LJK + J
         DO 81 K = JP1, NBVEC
            KM1 = K - 1
            JK = LJK + K
            KK = KM1 * NBVEC - KM1 * K /2  + K
            EPSA = ABS(AR(JK) * AR(JK))
            COMPA = EPS * ABS(AR(JJ) * AR(KK))
            EPSB = ABS(BR(JK) * BR(JK))
            COMPB = EPS * ABS(BR(JJ) * BR(KK))
            IF (EPSA .GE. COMPA .OR. EPSB .GE. COMPB) THEN
               ICONV = .FALSE.
               GOTO 9998
            ENDIF
 81      CONTINUE
 80   CONTINUE
C
 9998 CONTINUE
C
C     ---  SI ON N'A PAS CONVERGE ---
C
      IF (.NOT.ICONV) THEN
C
C        --- TRANSLATION DES VALEURS PROPRES ---
C
         DO 82 I = 1, NBVEC
            VALAUX(I) = VALPRO(I)
 82      CONTINUE
C
C        --- TEST SUR LE NOMBRE D'ITERATIONS ---
C
         IF (NITER .LT. NPERM) GOTO 30
C
      ENDIF
C
C     --- SI CONVERGENCE OU NOMBRE MAX D'ITERATIONS ATTEINT ---
C     ---         MISE A JOUR DES VECTEURS PROPRES          ---
C
      II = 1
      DO 90 I = 1, NBVEC
         IF (BR(II) .GE. 0.0D0) THEN
            BB = SQRT(BR(II))
         ELSE
            BB = - SQRT(ABS(BR(II)))
         ENDIF
         DO 91 K = 1, NBVEC
            VECPRO(K,I) = VECPRO(K,I) / BB
 91      CONTINUE
         II = II + NBVEC + 1 - I
 90   CONTINUE
C
      NITJAC = NITER
C
C     --- CLASSEMENT DES MODES
      IF ((TYPE.EQ.2).OR.(IORDRE.EQ.2)) THEN
      ELSE
        CALL VPORDO(TYPE,IORDRE,NBVEC,VALPRO,VECPRO,NBVEC)
      ENDIF
      END
