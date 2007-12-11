      SUBROUTINE RCJACO( AR, BR, VALPRO )
      IMPLICIT   NONE
      REAL *8  AR(*), BR(*), VALPRO(3)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C BUT : ROUTINE SIMPLIFIEE DE JACOBI POUR PERFORMANCE
C
C ----------------------------------------------------------------------
      INTEGER    NPERM, I, II, NITER, J, JP1, JM1, LJK, JJ, KP1, KM1,
     &           JK, KK, IM1, IJ, IK, LJI, LKI, JI, KI, K
      REAL*8     TOL, TOLDYN, VALAUX(3), EPS, AKK, AJJ, AB, VERIF 
      REAL*8     EPTOLA, EPCOMA, EPTOLB, EPCOMB, RACI, D1, D2, DEN, 
     &           CA, CG, AJ, BJ, AK, BK, RTOL, DIF, EPSA, COMPA,
     &           EPSB, COMPB
      LOGICAL    ICONV
      DATA   NPERM, TOL, TOLDYN / 12, 1.D-10, 1.D-2 /
C ----------------------------------------------------------------------
C
C     ---       INITIALISATION DES VALEURS PROPRES      ---
C     --- TERME DIAGONAL RAIDEUR / TERME DIAGONAL MASSE ---
C
      II = 1
      DO 10 I = 1, 3
         IF (BR(II) .EQ. 0.0D0) THEN
            CALL U2MESS('F', 'ALGELINE4_19')
         ENDIF
         VALAUX(I) = AR(II) / BR(II)
         VALPRO(I) = VALAUX(I)
         II = II + 3 + 1 - I
 10   CONTINUE
C
C     ------------------------------------------------------------------
C     ------------------- ALGORITHME DE JACOBI -------------------------
C     ------------------------------------------------------------------
C
      NITER = 0
C
 30   CONTINUE
C
      NITER = NITER + 1
      EPS = (TOLDYN**NITER)**2
C
C     --- BOUCLE SUR LES LIGNES ---
      DO 40 J = 1, 3 - 1
         JP1 = J + 1
         JM1 = J - 1
         LJK = JM1 * 3 - JM1 * J / 2
         JJ = LJK + J
C        ---- BOUCLE SUR LES COLONNES ---
         DO 41 K = JP1, 3
            KP1 = K + 1
            KM1 = K - 1
            JK = LJK + K
            KK = KM1 * 3 - KM1 * K / 2 + K
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
               D1 = AB*0.5D0 + RACI
               D2 = AB*0.5D0 - RACI
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
               IF (JM1-1 .GE. 0) THEN
                  DO 51 I = 1, JM1
                     IM1 = I - 1
                     IJ = IM1 * 3 - IM1 * I / 2 + J
                     IK = IM1 * 3 - IM1 * I / 2 + K
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
               IF (KP1-3 .LE. 0) THEN
                  LJI = JM1 * 3 - JM1 * J / 2
                  LKI = KM1 * 3 - KM1 * K / 2
                  DO 52 I = KP1, 3
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
                  LJI = JM1 * 3 - JM1 * J /2
                  DO 53 I = JP1, KM1
                     JI = LJI + I
                     IM1 = I - 1
                     IK = IM1 * 3 - IM1 * I / 2 + K
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
            AK = AR(KK)
            BK = BR(KK)
            AR(KK) = AK + 2.0D0 * CA * AR(JK) + CA * CA * AR(JJ)
            BR(KK) = BK + 2.0D0 * CA * BR(JK) + CA * CA * BR(JJ)
            AR(JJ) = AR(JJ) + 2.0D0 * CG * AR(JK) + CG * CG * AK
            BR(JJ) = BR(JJ) + 2.0D0 * CG * BR(JK) + CG * CG * BK
            AR(JK) = 0.0D0
            BR(JK) = 0.0D0
C
 41      CONTINUE
 40   CONTINUE
C
C     --- CALCUL DES NOUVELLES VALEURS PROPRES ---
C
      II = 1
      DO 60 I = 1, 3
         IF (BR(II) .EQ. 0.0D0) THEN
            CALL U2MESS('F', 'ALGELINE4_19')
         ENDIF
         VALPRO(I) = AR(II) / BR(II)
         II = II + 3 + 1 - I
 60   CONTINUE
C
C     --- TEST DE CONVERGENCE SUR LES VALEURS PROPRES ---
C
      ICONV = .TRUE.
      DO 70 I = 1, 3
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
      DO 80 J = 1, 3 - 1
         JM1 = J - 1
         JP1 = J + 1
         LJK = JM1 * 3 - JM1 * J /2
         JJ = LJK + J
         DO 81 K = JP1, 3
            KM1 = K - 1
            JK = LJK + K
            KK = KM1 * 3 - KM1 * K /2  + K
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
         DO 82 I = 1, 3
            VALAUX(I) = VALPRO(I)
 82      CONTINUE
C
C        --- TEST SUR LE NOMBRE D'ITERATIONS ---
C
         IF (NITER .LT. NPERM) GOTO 30
C
      ENDIF
C
      END
