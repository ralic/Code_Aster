      SUBROUTINE HUJCI1 (CRIT, MATER, DEPS, SIGD, I1F, TRACT, IRET)
      IMPLICIT NONE
C          CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C -----------------------------------------------------------------
C       HUJEUX : CALCUL DE I1F  --> I1 A T+DT
C       RESOLUTION DE L'EQUATION SCALAIRE F(I1) = 0 DU COMPORTEMENT
C       ELASTIQUE NON LINEAIRE AVEC
C
C       F(I1) = I1F - I1D - [ KOE * TRACE(DEPS) ] * (----------)**N
C       OU   I1  = TRACE(SIGMA) /3
C       ET   KOE = K = E /3/(1 - 2*NU)
C
C -----------------------------------------------------------------
C IN  CRIT  : CRITERES DE CONVERGENCE
C IN  MATER : COEFFICIENTS MATERIAU A T+DT
C IN  DEPS  : INCREMENT DE DEFORMATION
C IN  SIGD  : CONTRAINTE A T
C OUT I1    : 1/3*TRACE DE SIG A T+DT
C     TRACT : VARIABLE LOGIQUE INDIQUANT LA TRACTION (I1F > 0.D0)
C OUT IRET  : CODE RETOUR DE LORS DE LA RESOLUTION DE L'EQUATION
C             SCALAIRE
C                 IRET=0 => PAS DE PROBLEME
C                 IRET=1 => ECHEC
C -----------------------------------------------------------------
      INTEGER NDT, NDI, IRET, IFM, NIV
      REAL*8  MATER(22,2), CRIT(*), DEPS(6), SIGD(6), I1D, I1F
      REAL*8  TRDEPS, COEF, PREC, ALPHA, THETA
      REAL*8  X(4), Y(4)
      REAL*8  YOUNG, POISSO, N, PA, PISO
      REAL*8  ZERO, UN, DEUX, D13, C11, C12, C13, C22, C23, C33
      REAL*8  E1, E2, E3, NU12, NU13, NU23, NU21, NU31, NU32, DELTA
      LOGICAL TRACT, DEBUG
      INTEGER I, NITER, ICMPT

      COMMON /TDIM/   NDT, NDI
      COMMON /MESHUJ/ DEBUG

      DATA ZERO /0.D0/
      DATA UN   /1.D0/
      DATA DEUX /2.D0/
      DATA D13  /0.33333333333334D0/

      CALL INFNIV (IFM,NIV)


C       METHODE DE LA SECANTE
C       =====================
      YOUNG   = MATER(1,1)
      POISSO  = MATER(2,1)
      PA      = MATER(8,2)
      N       = MATER(1,2)
      PISO    = 1.5D0*MATER(21,2)
      PISO    = ZERO
      IRET    = 0
      THETA   = UN


C---> DETERMINATION DU TERME COEF = K0 x DEPS_VOLUMIQUE
      IF (MATER(17,1).EQ.UN) THEN

        TRDEPS = ZERO
        DO 5 I = 1, NDI
          TRDEPS = TRDEPS + DEPS(I)
  5       CONTINUE

C        COEF = YOUNG*D13 /(UN-N)/(UN-DEUX*POISSO) * TRDEPS
        COEF = YOUNG*D13 /(UN-DEUX*POISSO) * TRDEPS

      ELSEIF (MATER(17,1).EQ.DEUX) THEN

        E1   = MATER(1,1)
        E2   = MATER(2,1)
        E3   = MATER(3,1)
        NU12 = MATER(4,1)
        NU13 = MATER(5,1)
        NU23 = MATER(6,1)
        NU21 = MATER(13,1)
        NU31 = MATER(14,1)
        NU32 = MATER(15,1)
        DELTA= MATER(16,1)

        C11 = (UN - NU23*NU32)*E1/DELTA
        C12 = (NU21 + NU31*NU23)*E1/DELTA
        C13 = (NU31 + NU21*NU32)*E1/DELTA
        C22 = (UN - NU13*NU31)*E2/DELTA
        C23 = (NU32 + NU31*NU12)*E2/DELTA
        C33 = (UN - NU21*NU12)*E3/DELTA

        COEF = (C11+C12+C13)*DEPS(1) + (C12+C22+C23)*DEPS(2)
     &         + (C13+C23+C33)*DEPS(3)
        COEF = D13*COEF /(UN-N)

      ENDIF

      I1D = ZERO
      DO 10 I = 1, NDI
        I1D = I1D + D13*SIGD(I)
  10    CONTINUE

      I1D =I1D -PISO

      IF (I1D .GE. ZERO) THEN
        I1D = 1.D-6 * PA
        CALL U2MESS('A', 'COMPOR1_18')
      ENDIF
      IF (TRDEPS.EQ.ZERO) THEN
        I1F = I1D
        GOTO 9999
      ENDIF

C ---> COEF < 0 => ON VERIFIE UN CRITERE APPROXIMATIF
C                  D'EXISTENCE DE LA SOLUTION AVEC P+ < P- < 0
C       IF (COEF .GE. ZERO) GOTO 35
C
C       EXIST = DEUX*I1D - PA * (PA /COEF /N)**(UN-N)
C
C       IF (EXIST .LE. ZERO) THEN
C         IF (DEBUG) CALL U2MESS ('A', 'COMPOR1_13')
C         X(4)  = ZERO
C         THETA = ZERO
C         GOTO 50
C       ENDIF
C
C   35  CONTINUE

      TRACT = .FALSE.
      IF (N .EQ. ZERO) THEN
        I1F = I1D + COEF
        IF (I1F .GE. ZERO) TRACT = .TRUE.
        GOTO 9999
      ENDIF


C
C --- DETERMINATION DES BORNES DE RECHERCHE DE LA SOLUTION
C     ====================================================
      ALPHA = 4.D0

      IF (COEF .LT. ZERO) THEN

        X(1) = I1D
        Y(1) = COEF*(X(1)/PA)**N
        ICMPT = 1
  45    CONTINUE
        X(2) = ALPHA*X(1)
        Y(2) = COEF*(X(2)/PA)**N - X(2) + I1D

        IF (Y(2) .LE. ZERO .AND. ICMPT.LE.20) THEN
          X(1) = X(2)
          Y(1) = COEF*(X(1)/PA)**N
          ICMPT= ICMPT+1
          GOTO 45
        ELSEIF (Y(2) .LE. ZERO) THEN
          IF (DEBUG) CALL U2MESS ('A', 'COMPOR1_17')
          X(4)  = ZERO
          THETA = ZERO
          GOTO 50
        ENDIF


C ---> COEF > 0 => LA SOLUTION EXISTE NECESSAIREMENT ET P- < P+ < 0
      ELSEIF (COEF .GT. ZERO) THEN

        X(2) = I1D
        Y(2) = COEF*(X(2)/PA)**N
        X(1) = ZERO
        Y(1) = I1D


C ---> COEF = 0 => LA SOLUTION N'EXISTE PAS :: ERREUR FATALE!
      ELSE
        CALL U2MESS ('F', 'COMPOR1_12')
      ENDIF


C
C --- CALCUL DE X(4), SOLUTION DE L'EQUATION F = 0 :
C     ===========================================

      X(3) = X(1)
      Y(3) = Y(1)
      X(4) = X(2)
      Y(4) = Y(2)
      NITER = INT(CRIT(1))
      PREC  = CRIT(3)
      ICMPT = 0

  41  CONTINUE
      DO 40 I = 1, NITER

        IF (ABS(Y(4)/PA).LT.PREC) GOTO 50
        CALL ZEROCO(X,Y)
        Y(4) = COEF*(X(4)/PA)**N - X(4) + I1D

  40    CONTINUE

      ICMPT = ICMPT+1
      IF (ICMPT .LT. 5) GOTO 41

      IF(DEBUG)THEN
        WRITE (IFM,*) 'MODELE DE HUJEUX : ATTENTION DANS HUJCI1'
        WRITE (IFM,*) 'NON CONVERGENCE A LA PRECISION DEMANDEE',PREC
        WRITE (IFM,*) 'AU BOUT DU NOMBRE D ITERATION DEMANDE',NITER
        WRITE (IFM,*) 'VALEUR DE F ACTUELLE', Y(4),' ET P=',X(4)
        WRITE (IFM,*) 'AUGMENTER ITER_INTE_MAXI'
      ENDIF

      IF ((Y(4)/(COEF*(I1D/PA)**N + I1D)) .GT. 1.D-2) THEN
        X(4)  = ZERO
        THETA = ZERO
      ENDIF

  50  CONTINUE

      I1F = (UN-THETA)*(COEF*(I1D/PA)**N + I1D) + THETA*X(4) +PISO
      IF (I1F .GE. PISO) TRACT = .TRUE.

 9999 CONTINUE
      END
