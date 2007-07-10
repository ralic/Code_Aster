      SUBROUTINE HUJCI1 (CRIT, MATER, DEPS, SIGD, I1F, TRACT, IRET)
      IMPLICIT NONE
C          CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/07/2007   AUTEUR KHAM M.KHAM 
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
C TOLE CRP_20
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
      REAL*8  MATER(20,2), CRIT(*), DEPS(6), SIGD(6), I1D, I1F
      REAL*8  TRDEPS, COEF, EXIST, PREC, ALPHA, THETA
      REAL*8  X(4), Y(4)
      REAL*8  YOUNG, POISSO, N, PA
      REAL*8  ZERO, UN, DEUX, D13
      LOGICAL TRACT, DEBUG
      INTEGER I, NITER

      COMMON /TDIM/   NDT, NDI
      COMMON /MESHUJ/ DEBUG

      DATA ZERO /0.D0/
      DATA UN   /1.D0/
      DATA DEUX /2.D0/
      DATA D13  /0.33333333333334D0/
      
      CALL INFNIV (IFM,NIV)
      

C
C       METHODE DE LA SECANTE
C       =====================

      YOUNG   = MATER(1,1)
      POISSO  = MATER(2,1)
      PA      = MATER(8,2)
      N       = MATER(1,2)
      IRET    = 0
      THETA   = UN


C---> DETERMINATION DU TERME COEF = K0 x DEPS_VOLUMIQUE
      TRDEPS = ZERO
      DO 5 I = 1, NDI
        TRDEPS = TRDEPS + DEPS(I)
  5     CONTINUE

      COEF = YOUNG*D13 /(UN-DEUX*POISSO) * TRDEPS

      I1D = ZERO
      DO 10 I = 1, NDI
        I1D = I1D + D13*SIGD(I)
  10    CONTINUE
  
      IF (I1D .GE. ZERO) THEN
        I1D = 1.D-12 * PA
        CALL U2MESS('A', 'COMPOR1_18')
      ENDIF
      
      
C ---> COEF < 0 => ON VERIFIE UN CRITERE APPROXIMATIF
C                  D'EXISTENCE DE LA SOLUTION AVEC P+ < P- < 0
      IF (COEF .GE. ZERO) GOTO 35
      
      EXIST = DEUX*I1D - PA * (PA /COEF /N)**(UN-N)
      
      IF (EXIST .LE. ZERO) THEN
        IF (DEBUG) CALL U2MESS ('A', 'COMPOR1_13')
C         IRET = 1
C         GOTO 9999
        X(4)  = ZERO
        THETA = ZERO
        GOTO 50
      ENDIF

  35  CONTINUE

      TRACT = .FALSE.
      IF (N .EQ. ZERO) THEN
        I1F = I1D + COEF
        IF (I1F .GE. ZERO) TRACT = .TRUE.
        GOTO 9999
      ENDIF


C
C --- DETERMINATION DES BORNES DE RECHERCHE DE LA SOLUTION
C     ====================================================

      ALPHA = 100.D0

      IF (COEF .LT. ZERO) THEN
      
        X(1) = I1D
        Y(1) = COEF*(X(1)/PA)**N
  45    CONTINUE
        X(2) = ALPHA*I1D
        Y(2) = COEF*(X(2)/PA)**N - X(2) + I1D
      
        IF (Y(2) .LE. ZERO .AND. ALPHA .EQ. 100.D0) THEN
          ALPHA = 100.D0*ALPHA
          GOTO 45
        ELSEIF (Y(2) .LE. ZERO) THEN
          IF (DEBUG) CALL U2MESS ('A', 'COMPOR1_17')
C           IRET = 1
C           GOTO 9999
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

      DO 40 I = 1, NITER

        IF (ABS(Y(4)/PA).LT.PREC) GOTO 50
        CALL ZEROCO(X,Y)
        Y(4) = COEF*(X(4)/PA)**N - X(4) + I1D

  40    CONTINUE

      WRITE (IFM,*) 'MODELE DE HUJEUX : ATTENTION DANS HUJCI1'
      WRITE (IFM,*) 'PAS DE CONVERGENCE  A LA PRECISION DEMANDEE', PREC
      WRITE (IFM,*) 'AU BOUT DU NOMBRE D ITERATION DEMANDE', NITER
      WRITE (IFM,*) 'VALEUR DE F ACTUELLE', Y(4)
      WRITE (IFM,*) 'AUGMENTER ITER_INTE_MAXI'

C      IRET = 1
C      GOTO 9999
      X(4)  = ZERO
      THETA = ZERO

  50  CONTINUE

C      I1F = X(4)
C      IF (I1F .GE. ZERO) TRACT = .TRUE.
      
      I1F = (UN-THETA)*(COEF*(I1D/PA)**N + I1D) + THETA*X(4)
      IF (I1F .GE. ZERO) TRACT = .TRUE.

 9999 CONTINUE
      END
