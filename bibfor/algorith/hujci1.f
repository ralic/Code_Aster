        SUBROUTINE HUJCI1 (CRIT, MATER, DEPS, SIGD, I1F, TRACT, IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/02/2007   AUTEUR KHAM M.KHAM 
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
C -------------------------------------------------------------------
C       HUJEUX : CALCUL DE I1F  --> I1 A T+DT
C       RESOLUTION DE L'EQUATION SCALAIRE F(I1) = 0 DU COMPORTEMENT
C       ELASTIQUE NON LINEAIRE AVEC
C        
C       F(I1) = I1F - I1D - [ KOE * TRACE(DEPS) ] * (----------)**N
C       OU   I1  = TRACE(SIGMA) /3
C       ET   KOE = K = E /3/(1 - 2*NU)
C
C -------------------------------------------------------------------
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
C -------------------------------------------------------------------
        INTEGER NDT, NDI, IMAX, IRET
        PARAMETER (IMAX = 60)
        REAL*8  MATER(20,2), CRIT(*), DEPS(6), SIGD(6), I1D, I1F
        REAL*8  TRDEPS, COEF, MULTI
        REAL*8  X0, X1, X2, OLDX2, Y0, Y1, Y2
        REAL*8  YOUNG, POISSO, N, PA
        REAL*8  ZERO, UN, DEUX, D13
        LOGICAL TRACT
        INTEGER I

        COMMON /TDIM/ NDT, NDI

        DATA ZERO /0.D0/
        DATA UN   /1.D0/
        DATA DEUX /2.D0/
        DATA D13  /0.33333333333334D0/

C--------------------------------------------------------------------
C       METHODE DE LA SECANTE
C--------------------------------------------------------------------
        YOUNG   = MATER(1,1)
        POISSO  = MATER(2,1)
        PA      = MATER(8,2)
        N       = MATER(1,2)


C--->   DETERMINATION DE TERME COEF = K0 x DEPS_VOLU
        TRDEPS = ZERO
        DO 5 I = 1, NDI
          TRDEPS = TRDEPS + DEPS(I)
  5       CONTINUE

        COEF = YOUNG*D13 /(UN-DEUX*POISSO) * TRDEPS

        I1D = ZERO
        DO 10 I = 1, NDI
          I1D = I1D + D13*SIGD(I)
  10      CONTINUE
  
        IF (I1D .GE. ZERO) THEN
          I1D = 1.D-12 * PA
C           CALL UTMESS('F','HUJEUX :: HUJCI1','CAS DE TRACTION'//
C     &                       ' NON PREVU')
        ENDIF

        TRACT = .FALSE.
        IF (N .EQ. ZERO) THEN
           I1F = I1D + COEF
           IF (I1F .GE. ZERO) TRACT = .TRUE.
           GOTO 9999
        ENDIF
        

C--->  TRAITEMENT DE L'EQUATION EN FONCTION DE TRACE DE DEPS
C       CAS N.1: TRACE NULLE
C       ++++++++++++++++++++
        IF (TRDEPS .EQ. ZERO)  THEN
          I1F = I1D


C - CAS N.2: TRACE NEGATIVE (CHARGEMENT)
C   ++++++++++++++++++++++++++++++++++++
        ELSEIF (TRDEPS .LT. ZERO) THEN


C DERTEMINATION DES BORNES DE L'INTERVALLE DE RECHERCHE
C       AVEC Y0>0 ET Y1<0
           X0    = I1D
           Y0    = X0 - I1D - COEF*(X0/PA)**N
           MULTI = DEUX
           X1    = X0
           DO 20 I = 1, IMAX
             X1 = MULTI*X1
             Y1 = X1 - I1D - COEF*(X1/PA)**N
             IF (Y1 .LT. ZERO) GOTO 25
  20         CONTINUE
           IRET = 1
           GOTO 9999
  25       CONTINUE


C ---- RECHERCHE DU ZERO DE LA FONCTION ENTRE (X0,Y0) ET (X1,Y1)
           OLDX2 = ZERO

           DO 30 I = 1, INT(ABS(CRIT(1)))
             X2 = (X0*Y1-X1*Y0) /(Y1-Y0)
             Y2 = X2 - I1D - COEF*(X2/PA)**N
           
             IF (ABS((X2-OLDX2)/X2) .LT. CRIT(3) .OR.
     &           Y2 .EQ. ZERO) GOTO 40

             OLDX2 = X2
             IF (Y2 .GT. ZERO) THEN
                X0 = X2
                Y0 = Y2
             ELSE
                X1 = X2
                Y1 = Y2
             ENDIF

  30        CONTINUE
  
           IRET = 1
           GOTO 9999
  40     CONTINUE

         I1F = X2


C - CAS N.3: TRACE POSITIVE (DECHARGEMENT)
C   ++++++++++++++++++++++++++++++++++++++
        ELSEIF (TRDEPS .GT. ZERO) THEN


C ---- DERTEMINATION DES BORNES DE L'INTERVALLE DE
C      RECHERCHE AVEC Y0<0 ET Y1>0
           X0 = I1D
           Y0 = X0 - I1D - COEF*(X0/PA)**N
           X1 = ZERO
           Y1 = X1 - I1D


C ---- RECHERCHE DU ZERO DE LA FONCTION ENTRE (X0,Y0) ET (X1,Y1)
           OLDX2 = ZERO

           DO 60 I = 1, INT(ABS(CRIT(1)))
             X2 = (X0*Y1-X1*Y0) /(Y1-Y0)
             Y2 = X2 - I1D - COEF*(X2/PA)**N

             IF (ABS((X2-OLDX2)/X2) .LT. CRIT(3) .OR.
     &           Y2 .EQ. ZERO) GOTO 70

             OLDX2 = X2
             IF(Y2 .GT. ZERO) THEN
               X1 = X2
               Y1 = Y2
             ELSE
               X0 = X2
               Y0 = Y2
             ENDIF

  60         CONTINUE
  
           IRET = 1
           GOTO 9999
  70       CONTINUE

         I1F = X2

        ENDIF

 9999   CONTINUE
        END
