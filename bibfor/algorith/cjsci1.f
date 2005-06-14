        SUBROUTINE CJSCI1 ( CRIT, MATER, DEPS, SIGD, I1F, TRACT )
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C CJS : CALCUL DE I1F  --> I1 A T+DT
C       RESOLUTION DE L'EQUATION SCALAIRE F(I1) = 0 DU COMPORTEMENT
C       ELASTIQUE NON LINEAIRE AVEC
C                                            I1F+QINIT
C       F(I1) = I1F - I1D - 3 KOE TR(DEPS) (----------)**N
C                                           3 PA
C
C ----------------------------------------------------------------------
C IN  CRIT  : CRITERES DE CONVERGENCE
C IN  MATER : COEFFICIENTS MATERIAU A T+DT
C IN  DEPS  : INCREMENT DE DEFORMATION
C IN  SIGD  : CONTRAINTE A T
C OUT I1    : TRACE DE SIG A T+DT
C     TRACT : VARIABLE LOGIQUE INDIQUANT LA TRACTION (I1F > QINIT)
C ----------------------------------------------------------------------

        INTEGER NDT, NDI, IMAX
        PARAMETER (IMAX = 60)
        REAL*8  MATER(14,2), CRIT(*), DEPS(6), SIGD(6), I1D, I1F
        REAL*8  TRDEPS, COEF, PA, N, MULTI
        REAL*8  X0, X1, X2, OLDX2, Y0, Y1, Y2
        REAL*8  ZERO, UN, DEUX, TROIS,QINIT
        LOGICAL TRACT
        INTEGER I

        COMMON /TDIM/   NDT , NDI


        DATA    ZERO  /0.D0/
        DATA    UN    /1.D0/
        DATA    DEUX  /2.D0/
        DATA    TROIS /3.D0/

C-----------------------------------------------------------------------
C       METHODE DE LA SECANTE
C-----------------------------------------------------------------------


        QINIT = MATER(13,2)
C--->   DETERMINATION DE TERME COEF = 3 KOE TR(DEPS)

        TRDEPS = ZERO
        DO 5 I = 1,NDI
        TRDEPS = TRDEPS + DEPS(I)
  5     CONTINUE

        COEF = MATER(1,1)/( UN - DEUX*MATER(2,1) )*TRDEPS
        PA = MATER(12,2)
        N = MATER(3,2)

        I1D = ZERO
        DO 10 I = 1,NDI
        I1D = I1D + SIGD(I)
  10    CONTINUE
        IF((I1D +QINIT).GE. 0.D0 ) THEN
         I1D = -QINIT+1.D-12 * PA
        ENDIF


C--->  TRAITEMENT EXPLICITE DE L'EQUATION POUR LE NIVEAU CJS 1
C - CAS N.0: NIVEAU CJS 1
C   +++++++++++++++++++++

        TRACT = .FALSE.
        IF(N .EQ. ZERO) THEN
           I1F = I1D + COEF
           IF(I1F .GE. (-QINIT)) THEN
              TRACT = .TRUE.
           ENDIF
           GOTO 9999
        ENDIF

C--->  TRAITEMENT DE L'EQUATION EN FONCTION DE TRACE DE DEPS
C - CAS N.1: TRACE NULLE
C   ++++++++++++++++++++
        IF(TRDEPS .EQ. ZERO) THEN
           I1F = I1D
        ENDIF

C - CAS N.2: TRACE NEGATIVE (CHARGEMENT)
C   ++++++++++++++++++++++++++++++++++++

        IF(TRDEPS .LT. ZERO) THEN

C       DERTEMINATION DES BORNES DE L'INTERVALLE DE RECHERCHE
C       AVEC Y0>0 ET Y1<0

           X0 = I1D
           Y0 = X0-I1D-COEF*((X0+QINIT)/TROIS/PA)**N
           MULTI=2.D0
           X1 = X0 + QINIT
           DO 20 I=1,IMAX
             X1 = MULTI*X1
             Y1 = X1-I1D-COEF*((X1+QINIT)/TROIS/PA)**N
             IF(Y1 .LT. ZERO) GOTO 25
  20       CONTINUE
           CALL UTMESS('F','CJSCI1','CALCUL I1 : PAS DE CONVERGENCE'//
     &      ' DANS LA RECHERCHE DE (X1,Y1) CAS N.2 : ')
  25       CONTINUE



C       RECHERCHE DU ZERO DE LA FONCTION ENTRE (X0,Y0) ET (X1,Y1)
C
           OLDX2=ZERO

           DO 30 I=1, INT(ABS(CRIT(1)))
           X2 = (X0*Y1-X1*Y0)/(Y1-Y0)
           Y2 = X2-I1D-COEF*((X2+QINIT)/TROIS/PA)**N

           IF(ABS((X2-OLDX2)/X2) .LT. CRIT(3) .OR. Y2 .EQ. ZERO) GOTO 40

           OLDX2=X2
           IF( Y2 .GT. ZERO ) THEN
            X0 = X2
            Y0 = Y2
           ELSE
            X1 = X2
            Y1 = Y2
           ENDIF

  30       CONTINUE
           CALL UTMESS('F','CJSCI1','CALCUL I1 : PAS DE CONVERGENCE')
  40       CONTINUE

           I1F=X2

        ENDIF

C - CAS N.3: TRACE POSITIVE (DECHARGEMENT)
C   ++++++++++++++++++++++++++++++++++++++

        IF(TRDEPS .GT. ZERO) THEN

C       DERTEMINATION DES BORNES DE L'INTERVALLE DE RECHERCHE
C       AVEC Y0<0 ET Y1>0

           X0 = I1D
           Y0 = X0-I1D-COEF*((X0+QINIT)/TROIS/PA)**N
           X1 = -QINIT
           Y1 = X1-I1D
C
C       RECHERCHE DU ZERO DE LA FONCTION ENTRE (X0,Y0) ET (X1,Y1)
C
           OLDX2=ZERO

           DO 60 I=1, INT(ABS(CRIT(1)))
           X2 = (X0*Y1-X1*Y0)/(Y1-Y0)
           Y2 = X2-I1D-COEF*((X2+QINIT)/TROIS/PA)**N

           IF(ABS((X2-OLDX2)/X2) .LT. CRIT(3) .OR. Y2 .EQ. ZERO) GOTO 70

           OLDX2=X2
           IF( Y2 .GT. ZERO ) THEN
            X1 = X2
            Y1 = Y2
           ELSE
            X0 = X2
            Y0 = Y2
           ENDIF

  60       CONTINUE
           CALL UTMESS('F','CJSCI1','CALCUL I1 : PAS DE CONVERGENCE')
  70       CONTINUE

           I1F=X2

        ENDIF

 9999   CONTINUE

        END
