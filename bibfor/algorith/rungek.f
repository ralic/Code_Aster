      SUBROUTINE RUNGEK(NEQ,NPILOT,RKLOI,LOI,VRINT0,VMAX,ERRMAX,COMPTM,
     &VRINT1,DPILOT,DVRINT,VROUT1,VROUT2,VECT,DVAR,RK1,RK2,RK3,RK4,VRI0)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C      METHODE D'INTEGRATION DE RUNGE-KUTTA A PAS VARIABLES
C      ----------------------------------------------------
C
C      IN  :  NEQ     : NOMBRE D'EQUATION
C             NPILOT  : NOMBRE DE VARIABLES DE PILOTAGES
C             RKLOI   : NOM DE LA LOI A INTEGRER
C             LOI     : NOM JEVEUX VECTEUR ENTIER
C             VRINT0  : VARIABLES INITIALES (T-)
C             VMAX    : VALEURS MAXIMUM DES VARIABLES
C             ERRMAX  : ERREUR MAX SUR L'INTEGRATION DE CHAQUE VARIABLE


C             COMPTM  : NB D'ITERATION MAXIMALE
C
C     OUT :   VRINT1  : VARIABLES INTEGREES (T+)

      INTEGER NEQ,NPILOT
      REAL*8 LOI(*)
C ---------------------------------
      REAL*8 VRINT0(NEQ),VRINT1(NEQ),VMAX(NEQ),ERRMAX,VRI0(NEQ)
      REAL*8 DPILOT(NPILOT),DVRINT(NEQ),VROUT1(NEQ),VROUT2(NEQ)
      REAL*8 VECT(NEQ),DVAR(NEQ),RK1(NEQ),RK2(NEQ),RK3(NEQ),RK4(NEQ)
      INTEGER COMPTM
C ---------------------------------
C TOLE CRP_7
      EXTERNAL RKLOI
C ---------------------------------
      REAL*8 NORPIL
      REAL*8 DELTA,ERREUR
      LOGICAL ENCORE
      INTEGER COMPTE,I,J
CJLF      INTEGER IXX

C     ----------------------------------------------
      IF ( NPILOT .GE. NEQ ) THEN
        CALL U2MESS('F','ALGORITH10_53')
      ENDIF
C     ----------------------------------------------
C     -        INCREMENT DE CHARGEMENT
C     ----------------------------------------------
      DO 300, I = 1 , NPILOT
        DVRINT ( I ) = VRINT1(I) - VRINT0(I)
        DPILOT ( I ) = DVRINT( I )
300   CONTINUE
      CALL SCVHVV(NORPIL,DPILOT,DPILOT,NPILOT)
C     ----------------------------------------------
C     -     SI L'INCREMENT DE PILOTAGE EST NUL
C     ----------------------------------------------
      IF ( NORPIL .LE. 0.D0 ) THEN
        DO 328, I = 1 , NEQ
          VRINT1(I) = VRINT0(I)
328     CONTINUE
        GOTO 9999
      ENDIF
C     ----------------------------------------------
C     -        SAUVEGARDE DES VARIABLES INITIALES
C     ----------------------------------------------
      DO 327, I = 1 , NEQ
        VRI0(I) = VRINT0(I)
327   CONTINUE
      DO 322, I = NPILOT+1 , NEQ
        DVRINT(I) = 0.D0
        VRINT1(I) = 0.D0
322   CONTINUE
      COMPTE = 0
CJLF      IXX = 0
100   CONTINUE
CJLF        IXX = IXX + 1
        COMPTE = COMPTE + 1
        CALL RRKK4(NEQ,2,RKLOI,LOI,VRINT0,DVRINT,VROUT1,
     &   VECT,DVAR,RK1,RK2,RK3,RK4)
        CALL RRKK4(NEQ,1,RKLOI,LOI,VRINT0,DVRINT,VROUT2,
     &   VECT,DVAR,RK1,RK2,RK3,RK4)
        ERREUR = 0.D0
        DO 302, I= NPILOT+1 , NEQ
          DELTA = ABS( VROUT1(I) - VROUT2(I) ) / VMAX(I)
          ERREUR = MAX( DELTA , ERREUR )
302     CONTINUE
        ERREUR = ERREUR / ERRMAX
        ENCORE = .FALSE.

CJLF ERREUR :   1   ==> / 1.2
CJLF ERREUR : 100   ==> / 5.0
C       X1 = 0.688D0
C       X2 = 0.348D0
CJLF ERREUR :   1   ==> / 1.2
CJLF ERREUR : 100   ==> /10.0
            X1 = 0.442D0
            X2 = 0.498D0
        IF ( ERREUR .GT. 1.0D+02 ) THEN
          ERREUR =  1.0D+02
          DO 303, J = 1 , NPILOT
            DVRINT(J) = DVRINT(J)/((ERREUR+X1)**X2)
303       CONTINUE
          ENCORE = .TRUE.
        ELSE IF ( ERREUR .GT. 1.D0 ) THEN
          DO 304, J = 1 , NPILOT
            DVRINT(J) = DVRINT(J)/((ERREUR+X1)**X2)
304       CONTINUE
          ENCORE = .TRUE.
        ELSE
          DO 308, J = 1 , NPILOT
            DVRINT(J) = VRINT1(J) - VROUT1(J)
308       CONTINUE
        ENDIF
        IF ( COMPTE .GT. COMPTM ) THEN
          CALL U2MESS('F','ALGORITH10_54')
        ENDIF
        ERREUR =  ABS( DVRINT(1) )
        DO 310, I = 2 , NPILOT
          ERREUR = MAX( ERREUR , ABS( DVRINT(I) ) )
310     CONTINUE
        IF ( (ERREUR .LT. 1.0D-30).AND.(ENCORE) ) THEN
          CALL U2MESS('F','ALGORITH10_55')
        ENDIF
      IF ( ENCORE ) GOTO 100
      ERREUR = 0.D0
      DO 312, I = 1 , NPILOT
        DELTA = ABS( VRINT1(I) - VROUT1(I) ) / VMAX(I)
        ERREUR = MAX( ERREUR , DELTA )
312   CONTINUE
      ERREUR = ERREUR / ERRMAX
      IF ( ERREUR .GT. 1.D0 ) THEN
        DO 318, I = 1 , NEQ
          VRINT0(I) = VROUT1(I)
318     CONTINUE
        COMPTE = 0
        GOTO 100
      ENDIF
C     ----------------------------------------------------
C     -  RETOUR DES VARIABLES : INITIALES ET INTEGREES
C     ----------------------------------------------------
      DO 320, I = 1 , NEQ
        VRINT1(I) = VROUT1(I)
        VRINT0(I) = VRI0(I)
320   CONTINUE
9999  CONTINUE
      END
