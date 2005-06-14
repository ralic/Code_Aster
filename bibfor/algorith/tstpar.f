      SUBROUTINE TSTPAR(ITEST,NBM,AMOR,AMOR0,PULS,PULS0,DT,DT0)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2000   AUTEUR KXBADNG T.KESTENS 
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
C-----------------------------------------------------------------------
C DESCRIPTION : TEST SUR L'EVOLUTION DES PARAMETRES AMORTISSEMENT,
C -----------   PULSATION ET PAS DE TEMPS ENTRE LES INSTANTS N ET N+1
C
C               APPELANT : CALCMI
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER    ITEST, NBM
      REAL*8     AMOR(*), AMOR0(*), PULS(*), PULS0(*), DT, DT0
C
C VARIABLES LOCALES
C -----------------
      INTEGER    I
      REAL*8     TEMP, TOL1, TOL2, TOL3
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  ABS
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL   UTMESS
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IF ( DT0.EQ.0.0D0 ) THEN
         ITEST = 0
         GO TO 999
      ENDIF
C
      ITEST = 1
      TOL1  = 1.0D-05
      TOL2  = 1.0D-05
      TOL3  = 1.0D-05
C
C  1. TESTS PORTANT SUR LES AMORTISSEMENTS
C     ------------------------------------
C
      DO 10 I = 1, NBM
         IF ( AMOR(I).EQ.0.0D0 ) THEN
            CALL UTMESS('I','TSTPAR','LA VARIABLE AMOR EST NULLE')
            TEMP = ABS((AMOR(I) - AMOR0(I)) / AMOR0(I))
         ELSE
            TEMP = ABS((AMOR(I) - AMOR0(I)) / AMOR(I))
         ENDIF
         IF ( TEMP.GT.TOL1 ) THEN
            ITEST = 0
            GO TO 999
         ENDIF
  10  CONTINUE
C
C  2. TESTS PORTANT SUR LES PULSATIONS
C     --------------------------------
C
      DO 20 I = 1, NBM
         TEMP = ABS((PULS(I) - PULS0(I)) / PULS(I))
         IF ( TEMP.GT.TOL2 ) THEN
            ITEST = 0
            GO TO 999
         ENDIF
  20  CONTINUE
C
C  3. TEST PORTANT SUR LE PAS DE TEMPS
C     --------------------------------
C
      TEMP = ABS((DT - DT0)/DT)
      IF ( TEMP.GT.TOL3 ) ITEST = 0
C
 999  CONTINUE
C
C --- FIN DE TSTPAR.
      END
