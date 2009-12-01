      SUBROUTINE VARDEC(XLOC,XLOC0,IVAR,DT0,TOLE)
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
C DESCRIPTION : TEST SUR LA VARIATION DES COORDONNEES LOCALES ENTRE
C -----------   LES INSTANTS N ET N+1
C
C               APPELANT : TESTCH
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      REAL*8     XLOC(*), XLOC0(*)
      INTEGER    IVAR
      REAL*8     DT0, TOLE
C
C VARIABLES LOCALES
C -----------------
      REAL*8     TEMP  
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  ABS
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IVAR = 0
C
      IF ( DT0.EQ.0.0D0 ) GO TO 999
C
C  1. TEST PORTANT SUR LA PREMIERE COORDONNEE LOCALE
C     ----------------------------------------------
      IF ( XLOC(1).NE.0.0D0 ) THEN
         TEMP = ABS(XLOC(1) - XLOC0(1))
         IF ( TEMP.GT.TOLE ) THEN
            IVAR = 1
            GO TO 999
         ENDIF
      ENDIF
C
C  2. TEST PORTANT SUR LA DEUXIEME COORDONNEE LOCALE
C     ----------------------------------------------
      IF ( XLOC(2).NE.0.0D0 ) THEN
         TEMP = ABS(XLOC(2) - XLOC0(2))
         IF ( TEMP.GT.TOLE ) THEN
            IVAR = 1
            GO TO 999
         ENDIF
      ENDIF
C
C  3. TEST PORTANT SUR LA TROISIEME COORDONNEE LOCALE
C     -----------------------------------------------
      IF ( XLOC(3).NE.0.0D0 ) THEN
         TEMP = ABS(XLOC(3) - XLOC0(3))
         IF ( TEMP.GT.TOLE ) THEN
            IVAR = 1
         ENDIF
      ENDIF
C
 999  CONTINUE
C
C --- FIN DE VARDEC.
      END
