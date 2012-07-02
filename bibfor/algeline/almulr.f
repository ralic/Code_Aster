      SUBROUTINE ALMULR ( CZERO, TABLE, NBVAL, MANTIS, EXPO )
      IMPLICIT NONE
      CHARACTER*(*)       CZERO
      INTEGER                           NBVAL,         EXPO
      REAL*8                     TABLE(NBVAL), MANTIS
C     ------------------------------------------------------------------
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
C     PRODUIT DE N NOMBRE AVEC TEST DE L OVERFLOW ET DE L'UNDERFLOW
C     AVEC CUMUL DE VALEUR ANTERIEUR OU REMISE A ZERO.
C     ------------------------------------------------------------------
C IN  CZERO  : K4  : DEMANDE DE REMISE A ZERO 'ZERO' OU DE CUMUL
C IN  TABLE  : R8  : TABLEAU DES VALEURS A MULTIPLIER
C IN  NBVAL  : R8  : NOMBRE DE VALEURS A MULTIPLIER
C VAR MANTIS : R8  : MANTISSE DU RESULTAT
C VAR EXPO   : IS  : EXPOSANT DU RESULTAT
C     ------------------------------------------------------------------
C     LE RESULTAT DU PRODUIT EST    MANTISS * 10 ** EXPO
C     UN "BON FORMAT" D'IMPRESSION EST
C         WRITE(?,'(10X,A,F16.10,A,I8)') 'PRODUIT = ',MANTIS,' E',EXPO
C     ------------------------------------------------------------------
C
      REAL*8  TRENT, TRENT1, ZERO, DIX
      INTEGER ITRENT
C
C-----------------------------------------------------------------------
      INTEGER IE ,IVAL 
C-----------------------------------------------------------------------
      TRENT  = 1.D30
      ITRENT = 30
      TRENT1 = 1.D-30
      ZERO   = 0.D0
      DIX    = 10.D0
C
      IF ( CZERO .EQ. 'ZERO' ) THEN
         MANTIS = 1.D0
         EXPO   = 0
      ENDIF
C
      DO 10 IVAL = 1,NBVAL
         MANTIS = MANTIS*TABLE(IVAL)
         IF (ABS(MANTIS).GE.TRENT) THEN
            MANTIS = MANTIS*TRENT1
            EXPO = EXPO + ITRENT
         ELSE IF (ABS(MANTIS).LE.TRENT1) THEN
            MANTIS = MANTIS*TRENT
            EXPO = EXPO - ITRENT
         ENDIF
   10 CONTINUE
C
      IF (MANTIS .NE. ZERO) THEN
          IE = NINT(LOG10(ABS(MANTIS)))
          MANTIS = MANTIS/ (DIX**IE)
          EXPO = EXPO + IE
      ENDIF
C
      END
