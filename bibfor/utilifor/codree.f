      SUBROUTINE CODREE( REEL , MODE , CHAINE )
      IMPLICIT NONE
      REAL*8             REEL
      CHARACTER*(*)             MODE , CHAINE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     CONVERSION D'UN REEL EN CHAINE
C     ------------------------------------------------------------------
C IN   REEL   : R8 : REEL A ECRIRE
C IN   MODE   : K  : MODE DE CONVERSION (ECRITURE) DU REEL
C                     'E' : FORMAT EXPOSANT
C                     'F' : FORMAT FLOTTANT
C                     'G' : FORMAT GENERALISE
C OUT  CHAINE : K* : CHAINE RECEPTACLE
C     ------------------------------------------------------------------
C     REMARQUE: ON A TOUJOURS LE POINT DECIMAL ET UN CHIFFRE POUR LA
C               PARTIE ENTIERE
C     ------------------------------------------------------------------
C     CONVENTION : SI LA CONVERSION EST IMPOSSIBLE ON METS DES ETOILES
C     ------------------------------------------------------------------
C     CONVENTION EN MODE 'E' :
C         - ON A TOUJOURS UN CHIFFRE POUR LA PARTIE ENTIERE
C         - L'EXPOSANT EST SUPPOSE ETRE A DEUX CHIFFRES
C     EXEMPLE  0.25 SERA CONVERTI EN 2.50 ... 0E-01
C     ------------------------------------------------------------------
      CHARACTER*2  P
      CHARACTER*10 FORMAT
      INTEGER      LONG, NBCHIF
      REAL*8       VALEUR
      LOGICAL      MARKTR
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IENT ,II ,IL ,IM ,NDEC 
C-----------------------------------------------------------------------
      LONG = LEN(CHAINE)
      IF ( LONG .LT. 2 ) GO TO 900
      NBCHIF = LONG-1
      IF ( REEL .GE. 0.D0 ) THEN
         VALEUR = REEL
      ELSE
         VALEUR = -REEL
         NBCHIF = NBCHIF-1
      ENDIF
      IF ( NBCHIF .LT. 1 ) GO TO 900
C     VERIFIER QUE NBCHIF <= NB_CHIF_MAX_MACHINE
C
C
      IF ( MODE .EQ. 'E' ) THEN
         P    =  '1P'
         NDEC = NBCHIF-6
         IF ( NDEC .LT. 0 ) GOTO 900
      ELSEIF ( MODE .EQ. 'F' ) THEN
C        NOMBRE DE CHIFFRE DE LA PARTIE ENTIERE
         P    =  '  '
         IENT = INT( LOG10(VALEUR)) + 1
         IF ( IENT .GE. 0 ) THEN
            NDEC = NBCHIF - IENT
         ELSE
            NDEC = NBCHIF
         ENDIF
         IF ( NDEC .LT. 0 ) GOTO 900
      ELSEIF ( MODE .EQ. 'G' ) THEN
         P    =  '  '
         NDEC = NBCHIF-5
         IF ( NDEC .LT. 0 ) GOTO 900
      ELSE
         GOTO 900
      ENDIF
      WRITE( FORMAT, '( ''('',A2,A1,I2,''.'',I2,'')'' )' ) P,MODE,
     +      NBCHIF,NDEC
      WRITE( CHAINE, FORMAT ) REEL
      IF ( MODE .EQ. 'E' ) THEN
         IM = 0
         MARKTR = .FALSE.
         DO 50, IL = 1,LONG
            II = LONG-IL+1
            IF ( ((CHAINE(II:II).EQ.'+').OR.(CHAINE(II:II).EQ.'-'))
     &            .AND.(.NOT.MARKTR) ) THEN
               IM = II
               MARKTR = .TRUE.
            ENDIF
            IF ( CHAINE(II:II).EQ.'E' ) GOTO 999
   50    CONTINUE
         IF ( IM.GT.1 ) CHAINE(IM-1:IM-1) = 'E'
      ENDIF
      GOTO 999
C     ------------------------------------------------------------------
 900  CONTINUE
      DO 910 IL = 1,LONG
         CHAINE(IL:IL) = '*'
 910  CONTINUE
C     ------------------------------------------------------------------
 999  CONTINUE
      END
