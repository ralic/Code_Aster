      SUBROUTINE CODENT( ENTIER , CADRE , CHAINE  )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            ENTIER
      CHARACTER*(*)               CADRE , CHAINE
C
C     ------------------------------------------------------------------
C     CODAGE D'UN ENTIER SUR UNE CHAINE DE CARACTERE
C     ------------------------------------------------------------------
C IN  ENTIER : IS    : ENTIER A CONVERTIR EN CHAINE
C IN  CADRE  : CH(*) : TYPE DE CADRAGE
C          D     CADRAGE A DROITE
C          D0    CADRAGE A DROITE ET ON COMPLETE A GAUCHE PAR DES ZERO
C          G     CADRAGE A GAUCHE
C OUT CHAINE : CH(*) : CHAINE RECEPTACLE, ON UTILISE TOUTE LA LONGUEUR
C                      DE LA CHAINE
C     ------------------------------------------------------------------
C     REMARQUE : EN CAS D'ERREUR (A LA TRANSCRIPTION OU DANS LE TYPE DE
C                CADRAGE)   LA CHAINE EST REMPLIE D'ETOILE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 04/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         LEN    MOD
C     ------------------------------------------------------------------
C FIN CODENT
C     ------------------------------------------------------------------
C
C
      INTEGER     LG , ENT , IVAL
      LOGICAL     NEG
      CHARACTER*1 CHIFFR(0:9)
      DATA        CHIFFR/'0','1','2','3','4','5','6','7','8','9'/
C
C
      IER    = 0
      CHAINE = ' '
C
      ENT    = ENTIER
      NEG    = ENT .LT. 0
      IF ( NEG ) ENT = -ENT
      LG     = LEN(CHAINE)
C
C     ON CADRE A DOITE A PRIORI   CADRAGE A DROITE
      IL = LG + 1
  10  CONTINUE
        IL = IL - 1
        IF (IL .LE.0) THEN
           IER = 1
           GOTO 99000
        ELSE
           IVAL = MOD(ENT,10)
           CHAINE(IL:IL) = CHIFFR(IVAL)
           ENT  = ENT / 10
        ENDIF
      IF (ENT.NE.0) GOTO 10
C
      IF ( NEG ) THEN
         IL = IL - 1
         IF (IL .LE.0) THEN
            IER = 1
            GOTO 99000
         ELSE
            CHAINE(IL:IL) = '-'
         ENDIF
      ENDIF
C
      IF ( CADRE(1:1) .EQ. 'D' ) THEN
C        --- CADRAGE A DROITE ---
         IF (LEN(CADRE).GT.1 ) THEN
            IF ( CADRE(2:2).EQ.'0') THEN
               IF ( NEG ) CHAINE(IL:IL) = '0'
               DO 20 I = IL-1 , 1 , -1
                  CHAINE(I:I) = '0'
   20          CONTINUE
               IF ( NEG ) CHAINE(1:1) = '-'
            ENDIF
         ENDIF
C
      ELSEIF ( CADRE(1:1) .EQ. 'G' ) THEN
C        --- CADRAGE A GAUCHE ---
         IL1 = IL-1
         DO 30 I = 1 , LG-IL1
             CHAINE(I:I) = CHAINE(I+IL1:I+IL1)
   30    CONTINUE
         CHAINE(LG-IL1+1:) = ' '
      ELSE
         IER = 1
      ENDIF
C
C     SORTIE -----------------------------------------------------------
99000 CONTINUE
      IF ( IER. NE. 0 ) THEN
         DO 9001 I = 1 , LG
            CHAINE(I:I) = '*'
 9001    CONTINUE
      ENDIF
C
      END
