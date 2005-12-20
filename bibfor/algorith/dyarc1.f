      SUBROUTINE DYARC1 ( INSTC, NBPAS, INSTA, NBINST, ARCH,
     +                    EPSI, CRIT  )
      IMPLICIT   NONE
      INTEGER             NBPAS, NBINST, ARCH(*)
      REAL*8              EPSI, INSTC(*), INSTA(*)
      CHARACTER*8         CRIT
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2001   AUTEUR CIBHHLV L.VIVAN 
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
C     SAISIE DU MOT CLE FACTEUR "ARCHIVAGE"
C
C IN  : INSTC  : INSTANTS DE CALCUL
C IN  : NBPAS  : NOMBRE DE PAS DE CALCUL
C IN  : INSTA  : INSTANTS D'ARCHIVAGE
C IN  : NBINST : NOMBRE DE PAS D'ARCHIVAGE
C IN  : LISARC : LISTE D'ARCHIVAGE DES PAS DE CALCUL
C OUT : ARCH   : NUMERO D'ORDRE DES INATANTS A ARCHIVER
C IN  : EPSI   : PRECISION DE RECHERCHE
C IN  : CRIT   : CRITERE DE RECHERCHE
C ----------------------------------------------------------------------
      INTEGER      NBTROU, I, J
      REAL*8       RVAL
      LOGICAL      TROUVE
      CHARACTER*8  K8B
      CHARACTER*16 TYPCON, NOMCMD
C     ------------------------------------------------------------------
C
      CALL GETRES ( K8B, TYPCON, NOMCMD )
C
      DO 10 I = 1 , NBINST
         NBTROU = 0
         RVAL = INSTA(I)
         DO 20 J = 1 , NBPAS
            IF ( CRIT(1:4) .EQ. 'RELA' ) THEN
               IF (ABS(INSTC(J)-RVAL).LE.ABS(EPSI*RVAL)) THEN
                  TROUVE = .TRUE.
               ELSE
                  TROUVE = .FALSE.
               END IF
            ELSE IF ( CRIT(1:4) .EQ. 'ABSO' ) THEN
               IF (ABS(INSTC(J)-RVAL).LE.ABS(EPSI)) THEN
                  TROUVE = .TRUE.
               ELSE
                  TROUVE = .FALSE.
               ENDIF
            ELSE
               CALL UTMESS('F',NOMCMD,'CRITERE INCONNU : '//CRIT)
            ENDIF
            IF ( TROUVE ) THEN
               NBTROU = NBTROU + 1
C               IF ( J .EQ. 1 ) THEN
C                  ARCH(1) = 1
C               ELSE
C                  ARCH(J-1) = 1
C               ENDIF
               ARCH(J) = 1
            ENDIF
 20      CONTINUE
         IF ( NBTROU .EQ. 0 ) THEN
            CALL UTDEBM ( 'F', NOMCMD, 'DONNEES ERRONEES' )
            CALL UTIMPR ( 'L','PAS D''INSTANT DE CALCUL POUR '//
     +                    'L''INSTANT D''ARCHIVAGE: ', 1, RVAL )
            CALL UTFINM ( )
         ELSEIF ( NBTROU .NE. 1 ) THEN
            CALL UTDEBM ( 'F', NOMCMD, 'DONNEES ERRONEES' )
            CALL UTIMPR ( 'L','PLUSIEURS INSTANTS DE CALCUL POUR '//
     +                    'L''INSTANT D''ARCHIVAGE: ',  1, RVAL )
            CALL UTFINM ( )
         ENDIF
 10   CONTINUE
C
      END
