      SUBROUTINE LXPOSI( JLIG , JCOL , JECR )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            JLIG,  JCOL , JECR
C
C     ------------------------------------------------------------------
C     SE POSITIONNE SUR LA JLIG-EME LIGNE DU FICHIER EN JCOL COLONNE
C     ------------------------------------------------------------------
C IN  JLIG  :   NUMERO DE LA LIGNE A ANALYSER
C IN  JCOL  :   NUMERO DU DEBUT DE LA COLONNE A ANALYSER
C IN  JECR  :   UNITE D'ECRITURE POUR LES LIGNES SAUTEES
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C         LXRWD
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN LXPOSI
C     ------------------------------------------------------------------
C
      PARAMETER  ( MXCOLS = 80 )
C
C     VARIABLES GLOBALES DE LECTURE-ECRITURE ---------------------------
      CHARACTER*(MXCOLS) CARTE
      COMMON /LXCC02/    CARTE
      INTEGER            ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      COMMON /LXCN02/    ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      CHARACTER*20       FMTWRT
      COMMON /LXCC03/    FMTWRT
C     ------------------------------------------------------------------
      INTEGER JLIG0
C     ------------------------------------------------------------------
C
C     --- DETERMINATION DU NOMBRE REEL DE LIGNES A LIRE ---
      IF ( JLIG .LT. ILIG )  THEN
C        --- ON REMBOBINE ---
         CALL LXRWD
         JLIG0 = JLIG
      ELSE
         JLIG0 = JLIG - ILIG
      ENDIF
C
C     --- ON SAUTE LES LIGNES ---
      DO 10 I = 1, JLIG0-1
         READ(ILECT,'(A)',END=999,ERR=999) CARTE(1:LRECL)
         ILIG   = ILIG + 1
         IF (JECR.GT.0) WRITE(JECR ,FMTWRT)  ILIG, CARTE(1:LRECL)
 10   CONTINUE
C
C     --- LECTURE DE LA NOUVELLE LIGNE COURANTE ---
      IF ( JLIG0 .GT. 0 ) THEN
         READ(ILECT,'(A)',END=999,ERR=999) CARTE(1:LRECL)
         ILIG   = ILIG + 1
         IF (IECR.GT.0) WRITE(IECR ,FMTWRT)  ILIG, CARTE(1:LRECL)
         ICOL = JCOL
      ELSEIF ( JLIG .GT. 0 ) THEN
C        --- REDEFINITION DE LA COLONNE DE DEPART DE LA LIGNE COURANTE -
         ICOL = JCOL
      ENDIF
      GOTO 9999
C
  999 CONTINUE
      IEOF = 1
      ICOL = LRECL+1
C
 9999 CONTINUE
      END
