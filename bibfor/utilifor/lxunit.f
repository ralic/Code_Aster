      SUBROUTINE LXUNIT(IREAD,LREC,IWRITE,CVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IREAD,LREC,IWRITE
      CHARACTER*(*)                       CVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/06/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C     DEFINITION DE L'UNITE COURANTE DE LECTURE ET/OU D'ECRITURE POUR
C     ------------------------------------------------------------------
C IN  IREAD  : IS    : UNITE DE LECTURE
C IN  LREC   : IS    : LONGUEUR DE L'ENREGISTREMENT EN LECTURE <= 80
C IN  IWRITE : IS    : UNITE D'ECRITURE DES LIGNES LUES ET DES ERREURS
C IN  CVAL   : CH(8) : NOM SYMBOLIQUE ASSOCIE A (IREAD,LREC)
C     ------------------------------------------------------------------
C     CONVENTION :
C        SI IREAD = 0
C           ALORS ON REPREND L'UNITE DE LECTURE DEFINIE PAR CVAL EN RE -
C                 ACTUALISANT LES AUTRES PARAMETRES S'ILS SONT NON NULS
C        SI IREAD < 0
C           ALORS ON RETIRE DES TABLES L'UNITE DE LECTURE DEFINIE
C                 PAR CVAL
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         LXERR
C     ROUTINE(S) FORTRAN     :
C         MIN    WRITE
C     ------------------------------------------------------------------
C FIN LXUNIT
C     ------------------------------------------------------------------
C
      PARAMETER  ( MXCOLS = 80 , MXFILE = 30 )
C
C     VARIABLES GLOBALES DE LECTURE-ECRITURE ---------------------------
      CHARACTER*(MXCOLS) CARTE
      COMMON /LXCC02/    CARTE
      INTEGER            ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      COMMON /LXCN02/    ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      CHARACTER*20       FMTWRT
      COMMON /LXCC03/    FMTWRT
C     ------------------------------------------------------------------
C     VARIABLES POUR LA DEFINITION DES UNITES LOGIQUES (I/O)
      CHARACTER*8       FILE(MXFILE)
      INTEGER     NBFILE,FILERE(MXFILE), FILEWR(MXFILE), FILELN(MXFILE)
      COMMON /LXCC04/   FILE
      COMMON /LXCN04/   NBFILE,FILERE,FILEWR,FILELN
      CHARACTER*8 CH8
C     ------------------------------------------------------------------
C     VARIABLES POUR LA DEFINITION DES CONTENUS DES UNITES LOGIQUES
      CHARACTER*(MXCOLS) CARTES(MXFILE)
      INTEGER            ILIGS(MXFILE), ICOLS(MXFILE), EOFILE(MXFILE)
      SAVE      CARTES,  ILIGS,         ICOLS,         EOFILE
C
      CHARACTER*(MXCOLS) CARTE2
C     ------------------------------------------------------------------
      DATA        MBFILE/0/
C     ------------------------------------------------------------------
C             DEFINITION DES UNITES LOGIQUES LECTURE/ECRITURE
C
C     --- INITIALISATION DES TABLEAUX ET DU FORMAT D'ECRITURE ---
      IF ( MBFILE .EQ. 0 ) THEN
         FMTWRT = '(1X,I5,''!'',A,''!'')'
         DO 500  IFILE = 1 , MXFILE
            FILE(IFILE) = '   '
  500    CONTINUE
         MBFILE = 1
         NBFILE = 0
      ENDIF
C
C     ---- APPEL A ULDEFI SUR L'UNITE DE LECTURE : COHERENCE ASTER -----
      CALL ULDEFI( IREAD , CVAL , 'A' , 'N' , 'O')
C
      CH8   = CVAL
      IF ( IREAD .LT. 0 ) THEN
         DO 1000 IFILE = 1 , MXFILE
            IF ( FILE(IFILE) .EQ. CH8 ) THEN
               FILE(IFILE) = '   '
               GOTO 1001
            ENDIF
 1000    CONTINUE
         CALL LXERR(CH8,' UNITE INDEFINIE, DESALLOCATION IMPOSSIBLE')
 1001    CONTINUE
C
      ELSE
C
         IF (NBFILE .GT. 0 ) THEN
C           --- SAUVEGARDE DE L'UNITE COURANTE ---
            IF ( FILE(NBFILE) .NE. '  ' ) THEN
               FILERE(NBFILE) = ILECT
               FILEWR(NBFILE) = IECR
               FILELN(NBFILE) = LRECL
               ICOLS (NBFILE) = ICOL
               ILIGS (NBFILE) = ILIG
               CARTES(NBFILE) = CARTE
               EOFILE(NBFILE) = IEOF
            ENDIF
         ENDIF
C
         IF ( IREAD .EQ. 0 ) THEN
C            --- REDEFINITION DE L'UNITE COURANTE ---
             DO 2000 IFILE = 1 , MXFILE
                IF ( FILE(IFILE) .EQ. CH8 ) THEN
                   NBFILE = IFILE
                   GOTO 9000
                 ENDIF
 2000        CONTINUE
             CALL LXERR(CH8,' UNITE INDEFINIE,  ALLOCATION IMPOSSIBLE')
             GOTO 9999
C
         ELSE
             DO 3000 IFILE = 1 , MXFILE
               IF ( FILE(IFILE) .EQ. '  ' ) THEN
                  NBFILE = IFILE
                  GOTO 3001
               ENDIF
 3000        CONTINUE
             CALL LXERR(CH8,' -- TROP D''UNITES DEFINIES SIMULTANEMENT')
             GOTO 9999
 3001        CONTINUE
             FILE  (NBFILE) = CH8
             FILERE(NBFILE) = IREAD
             FILEWR(NBFILE) = IWRITE
             FILELN(NBFILE) = MIN( LREC , MXCOLS )
             ICOLS (NBFILE) = MXCOLS+1
             ILIGS (NBFILE) = 0
             EOFILE(NBFILE) = 0
         ENDIF
C
 9000    CONTINUE
         ILECT = FILERE(NBFILE)
         IECR  = FILEWR(NBFILE)
         IF ( IWRITE .GT. 0 ) IECR  = IWRITE
         LRECL = FILELN(NBFILE)
         ICOL  = ICOLS (NBFILE)
         ILIG  = ILIGS (NBFILE)
         CARTE = CARTES(NBFILE)
         IEOF  = EOFILE(NBFILE)
         IF (IECR.GT.0 .AND. ICOL.LE.MXCOLS .AND. ILIG.GT.0) THEN
            IF ( ICOL .LT. 2 ) THEN
               WRITE(IECR ,FMTWRT)   ILIG, CARTE(1:LRECL)
            ELSE
               CARTE2 = ' '
               CARTE2(ICOL:) = CARTE(ICOL:)
               WRITE(IECR ,FMTWRT)  ILIG,  CARTE2(1:LRECL)
            ENDIF
         ENDIF
      ENDIF
      GOTO 9999
C     --- SORTIE EN ERREUR ---
 9999 CONTINUE
C
      END
