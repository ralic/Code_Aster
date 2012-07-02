      SUBROUTINE IBBASE ( IER , FICHDF)
      IMPLICIT NONE
      INTEGER             IER
      CHARACTER*(*)             FICHDF
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     ALLOCATION ET OUVERTURE DES BASES DE DONNEES
C     ------------------------------------------------------------------
C IN  COMMAND : CH* : NOM DE LA COMMANDE APPELANTE  (DEBUT OU POURSUITE)
C OUT IER     : IS  : CODE RETOUR D'EXECUTION
C         0 ==> PAS DE PROBLEME
C         1 ==> PROBLEME D'ALLOCATION DES BASES DE DONNEES
C         2 ==> PROBLEME D'OUVERTURE DES BASES DE DONNEES
C     ------------------------------------------------------------------
C
C     --- VARIABLES LOCALES --------------------------------------------
C     NOM DES BASES DE DONNEES AUTORISEES
C     REMARQUE :  UN DDNAME OU SHORT NAME NE PEUT EXCEDER 7 CARACTERES
C     ------------------------------------------------------------------
C
      CHARACTER*16  MOTFAC, NOMRES, CONCEP, NOMCMD
C
C     --- VARIABLES LOCALES --------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IBASE ,IDEB ,INDBAS ,INDCAS ,LOISEM ,LTT 
      INTEGER MOFIEM ,MXBASE ,MXCAS ,NB ,NBBASE 
C-----------------------------------------------------------------------
      PARAMETER   ( MXBASE = 2 )
      INTEGER      BANBBL(MXBASE), BALGBL(MXBASE), BALGRE(MXBASE)
C
C     --- VALEURS PAR DEFAUTS DES BASES --------------------------------
      INTEGER      PRESBA(MXBASE)
      CHARACTER*16 NOMBA (MXBASE) , NOM
      CHARACTER*16 STIN  (MXBASE) , STOUT  (MXBASE)
      CHARACTER*16  CAS
      CHARACTER*32 TITRBA(MXBASE)
C
C     --- VALEURS PAR DEFAUTS DES CAS ----------------------------------
      PARAMETER   ( MXCAS  = 3 )
      CHARACTER*16 CASCA (MXCAS)
      CHARACTER*24 VALK(3)
      INTEGER      NBBLCA(MXBASE,MXCAS), LGBLCA(MXBASE,MXCAS)
      INTEGER      LGRECA(MXBASE,MXCAS)
      INTEGER      VALI(2),INFO
      INTEGER      IARG
C
      DATA      NOMBA  /'GLOBALE '   , 'VOLATILE'   /
      DATA      PRESBA /    0        ,     0        /
      DATA      TITRBA /'BASEGLOBALE', 'BASEVOLATILE'/
      DATA      STIN   /'........'   , 'DEBUT   '    /
      DATA      STOUT  /'SAUVE   '   , 'SAUVE   '    /
C
C
      DATA
     &CASCA  /'PETIT           ','MOYEN           ','GROS            '/
C
C     TAILLE(GLOBALE)        PETIT   MOYEN      GROS
      DATA
     &  (NBBLCA(1,I),I=1,3)/   0      , 0     ,    0        /,
     &  (LGBLCA(1,I),I=1,3)/ 100   ,  100     ,  100        /,
     &  (LGRECA(1,I),I=1,3)/2000   , 4000     , 6000        /
C
C     TAILLE(VOLATILE)       PETIT   MOYEN      GROS
      DATA
     &  (NBBLCA(2,I),I=1,3)/   0   ,    0     ,    0         /,
     &  (LGBLCA(2,I),I=1,3)/ 100   ,  100     ,  100         /,
     &  (LGRECA(2,I),I=1,3)/2000   , 2000     , 2000         /
C
C     ------------------------------------------------------------------
C
C     INITIALISATION DU CODE RETOUR
      IER = 0
C
C     --- RECUPERATION DU NOM DE LA COMMANDE UTILISATEUR ---
      CALL GETRES( NOMRES , CONCEP , NOMCMD )
      STIN(1) = NOMCMD
C
      INDCAS = 1
      DO 12 INDBAS = 1, MXBASE
         BANBBL(INDBAS) =  NBBLCA(INDBAS,INDCAS)
         BALGBL(INDBAS) =  LGBLCA(INDBAS,INDCAS)
         BALGRE(INDBAS) =  LGRECA(INDBAS,INDCAS)
   12 CONTINUE
C
C     --- NOMBRE DE BASES SPECIFIEES PAR L'UTILISATEUR -----------------
      MOTFAC = 'BASE'
      CALL GETFAC(MOTFAC,NBBASE)
C
      DO 100 IBASE = 1, NBBASE
C
C        --- MOT CLE "FICHIER" ANCIENNEMENT "NOM" ---------------------
         CALL GETVTX(MOTFAC,'FICHIER',IBASE,IARG,1,NOM,NB)
         CALL UTREMT( NOM, NOMBA, MXBASE, INDBAS )
         IF ( INDBAS .EQ. 0 ) THEN
            INDBAS = 1
            IER    = IER + 1
            VALI (1)   = MXBASE
            VALK (1) = NOM
            VALK (2) = NOMBA(1)
            VALK (3) = NOMBA(2)
          CALL U2MESG('E','SUPERVIS_81',3,VALK,1,VALI,0,0.D0)
         ELSE
            IF ( PRESBA(INDBAS) .NE. 0 ) THEN
               IER = IER + 1
             CALL U2MESK('E','SUPERVIS_13',1,NOM)
            ELSE
               PRESBA(INDBAS) = 1
            ENDIF
         ENDIF
C
C        --- MOT CLE "CAS" ---------------------------------------------
C
         CALL GETVTX(MOTFAC,'CAS',IBASE,IARG,1,CAS,NB)
         IF ( NB.GT.0 ) THEN
            CALL UTREMT( CAS , CASCA , MXCAS , INDCAS )
            IF ( INDCAS .EQ. 0 ) THEN
               INDCAS = 1
               IER = IER + 1
               VALI (1)= MXCAS
               VALK (1) = CAS
               VALK (2) = CASCA(1)
               VALK (3) = CASCA(2)
               CALL U2MESG('E','SUPERVIS_82',3,VALK,1,VALI,0,0.D0)
            ENDIF
         ENDIF
C
C        ---NOMBRE DE BLOC D'ENREGISTREMENT ----------------------------
         BANBBL(INDBAS) =  NBBLCA(INDBAS,INDCAS)
         CALL GETVIS(MOTFAC,'NMAX_ENRE',IBASE,IARG,1,BANBBL(INDBAS),NB)
C
C        --- LONGUEUR D'UN BLOC D'ENREGISTREMENT -----------------------
         BALGBL(INDBAS) =  LGBLCA(INDBAS,INDCAS)
         CALL GETVIS(MOTFAC,'LONG_ENRE',IBASE,IARG,1,BALGBL(INDBAS),NB)
C
         LTT = BANBBL(INDBAS)*BALGBL(INDBAS)*LOISEM()
         IF ( LTT .GT. MOFIEM() ) THEN
            IER = IER + 1
            VALI (1) = LTT
            VALI (2) = MOFIEM()
            CALL U2MESG('E','SUPERVIS_83',0,' ',2,VALI,0,0.D0)
         ENDIF

C        --- MOT CLE "LONG_REPE" ---------------------------------------
         BALGRE(INDBAS) =  LGRECA(INDBAS,INDCAS)
         CALL GETVIS(MOTFAC,'LONG_REPE',IBASE,IARG,1,BALGRE(INDBAS),NB)
C
C        --- MOT CLE "TITRE" -------------------------------------------
         CALL GETVTX(MOTFAC,'TITRE',IBASE,IARG,1,TITRBA(INDBAS),NB)
C
  100 CONTINUE
C
C
C     --- QUELQUES CONTROLES SUPPLEMENTAIRES SUR LA GLOBALE EN POURSUITE
      IF ( NOMCMD .EQ. 'POURSUITE' ) THEN
         CALL UTREMT( 'GLOBALE', NOMBA, MXBASE, INDBAS )
         IF ( INDBAS .GT. 0 ) THEN
            IF ( STIN(INDBAS).NE. 'POURSUITE' ) THEN
               IER = IER + 1
               CALL U2MESK('E','SUPERVIS_14',1,STIN(INDBAS))
            ENDIF
         ENDIF
      ENDIF
C
C     --- DEFINITION DES UNITES LOGIQUES DES BASES DE DONNEES ---
C
      IF ( IER .EQ. 0 ) THEN
C
C        --- DESTRUCTION DE LA BASE TEMPORAIRE VOLATILE ---
         INFO = 0
         CALL JELIBF('DETRUIT','V',INFO)
C
C        --- RE-DEFINITION DE L'ENVIRONNEMENT SELON DESIRS UTILISATEUR -
C
C        --- INITIALISATION DE CHAQUE BASE ---
         IF ( FICHDF .NE. ' ') THEN
            CALL JELIHD ('GLOBALE ',FICHDF,'G')
C           --- DESTRUCTION DU FICHIER POUR QU'ON NE CONFONDE PAS PLUS
C               TARD AVEC UNE EVENTUELLE BASE HDF EN RESULTAT ---
            INFO=1
            CALL RMFILE(FICHDF,INFO)
         ENDIF
         IDEB = 1
         IF ( FICHDF .NE. ' ') IDEB = 2

         DO 300 IBASE = IDEB, MXBASE
            CALL JEINIF( STIN(IBASE), STOUT(IBASE),
     &                   NOMBA(IBASE)(1:8), NOMBA(IBASE)(1:1),
     &                   BALGRE(IBASE),BANBBL(IBASE),BALGBL(IBASE))
  300    CONTINUE
      ELSE
C
         CALL U2MESS('E','SUPERVIS_15')
      ENDIF
C
      END
