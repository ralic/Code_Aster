      SUBROUTINE IBBASE ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 13/01/2003   AUTEUR CIBHHLV L.VIVAN 
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
      CHARACTER*80  BANOM
      INTEGER       NBBL
      LOGICAL       BASAUV
C
      CHARACTER*16  MOTFAC, NOMRES, CONCEP, NOMCMD
C
C     DEFINITION DU KILO INFORMATIQUE
      INTEGER KILO
C
C     --- VARIABLES LOCALES --------------------------------------------
      PARAMETER   ( MXBASE = 3 )
      INTEGER      BANBBL(MXBASE), BALGBL(MXBASE), BALGRE(MXBASE)
C
C     --- VALEURS PAR DEFAUTS DES BASES --------------------------------
      INTEGER      PRESBA(MXBASE)
      CHARACTER*8  CALCBA(MXBASE) , CALCUL
      CHARACTER*16 NOMBA (MXBASE) , NOM
      CHARACTER*16 STIN  (MXBASE) , STOUT  (MXBASE) , STATUT
      CHARACTER*16 CASBA (MXBASE) , CAS
      CHARACTER*32 TITRBA(MXBASE)
C
C     --- VALEURS PAR DEFAUTS DES CAS ----------------------------------
      PARAMETER   ( MXCAS  = 3 )
      CHARACTER*16 CASCA (MXCAS)
      INTEGER      NBBLCA(MXBASE,MXCAS), LGBLCA(MXBASE,MXCAS)
      INTEGER      LGRECA(MXBASE,MXCAS)
C
      DATA      NOMBA  /'GLOBALE '   , 'VOLATILE'    , 'LOCALE  ' /
      DATA      PRESBA /    0        ,     0         ,      0     /
      DATA      TITRBA /'BASEGLOBALE', 'BASEVOLATILE', 'BASELOCALE'/
      DATA      STIN   /'........'   , 'DEBUT   '    , 'DEBUT   ' /
      DATA      STOUT  /'SAUVE   '   , 'SAUVE   '    , 'SAUVE   ' /
C
C
      DATA
     +CASCA  /'PETIT           ','MOYEN           ','GROS            '/
C
C     TAILLE(GLOBALE)        PETIT   MOYEN      GROS
      DATA
     +  (NBBLCA(1,I),I=1,3)/   0      , 0     ,    0        /,
     +  (LGBLCA(1,I),I=1,3)/ 100   ,  100     ,  100        /,
     +  (LGRECA(1,I),I=1,3)/2000   , 4000     , 6000        /
C
C     TAILLE(VOLATILE)       PETIT   MOYEN      GROS
      DATA
     +  (NBBLCA(2,I),I=1,3)/   0   ,    0     ,    0         /,
     +  (LGBLCA(2,I),I=1,3)/ 100   ,  100     ,  100         /,
     +  (LGRECA(2,I),I=1,3)/2000   , 2000     , 2000         /
C
C
C     TAILLE(LOCALE)         PETIT   MOYEN      GROS
      DATA
     +  (NBBLCA(3,I),I=1,3)/ 512   ,  512     ,  512         /,
     +  (LGBLCA(3,I),I=1,3)/ 100   ,  100     ,  100         /,
     +  (LGRECA(3,I),I=1,3)/2000   , 4000     , 6000         /
C
C     ------------------------------------------------------------------
      DATA    KILO/1024/
C     ------------------------------------------------------------------
C
C     INITIALISATION DU CODE RETOUR
      IER = 0
C
C     --- RECUPERATION DU NOM DE LA COMMANDE UTILISATEUR ---
      CALL GETRES( NOMRES , CONCEP , NOMCMD )
      STIN(1) = NOMCMD
C
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
         CALL GETVTX(MOTFAC,'FICHIER',IBASE,1,1,NOM,NB)
         CALL UTREMT( NOM, NOMBA, MXBASE, INDBAS )
         IF ( INDBAS .EQ. 0 ) THEN
            INDBAS = 1
            IER    = IER + 1
            CALL UTDEBM('E',NOMCMD,'"'//NOM//'" NOM SYMBOLIQUE INCONNU')
            CALL UTIMPK('L','VALEURS ATTENDUES',MXBASE,NOMBA)
            CALL UTFINM()
         ELSE
            IF ( PRESBA(INDBAS) .NE. 0 ) THEN
               IER = IER + 1
               CALL UTMESS('E',NOMCMD,NOM//' NOM DE BASE DEJA DEFINIE')
            ELSE
               PRESBA(INDBAS) = 1
            ENDIF
         ENDIF
C
C        --- MOT CLE "CAS" ---------------------------------------------
C
         CALL GETVTX(MOTFAC,'CAS',IBASE,1,1,CAS,NB)
         IF ( NB.GT.0 ) THEN
            CALL UTREMT( CAS , CASCA , MXCAS , INDCAS )
            IF ( INDCAS .EQ. 0 ) THEN
               INDCAS = 1
               IER = IER + 1
               CALL UTDEBM('E',NOMCMD,'L''ARGUMENT DU MOT CLE '//
     +                            '"CAS"  EST ERRONE ')
               CALL UTIMPK('L','VALEUR LUE',1,CAS)
               CALL UTIMPK('L','VALEURS ATTENDUES',MXCAS,CASCA)
               CALL UTFINM()
            ENDIF
         ENDIF
C
C        ---NOMBRE DE BLOC D'ENREGISTREMENT ----------------------------
         BANBBL(INDBAS) =  NBBLCA(INDBAS,INDCAS)
         CALL GETVIS(MOTFAC,'NMAX_ENRE',IBASE,1,1,BANBBL(INDBAS),NB)
C
C        --- LONGUEUR D'UN BLOC D'ENREGISTREMENT -----------------------
         BALGBL(INDBAS) =  LGBLCA(INDBAS,INDCAS)
         CALL GETVIS(MOTFAC,'LONG_ENRE',IBASE,1,1,BALGBL(INDBAS),NB)
C
         LKM = 1024*LOISEM()
         LTT = BANBBL(INDBAS)*BALGBL(INDBAS)*LKM         
         IF ( LTT .GT. MOFIEM() ) THEN
            IER = IER + 1
            CALL UTDEBM('E',NOMCMD,'LE NOMBRE D''ENREGISTREMENTS '
     +        //'(NMAX_ENRE) ET LEURS LONGUEURS (LONG_ENRE) CONDUISENT'
     +        //' A UN FICHIER DONT LA TAILLE MAXIMALE EN OCTETS EST')
            CALL UTIMPI('S',' : ',1,LTT)
            CALL UTIMPI('S','SUPERIEURE A LIMITE AUTORISEE : ',
     +                   1,MOFIEM())
            CALL UTFINM()
         ENDIF

C        --- MOT CLE "LONG_REPE" ---------------------------------------
         BALGRE(INDBAS) =  LGRECA(INDBAS,INDCAS)
         CALL GETVIS(MOTFAC,'LONG_REPE',IBASE,1,1,BALGRE(INDBAS),NB)
C
C        --- MOT CLE "TITRE" -------------------------------------------
         CALL GETVTX(MOTFAC,'TITRE',IBASE,1,1,TITRBA(INDBAS),NB)
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
               CALL UTMESS('E',NOMCMD,STIN(INDBAS)//
     +                       ' STATUT IMPOSSIBLE POUR LA BASE GLOBALE')
            ENDIF
         ENDIF
      ENDIF
C
C     --- DEFINITION DES UNITES LOGIQUES DES BASES DE DONNEES ---
C
      IF ( IER .EQ. 0 ) THEN
C
C        --- DESTRUCTION DU FICHIER TAMPON DUMMY ---
         CALL JELIBF('DETRUIT','L')
C
C        --- RE-DEFINITION DE L'ENVIRONNEMENT SELON DESIRS UTILISATEUR -
C
C        --- INITIALISATION DE CHAQUE BASE ---
         DO 300 IBASE = 1, MXBASE
            CALL JEINIF( STIN(IBASE), STOUT(IBASE),
     +                   NOMBA(IBASE)(1:8), NOMBA(IBASE)(1:1),
     +                   BALGRE(IBASE),BANBBL(IBASE),BALGBL(IBASE)*KILO)
  300    CONTINUE
      ELSE
C
         CALL UTMESS('E',NOMCMD,
     +                   'PROBLEME D''ALLOCATION DES BASES DE DONNEES')
      ENDIF
C
      END
