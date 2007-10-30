      SUBROUTINE GCUINI ( MXCMDU , BASE  , IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            MXCMDU         , IER
      CHARACTER*(*)               BASE
C     ------------------------------------------------------------------
C     INITIALISATION DES TABLEAUX POUR LES COMMANDES UTILISATEURS
C     ------------------------------------------------------------------
C IN  MXCMDU : IS : NOMBRE DE COMMANDES UTILISATEURS MAXIMUM
C IN  BASE   : CH : TYPE DE LA BASE 'L' 'G' 'V' ....
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 29/10/2007   AUTEUR PELLET J.PELLET 
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     --- VARIABLES GLOBALES -------------------------------------------
      CHARACTER*4     CBID
C
C     --- VARIABLES LOCALES --------------------------------------------
      CHARACTER*1    DDBASE
      CHARACTER*8    NOMUSR,NOMOBJ
      CHARACTER*16   NOMCMD, TYPCON
      CHARACTER*24   SPVR,OBJ1,OBJ2,VALK(2)
      CHARACTER*80   INIT
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IER    = 0
      NOMUSR = '  '
C     ------------------------------------------------------------------
C     DESCRIPTION DE '&&SYS.KRESU' :
C          ZK80(ICMD)( 1: 8) = NOM UTILISATEUR DU RESULTAT
C          ZK80(ICMD)( 9:24) = NOM DU CONCEPT DU RESULTAT
C          ZK80(ICMD)(25:40) = NOM DE L'OPERATEUR
C          ZK80(ICMD)(41:48) = STATUT DE L'OBJET
C              STATUT = '&A FAIRE' : VALEUR INITIALE
C                       '&ATTENTE' : OPERATEUR DECODE, NON CHARGER
C                       '&ENCOURS' : OPERATEUR EN COURS D'EXECUTION
C                       '&EXECUTE' : OPERATEUR TERMINER CORRECTEMENT
C     ------------------------------------------------------------------
      INIT   = '&ABSENT &PAS DE CONCEPT &PAS DE COMMANDE&A FAIRE'
      CALL JEEXIN ('&&SYS.KRESU',IER)
      IF ( IER .EQ. 0 ) THEN
C        --- INITIALISATION (DEBUT) ---
         IF ( MXCMDU .GT. 0 ) THEN
            DDBASE = BASE(1:1)
            CALL WKVECT('&&SYS.KRESU',DDBASE//' V K80',MXCMDU,LGRESU)
            DO 10 ICMD = LGRESU , LGRESU + MXCMDU - 1
               ZK80(ICMD) = INIT
  10        CONTINUE
            CALL JEECRA ('&&SYS.KRESU','LONUTI',0,CBID)
         ELSE
           CALL U2MESS('F','SUPERVIS_9')
         ENDIF
      ELSE
C           --- VERIFICATION (POURSUITE) ---
         CALL JELIRA ('&&SYS.KRESU','LONUTI',LONUTI,CBID)
         CALL JEVEUO ('&&SYS.KRESU','E',LGRESU)
         JCMD = LONUTI
         IF ( ZK80(LGRESU+JCMD)(41:48) .EQ. '&ENCOURS') THEN
           IF ( ZK80(LGRESU+JCMD)(9:24).EQ.' ') THEN
              IER = 1
           ELSE
              IER = 2
           ENDIF
           JCMD = JCMD + 1
         ELSEIF ( ZK80(LGRESU+JCMD-1)(41:52).EQ.'&EXECUTESPVR') THEN
           IER = 3
         ELSE
           IER = 0
         ENDIF
C
C        ---- MESSAGES RELATIF A L'EXECUTION PRECEDENTE ---
         NOMUSR = ZK80(LGRESU+JCMD-1)(1:8)
         TYPCON = ZK80(LGRESU+JCMD-1)(9:24)
         NOMCMD = ZK80(LGRESU+JCMD-1)(25:40)
         CALL CODENT( JCMD,'D0',CBID)
         VALK (1)= CBID
         CALL U2MESG('I+','SUPERVIS_71',1,VALK,0,0,0,0.D0)
         IF (IER .EQ. 0 ) THEN
             CALL U2MESG('I+','SUPERVIS_72',0,' ',0,0,0,0.D0)
         ELSEIF (IER .EQ. 1 ) THEN
             LG = LXLGUT(NOMCMD)
             VALK (1)= NOMCMD(1:LG)
             CALL U2MESG('I+','SUPERVIS_73',1,VALK,0,0,0,0.D0)
         ELSEIF (IER .EQ. 2 ) THEN
             LG = LXLGUT(NOMCMD)
             VALK (1) = NOMCMD(1:LG)
             CALL U2MESG('I+','SUPERVIS_74',1,VALK,0,0,0,0.D0)
             LG1 = LXLGUT(NOMUSR)
             LG2 = LXLGUT(TYPCON)
             VALK(1) = NOMUSR(1:LG1)
             VALK(2) = TYPCON(1:LG2)
             CALL U2MESG('I+','SUPERVIS_75',2,VALK,0,0,0,0.D0)
         ELSEIF (IER .EQ. 3 ) THEN
             LG = LXLGUT(NOMCMD)
             VALK(1) = NOMCMD(1:LG)
             CALL U2MESG('I+','SUPERVIS_76',1,VALK,0,0,0,0.D0)
             LG1 = LXLGUT(NOMUSR)
             LG2 = LXLGUT(TYPCON)
             VALK(1) = NOMUSR(1:LG1)
             VALK(2) = TYPCON(1:LG2)
             CALL U2MESG('I+','SUPERVIS_77',2,VALK,0,0,0,0.D0)
             CALL UTSAUT()
            SPVR = NOMUSR
            SPVR(20:24) = '.SPVR'
            CALL JEEXIN(SPVR,IRET)
            IF ( IRET. NE. 0 )  THEN
               CALL JEVEUO(SPVR,'L',LSPVR)
               CALL JELIRA(SPVR,'LONMAX',LONMAX,CBID)
               VALK (1) = ZK80(LSPVR)
               CALL U2MESG('I+','SUPERVIS_78',1,VALK,0,0,0,0.D0)
            ELSE
               CALL U2MESG('I+','SUPERVIS_79',0,' ',0,0,0,0.D0)
            ENDIF
            NOMUSR = '   '
            CALL UTSAUT()
         ENDIF
        CALL U2MESG('I','SUPERVIS_80',0,' ',0,0,0,0.D0)
C
C     --- SUPPRESSION DES CONCEPTS TEMPORAIRES DES MACRO
      CALL JEDETC('G','.',1)
      ICMC   = LONUTI + 1
      CALL GCDETP(ICMC,'.')
C
C        --- DESTRUCTION DU CONCEPT VEROLE ---
        IF ( NOMUSR .NE. '  ' ) THEN
           LG = LXLGUT(NOMUSR)
           CALL U2MESK('I','SUPERVIS_10',1,NOMUSR(1:LG))
           CALL JEDETC( ' ' , NOMUSR , 1 )
        ENDIF
C
C       --- ON REINITILISE LE TABLEAU ET ON DETRUIT ----
        IF ( ZK80(LGRESU+LONUTI)(41:48) .NE. '&A FAIRE') THEN
           ICMD   = LONUTI + 1
           LDEC   = LGRESU + LONUTI
  20       CONTINUE
           IF ( ZK80(LDEC)(41:48) .NE. '&A FAIRE') THEN
              NOMOBJ = '&USR0000'
              CALL CODENT( ICMD , 'D0' , NOMOBJ( 5: 8) )
              CALL JEDETC('L', NOMOBJ, 1 )
              ZK80(LDEC) = INIT
              LDEC = LDEC + 1
              ICMD = ICMD + 1
             GOTO 20
           ENDIF
         ENDIF
      ENDIF
C     --- VECTEUR DE K80 DE STATISTIQUES ---
      CALL JEDEMA()
      END
