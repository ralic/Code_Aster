      SUBROUTINE FOVECA (NOMOPE,NOMF,IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      NOMOPE,NOMF
      INTEGER                        IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C     VERIFIE LES PRE-CONDITIONS SUR LE CALCUL DES FONCTIONS
C     ------------------------------------------------------------------
C OUT IER   : I :  CODE RETOUR  (NOMBRE D'ERREURS RENCONTREES)
C     ------------------------------------------------------------------
C     OPERATIONS  DISPONIBLES       SUR FONCTION    NAPPE
C       -  SPECTRE D'OSCILLATEUR        OUI         NON
C       -  DERIVATION                   OUI         NON
C       -  INTEGRATION                  OUI         NON
C       -  MAXIMUM D'UNE FONCTION       OUI         OUI
C       -  COMBINAISON LINEAIRE         OUI         OUI
C       -  RECHERCHE D'ENVELOPPE        OUI         OUI
C*+*       -  RECHERCHE DE PICS            OUI         OUI
C       -  CORRECTION DERIVE ACCELERO   OUI         NON
C     ------------------------------------------------------------------
C
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      CHARACTER*16 NOMCMD, CBID, NOMPAR, NOMRES , TYPFON
      CHARACTER*19 NOMFON
      CHARACTER*24 PROL
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMFON = NOMF
      PROL = NOMFON//'.PROL'
      CALL JEVEUO(PROL,'L',LPRO)
      TYPFON = ZK16(LPRO  )
      NOMPAR = ZK16(LPRO+2)
C
      CALL GETRES(CBID,CBID,NOMCMD)
      CBID   = NOMOPE
C
      IF( NOMOPE .EQ. 'SPEC_OSCI' ) THEN
C
C        --- SPECTRE D'OSCILLATEUR D'UN SIGNAL TEMPOREL ---
         IF ( TYPFON .NE.'FONCTION')  THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.01)',
     +               'SEULES LES FONCTIONS DE TYPE "FONCTION" SONT '//
     +               'ACCEPTEES ET PAS LES FONCTIONS DE TYPE '//TYPFON)
         ENDIF
         IF ( NOMPAR .NE.'INST' ) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.03)',
     +               'LA FONCTION N''EST PAS DEPENDANTE DU TEMPS '//
     +               'PARAMETRE "INST".')
         ENDIF
C
      ELSEIF( NOMOPE .EQ. 'DERIVE' ) THEN
C
C        --- DERIVATION D'UN SIGNAL TEMPOREL ---
         IF ( TYPFON .NE.'FONCTION')  THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.01)',
     +               'SEULES LES FONCTIONS DE TYPE "FONCTION" SONT '//
     +               'ACCEPTEES ET PAS LES FONCTIONS DE TYPE '//TYPFON)
         ENDIF
C         IF ( NOMRES(1:4).NE.'DEPL' .AND. NOMRES(1:4).NE.'VITE' ) THEN
C            IER = IER + 1
C            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.02)',
C     +                 'SEUL DEPLACEMENT ET VITESSE SONT DERIVABLES.')
C         ENDIF
C         IF ( NOMPAR .NE.'INST' ) THEN
C            IER = IER + 1
C            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.03)',
C     +               'LA FONCTION N''EST PAS DEPENDANTE DU TEMPS '//
C     +               'PARAMETRE "INST".')
C         ENDIF
C
      ELSEIF( NOMOPE .EQ. 'INTEGRE' ) THEN
C
C        --- INTEGRATION D'UN SIGNAL TEMPOREL ---
         IF ( TYPFON .NE.'FONCTION')  THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.01)',
     +               'SEULES LES FONCTIONS DE TYPE "FONCTION" SONT '//
     +               'ACCEPTEES ET PAS LES FONCTIONS DE TYPE '//TYPFON)
         ENDIF
C         IF ( NOMRES(1:4).NE.'ACCE' .AND. NOMRES(1:4).NE.'VITE' ) THEN
C            IER = IER + 1
C            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.02)',
C     +              'SEULES VITESSE ET ACCELERATION SONT INTEGRABLES')
C         ENDIF
C         IF ( NOMPAR .NE.'INST' ) THEN
C            IER = IER + 1
C            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.03)',
C     +               'LA FONCTION N''EST PAS DEPENDANTE DU TEMPS '//
C     +               'PARAMETRE "INST".')
C         ENDIF
C
      ELSEIF( NOMOPE .EQ. 'MAX' ) THEN
C
C        --- CALCUL DES MAX D'UNE FONCTION ---
C
      ELSEIF( NOMOPE .EQ. 'COMB' ) THEN
C
C        --- COMBINAISON LINEAIRE DE FONCTIONS ---
C
      ELSEIF( NOMOPE .EQ. 'ENVELOPPE' )THEN
C
C        --- RECHERCHE DE L'ENVELOPPE D'UNE FONCTION ---
C
C*+*      ELSEIF( NOMOPE .EQ. 'PIC' ) THEN
C
C        --- RECHERCHE DES PICS D'UNE FONCTION ---
C
      ELSEIF( NOMOPE .EQ. 'CORR_ACCE' ) THEN
C
C        --- CORRECTION DERIVE ACCELEROGRAMME REEL ---
         IF ( TYPFON .NE.'FONCTION')  THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.01)',
     +               'SEULES LES FONCTIONS DE TYPE "FONCTION" SONT '//
     +               'ACCEPTEES ET PAS LES FONCTIONS DE TYPE '//TYPFON)
         ENDIF
         IF ( NOMPAR .NE.'INST' ) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD//'.'//CBID//'(ERREUR.03)',
     +               'LA FONCTION N''EST PAS DEPENDANTE DU TEMPS '//
     +               'PARAMETRE "INST".')
         ENDIF
C
      ENDIF
      CALL JEDEMA()
      END
