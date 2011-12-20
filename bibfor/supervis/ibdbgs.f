      SUBROUTINE IBDBGS ()
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 20/12/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     TRAITEMENT DES MOTS CLE DEBUG/MESURE_TEMPS/MEMOIRE
C     DES COMMANDES DEBUT ET POURSUITE
C     ------------------------------------------------------------------
C            0 TOUT C'EST BIEN PASSE
C            1 ERREUR DANS LA LECTURE DE LA COMMANDE
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ----- DEBUT COMMON DE DEBUG JEVEUX
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
      REAL*8           TBLOC
      COMMON /RTBLJE/  TBLOC

C     -- COMMON MESTP1 POUR MESURE_TEMPS
      INTEGER          MTPNIV,MTPSTA
      COMMON /MESTP1/  MTPNIV,MTPSTA

C ----------------------------------------------------------------------
      CHARACTER*3  REPONS
      INTEGER L,NCODE,NDBG
      INTEGER      IARG
C
C     --- OPTIONS PAR DEFAUT ---
      CALL JEMARQ()
      TBLOC=800.D0

C     -- WARNING SUR LES MOTS-CLES CODE ET DEBUG
      CALL GETFAC('CODE',NCODE)
      CALL GETFAC('DEBUG',NDBG)
      IF (NCODE.GT.0 .OR. NDBG.GT.0) THEN
         CALL U2MESS('I','SUPERVIS_22')
      ENDIF

C     -- DEBUG / JXVERI :
      REPONS = 'NON'
      CALL GETVTX('DEBUG','JXVERI',1,IARG,1,REPONS,L)
      IF (L.EQ.0) THEN
         IF ( REPONS .EQ. 'OUI') THEN
            CALL U2MESS('I','SUPERVIS_23')
C           LE "FLAG" JXVERI=OUI EST POSTIONNE DANS LE JDC
C           VOIR ROUTINE EXPASS.F
         ENDIF
      ENDIF

C     -- DEBUG / SDVERI :
      REPONS = 'NON'
      CALL GETVTX('DEBUG','SDVERI',1,IARG,1,REPONS,L)
      IF (L.EQ.0) THEN
        IF (NCODE.GT.0) THEN
C          UN JOUR, ON METTRA 'OUI' PAR DEFAUT ...
           REPONS='NON'
        ELSE
           REPONS='NON'
        ENDIF
      ENDIF

      IF ( REPONS .EQ. 'OUI') THEN
         CALL JDCSET('sdveri', 1)
         CALL U2MESS('I','SUPERVIS_24')
      ELSE
         CALL JDCSET('sdveri', 0)
      ENDIF


C     -- DEBUG / JEVEUX :
C     -----------------------------------------------------
      REPONS = 'NON'
      CALL GETVTX('DEBUG','JEVEUX',1,IARG,1,REPONS,L)
      CALL ASSERT ( REPONS.EQ.'OUI' .OR. REPONS.EQ.'NON')
      IF ( REPONS .EQ. 'OUI') THEN
         CALL U2MESS('I','SUPERVIS_12')
         IDEBUG = 1
      ENDIF


C     -- DEBUG / ENVIMA :
C     -----------------------------------------------------
      REPONS = 'NON'
      CALL GETVTX('DEBUG','ENVIMA',1,IARG,1,REPONS,L)
      IF ( REPONS .EQ. 'TES' ) THEN
         IFI = IUNIFI ( 'RESULTAT' )
         CALL IMPVEM  ( IFI )
      ENDIF


C     -- MESURE_TEMPS:
C     -----------------------------------------------------
      CALL GETVIS('MESURE_TEMPS','NIVE_DETAIL',1,IARG,1,MTPNIV,L)
      REPONS = 'NON'
      CALL GETVTX('MESURE_TEMPS','MOYENNE',1,IARG,1,REPONS,L)
      IF (REPONS .EQ. 'OUI') THEN
        MTPSTA = 1
      ELSE
        MTPSTA = 0
      ENDIF

C     -- MEMOIRE  :
C     -----------------------------------------------------

      CALL GETVR8('MEMOIRE','TAILLE_BLOC',1,IARG,1,TBLOC,L)

      CALL JEDEMA()
      END
