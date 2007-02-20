      SUBROUTINE IBDBGS ()
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 19/02/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C     OPTION DE DEBUG DEMANDE
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

C ----------------------------------------------------------------------
      CHARACTER*3  REPONS
      CHARACTER*16 CBID,MEMOIR, CMPIN, CMPOUT
      INTEGER SEGJVX,LSEGJV, LOUT,L,NCODE,IVAL
      REAL*8 VPARJV
C
C     --- OPTIONS PAR DEFAUT ---
      CALL JEMARQ()
      REPONS = 'NON'
      MEMOIR = 'RAPIDE'
      TBLOC=800.D0

C     -- DEBUG / JXVERI :
C     -----------------------------------------------------
      CALL GETVTX('DEBUG','JXVERI',1,1,1,REPONS,L)
      IF ( REPONS .EQ. 'OUI') THEN
         CALL U2MESS('I','SUPERVIS_23')
C        LE "FLAG" JXVERI=OUI EST POSTIONNE DANS LE JDC
C        VOIR ROUTINE EXPASS.F
      ENDIF


C     -- DEBUG / SDVERI :
C     -----------------------------------------------------
      CALL GETVTX('DEBUG','SDVERI',1,1,1,REPONS,L)
      IF (L.EQ.0) THEN
        CALL GETFAC('CODE',NCODE)
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
      CALL GETVTX('DEBUG','JEVEUX',1,1,1,REPONS,L)
      CALL ASSERT ( REPONS.EQ.'OUI' .OR. REPONS.EQ.'NON')
      IF ( REPONS .EQ. 'OUI') THEN
         CALL U2MESS('I','SUPERVIS_12')
         IDEBUG = 1
      ENDIF


C     -- DEBUG / ENVIMA :
C     -----------------------------------------------------
      CALL GETVTX('DEBUG','ENVIMA',1,1,1,REPONS,L)
      IF ( REPONS .EQ. 'TES' ) THEN
         IFI = IUNIFI ( 'RESULTAT' )
         CALL IMPVEM  ( IFI )
      ENDIF

C     -- ERREUR / ERREUR_F :
C     -----------------------------------------------------
      CMPIN='ABORT'
      CALL GETVTX('ERREUR','ERREUR_F',1,1,1,CMPIN, L)
      IF(L.EQ.1)THEN
         CALL ONERRF(CMPIN, CMPOUT, LOUT)
      ENDIF

C     -- MEMOIRE / GESTION ...  :
C     -----------------------------------------------------
      CALL GETVTX('MEMOIRE','GESTION',1,1,1,MEMOIR,L)
      CALL GETVIS('MEMOIRE','TYPE_ALLOCATION',1,1,1,ISEG,L)
      IF (L.LE.0) ISEG = SEGJVX(-1)
      CALL GETVIS('MEMOIRE','TAILLE',1,1,1,ITAIL,L)
      IF (L.LE.0) ITAIL = LSEGJV(-1)
      CALL GETVR8('MEMOIRE','PARTITION',1,1,1,RVAL,L)
      IF (L.LE.0) THEN
        R8BID = -1.0D0
        RVAL = VPARJV(R8BID)
      ENDIF
      CALL GETVIS('MEMOIRE','DYNAMIQUE',1,1,1,IVAL,L)
      CALL JEALDY ( L, IVAL )

      CALL GETVR8('MEMOIRE','TAILLE_BLOC',1,1,1,TBLOC,L)

      IF ( MEMOIR(1:8) .EQ. 'COMPACTE') THEN
         CALL U2MESS('I','SUPERVIS_25')
         CALL JETYPR('DEBUT','XD',ISEG,ITAIL,RVAL)
      ELSE
         CALL JETYPR('DEFAUT','XX',ISEG,ITAIL,RVAL)
      ENDIF
      IF (ISEG .EQ. 2) THEN
        CALL U2MESS('I','SUPERVIS_26')
      ELSE IF (ISEG .EQ. 3) THEN
        CALL U2MESS('I','SUPERVIS_27')
        CALL U2MESI('I','JEVEUX_44',1,ITAIL)
      ELSE IF (ISEG .EQ. 4) THEN
        CALL U2MESS('I','SUPERVIS_28')
        CALL U2MESI('I','JEVEUX_44',1,ITAIL)
        CALL U2MESR('I','JEVEUX_45',1,RVAL)
      ENDIF

      CALL JEDEMA()
      END
