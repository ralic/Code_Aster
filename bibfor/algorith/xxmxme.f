      SUBROUTINE XXMXME(NOMA  ,NOMO  ,FONACT,DEFICO,RESOCO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER      FONACT(*)
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*8  NOMA,NOMO
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES XFEM)
C
C CREATION SD DE RESOLUTION RESOCO
C
C ----------------------------------------------------------------------
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  NOMO   : NOM DU MODELE
C IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      NFISS,JNFIS,NFISMX
      PARAMETER    (NFISMX=100)
      INTEGER      IFM,NIV
      CHARACTER*24 TABFIN
      INTEGER      JTABF
      INTEGER      CFDISI,NTPC
      INTEGER      CFMMVD,ZTABF
      CHARACTER*19 LIGREL
      CHARACTER*19 XINDC0,XSEUC0,XCOHE0
      LOGICAL      ISFONC,CFDISL,LXFFM,LXFCM,LXCZM
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('CONTACT',IFM,NIV)
C
C --- FONCTIONNALITES ACTIVEES
C
      NTPC   = CFDISI(DEFICO,'NTPC'     )
      LXFCM  = ISFONC(FONACT,'CONT_XFEM')
      LXFFM  = ISFONC(FONACT,'FROT_XFEM')
      LXCZM  = CFDISL(DEFICO,'EXIS_XFEM_CZM')
      CALL ASSERT(LXFCM)
C
C --- INITIALISATIONS
C
      LIGREL = NOMO//'.MODELE'
C
C --- NOMBRE DE FISSURES
C
      CALL JEVEUO(NOMO//'.NFIS','L',JNFIS)
      NFISS  = ZI(JNFIS)
      IF (NFISS .GT. NFISMX) THEN
        CALL U2MESI('F', 'XFEM_2', 1, NFISMX)
      ENDIF
      IF (NFISS .LE. 0) THEN
        CALL U2MESS('F', 'XFEM_3')
      ENDIF
C
C --- NOM DES CHAMPS
C
      XINDC0 = RESOCO(1:14)//'.XFI0'
      XSEUC0 = RESOCO(1:14)//'.XFS0'
      XCOHE0 = RESOCO(1:14)//'.XCO0'
      ZTABF  = CFMMVD('ZTABF')
C
C --- FONCTIONNALITES ACTIVEES
C
      NTPC   = CFDISI(DEFICO,'NTPC'     )
      LXFCM  = ISFONC(FONACT,'CONT_XFEM')
      LXFFM  = ISFONC(FONACT,'FROT_XFEM')
      LXCZM  = CFDISL(DEFICO,'EXIS_XFEM_CZM')
C
C --- TABLEAU CONTENANT LES INFORMATIONS DIVERSES
C
      TABFIN = RESOCO(1:14)//'.TABFIN'
      CALL WKVECT(TABFIN,'V V R',ZTABF*NTPC+1,JTABF)
      ZR(JTABF) = NTPC
C
C --- PREPARATION CHAM_ELEM VIERGES
C
      CALL XMELE1(NOMA  ,NOMO  ,DEFICO,LIGREL,NFISS ,
     &            XINDC0,'PINDCOI','RIGI_CONT')
      IF (LXCZM) THEN
        CALL XMELE1(NOMA  ,NOMO  ,DEFICO,LIGREL,NFISS ,
     &              XCOHE0,'PCOHES','RIGI_CONT')
      ENDIF
      IF (LXFFM) THEN
        CALL XMELE1(NOMA  ,NOMO  ,DEFICO,LIGREL,NFISS ,
     &              XSEUC0,'PSEUIL','RIGI_CONT')
      ENDIF
C
      CALL JEDEMA()

      END
