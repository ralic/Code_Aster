      SUBROUTINE OP0196()
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 06/07/2010   AUTEUR CARON A.CARON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT
C     =================================================================
C                      OPERATEUR POST_CHAM_XFEM
C                      ------------------------
C     BUT : DETERMNATION DES CHAMPS DE DEPLACEMENTS, DE CONTRAINTES
C           ET DE VARIABLES INTERNES SUR LE MAILLAGE FISSURE X-FEM
C     =================================================================
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------

      INTEGER      JLICHA,NBSY,NBORDR,IOR,JORD,IORD,JINST1,JINST2,NBCHAM
      INTEGER      IBID,IRET,NSETOT,NNNTOT,NCOTOT,NBNOC,NBMAC,IFM,NIV,IC
      INTEGER      JMOD
      CHARACTER*1  KBID
      CHARACTER*2  K2B(5)
      CHARACTER*8  MAXFEM,MO,MALINI,RESUCO,RESUX,MODVIS,K8B
      CHARACTER*16 TYSD,NOMCHA
      CHARACTER*19 CNS1,CNS2,CES1,CES2,CEL2,CH,CESVI1,CESVI2
      CHARACTER*24 MAILX,MAILC,ORDR,LICHAM,LISTNO,LOGRMA,K24,LISTGR

C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
C
C     ------------------------------------------------------------------
C     1. RECUPERATION DES CONCEPTS UTILISATEURS
C     ------------------------------------------------------------------
C
      IF (NIV.GT.1) WRITE(IFM,*)' '
      IF (NIV.GT.1) WRITE(IFM,*)'1. XPOINI'
      LICHAM = '&&OP0196.LICHAM'
      CALL XPOINI(MAXFEM,MO,MALINI,LICHAM,NBSY,RESUCO,RESUX,K2B,K8B)
      CALL GETVID(' ','MODELE_VISU',0,1,1,MODVIS,IRET)
C
C     ------------------------------------------------------------------
C     2. SEPARATION DES MAILLES DE MALINI EN 2 GROUPES
C              - MAILC : MAILLES NON AFFECTEES D'UN MODELE
C                        OU NON SOUS-DECOUPEES (CLASSIQUE)
C              - MAILX : MAILLES SOUS-DECOUPEES (X-FEM)
C     ------------------------------------------------------------------
C
      IF (NIV.GT.1) WRITE(IFM,*)' '
      IF (NIV.GT.1) WRITE(IFM,*)'2. XPOSEP'
      MAILC = '&&OP0196.MAILC'
      MAILX = '&&OP0196.MAILX'
      LOGRMA = '&&OP0196.LOGRMA'
      LISTGR = '&&OP0196.LISTGR'
      CALL XPOSEP(MO,MALINI,MAILC,MAILX,NSETOT,NNNTOT,NCOTOT,
     &                                  LOGRMA,LISTGR)


C     CREATION DE LA NOUVELLE SD RESULTAT
      ORDR=RESUCO//'           .ORDR'
      CALL JEVEUO(ORDR,'L',JORD)
      CALL JELIRA(ORDR,'LONUTI',NBORDR,KBID)
      CALL GETTCO(RESUCO,TYSD)
      CALL RSCRSD('G',RESUX,TYSD,NBORDR)

C     BOUCLE SUR LES NBORDR NUMEROS D'ORDRE
      DO 10 IOR = 1 , NBORDR

        IORD=ZI(JORD-1+IOR)
C       ----------------------------------------------------------------
C       3. DIMENSIONNEMENT DES OBJETS DU RESU X-FEM
C       ----------------------------------------------------------------

        IF (NIV.GT.1) WRITE(IFM,*)' '
        IF (NIV.GT.1) WRITE(IFM,*)'3. XPODIM'
        CNS1   = '&&OP0196.CNS1'
        CES1   = '&&OP0196.CES1'
        CESVI1   = '&&OP0196.CESVI1'
        CNS2   = '&&OP0196.CNS2'
        CES2   = '&&OP0196.CES2'
        CESVI2   = '&&OP0196.CESVI2'
        CEL2   = '&&OP0196.CEL2'
        LISTNO = '&&OP0196.LISTNO'
        CALL XPODIM(MALINI,MAILC,NSETOT,NNNTOT,NCOTOT,LISTNO,CNS1,
     &              CNS2,CES1,CES2,CEL2,CESVI1,CESVI2,
     &              IOR,RESUCO,NBNOC,NBMAC,LOGRMA,K24,MAXFEM)

C       ----------------------------------------------------------------
C       4. TRAITEMENT DES MAILLES DE MAILC
C       ----------------------------------------------------------------

        IF (NIV.GT.1) WRITE(IFM,*)' '
        IF (NIV.GT.1) WRITE(IFM,*)'4. XPOMAC'
        CALL XPOMAC(MALINI,MAILC,LISTNO,NBNOC,NBMAC,MAXFEM,K24,
     &              CNS1,CNS2,CES1,CES2,CESVI1,CESVI2,RESUCO)

C       ----------------------------------------------------------------
C       5. TRAITEMENT DES MAILLES DE MAILX
C       ----------------------------------------------------------------

        IF (NIV.GT.1) WRITE(IFM,*)' '
        IF (NIV.GT.1) WRITE(IFM,*)'5. XPOMAX'
        CALL XPOMAX(MO,MALINI,MAILX,NBNOC,NBMAC,K2B,K8B,MAXFEM,
     &              CNS1,CNS2,CES1,CES2,CESVI1,CESVI2,LISTGR,K24,K24,
     &              RESUCO)

C       ----------------------------------------------------------------
C       6. ENREGISTREMENT DES CHAMPS DE SORTIES
C       ----------------------------------------------------------------

        CALL JELIRA(LICHAM,'LONMAX',NBCHAM,K8B)
        CALL JEVEUO(LICHAM,'L',JLICHA)
        DO 20 IC=1,NBCHAM
          NOMCHA = ZK16(JLICHA-1+IC)
          IF (NIV.GT.1) WRITE(IFM,*)'6. ENREGISTREMENT DE ',NOMCHA
          CALL RSEXCH(RESUX,NOMCHA,IORD,CH,IRET)
          IF (NOMCHA.EQ.'DEPL') THEN
            CALL CNSCNO(CNS2,' ','NON','G',CH,'F',IBID)
          ELSEIF (NOMCHA.EQ.'SIEF_ELGA') THEN
            CALL CESCEL(CES2,MODVIS//'.MODELE','FULL_MECA','PCONTMR',
     &                  'NON',IBID,'G',CH,'F',IBID)
          ELSEIF (NOMCHA.EQ.'VARI_ELGA') THEN
            CALL CESCEL(CESVI2,MODVIS//'.MODELE','FULL_MECA','PVARIMR',
     &                  'NON',IBID,'G',CH,'F',IBID)
          ENDIF
          CALL RSNOCH(RESUX,NOMCHA,IORD,' ')
 20     CONTINUE

        IF(TYSD(1:9).EQ.'EVOL_NOLI') THEN
          CALL RSADPA(RESUCO,'L',1,'INST',IORD,0,JINST1,KBID)
          CALL RSADPA(RESUX ,'E',1,'INST',IORD,0,JINST2,KBID)
          ZR(JINST2) = ZR(JINST1)
          CALL RSADPA(RESUX ,'E',1,'MODELE',IORD,0,JMOD,KBID)
          ZK8(JMOD)=MODVIS
        ELSEIF(TYSD(1:9).EQ.'MODE_MECA') THEN
          CALL RSADPA(RESUCO,'L',1,'FREQ',IORD,0,JINST1,KBID)
          CALL RSADPA(RESUX ,'E',1,'FREQ',IORD,0,JINST2,KBID)
          ZR(JINST2) = ZR(JINST1)

          CALL RSADPA(RESUX ,'E',1,'MODELE',IORD,0,JMOD,KBID)
          ZK8(JMOD)=MO

          CALL AJREFD(RESUCO,RESUX,'COPIE')
        ENDIF

        CALL DETRSD('CHAM_NO_S',CNS1)
        CALL DETRSD('CHAM_NO_S',CNS2)
        CALL DETRSD('CHAM_ELEM_S',CES1)
        CALL DETRSD('CHAM_ELEM_S',CES2)
        CALL DETRSD('CHAM_ELEM',CEL2)
        CALL DETRSD('CHAM_ELEM_S',CESVI1)
        CALL DETRSD('CHAM_ELEM_S',CESVI2)

 10   CONTINUE

      CALL JEEXIN(MAILC,IRET)
      IF (IRET.NE.0) CALL JEDETR(MAILC)
      CALL JEEXIN(MAILX,IRET)
      IF (IRET.NE.0) CALL JEDETR(MAILX)
      CALL JEDETR(LICHAM)
      CALL JEEXIN(LISTGR,IRET)
      IF (IRET.NE.0) CALL JEDETR(LISTGR)

      IF (NIV.GT.1) WRITE(IFM,*)'FIN DE POST_CHAM_XFEM'

      CALL JEDEMA()
C
      END
