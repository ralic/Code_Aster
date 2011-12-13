      SUBROUTINE COPISD(TYPESD,BASE,SD1,SD2)
      IMPLICIT NONE
      CHARACTER*(*) TYPESD,BASE,SD1,SD2
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/12/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE VABHHTS J.PELLET

C   BUT:
C   DUPLIQUER UNE STRUCTURE DE DONNEES SOUS UN AUTRE NOM.
C   (SI SD2 EXISTE DEJA, ON L'ECRASE)

C     IN:
C     TYPESD  : TYPE DE LA SD A DUPPLIQUER
C               ' '(INCONNU)      'CHAMP' (OU 'CHAMP_GD')
C               'FONCTION'  (POUR FONCTIONS ET NAPPES)
C               'CORRESP_2_MAILLA'
C               'CHAM_NO_S'       'CHAM_ELEM_S'
C               'VARI_COM'        'TABLE'
C               'RESULTAT'        'NUME_DDL'
C               'MAILLAGE'        'LIGREL'
C               'MATR_ASSE_GENE'  'MATR_ASSE'
C               'PROF_CHNO'       'MATR_ELEM'
C               'VECT_ELEM'       'SOLVEUR'
C     BASE     : 'G' , 'V' , ... : BASE DE CREATION DE SD2
C     SD1 (K*) : NOM DE LA SD A DUPPLIQUER
C     SD2 (K*) : NOM DE LA SD A CREER

C     OUT:
C     SD2 EST CREEE ET A LE MEME CONTENU QUE SD1

C-----------------------------------------------------------------------
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER       IRET, I, NBTU, JLTN1, JLTN2,IDD,NBSD,ILIMPI
      CHARACTER*1   BAS2
      CHARACTER*8   K8B, MAIL1, MAIL2
      CHARACTER*14  COM1, COM2, NU1, NU2
      CHARACTER*16  TYP2SD, CORR1, CORR2
      CHARACTER*19  CH1, CH2, SDR1, K191, K192
      CHARACTER*24  MASFE1,MASFE2
      INTEGER       IFETM1,IFETM2
      LOGICAL       LFETI


C DEB-------------------------------------------------------------------

      CALL JEMARQ
      BAS2 = BASE

C ----------------------------------------------------------------------
C     SUPRESSION DE SD2 :
      CALL DETRSD(TYPESD,SD2)

C ----------------------------------------------------------------------
      IF (TYPESD.EQ.' ') THEN
C     -----------------------
C       -- TYPESD INCONNU => ON UTILISE JEDUPC => COUTEUX EN CPU
        CALL JEDUPC(' ',SD1,1,BASE,SD2,.TRUE.)

C ----------------------------------------------------------------------
      ELSE IF ((TYPESD.EQ.'CHAMP') .OR. (TYPESD.EQ.'CHAMP_GD')) THEN
C     ----------------------------------------------------------------
        CH1 = SD1
        CH2 = SD2
        CALL COPICH(BAS2,CH1,CH2)

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'VARI_COM') THEN
C     -----------------------------------
        COM1 = SD1
        COM2 = SD2

        CALL EXISD('CHAMP',COM1//'.TEMP',IRET)
        IF (IRET.GT.0) CALL COPICH(BAS2,COM1//'.TEMP',COM2//'.TEMP')
        CALL EXISD('CHAMP',COM1//'.HYDR',IRET)
        IF (IRET.GT.0) CALL COPICH(BAS2,COM1//'.HYDR',COM2//'.HYDR')
        CALL EXISD('CHAMP',COM1//'.SECH',IRET)
        IF (IRET.GT.0) CALL COPICH(BAS2,COM1//'.SECH',COM2//'.SECH')
        CALL EXISD('CHAMP',COM1//'.PHAS',IRET)
        IF (IRET.GT.0) CALL COPICH(BAS2,COM1//'.PHAS',COM2//'.PHAS')
        CALL EXISD('CHAMP',COM1//'.EPAN',IRET)
        IF (IRET.GT.0) CALL COPICH(BAS2,COM1//'.EPAN',COM2//'.EPAN')
        CALL EXISD('CHAMP',COM1//'.INST',IRET)
        IF (IRET.GT.0) CALL COPICH(BAS2,COM1//'.INST',COM2//'.INST')
        CALL EXISD('CHAMP',COM1//'.TOUT',IRET)
        IF (IRET.GT.0) CALL COPICH(BAS2,COM1//'.TOUT',COM2//'.TOUT')

        CALL JEDUP1(COM1//'.EXISTENCE',BAS2,COM2//'.EXISTENCE')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'SOLVEUR') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2

        CALL JEDUP1(K191//'.SLVK',BAS2,K192//'.SLVK')
        CALL JEDUP1(K191//'.SLVI',BAS2,K192//'.SLVI')
        CALL JEDUP1(K191//'.SLVR',BAS2,K192//'.SLVR')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'FONCTION') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2

        CALL JEDUP1(K191//'.PARA',BAS2,K192//'.PARA')
        CALL JEDUP1(K191//'.PROL',BAS2,K192//'.PROL')
        CALL JEDUP1(K191//'.VALE',BAS2,K192//'.VALE')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'CORRESP_2_MAILLA') THEN
C        ---------------------------------
        CORR1 = SD1
        CORR2 = SD2
        CALL JEDUP1(CORR1//'.PJXX_K1',BAS2,CORR2//'.PJXX_K1')
        CALL JEDUP1(CORR1//'.PJEF_NU',BAS2,CORR2//'.PJEF_NU')
        CALL JEDUP1(CORR1//'.PJEF_NB',BAS2,CORR2//'.PJEF_NB')
        CALL JEDUP1(CORR1//'.PJEF_CF',BAS2,CORR2//'.PJEF_CF')
        CALL JEDUP1(CORR1//'.PJEF_M1',BAS2,CORR2//'.PJEF_M1')
        CALL JEDUP1(CORR1//'.PJEF_TR',BAS2,CORR2//'.PJEF_TR')
        CALL JEDUP1(CORR1//'.PJEF_CO',BAS2,CORR2//'.PJEF_CO')
        CALL JEDUP1(CORR1//'.PJEF_MP',BAS2,CORR2//'.PJEF_MP')
        CALL JEDUP1(CORR1//'.PJEF_EL',BAS2,CORR2//'.PJEF_EL')
        CALL JEDUP1(CORR1//'.PJNG_I1',BAS2,CORR2//'.PJNG_I1')
        CALL JEDUP1(CORR1//'.PJNG_I2',BAS2,CORR2//'.PJNG_I2')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'CHAM_NO_S') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2
        CALL JEDUP1(K191//'.CNSD',BAS2,K192//'.CNSD')
        CALL JEDUP1(K191//'.CNSK',BAS2,K192//'.CNSK')
        CALL JEDUP1(K191//'.CNSC',BAS2,K192//'.CNSC')
        CALL JEDUP1(K191//'.CNSL',BAS2,K192//'.CNSL')
        CALL JEDUP1(K191//'.CNSV',BAS2,K192//'.CNSV')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'CHAM_ELEM_S') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2
        CALL JEDUP1(K191//'.CESD',BAS2,K192//'.CESD')
        CALL JEDUP1(K191//'.CESK',BAS2,K192//'.CESK')
        CALL JEDUP1(K191//'.CESC',BAS2,K192//'.CESC')
        CALL JEDUP1(K191//'.CESL',BAS2,K192//'.CESL')
        CALL JEDUP1(K191//'.CESV',BAS2,K192//'.CESV')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'PROF_CHNO') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2
        CALL JEDUP1(K191//'.DEEQ',BAS2,K192//'.DEEQ')
        CALL JEDUP1(K191//'.NUEQ',BAS2,K192//'.NUEQ')
        CALL JEDUP1(K191//'.PRNO',BAS2,K192//'.PRNO')
        CALL JEDUP1(K191//'.LILI',BAS2,K192//'.LILI')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'NUME_EQUA') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2
        CALL COPIS2('PROF_CHNO',BAS2,K191,K192)
        CALL JEDUP1(K191//'.NEQU',BAS2,K192//'.NEQU')
        CALL JEDUP1(K191//'.REFN',BAS2,K192//'.REFN')
        CALL JEDUP1(K191//'.DELG',BAS2,K192//'.DELG')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'STOCKAGE') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2
        CALL JEDUP1(K191//'.SCBL',BAS2,K192//'.SCBL')
        CALL JEDUP1(K191//'.SCDI',BAS2,K192//'.SCDI')
        CALL JEDUP1(K191//'.SCDE',BAS2,K192//'.SCDE')
        CALL JEDUP1(K191//'.SCHC',BAS2,K192//'.SCHC')
        CALL JEDUP1(K191//'.SCIB',BAS2,K192//'.SCIB')

        CALL JEDUP1(K191//'.SMDI',BAS2,K192//'.SMDI')
        CALL JEDUP1(K191//'.SMDE',BAS2,K192//'.SMDE')
        CALL JEDUP1(K191//'.SMHC',BAS2,K192//'.SMHC')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'NUME_DDL') THEN
C     -----------------------------------
        NU1 = SD1
        NU2 = SD2
        CALL COPIS2('NUME_EQUA',BAS2,NU1//'.NUME',NU2//'.NUME')
        CALL COPIS2('STOCKAGE',BAS2,NU1//'.SLCS',NU2//'.SLCS')
        CALL COPIS2('STOCKAGE',BAS2,NU1//'.SMOS',NU2//'.SMOS')
        CALL JEDUP1(NU1//'.NSLV',BAS2,NU2//'.NSLV')

C --------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'MATR_ASSE_GENE' .OR.
     &         TYPESD.EQ.'MATR_ASSE') THEN
C     ---------------------------------------------
        K191 = SD1
        K192 = SD2
        CALL JEDUP1(K191//'.CCID',BAS2,K192//'.CCID')
        CALL JEDUP1(K191//'.CCII',BAS2,K192//'.CCII')
        CALL JEDUP1(K191//'.CCLL',BAS2,K192//'.CCLL')
        CALL JEDUP1(K191//'.CCVA',BAS2,K192//'.CCVA')
        CALL JEDUP1(K191//'.CONL',BAS2,K192//'.CONL')
        CALL JEDUP1(K191//'.DESC',BAS2,K192//'.DESC')
        CALL JEDUP1(K191//'.DIGS',BAS2,K192//'.DIGS')
        CALL JEDUP1(K191//'.LIME',BAS2,K192//'.LIME')
        CALL JEDUP1(K191//'.REFA',BAS2,K192//'.REFA')
        CALL JEDUP1(K191//'.UALF',BAS2,K192//'.UALF')
        CALL JEDUP1(K191//'.VALF',BAS2,K192//'.VALF')
        CALL JEDUP1(K191//'.WALF',BAS2,K192//'.WALF')
        CALL JEDUP1(K191//'.VALM',BAS2,K192//'.VALM')

C FETI OR NOT ?
        MASFE1 = K191//'.FETM'
        CALL JEEXIN(MASFE1,IRET)
        IF (IRET.GT.0) THEN
          LFETI = .TRUE.
        ELSE
          LFETI = .FALSE.
        END IF

        IF (LFETI) THEN
          MASFE2 = K192//'.FETM'
          CALL JEDUP1(MASFE1,BAS2,MASFE2)
          CALL JEDUP1(K191//'.FETF',BAS2,K192//'.FETF')
          CALL JEDUP1(K191//'.FETP',BAS2,K192//'.FETP')
          CALL JEDUP1(K191//'.FETR',BAS2,K192//'.FETR')

          CALL JELIRA(MASFE1,'LONMAX',NBSD,K8B)
          CALL JEVEUO(MASFE1,'L',IFETM1)
          CALL JEVEUO(MASFE2,'E',IFETM2)
          CALL JEVEUO('&FETI.LISTE.SD.MPI','L',ILIMPI)
          DO 30 IDD = 1,NBSD
            IF (ZI(ILIMPI+IDD).EQ.1) THEN
              ZK24(IFETM2+IDD-1) = ZK24(IFETM1+IDD-1)
            END IF
   30     CONTINUE

        END IF


C --------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'TABLE') THEN
C     -----------------------------------
        CALL EXISD(TYPESD,SD1,IRET)
        IF (IRET.EQ.0) THEN
          SDR1 = SD1
          CALL U2MESK('F','UTILITAI_40',1,SDR1)
        END IF

        CALL TBCOPI(BASE,SD1,SD2)

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'RESULTAT') THEN
C     -----------------------------------
        CALL EXISD(TYPESD,SD1,IRET)
        IF (IRET.EQ.0) THEN
          SDR1 = SD1
          CALL U2MESK('F','UTILITAI_40',1,SDR1)
        END IF

        CALL RSCOPI(BASE,SD1,SD2)

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'LIGREL') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2
        CALL JEDUP1(K191//'.LGNS',BAS2,K192//'.LGNS')
        CALL JEDUP1(K191//'.LIEL',BAS2,K192//'.LIEL')
        CALL JEDUP1(K191//'.NEMA',BAS2,K192//'.NEMA')
        CALL JEDUP1(K191//'.LGRF',BAS2,K192//'.LGRF')
        CALL JEDUP1(K191//'.NBNO',BAS2,K192//'.NBNO')
        CALL JEDUP1(K191//'.PRNM',BAS2,K192//'.PRNM')
        CALL JEDUP1(K191//'.PRNS',BAS2,K192//'.PRNS')
        CALL JEDUP1(K191//'.REPA',BAS2,K192//'.REPE')
        CALL JEDUP1(K191//'.SSSA',BAS2,K192//'.SSSA')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'SQUELETTE') THEN
C     -----------------------------------
        MAIL1 = SD1
        MAIL2 = SD2
        CALL COPIS2('MAILLAGE',BAS2,MAIL1,MAIL2)
C
        CALL JEDUP1(MAIL1//'.CORRES',BAS2,MAIL2//'.CORRES')
        CALL JEDUP1(MAIL1//'.INV.SKELETON',BAS2,MAIL2//'.INV.SKELETON')
        CALL JEDUP1(MAIL1//'         .NOMSST',BAS2,
     &              MAIL2//'         .NOMSST')
        CALL JEDUP1(MAIL1//'.ANGL_NAUT',BAS2,MAIL2//'.ANGL_NAUT')
        CALL JEDUP1(MAIL1//'.TRANS',BAS2,MAIL2//'.TRANS')

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'L_TABLE') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2
C
        CALL JEEXIN ( K191//'.LTNT', IRET )
        IF ( IRET .NE. 0 ) THEN
          CALL JEDUP1(K191//'.LTNS',BAS2,K192//'.LTNS')
          CALL JEDUP1(K191//'.LTNT',BAS2,K192//'.LTNT')
          CALL JELIRA(K191//'.LTNT','LONUTI',NBTU,K8B)
          CALL JEVEUO(K191//'.LTNS','L',JLTN1)
          CALL JEVEUO(K192//'.LTNS','E',JLTN2)
          K192(1:8) = K192
          DO 10 I = 1 , NBTU
             K191 = ZK24(JLTN1+I-1)(1:19)
             K192(9:19) = K191(9:19)
             CALL EXISD ( 'TABLE', K191, IRET )
             IF ( IRET .NE. 0 ) THEN
                CALL TBCOPI ( BAS2, K191, K192 )
             ELSE
                CALL U2MESK('F','UTILITAI_41',1,K191)
             ENDIF
             ZK24(JLTN2+I-1) = K192
 10       CONTINUE
        ENDIF

C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'MAILLAGE') THEN
C     -----------------------------------
        MAIL1 = SD1
        MAIL2 = SD2
        CALL COPICH(BAS2,MAIL1//'.COORDO',MAIL2//'.COORDO')
        CALL COPICH(BAS2,MAIL1//'.ABS_CURV',MAIL2//'.ABS_CURV')
C
        CALL JEDUP1(MAIL1//'.DIME',BAS2,MAIL2//'.DIME')
        CALL JEDUP1(MAIL1//'.NOMNOE',BAS2,MAIL2//'.NOMNOE')
        CALL JEDUP1(MAIL1//'.GROUPENO',BAS2,MAIL2//'.GROUPENO')
        CALL JEDUP1(MAIL1//'.NOMMAI',BAS2,MAIL2//'.NOMMAI')
        CALL JEDUP1(MAIL1//'.TYPMAIL',BAS2,MAIL2//'.TYPMAIL')
        CALL JEDUP1(MAIL1//'.CONNEX',BAS2,MAIL2//'.CONNEX')
        CALL JEDUP1(MAIL1//'.GROUPEMA',BAS2,MAIL2//'.GROUPEMA')
        CALL JEDUP1(MAIL1//'.NOMACR',BAS2,MAIL2//'.NOMACR')
        CALL JEDUP1(MAIL1//'.PARA_R',BAS2,MAIL2//'.PARA_R')
        CALL JEDUP1(MAIL1//'.SUPMAIL',BAS2,MAIL2//'.SUPMAIL')
        CALL JEDUP1(MAIL1//'.TYPL',BAS2,MAIL2//'.TYPL')
        CALL JEDUP1(MAIL1//'.TITR',BAS2,MAIL2//'.TITR')
C
        CALL COPIS2('L_TABLE',BAS2,MAIL1,MAIL2)
        CALL JEDUP1(MAIL1//'           .TITR',BAS2,
     &              MAIL2//'           .TITR')

C       -- OBJETS QUE JE NE CONNAIS PAS !! (JP) :
        CALL JEDUP1(MAIL1//'.ADAPTATION',BAS2,MAIL2//'.ADAPTATION')


C ----------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'MATR_ELEM' .OR. TYPESD.EQ.'VECT_ELEM') THEN
C     ---------------------------------------------------------------
        K191 = SD1
        K192 = SD2
        CALL JEDUP1(K191//'.RELR',BAS2,K192//'.RELR')
        CALL JEDUP1(K191//'.RERR',BAS2,K192//'.RERR')
        CALL JEDUP1(K191//'.RELC',BAS2,K192//'.RELC')
        CALL JEDUP1(K191//'.RECC',BAS2,K192//'.RECC')
        CALL JEDUP1(K191//'.TITR',BAS2,K192//'.TITR')
C       JE (JP) NE SAIS PAS FAIRE UNE COPIE "PROFONDE" :
C       QUEL NOM DONNER AUX CH2 ?
C       A PART _00000I, JE NE VOIS PAS ...
C
C       CALL JEEXIN(K192//'.RELR',IEXI)
C       IF (IEXI.GT.0) THEN
C         CALL JEVEUO(K191//'.RELR','E',JRELR1)
C         CALL JEVEUO(K192//'.RELR','E',JRELR2)
C         CALL JELIRA(K191//'.RELR','LONUTI',N1,KBID)
C         CALL JELIRA(K192//'.RELR','LONUTI',N2,KBID)
C         CALL ASSERT(N1.EQ.N2)
C         DO 20,K=1,N1
C           CH1=ZK24(JRELR1-1+K)
C           CH2=??
C           CALL COPICH(BAS2,CH1,CH2)
C           ZK24(JRELR2-1+K)=CH2
C 20      CONTINUE
C       ENDIF
C     ------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'LISTE_CHARGES') THEN
C     -----------------------------------
        K191 = SD1
        K192 = SD2
        CALL JEDUP1(K191//'.INFC',BAS2,K192//'.INFC')
        CALL JEDUP1(K191//'.LCHA',BAS2,K192//'.LCHA')
        CALL JEDUP1(K191//'.FCHA',BAS2,K192//'.FCHA')


C ----------------------------------------------------------------------
      ELSE
        TYP2SD = TYPESD
        CALL U2MESK('F','UTILITAI_42',1,TYP2SD)
      END IF

      CALL JEDEMA
      END
