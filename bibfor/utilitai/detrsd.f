      SUBROUTINE DETRSD(TYPESD,NOMSD)
      IMPLICIT   NONE
      CHARACTER*(*) TYPESD,NOMSD
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 22/11/2004   AUTEUR MCOURTOI M.COURTOIS 
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
C RESPONSABLE                            VABHHTS J.PELLET
C ----------------------------------------------------------------------
C  BUT : DETRUIRE UNE STRUCTURE DE DONNEE DONT ON CONNAIT LE TYPE
C  ATTENTION : QUAND ON UTILISE TYPESD=' ', ON APPELLE LA ROUTINE JEDETC
C              QUI EST TRES COUTEUSE EN CPU.
C  IN   TYPESD : TYPE DE LA STRUCTURE DE DONNEE A DETRUIRE
C          'NUME_DDL'     'PROF_CHNO'    'MLTF'
C          'MATR_ASSE'    'VECT_ASSE'    'MATR_ASSE_GENE'
C          'MATR_ELEM'    'VECT_ELEM'
C          'VARI_COM'     'FONCTION' (POUR LES FONCTIONS OU NAPPES)
C          'TABLE'        'DEFI_CONT'    'RESO_CONT'
C          'SOLVEUR'      'CORRESP_2_MAILLA'
C          'CHAM_NO_S'    'CHAM_ELEM_S'
C          'CHAM_NO'      'CHAM_ELEM'  'CARTE'
C          'CHAMP_GD' (CHAPEAU AUX CHAM_NO/CHAM_ELEM/CARTE/RESUELEM)
C          'RESULTAT'  'LIGREL'  'NUAGE'  'MAILLAGE'
C          (OU ' ' QUAND ON NE CONNAIT PAS LE TYPE).
C       NOMSD   : NOM DE LA STRUCTURE DE DONNEES A DETRUIRE
C          NUME_DDL(K14),MATR_ASSE(K19),VECT_ASSE(K19)
C          CHAMP_GD(K19),MATR_ELEM(K8),VECT_ELEM(K8),VARI_COM(K14)
C          DEFI_CONT(K16),RESO_CONT(K14),TABLE(K19)
C          CHAM_NO(K19),CHAM_NO_S(K19),CHAM_ELEM(K19),CHAM_ELEM_S(K19)

C     RESULTAT:
C     ON DETRUIT TOUS LES OBJETS JEVEUX CORRESPONDANT A CES CONCEPTS.
C ----------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER IRET,IAD,LONG,I,NBCH,ILIRES
      CHARACTER*1 K1BID
      CHARACTER*8 MATEL,MAILLA
      CHARACTER*14 NU,RESOCO,COM
      CHARACTER*16 DEFICO,TYP2SD,CORRES
      CHARACTER*19 CHAMP,MATAS,TABLE,SOLVEU,CNS,CES,CNO,CEL,FNC
      CHARACTER*19 LIGREL,CARTE,NUAGE,LIGRET,MLTF

C -DEB------------------------------------------------------------------

      CALL JEMARQ()
      TYP2SD = TYPESD

C     ------------------------------------------------------------------
      IF (TYP2SD.EQ.' ') THEN
C     -----------------------
C       TYPE_SD INCONNU => CALL JEDETC => COUT CPU IMPORTANT
        CALL JEDETC(' ',NOMSD,1)

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'CHAM_NO_S') THEN
C     ------------------------------------
        CNS = NOMSD
        CALL JEDETR(CNS//'.CNSD')
        CALL JEDETR(CNS//'.CNSK')
        CALL JEDETR(CNS//'.CNSC')
        CALL JEDETR(CNS//'.CNSL')
        CALL JEDETR(CNS//'.CNSV')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'CHAM_ELEM_S') THEN
C     --------------------------------------
        CES = NOMSD
        CALL JEDETR(CES//'.CESD')
        CALL JEDETR(CES//'.CESK')
        CALL JEDETR(CES//'.CESC')
        CALL JEDETR(CES//'.CESL')
        CALL JEDETR(CES//'.CESV')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'CORRESP_2_MAILLA') THEN
C     -------------------------------------------
        CORRES = NOMSD
        CALL JEDETR(CORRES//'.PJEF_NO')
        CALL JEDETR(CORRES//'.PJEF_NU')
        CALL JEDETR(CORRES//'.PJEF_NB')
        CALL JEDETR(CORRES//'.PJEF_M1')
        CALL JEDETR(CORRES//'.PJEF_CF')
        CALL JEDETR(CORRES//'.PJEF_TR')

C     ------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'FONCTION') THEN
C     -----------------------------------
        FNC = NOMSD
        CALL JEDETR(FNC//'.PARA')
        CALL JEDETR(FNC//'.PROL')
        CALL JEDETR(FNC//'.VALE')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'SOLVEUR') THEN
C     ----------------------------------
        SOLVEU = NOMSD
        CALL JEDETR(SOLVEU//'.SLVI')
        CALL JEDETR(SOLVEU//'.SLVK')
        CALL JEDETR(SOLVEU//'.SLVR')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'LIGREL') THEN
C     ----------------------------------
        LIGREL = NOMSD
        CALL JEDETR(LIGREL//'.LGNS')
        CALL JEDETR(LIGREL//'.LIEL')
        CALL JEDETR(LIGREL//'.NEMA')
        CALL JEDETR(LIGREL//'.NOMA')
        CALL JEDETR(LIGREL//'.NBNO')
        CALL JEDETR(LIGREL//'.PRNM')
        CALL JEDETR(LIGREL//'.PRNS')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'LIGRET') THEN
C     ----------------------------------
        LIGRET = NOMSD
        CALL JEDETR(LIGRET//'.APMA')
        CALL JEDETR(LIGRET//'.APNO')
        CALL JEDETR(LIGRET//'.LIMA')
        CALL JEDETR(LIGRET//'.LINO')
        CALL JEDETR(LIGRET//'.LITY')
        CALL JEDETR(LIGRET//'.MATA')
        CALL JEDETR(LIGRET//'.MODE')
        CALL JEDETR(LIGRET//'.NOMA')
        CALL JEDETR(LIGRET//'.PHEN')
        CALL JEDETR(LIGRET//'.POMA')
        CALL JEDETR(LIGRET//'.PONO')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'MAILLAGE') THEN
C     ----------------------------------
        MAILLA = NOMSD
        CALL JEDETR(MAILLA//'           .LTNS')
        CALL JEDETR(MAILLA//'           .LTNT')
        CALL JEDETR(MAILLA//'           .TITR')
        CALL JEDETR(MAILLA//'.CONNEX')
        CALL JEDETR(MAILLA//'.COORDO    .DESC')
        CALL JEDETR(MAILLA//'.COORDO    .REFE')
        CALL JEDETR(MAILLA//'.COORDO    .VALE')
        CALL JEDETR(MAILLA//'.DIME')
        CALL JEDETR(MAILLA//'.GROUPEMA')
        CALL JEDETR(MAILLA//'.GROUPENO')
        CALL JEDETR(MAILLA//'.NOMACR')
        CALL JEDETR(MAILLA//'.NOMMAI')
        CALL JEDETR(MAILLA//'.NOMNOE')
        CALL JEDETR(MAILLA//'.PARA_R')
        CALL JEDETR(MAILLA//'.SUPMAIL')
        CALL JEDETR(MAILLA//'.TYPL')
        CALL JEDETR(MAILLA//'.TYPMAIL')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'NUAGE') THEN
C     ----------------------------------
        NUAGE = NOMSD
        CALL JEDETR(NUAGE//'.NUAI')
        CALL JEDETR(NUAGE//'.NUAX')
        CALL JEDETR(NUAGE//'.NUAV')
        CALL JEDETR(NUAGE//'.NUAL')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'TABLE') THEN
C     --------------------------------
        TABLE = NOMSD
        CALL JEEXIN(TABLE//'.TBLP',IRET)
        IF (IRET.NE.0) THEN
          CALL JEVEUO(TABLE//'.TBLP','L',IAD)
          CALL JELIRA(TABLE//'.TBLP','LONMAX',LONG,K1BID)
          DO 10,I = 1,LONG
            CALL JEDETR(ZK24(IAD-1+I))
   10     CONTINUE
          CALL JEDETR(TABLE//'.TBLP')
          CALL JEDETR(TABLE//'.TBNP')
          CALL JEDETR(TABLE//'.TBBA')
        END IF
        CALL JEEXIN(TABLE//'.TITR',IRET)
        IF (IRET.NE.0) CALL JEDETR(TABLE//'.TITR')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'MATR_ASSE_GENE') THEN
C     -----------------------------------------
        MATAS = NOMSD
        CALL JEDETR(MATAS//'.DESC')
        CALL JEDETR(MATAS//'.LIME')
        CALL JEDETR(MATAS//'.CONL')
        CALL JEDETR(MATAS//'.REFA')
        CALL JEDETR(MATAS//'.VALE')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'MATR_ASSE') THEN
C     ------------------------------------
        MATAS = NOMSD
        CALL JEDETR(MATAS//'.ABLI')
        CALL JEDETR(MATAS//'.ALIG')
        CALL JEDETR(MATAS//'.COND')
        CALL JEDETR(MATAS//'.CONI')
        CALL JEDETR(MATAS//'.CONL')
        CALL JEDETR(MATAS//'.JDRF')
        CALL JEDETR(MATAS//'.JDDC')
        CALL JEDETR(MATAS//'.JDFF')
        CALL JEDETR(MATAS//'.JDHF')
        CALL JEDETR(MATAS//'.JDPM')
        CALL JEDETR(MATAS//'.JDES')
        CALL JEDETR(MATAS//'.JDVL')
        CALL JEDETR(MATAS//'.LILI')
        CALL JEDETR(MATAS//'.LIME')
        CALL JEDETR(MATAS//'.LLIG')
        CALL JEDETR(MATAS//'.REFA')
        CALL JEDETR(MATAS//'.TMP1')
        CALL JEDETR(MATAS//'.TMP2')
        CALL JEDETR(MATAS//'.VALE')
        CALL JEDETR(MATAS//'.VALF')
        CALL JEDETR(MATAS//'.VALI')
        CALL JEDETR(MATAS//'.&TRA')
        CALL JEDETR(MATAS//'.&VDI')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'VECT_ASSE') THEN
C     ------------------------------------

        GOTO 30

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'CHAM_NO') THEN
C     ----------------------------------
        CNO = NOMSD
        CALL JEDETR(CNO//'.DESC')
        CALL JEDETR(CNO//'.REFE')
        CALL JEDETR(CNO//'.VALE')
C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'CARTE') THEN
C     ----------------------------------
        CARTE = NOMSD
        CALL JEDETR(CARTE//'.DESC')
        CALL JEDETR(CARTE//'.VALE')
        CALL JEDETR(CARTE//'.NOMA')
        CALL JEDETR(CARTE//'.NOLI')
        CALL JEDETR(CARTE//'.LIMA')
        CALL JEDETR(CARTE//'.PTMA')
        CALL JEDETR(CARTE//'.PTMS')
        CALL JEDETR(CARTE//'.NCMP')
        CALL JEDETR(CARTE//'.VALV')
C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'PROF_CHNO') THEN
C     ------------------------------------
        CNO = NOMSD
        CALL JEDETR(CNO//'.LILI')
        CALL JEDETR(CNO//'.PRNO')
        CALL JEDETR(CNO//'.NUEQ')
        CALL JEDETR(CNO//'.LPRN')
        CALL JEDETR(CNO//'.DEEQ')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'CHAM_ELEM') THEN
C     ------------------------------------
        CEL = NOMSD
        CALL JEDETR(CEL//'.CELD')
        CALL JEDETR(CEL//'.CELK')
        CALL JEDETR(CEL//'.CELV')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'MLTF') THEN
C     -----------------------------------
        MLTF = NOMSD
        CALL JEDETR(MLTF//'.GLOB')
        CALL JEDETR(MLTF//'.LOCL')
        CALL JEDETR(MLTF//'.ADNT')
        CALL JEDETR(MLTF//'.PNTI')
        CALL JEDETR(MLTF//'.DESC')
        CALL JEDETR(MLTF//'.DIAG')
        CALL JEDETR(MLTF//'.ADRE')
        CALL JEDETR(MLTF//'.SUPN')
        CALL JEDETR(MLTF//'.PARE')
        CALL JEDETR(MLTF//'.FILS')
        CALL JEDETR(MLTF//'.FRER')
        CALL JEDETR(MLTF//'.LGSN')
        CALL JEDETR(MLTF//'.LFRN')
        CALL JEDETR(MLTF//'.NBAS')
        CALL JEDETR(MLTF//'.DEBF')
        CALL JEDETR(MLTF//'.DEFS')
        CALL JEDETR(MLTF//'.ADPI')
        CALL JEDETR(MLTF//'.ANCI')
        CALL JEDETR(MLTF//'.NBLI')
        CALL JEDETR(MLTF//'.LGBL')
        CALL JEDETR(MLTF//'.NCBL')
        CALL JEDETR(MLTF//'.DECA')
        CALL JEDETR(MLTF//'.NOUV')
        CALL JEDETR(MLTF//'.SEQU')
        CALL JEDETR(MLTF//'.RENU')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'NUME_DDL') THEN
C     -----------------------------------
        NU = NOMSD
        CALL JEDETR(NU//'.NUME.DEEQ')
        CALL JEDETR(NU//'.NUME.DELG')
        CALL JEDETR(NU//'.NUME.NEQU')
        CALL JEDETR(NU//'.NUME.NUEQ')
        CALL JEDETR(NU//'.NUME.PRNO')
        CALL JEDETR(NU//'.NUME.LILI')
        CALL JEDETR(NU//'.NUME.LPRN')
        CALL JEDETR(NU//'.NUME.REFN')
        CALL JEDETR(NU//'.SLCS.ABLO')
        CALL JEDETR(NU//'.SLCS.ADIA')
        CALL JEDETR(NU//'.SLCS.DESC')
        CALL JEDETR(NU//'.SLCS.HCOL')
        CALL JEDETR(NU//'.SLCS.IABL')
        CALL JEDETR(NU//'.SLCS.REFE')
        CALL JEDETR(NU//'.SMOS.ABLO')
        CALL JEDETR(NU//'.SMOS.ADIA')
        CALL JEDETR(NU//'.SMOS.DESC')
        CALL JEDETR(NU//'.SMOS.HCOL')
        CALL JEDETR(NU//'.SMOS.IABL')
        CALL JEDETR(NU//'.SMOS.REFE')
        CALL JEDETR(NU//'.MLTF.GLOB')
        CALL JEDETR(NU//'.MLTF.LOCL')
        CALL JEDETR(NU//'.MLTF.ADNT')
        CALL JEDETR(NU//'.MLTF.PNTI')

        CALL JEDETR(NU//'.MLTF.DESC')
        CALL JEDETR(NU//'.MLTF.DIAG')
        CALL JEDETR(NU//'.MLTF.ADRE')
        CALL JEDETR(NU//'.MLTF.SUPN')
        CALL JEDETR(NU//'.MLTF.PARE')
        CALL JEDETR(NU//'.MLTF.FILS')
        CALL JEDETR(NU//'.MLTF.FRER')
        CALL JEDETR(NU//'.MLTF.LGSN')
        CALL JEDETR(NU//'.MLTF.LFRN')
        CALL JEDETR(NU//'.MLTF.NBAS')
        CALL JEDETR(NU//'.MLTF.DEBF')
        CALL JEDETR(NU//'.MLTF.DEFS')
        CALL JEDETR(NU//'.MLTF.ADPI')
        CALL JEDETR(NU//'.MLTF.ANCI')
        CALL JEDETR(NU//'.MLTF.NBLI')
        CALL JEDETR(NU//'.MLTF.LGBL')
        CALL JEDETR(NU//'.MLTF.NCBL')
        CALL JEDETR(NU//'.MLTF.DECA')
        CALL JEDETR(NU//'.MLTF.NOUV')
        CALL JEDETR(NU//'.MLTF.SEQU')
        CALL JEDETR(NU//'.MLTF.RENU')

        CALL JEDETR(NU//'.DERLI    ')
        CALL JEDETR(NU//'.EXISTE   ')
        CALL JEDETR(NU//'.NUM2     ')
        CALL JEDETR(NU//'.NUM21    ')
        CALL JEDETR(NU//'.LSUIVE   ')
        CALL JEDETR(NU//'.PSUIVE   ')
        CALL JEDETR(NU//'.VSUIVE   ')
        CALL JEDETR(NU//'.OLDN')
        CALL JEDETR(NU//'.NEWN')
C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'VARI_COM') THEN
C     -------------------------------------
        COM = NOMSD
        CALL ASSDE1(COM//'.TEMP')
        CALL ASSDE1(COM//'.HYDR')
        CALL ASSDE1(COM//'.SECH')
        CALL ASSDE1(COM//'.IRRA')
        CALL ASSDE1(COM//'.PHAS')
        CALL ASSDE1(COM//'.EPAN')
        CALL ASSDE1(COM//'.INST')
        CALL ASSDE1(COM//'.CORR')
        CALL JEDETR(COM//'.EXISTENCE')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'CHAMP_GD') THEN
C     ---------------------------------------
C       POUR LES CARTE, CHAM_NO, CHAM_ELEM, ET RESU_ELEM :
        CHAMP = NOMSD
        CALL ASSDE1(CHAMP)

C     ------------------------------------------------------------------
      ELSE IF ((TYP2SD.EQ.'MATR_ELEM') .OR.
     &         (TYP2SD.EQ.'VECT_ELEM')) THEN
C     ---------------------------------------
        MATEL = NOMSD
        CALL JEDETR(MATEL//'.REFE_RESU')
        CALL JEEXIN(MATEL//'.LISTE_RESU',IRET)
        IF (IRET.LE.0) GO TO 30
        CALL JELIRA(MATEL//'.LISTE_RESU','LONUTI',NBCH,K1BID)
        CALL JEVEUO(MATEL//'.LISTE_RESU','L',ILIRES)
        DO 20,I = 1,NBCH
          CALL ASSDE1(ZK24(ILIRES-1+I) (1:19))
   20   CONTINUE
        CALL JEDETR(MATEL//'.LISTE_RESU')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'DEFI_CONT') THEN
C     ------------------------------------
        DEFICO = NOMSD
        CALL JEDETR(DEFICO//'.METHCO')
        CALL JEDETR(DEFICO//'.PZONECO')
        CALL JEDETR(DEFICO//'.MAILCO')
        CALL JEDETR(DEFICO//'.PSUMACO')
        CALL JEDETR(DEFICO//'.MANOCO')
        CALL JEDETR(DEFICO//'.PMANOCO')
        CALL JEDETR(DEFICO//'.MAMACO')
        CALL JEDETR(DEFICO//'.PMAMACO')
        CALL JEDETR(DEFICO//'.NOEUCO')
        CALL JEDETR(DEFICO//'.PSUNOCO')
        CALL JEDETR(DEFICO//'.NOMACO')
        CALL JEDETR(DEFICO//'.PNOMACO')
        CALL JEDETR(DEFICO//'.NDIMCO')
        CALL JEDETR(DEFICO//'.DDLCO')
        CALL JEDETR(DEFICO//'.MAESCL')
        CALL JEDETR(DEFICO//'.TABFIN')
        CALL JEDETR(DEFICO//'.CARACF')
        CALL JEDETR(DEFICO//'.ECPDON')
C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'RESO_CONT') THEN
C     ------------------------------------
        RESOCO = NOMSD
        CALL JEDETR(RESOCO//'.APPARI')
        CALL JEDETR(RESOCO//'.APMEMO')
        CALL JEDETR(RESOCO//'.APPOIN')
        CALL JEDETR(RESOCO//'.NORINI')
        CALL JEDETR(RESOCO//'.NORMCO')
        CALL JEDETR(RESOCO//'.TANGCO')
        CALL JEDETR(RESOCO//'.APNOEU')
        CALL JEDETR(RESOCO//'.APDDL')
        CALL JEDETR(RESOCO//'.APCOEF')
        CALL JEDETR(RESOCO//'.APCOFR')
        CALL JEDETR(RESOCO//'.APJEU')
        CALL JEDETR(RESOCO//'.APJEFX')
        CALL JEDETR(RESOCO//'.APJEFY')
        CALL JEDETR(RESOCO//'.APREAC')
        CALL JEDETR(RESOCO//'.COCO')
        CALL JEDETR(RESOCO//'.LIAC')
        CALL JEDETR(RESOCO//'.LIOT')
        CALL JEDETR(RESOCO//'.MU')
        CALL JEDETR(RESOCO//'.COEFMU')
        CALL JEDETR(RESOCO//'.ATMU')
        CALL JEDETR(RESOCO//'.AFMU')
        CALL JEDETR(RESOCO//'.DEL0')
        CALL JEDETR(RESOCO//'.DELT')
        CALL JEDETR(RESOCO//'.CM1A')
        CALL JEDETR(RESOCO//'.CM2A')
        CALL JEDETR(RESOCO//'.CM3A')
        CALL JEDETR(RESOCO//'.MATR'//'.VALE')
        CALL JEDETR(RESOCO//'.MATR'//'.REFA')
        CALL JEDETR(RESOCO//'.MATR'//'.LIME')
        CALL JEDETR(RESOCO//'.MATR'//'.&VDI')
        CALL JEDETR(RESOCO//'.MATR'//'.&TRA')
        CALL JEDETR(RESOCO//'.MATR'//'.LIME')
        CALL JEDETR(RESOCO//'.SLCS'//'.REFE')
        CALL JEDETR(RESOCO//'.SLCS'//'.DESC')
        CALL JEDETR(RESOCO//'.SLCS'//'.HCOL')
        CALL JEDETR(RESOCO//'.SLCS'//'.ADIA')
        CALL JEDETR(RESOCO//'.SLCS'//'.ABLO')
        CALL JEDETR(RESOCO//'.SLCS'//'.IABL')

C     ------------------------------------------------------------------
      ELSE IF (TYP2SD.EQ.'RESULTAT') THEN
C     -----------------------------------
        CNS = NOMSD
        CALL JEEXIN(CNS//'.DESC',IRET)
        IF (IRET.GT.0) CALL RSDLSD(NOMSD)

C     ------------------------------------------------------------------
      ELSE
        CALL UTMESS('F','DETRSD',' LE MOT CLE :'//TYP2SD//
     &              'N EST PAS AUTORISE.')
      END IF

   30 CONTINUE
      CALL JEDEMA()
      END
