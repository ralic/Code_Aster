      SUBROUTINE RSCRSD ( NOMSD, TYPESD, NBORDR )
      IMPLICIT NONE
      CHARACTER*(*) NOMSD,TYPESD
      INTEGER NBORDR
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/08/2008   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C ----------------------------------------------------------------------
C      CREATION D'UNE STRUCTURE DE DONNEES "RESULTAT-COMPOSE".
C      (SI CETTE STRUCTURE EXISTE DEJA, ON LA DETRUIT).
C     ------------------------------------------------------------------
C IN  NOMSD  : NOM DE LA STRUCTURE "RESULTAT" A CREER.
C IN  TYPESD : TYPE DE LA STRUCTURE "RESULTAT" A CREER.
C IN  NBORDR : NOMBRE MAX DE NUM. D'ORDRE.
C ----------------------------------------------------------------------
      INTEGER I,K,IBID,IRET
      INTEGER NAEVEL,NAEVNO,NAEVTH,NAFLAM,NAMOME,NBCHAM,NBNOVA,NCACOU
      INTEGER NCMEC1,NCMEC2,NCMEC3,NCMECA,NCTHER,NCTHET,NCVARC,NPACHA
      INTEGER NPDYHA,NPDYTR,NPEVEL,NPEVNO,NPEVTH,NPFLAM,NPMOME,NPMUEL
      INTEGER NPVARC
      CHARACTER*1 KBID
      CHARACTER*16 TYPES2
      CHARACTER*19 NOMS2
      CHARACTER*32 JEXNOM,JEXNUM
C     ------------------------------------------------------------------
C                      C H A M P _ M E C A N I Q U E
C     ------------------------------------------------------------------
      PARAMETER (NCMEC1=49)
      PARAMETER (NCMEC2=54)
      PARAMETER (NCMEC3=35)
      PARAMETER (NCMECA=NCMEC1+NCMEC2+NCMEC3)
      CHARACTER*16 CHMEC1(NCMEC1)
      CHARACTER*16 CHMEC2(NCMEC2)
      CHARACTER*16 CHMEC3(NCMEC3)
      CHARACTER*16 CHMECA(NCMECA)
C     ------------------------------------------------------------------
C                      C H A M P _ T H E R M I Q U E
C     ------------------------------------------------------------------
      PARAMETER (NCTHER=19)
      CHARACTER*16 CHTHER(NCTHER)
C     ------------------------------------------------------------------
C                      C H A M P _ V A R C
C     ------------------------------------------------------------------
      PARAMETER (NCVARC=6)
      CHARACTER*16 CHVARC(NCVARC)
C     ------------------------------------------------------------------
C                      C H A M P _ A C O U S T I Q U E
C     ------------------------------------------------------------------
      PARAMETER (NCACOU=11)
      CHARACTER*16 CHACOU(NCACOU)
C     ------------------------------------------------------------------
C                      C H A M P _ T H E T A
C     ------------------------------------------------------------------
      PARAMETER (NCTHET=2)
      CHARACTER*16 CHTHET(NCTHET)
C     ------------------------------------------------------------------
C                          E V O L _ E L A S
C     ------------------------------------------------------------------
      PARAMETER (NPEVEL=28,NAEVEL=9)
      CHARACTER*16 PAEVEL(NPEVEL)
C     ------------------------------------------------------------------
C                          E V O L _ N O L I
C     ------------------------------------------------------------------
      PARAMETER (NPEVNO=42,NAEVNO=9)
      CHARACTER*16 PAEVNO(NPEVNO)
C     ------------------------------------------------------------------
C                          E V O L _ T H E R
C     ------------------------------------------------------------------
      PARAMETER (NPEVTH=12,NAEVTH=7)
      CHARACTER*16 PAEVTH(NPEVTH)
C     ------------------------------------------------------------------
C                          E V O L _ V A R C
C     ------------------------------------------------------------------
      PARAMETER (NPVARC=5)
      CHARACTER*16 PAVARC(NPVARC)
C     ------------------------------------------------------------------
C                          M O D E _ M E C A
C     ------------------------------------------------------------------
      PARAMETER (NPMOME=27,NAMOME=10)
      CHARACTER*16 PAMOME(NPMOME)
C     ------------------------------------------------------------------
C                          M O D E _ F L A M B
C     ------------------------------------------------------------------
      PARAMETER (NPFLAM=7,NAFLAM=6)
      CHARACTER*16 PAFLAM(NPFLAM)
C     ------------------------------------------------------------------
C                          D Y N A _ T R A N S
C     ------------------------------------------------------------------
      PARAMETER (NPDYTR=5)
      CHARACTER*16 PADYTR(NPDYTR)
C     ------------------------------------------------------------------
C                          D Y N A _ H A R M O
C     ------------------------------------------------------------------
      PARAMETER (NPDYHA=5)
      CHARACTER*16 PADYHA(NPDYHA)
C     ------------------------------------------------------------------
C                          M U L T _ E L A S
C     ------------------------------------------------------------------
      PARAMETER (NPMUEL=5)
      CHARACTER*16 PAMUEL(NPMUEL)
C     ------------------------------------------------------------------
C                          A C O U _ H A R M O
C     ------------------------------------------------------------------
      PARAMETER (NPACHA=5)
      CHARACTER*16 PAACHA(NPACHA)
C     ------------------------------------------------------------------
C                          E V O L _ E L A S
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PAEVEL/
     & 'INST',            'MODELE',           'CHAMPMAT',
     & 'CARAELEM',        'EXCIT',            'ITER_GCPC',
     & 'METHODE',         'RENUM',            'STOCKAGE',
     & 'RESI_GCPC',       'EFFORT_N',         'MOMENT_MFY',
     & 'MOMENT_MFZ',      'DEFO_D_DX_X',      'DEFO_D_DRY_X',
     & 'DEFO_D_DRZ_X',    'EFFORT_D_VY_X',    'EFFORT_D_VZ_X',
     & 'MOMENT_D_MT_X',   'EFFORT_VY',        'EFFORT_VZ',
     & 'MOMENT_MT',
     & 'ERREUR_ERRE',     'ERREUR_ERZ1',      'ERREUR_ERZ2',
     & 'ERREUR_QIRE',     'ERREUR_QIZ1',      'ERREUR_QIZ2'/
C     ------------------------------------------------------------------
C                          E V O L _ N O L I
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PAEVNO/
     & 'INST',            'MODELE',           'CHAMPMAT',
     & 'CARAELEM',        'EXCIT',            'ITER_GLOB',
     & 'ITER_LINE',       'ITER_DASHPOT',     'GFITER',
     & 'RESI_GLOB_RELA',  'RESI_GLOB',        'RESI_REFE',
     & 'CHAR_MINI',       'ETA_PILOTAGE',     'RESI_GLOB_MOINS',
     & 'CHAR_CRIT',       'GFUM',             'GFUA',
     & 'GFUML',           'GFUI',             'GFVAG',
     & 'GFVFD',           'GFVAD',            'FREQ',
     & 'ERREUR_ERRE',     'ERREUR_ERZ1',      'ERREUR_ERZ2',
     & 'ERREUR_QIRE',     'ERREUR_QIZ1',      'ERREUR_QIZ2',
     & 'PARM_THETA',      'ERRE_TPS_LOC',     'ERRE_TPS_GLOB',
     & 'ERRE_MEC_LOC',    'ERRE_MEC_LOC_D',   'ERRE_HYD_LOC',
     & 'ERRE_MEC_GLOB',   'ERRE_MEC_GLOB_D',  'ERRE_HYD_GLOB',
     & 'ERRE_MEC',        'ERRE_HYD_S',       'ERRE_HYD_D'/
C     ------------------------------------------------------------------
C                          E V O L _ T H E R
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PAEVTH/
     & 'INST',            'MODELE',           'CHAMPMAT',
     & 'CARAELEM',        'EXCIT',            'ITER_GLOB',
     & 'ITER_LINE',       'RESI_GLOB_RELA',   'RESI_GLOB_MAXI',
     & 'RHO',             'PARM_THETA',       'DELTAT'/
C     ------------------------------------------------------------------
C                          E V O L _ V A R C
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PAVARC/
     & 'INST',            'MODELE',           'CHAMPMAT',
     & 'CARAELEM',        'EXCIT'/
C     ------------------------------------------------------------------
C                          M O D E _ M E C A
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PAMOME/
     & 'MODELE',          'CHAMPMAT',         'CARAELEM',
     & 'EXCIT',           'NORME',            'NUME_MODE',
     & 'NUME_DDL',        'TYPE_DEFO',        'NOEUD_CMP',
     & 'FREQ',            'OMEGA2',           'AMOR_REDUIT',
     & 'MASS_GENE',       'RIGI_GENE',        'AMOR_GENE',
     & 'MASS_EFFE_DX',    'MASS_EFFE_DY'   ,  'MASS_EFFE_DZ'   ,
     & 'FACT_PARTICI_DX', 'FACT_PARTICI_DY',  'FACT_PARTICI_DZ',
     & 'MASS_EFFE_UN_DX', 'MASS_EFFE_UN_DY',  'MASS_EFFE_UN_DZ',
     & 'COEF_X',          'COEF_Y',           'COEF_Z'/
C     ------------------------------------------------------------------
C                          M O D E _ F L A M B
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PAFLAM/
     & 'MODELE',          'CHAMPMAT',         'CARAELEM',
     & 'EXCIT',           'NUME_MODE',        'NORME',
     & 'CHAR_CRIT'/
C     ------------------------------------------------------------------
C                          D Y N A _ T R A N S
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PADYTR/
     & 'INST',            'MODELE',           'CHAMPMAT',
     & 'CARAELEM',        'EXCIT'/
C     ------------------------------------------------------------------
C                          D Y N A _ H A R M O
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PADYHA/
     & 'FREQ',            'MODELE',           'CHAMPMAT',
     & 'CARAELEM',        'EXCIT'/
C     ------------------------------------------------------------------
C                          M U L T _ E L A S
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PAMUEL/
     & 'NOM_CAS',         'MODELE',           'CHAMPMAT',
     & 'CARAELEM',        'EXCIT'/
C     ------------------------------------------------------------------
C                          A C O U _ H A R M O
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA PAACHA/
     & 'FREQ',            'MODELE',           'CHAMPMAT',
     & 'CARAELEM',        'EXCIT'/
C     ------------------------------------------------------------------

C     ------------------------------------------------------------------
C                      C H A M P _ M E C A N I Q U E
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHMEC1/
     & 'DEPL',            'VITE',            'ACCE','DEPL_ABSOLU',
     & 'VITE_ABSOLU',     'ACCE_ABSOLU',     'EFGE_ELNO_DEPL',
     & 'EFGE_NOEU_DEPL',  'EFGE_ELNO_CART',  'EFGE_NOEU_CART',
     & 'EPSI_ELGA_DEPL',  'EPSI_ELNO_DEPL',  'EPSI_NOEU_DEPL',
     & 'EPSI_ELNO_TUYO',  'SIEF_ELGA',       'SIEF_ELGA_DEPL',
     & 'SIEF_ELNO_ELGA',  'SIEF_NOEU_ELGA',  'SIEF_ELNO',
     & 'SIEF_NOEU',       'SIGM_ELNO_DEPL',  'SIGM_NOEU_DEPL',
     & 'EPEQ_ELNO_TUYO',  'SIEQ_ELNO_TUYO',  'SIGM_ELNO_CART',
     & 'SIGM_NOEU_CART',  'SIGM_NOZ1_ELGA',  'SIGM_NOZ2_ELGA',
     & 'SIRE_ELNO_DEPL',  'SIRE_NOEU_DEPL',  'SIPO_ELNO_DEPL',
     & 'SIPO_NOEU_DEPL',  'EQUI_ELGA_SIGM',  'EQUI_ELNO_SIGM',
     & 'EQUI_NOEU_SIGM',  'EQUI_ELGA_EPSI',  'EQUI_ELNO_EPSI',
     & 'EQUI_NOEU_EPSI',  'SIGM_ELNO_ZAC',   'EPSP_ELNO_ZAC',
     & 'VARI_ELGA_ZAC',   'SIGM_NOEU_ZAC',   'EPSP_NOEU_ZAC',
     & 'ALPH0_ELGA_EPSP', 'ALPHP_ELGA_ALPH0','VARI_NON_LOCAL',
     & 'LANL_ELGA',       'ARCO_ELNO_SIGM',  'ARCO_NOEU_SIGM'/
C
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHMEC2/
     & 'DEGE_ELNO_DEPL',  'DEGE_NOEU_DEPL',  'EPOT_ELEM_DEPL',
     & 'ECIN_ELEM_DEPL',  'FORC_NODA',       'REAC_NODA',
     & 'ERRE_ELEM_SIGM',  'ERRE_ELNO_ELEM',  'ERRE_NOEU_ELEM',
     & 'ERZ1_ELEM_SIGM',  'ERZ2_ELEM_SIGM',  'QIRE_ELEM_SIGM',
     & 'QIRE_ELNO_ELEM',  'QIRE_NOEU_ELEM',  'QIZ1_ELEM_SIGM',
     & 'QIZ2_ELEM_SIGM',  'EPSG_ELGA_DEPL',  'EPSG_ELNO_DEPL',
     & 'EPSG_NOEU_DEPL',  'EPSP_ELGA',       'EPSP_ELNO',
     & 'EPSP_NOEU',       'VARI_ELGA',       'VARI_ELNO',
     & 'VARI_NOEU',       'VARI_ELNO_ELGA',  'VARI_NOEU_ELGA',
     & 'VARI_ELNO_TUYO',  'EPSA_ELNO',       'EPSA_NOEU',
     & 'COMPORTEMENT',    'DCHA_ELGA_SIGM',  'DCHA_ELNO_SIGM',
     & 'DCHA_NOEU_SIGM',  'RADI_ELGA_SIGM',  'RADI_ELNO_SIGM',
     & 'RADI_NOEU_SIGM',  'ENDO_ELNO_SIGA',  'ENDO_ELNO_SINO',
     & 'ENDO_NOEU_SINO',  'PRES_DBEL_DEPL',  'SIGM_ELNO_COQU',
     & 'EPME_ELNO_DEPL',  'EPME_ELGA_DEPL',  'EPMG_ELNO_DEPL',
     & 'EPMG_ELGA_DEPL',  'ENEL_ELGA',       'ENEL_ELNO_ELGA',
     & 'ENEL_NOEU_ELGA',  'SIGM_NOEU_COQU',  'SIGM_ELNO_TUYO',
     & 'EPMG_NOEU_DEPL',  'SING_ELEM',       'SING_ELNO_ELEM'/
C
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHMEC3/
     & 'EQUI_ELGA_EPME',  'EQUI_ELNO_EPME',   'EQUI_NOEU_EPME',
     & 'DEDE_ELNO_DLDE',  'DEDE_NOEU_DLDE',   'DESI_ELNO_DLSI',
     & 'DESI_NOEU_DLSI',  'PMPB_ELGA_SIEF',   'PMPB_ELNO_SIEF',
     & 'PMPB_NOEU_SIEF',  'SIGM_ELNO_SIEF',   'SIPO_ELNO_SIEF',
     & 'SIGM_NOEU_SIEF',  'SIPO_NOEU_SIEF',   'EPFP_ELNO',
     & 'EPFP_ELGA',       'EPFD_ELNO',        'EPFD_ELGA',
     & 'EPVC_ELNO',       'EPVC_ELGA',        'VALE_CONT',
     & 'VARI_ELNO_COQU',  'CRIT_ELNO_RUPT',   'ETOT_ELGA',
     & 'ETOT_ELNO_ELGA',  'ETOT_ELEM',        'VALE_NCOU_MAXI',
     & 'MODE_FLAMB',      'ENDO_ELGA',        'ENDO_ELNO_ELGA',
     & 'INDI_LOCA_ELGA',  'EXTR_ELGA_VARI',   'EXTR_ELNO_VARI',
     & 'EXTR_NOEU_VARI',  'MODE_MECA'/

C     ------------------------------------------------------------------
C                      C H A M P _ T H E R M I Q U E
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHTHER/
     & 'TEMP',            'FLUX_ELGA_TEMP',  'FLUX_ELNO_TEMP',
     & 'FLUX_NOEU_TEMP',  'META_ELGA_TEMP',  'META_ELNO_TEMP',
     & 'META_NOEU_TEMP',  'DURT_ELGA_META',  'DURT_ELNO_META',
     & 'DURT_NOEU_META',  'HYDR_ELNO_ELGA',  'SOUR_ELGA_ELEC',
     & 'HYDR_NOEU_ELGA',  'DETE_ELNO_DLTE',  'DETE_NOEU_DLTE',
     & 'COMPORTHER',      'ERRE_ELEM_TEMP',  'ERRE_ELNO_ELEM',
     & 'ERRE_NOEU_ELEM'/
C     ------------------------------------------------------------------
C                      C H A M P _ V A R C
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHVARC/
     & 'IRRA',            'TEMP',            'HYDR_ELNO_ELGA',
     & 'HYDR_NOEU_ELGA',  'EPSA_ELNO',       'META_ELNO_TEMP'/
C     ------------------------------------------------------------------
C                      C H A M P _ A C O U S T I Q U E
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHACOU/
     & 'PRES',            'PRES_ELNO_DBEL',  'PRES_ELNO_REEL',
     & 'PRES_ELNO_IMAG',  'INTE_ELNO_ACTI',  'INTE_ELNO_REAC',
     & 'PRES_NOEU_DBEL',  'PRES_NOEU_REEL',  'PRES_NOEU_IMAG',
     & 'INTE_NOEU_ACTI',  'INTE_NOEU_REAC'/
C     ------------------------------------------------------------------
C                      C H A M P _ T H E T A _ R U P T
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHTHET/
     & 'THETA',           'GRAD_NOEU_THETA'/
C     ------------------------------------------------------------------

      NOMS2 = NOMSD
      TYPES2 = TYPESD

C     --- SI LA SD EXISTE DEJA, ON LA DETRUIT ---

      CALL JEEXIN(NOMS2//'.DESC',IRET)
      IF (IRET.GT.0) CALL RSDLSD(NOMSD)

C     --- CREATION DE .DESC, .NOVA, .ORDR ---

      CALL JECREO(NOMS2//'.DESC','G N K16')
      CALL JECREO(NOMS2//'.NOVA','G N K16')
      CALL JECREO(NOMS2//'.ORDR','G V I')
      CALL JEECRA(NOMS2//'.ORDR','LONMAX',NBORDR,' ')
      CALL JEECRA(NOMS2//'.ORDR','LONUTI',0,' ')

      DO 10 I = 1,NCMEC1
        CHMECA(I) = CHMEC1(I)
   10 CONTINUE
      DO 20 I = 1,NCMEC2
        CHMECA(I+NCMEC1) = CHMEC2(I)
   20 CONTINUE
      DO 30 I = 1,NCMEC3
        CHMECA(I+NCMEC1+NCMEC2) = CHMEC3(I)
   30 CONTINUE

C     ------------------------------------------------------------------
      IF (TYPES2.EQ.'EVOL_ELAS') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVEL')
        DO 40 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
   40   CONTINUE

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPEVEL,' ')
        DO 50 I = 1,NPEVEL
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAEVEL(I)))
   50   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPEVEL)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'INST','INST','R',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_GCPC','ITER','I',NBORDR)
        CALL UTACCE('P',NOMSD,'METHODE','METH','K16',NBORDR)
        CALL UTACCE('P',NOMSD,'RENUM','RENU','K16',NBORDR)
        CALL UTACCE('P',NOMSD,'STOCKAGE','STOC','K16',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        CALL UTPARA(NOMSD,NPEVEL,NAEVEL,PAEVEL,NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'MULT_ELAS') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MUEL')
        DO 60 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
   60   CONTINUE

        NBNOVA = NPMUEL
        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,' ')

        DO 65 I = 1,NPMUEL
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAMUEL(I)))
   65   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NBNOVA)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'NOM_CAS','NCAS','K16',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'FOURIER_ELAS') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'FOEL')
        DO 70 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
   70   CONTINUE

        NBNOVA = 6
        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,' ')
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','NUME_MODE'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','TYPE_MODE'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','MODELE'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','CHAMPMAT'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','CARAELEM'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','EXCIT'))

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NBNOVA)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'NUME_MODE','NUMO','I',NBORDR)
        CALL UTACCE('P',NOMSD,'TYPE_MODE','TYMO','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)

        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'FOURIER_THER') THEN

        NBCHAM = NCTHER
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'FOTH')
        DO 71 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHTHER(I)))
   71   CONTINUE

        NBNOVA = 6
        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,' ')
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','NUME_MODE'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','TYPE_MODE'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','MODELE'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','CHAMPMAT'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','CARAELEM'))
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','EXCIT'))

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NBNOVA)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'NUME_MODE','NUMO','I',NBORDR)
        CALL UTACCE('P',NOMSD,'TYPE_MODE','TYMO','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)

        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'EVOL_NOLI') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVNO')
        DO 80 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
   80   CONTINUE

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPEVNO,' ')
        DO 90 I = 1,NPEVNO
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAEVNO(I)))
   90   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPEVNO)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'INST','INST','R',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_GLOB','ITEG','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_LINE','ITEL','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_DASHPOT','ITDA','I',NBORDR)
        CALL UTACCE('P',NOMSD,'GFITER','ITGF','I',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        CALL UTPARA(NOMSD,NPEVNO,NAEVNO,PAEVNO,NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'DYNA_TRANS') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'DYTR')
        DO 100 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  100   CONTINUE
        NBNOVA = NPDYTR
        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,' ')

        DO 105 I = 1,NPDYTR
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PADYTR(I)))
  105   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NBNOVA)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'INST','INST','R',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'DYNA_HARMO') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'DYHA')
        DO 110 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  110   CONTINUE

        NBNOVA = NPDYHA
        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,' ')

        DO 115 I = 1,NPDYHA
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PADYHA(I)))
  115   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &            NBNOVA)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'FREQ','FREQ','R',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'HARM_GENE') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'HAGE')
        DO 120 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  120   CONTINUE
        GO TO 290

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'ACOU_HARMO') THEN

        NBCHAM = NCACOU
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'ACHA')
        DO 130 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHACOU(I)))
  130   CONTINUE

        NBNOVA = NPACHA
        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,' ')

        DO 135 I = 1,NPACHA
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAACHA(I)))
  135   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NBNOVA)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'FREQ','FREQ','R',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)

        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'EVOL_CHAR') THEN

        NBCHAM = 6
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVCH')
        CALL JECROC(JEXNOM(NOMS2//'.DESC','PRES'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','FVOL_3D'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','FVOL_2D'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','FSUR_3D'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','FSUR_2D'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','VITE_VENT'))
        GO TO 300

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'EVOL_THER') THEN

        NBCHAM = NCTHER
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVTH')
        DO 140 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHTHER(I)))
  140   CONTINUE

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPEVTH,' ')
        DO 150 I = 1,NPEVTH
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAEVTH(I)))
  150   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPEVTH)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,PAEVTH(1),'INST','R',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        CALL UTACCE('P',NOMSD,PAEVTH(6),'ITEG','I',NBORDR)
        CALL UTACCE('P',NOMSD,PAEVTH(7),'ITEL','I',NBORDR)
        CALL UTPARA(NOMSD,NPEVTH,NAEVTH,PAEVTH,NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'EVOL_VARC') THEN

        NBCHAM = NCVARC
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVVA')
        DO 160 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHVARC(I)))
  160   CONTINUE

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPVARC,' ')
        DO 170 I = 1,NPVARC
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAVARC(I)))
  170   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPVARC)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'INST','INST','R',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'MODE_MECA'     .OR.
     &         TYPES2.EQ.'MODE_MECA_C'   .OR.
     &         TYPES2.EQ.'MODE_GENE'     .OR.
     &         TYPES2(1:9).EQ.'MODE_STAT'.OR.
     &         TYPES2.EQ.'MODE_ACOU'     .OR.
     &         TYPES2.EQ.'DYNAMIQUE'     .OR.
     &         TYPES2.EQ.'BASE_MODALE'  ) THEN


        IF (TYPES2.EQ.'MODE_MECA') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOME')
        ELSEIF (TYPES2.EQ.'MODE_MECA_C') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOME')
        ELSEIF (TYPES2.EQ.'MODE_GENE') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOGE')
        ELSEIF (TYPES2(1:9).EQ.'MODE_STAT') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOST')
        ELSE IF (TYPES2.EQ.'DYNAMIQUE') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'BAMO')
        ELSE IF (TYPES2.EQ.'BASE_MODALE') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'BAMO')
        ELSE IF (TYPES2.EQ.'MODE_ACOU') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOAC')
        END IF

        IF (TYPES2.EQ.'MODE_ACOU') THEN
           NBCHAM = 1
           CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
           CALL JECROC(JEXNOM(NOMS2//'.DESC','PRES'))
        ELSE
           NBCHAM = NCMECA
           CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
           DO 200 I = 1,NBCHAM
             CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  200      CONTINUE
        END IF

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPMOME,' ')
        DO 210 I = 1,NPMOME
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAMOME(I)))
  210   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPMOME)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        IF (TYPES2.EQ.'BASE_MODALE' .OR.
     &      TYPES2.EQ.'DYNAMIQUE') THEN
           CALL UTACCE('A',NOMSD,'NOEUD_CMP','NOEU','K16',NBORDR)
           CALL UTACCE('A',NOMSD,'FREQ'     ,'FREQ','R'  ,NBORDR)
           CALL UTACCE('A',NOMSD,'NUME_MODE','NUMO','I'  ,NBORDR)
        ELSEIF (TYPES2(1:9).EQ.'MODE_STAT') THEN
           CALL UTACCE('A',NOMSD,'NOEUD_CMP','NOEU','K16',NBORDR)
           CALL UTACCE('P',NOMSD,'FREQ'     ,'FREQ','R'  ,NBORDR)
           CALL UTACCE('P',NOMSD,'NUME_MODE','NUMO','I'  ,NBORDR)
        ELSE
           CALL UTACCE('A',NOMSD,'FREQ'     ,'FREQ','R'  ,NBORDR)
           CALL UTACCE('A',NOMSD,'NUME_MODE','NUMO','I'  ,NBORDR)
           CALL UTACCE('P',NOMSD,'NOEUD_CMP','NOEU','K16',NBORDR)
        END IF
        CALL UTACCE('P',NOMSD,'NUME_DDL' ,'NUME','I'  ,NBORDR)
        CALL UTACCE('P',NOMSD,'TYPE_DEFO','TYPE','K16',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE'   ,'MODL','K8' ,NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT' ,'MATE','K8' ,NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM' ,'CARA','K8' ,NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT'    ,'CHAR','K24',NBORDR)
        CALL UTACCE('P',NOMSD,'NORME'    ,'NORM','K24',NBORDR)
        CALL UTPARA(NOMSD,NPMOME,NAMOME,PAMOME,NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'MODE_FLAMB') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOFL')
        DO 220 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  220   CONTINUE

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPFLAM,' ')
        DO 230 I = 1,NPFLAM
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAFLAM(I)))
  230   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPFLAM)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'NUME_MODE','NUMO','I'  ,NBORDR)
        CALL UTACCE('P',NOMSD,'NORME'    ,'NORM','K24',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE'   ,'MODL','K8' ,NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT' ,'MATE','K8' ,NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM' ,'CARA','K8' ,NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT'    ,'CHAR','K24',NBORDR)
        CALL UTPARA(NOMSD,NPFLAM,NAFLAM,PAFLAM,NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'COMB_FOURIER') THEN

        NBCHAM = NCMECA
        NBCHAM = NBCHAM + NCTHER-3
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'COFO')
        DO 270 I = 1,NCMECA
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  270   CONTINUE
        DO 271 I = 1,NCTHER-3
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHTHER(I)))
  271   CONTINUE

        NBNOVA = 1
        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,' ')
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','ANGL'))

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NBNOVA)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'ANGL','ANGL','R',NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'THETA_GEOM') THEN

        NBCHAM = NCTHET
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'THET')
        DO 280 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHTHET(I)))
  280   CONTINUE

C       -- AJOUT D'UN PARAMETRE BIDON : XXXX POUR QUE LE POINTEUR
C          DE NOM .NOVA SOIT UN OBJET JEVEUX NON VIDE :
        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',1,' ')
        CALL JECROC(JEXNOM(NOMS2//'.NOVA','XXXX'))

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',1)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('P',NOMSD,'XXXX' ,'XXXX','K8',NBORDR)

        GO TO 310

      ELSE
        CALL U2MESK('F','UTILITAI4_31',1,TYPES2)
      END IF

C     ------------------------------------------------------------------
  290 CONTINUE
      NBNOVA = 1
      CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,' ')
      CALL JECROC(JEXNOM(NOMS2//'.NOVA','FREQ'))

      CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &            NBNOVA)
      CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

      CALL UTACCE('A',NOMSD,'FREQ','FREQ','R',NBORDR)
      GO TO 310

C     ------------------------------------------------------------------
  300 CONTINUE
      NBNOVA = 1
      CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,' ')
      CALL JECROC(JEXNOM(NOMS2//'.NOVA','INST'))

      CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &            NBNOVA)
      CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

      CALL UTACCE('A',NOMSD,'INST','INST','R',NBORDR)

C     ------------------------------------------------------------------
  310 CONTINUE

C     --- CREATION DE .TACH ---
      CALL JECREC(NOMS2//'.TACH','G V K24','NU','CONTIG','CONSTANT',
     &            NBCHAM)
      CALL JEECRA(NOMS2//'.TACH','LONMAX',NBORDR,' ')


C     -- POUR QUE LES COLLECTIONS .TACH ET .TAVA SOIENT BIEN CREEES :
C     ---------------------------------------------------------------
      DO 320,K = 1,NBCHAM
        CALL JECROC(JEXNUM(NOMS2//'.TACH',K))
  320 CONTINUE
      CALL JELIRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,KBID)
      DO 330,K = 1,NBNOVA
        CALL JECROC(JEXNUM(NOMS2//'.TAVA',K))
  330 CONTINUE


      END
