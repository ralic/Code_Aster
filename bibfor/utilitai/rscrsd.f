      SUBROUTINE RSCRSD(NOMSD,TYPESD,NBORDR)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) NOMSD,TYPESD
      INTEGER NBORDR
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 25/10/2004   AUTEUR CIBHHLV L.VIVAN 
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
      CHARACTER*1 KBID
      CHARACTER*16 TYPES2
      CHARACTER*19 NOMS2
      CHARACTER*32 JEXNOM,JEXNUM
C     ------------------------------------------------------------------
C                      C H A M P _ M E C A N I Q U E
C     ------------------------------------------------------------------
      PARAMETER (NCMEC1=47)
      PARAMETER (NCMEC2=47)
      PARAMETER (NCMEC3=27)
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
      PARAMETER (NCVARC=1)
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
      PARAMETER (NPEVEL=22,NAEVEL=9)
      CHARACTER*16 PAEVEL(NPEVEL)
C     ------------------------------------------------------------------
C                          E V O L _ N O L I
C     ------------------------------------------------------------------
      PARAMETER (NPEVNO=21,NAEVNO=8)
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
      PARAMETER (NPMOME=30,NAMOME=15)
      CHARACTER*16 PAMOME(NPMOME)
C     ------------------------------------------------------------------
C                          M O D E _ F L A M B
C     ------------------------------------------------------------------
      PARAMETER (NPFLAM=8,NAFLAM=6)
      CHARACTER*16 PAFLAM(NPFLAM)
C     ------------------------------------------------------------------
C                          M O D E _ A C O U
C     ------------------------------------------------------------------
      PARAMETER (NPMOMA=21,NAMOMA=15)
      CHARACTER*16 PAMOMA(NPMOMA)
C     ------------------------------------------------------------------
C                          M O D E _ S T A T
C     ------------------------------------------------------------------
      PARAMETER (NPMOST=10,NAMOST=10)
      CHARACTER*16 PAMOST(NAMOST)
C     ------------------------------------------------------------------
C                          B A S E _ M O D A L E
C     ------------------------------------------------------------------
      PARAMETER (NPBAMO=12,NABAMO=9)
      CHARACTER*16 PABAMO(NPBAMO)
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
      DATA PAEVEL/'INST','MODELE','CHAMPMAT','CARAELEM','EXCIT',
     &     'ITER_GCPC','METHODE','RENUM','STOCKAGE',
     &     'RESI_GCPC','EFFORT_N','MOMENT_MFY','MOMENT_MFZ',
     &     'DEFO_D_DX_X','DEFO_D_DRY_X','DEFO_D_DRZ_X','EFFORT_D_VY_X',
     &     'EFFORT_D_VZ_X','MOMENT_D_MT_X','EFFORT_VY','EFFORT_VZ',
     &     'MOMENT_MT'/
C     ------------------------------------------------------------------
C                          E V O L _ N O L I
C     ------------------------------------------------------------------
      DATA PAEVNO/'INST','MODELE','CHAMPMAT','CARAELEM','EXCIT',
     &     'ITER_GLOB','ITER_LINE','ITER_DASHPOT','RESI_GLOB_RELA',
     &     'RESI_GLOB','CHAR_MINI','ETA_PILOTAGE','RESI_GLOB_MOINS',
     &     'CHAR_CRIT','GFUM','GFUA','GFUML','GFUI','GFVAG','GFVFD',
     &     'GFVAD'/
C     ------------------------------------------------------------------
C                          E V O L _ T H E R
C     ------------------------------------------------------------------
      DATA PAEVTH/'INST','MODELE','CHAMPMAT','CARAELEM','EXCIT',
     &     'ITER_GLOB','ITER_LINE','RESI_GLOB_RELA',
     &     'RESI_GLOB_MAXI','RHO','PARM_THETA','DELTAT'/
C     ------------------------------------------------------------------
C                          E V O L _ V A R C
C     ------------------------------------------------------------------
      DATA PAVARC/'INST','MODELE','CHAMPMAT','CARAELEM','EXCIT'/
C     ------------------------------------------------------------------
C                          M O D E _ M E C A
C     ------------------------------------------------------------------
      DATA PAMOME/'NUME_MODE','FREQ','MODELE','CHAMPMAT','CARAELEM',
     &     'EXCIT','NORME','METHODE','ITER_QR',
     &     'ITER_BATHE','ITER_ARNO','ITER_JACOBI','ITER_SEPARE',
     &     'ITER_AJUSTE','ITER_INVERSE','OMEGA2','AMOR_REDUIT  ',
     &     'ERREUR','MASS_GENE ','RIGI_GENE   ','AMOR_GENE',
     &     'MASS_EFFE_DX','MASS_EFFE_DY','MASS_EFFE_DZ',
     &     'FACT_PARTICI_DX','FACT_PARTICI_DY','FACT_PARTICI_DZ',
     &     'MASS_EFFE_UN_DX','MASS_EFFE_UN_DY','MASS_EFFE_UN_DZ'/
C     ------------------------------------------------------------------
C                          M O D E _ F L A M B
C     ------------------------------------------------------------------
      DATA PAFLAM/'NUME_MODE','NORME','MODELE','CHAMPMAT','CARAELEM',
     &            'EXCIT','CHAR_CRIT','ERREUR'/
C     ------------------------------------------------------------------
C                          M O D E _ A C O U
C     ------------------------------------------------------------------
      DATA PAMOMA/'NUME_MODE','FREQ','MODELE','CHAMPMAT','CARAELEM',
     &     'EXCIT','NORME','METHODE','ITER_QR',
     &     'ITER_BATHE','ITER_ARNO','ITER_JACOBI','ITER_SEPARE',
     &     'ITER_AJUSTE','ITER_INVERSE','OMEGA2','AMOR_REDUIT  ',
     &     'ERREUR','MASS_GENE ','RIGI_GENE   ','AMOR_GENE'/
C     ------------------------------------------------------------------
C                          M O D E _ S T A T
C     ------------------------------------------------------------------
      DATA PAMOST/'NOEUD_CMP','NUME_DDL','TYPE_DEFO','COEF_X','COEF_Y',
     &     'COEF_Z','MODELE','CHAMPMAT','CARAELEM','EXCIT'/
C     ------------------------------------------------------------------
C                          B A S E _ M O D A L E
C     ------------------------------------------------------------------
      DATA PABAMO/'NUME_MODE','FREQ','NOEUD_CMP','NORME','TYPE_DEFO',
     &     'MODELE','CHAMPMAT','CARAELEM','EXCIT',
     &     'OMEGA2','MASS_GENE','RIGI_GENE'/
C     ------------------------------------------------------------------
C                          D Y N A _ T R A N S
C     ------------------------------------------------------------------
      DATA PADYTR/'INST','MODELE','CHAMPMAT','CARAELEM','EXCIT'/
C     ------------------------------------------------------------------
C                          D Y N A _ H A R M O
C     ------------------------------------------------------------------
      DATA PADYHA/'FREQ','MODELE','CHAMPMAT','CARAELEM','EXCIT'/
C     ------------------------------------------------------------------
C                          M U L T _ E L A S
C     ------------------------------------------------------------------
      DATA PAMUEL/'NOM_CAS','MODELE','CHAMPMAT','CARAELEM','EXCIT'/
C     ------------------------------------------------------------------
C                          A C O U _ H A R M O
C     ------------------------------------------------------------------
      DATA PAACHA/'FREQ','MODELE','CHAMPMAT','CARAELEM','EXCIT'/
C     ------------------------------------------------------------------

C     ------------------------------------------------------------------
C                      C H A M P _ M E C A N I Q U E
C     ------------------------------------------------------------------
      DATA CHMEC1/'DEPL','VITE','ACCE','DEPL_ABSOLU','VITE_ABSOLU',
     &     'ACCE_ABSOLU','EFGE_ELNO_DEPL','EFGE_NOEU_DEPL',
     &     'EFGE_ELNO_CART','EFGE_NOEU_CART','EPSI_ELGA_DEPL',
     &     'EPSI_ELNO_DEPL','EPSI_NOEU_DEPL','EPSI_ELNO_TUYO',
     &     'SIEF_ELGA','SIEF_ELGA_DEPL','SIEF_ELNO_ELGA',
     &     'SIEF_NOEU_ELGA','SIEF_ELNO','SIEF_NOEU','SIGM_ELNO_DEPL',
     &     'SIGM_NOEU_DEPL','EPEQ_ELNO_TUYO','SIEQ_ELNO_TUYO',
     &     'SIGM_ELNO_CART','SIGM_NOEU_CART','SIGM_NOZ1_ELGA',
     &     'SIGM_NOZ2_ELGA','SIRE_ELNO_DEPL','SIRE_NOEU_DEPL',
     &     'SIPO_ELNO_DEPL','SIPO_NOEU_DEPL','EQUI_ELGA_SIGM',
     &     'EQUI_ELNO_SIGM','EQUI_NOEU_SIGM','EQUI_ELGA_EPSI',
     &     'EQUI_ELNO_EPSI','EQUI_NOEU_EPSI','SIGM_ELNO_ZAC',
     &     'EPSP_ELNO_ZAC','VARI_ELGA_ZAC','SIGM_NOEU_ZAC',
     &     'EPSP_NOEU_ZAC','ALPH0_ELGA_EPSP','ALPHP_ELGA_ALPH0',
     &     'VARI_NON_LOCAL','LANL_ELGA'/
      DATA CHMEC2/'DEGE_ELNO_DEPL','DEGE_NOEU_DEPL','EPOT_ELEM_DEPL',
     &     'ECIN_ELEM_DEPL','FORC_NODA','REAC_NODA','ERRE_ELGA_NORE',
     &     'ERRE_ELNO_ELGA','ERRE_NOEU_ELGA','ERRE_ELEM_NOZ1',
     &     'ERRE_ELEM_NOZ2','EPSG_ELGA_DEPL','EPSG_ELNO_DEPL',
     &     'EPSG_NOEU_DEPL','EPSP_ELGA','EPSP_ELNO','EPSP_NOEU',
     &     'VARI_ELGA','VARI_ELNO','VARI_NOEU','VARI_ELNO_ELGA',
     &     'VARI_NOEU_ELGA','VARI_ELNO_TUYO','EPSA_ELNO','EPSA_NOEU',
     &     'COMPORTEMENT','DCHA_ELGA_SIGM','DCHA_ELNO_SIGM',
     &     'DCHA_NOEU_SIGM','RADI_ELGA_SIGM','RADI_ELNO_SIGM',
     &     'RADI_NOEU_SIGM','ENDO_ELNO_SIGA','ENDO_ELNO_SINO',
     &     'ENDO_NOEU_SINO','PRES_DBEL_DEPL','SIGM_ELNO_COQU',
     &     'EPME_ELNO_DEPL','EPME_ELGA_DEPL','EPMG_ELNO_DEPL',
     &     'EPMG_ELGA_DEPL','ENEL_ELGA','ENEL_ELNO_ELGA',
     &     'ENEL_NOEU_ELGA','SIGM_NOEU_COQU','SIGM_ELNO_TUYO',
     &     'EPMG_NOEU_DEPL'/
      DATA CHMEC3/'EQUI_ELGA_EPME','EQUI_ELNO_EPME','EQUI_NOEU_EPME',
     &     'DEDE_ELNO_DLDE','DEDE_NOEU_DLDE','DESI_ELNO_DLSI',
     &     'DESI_NOEU_DLSI','PMPB_ELGA_SIEF','PMPB_ELNO_SIEF',
     &     'PMPB_NOEU_SIEF','SIGM_ELNO_SIEF','SIPO_ELNO_SIEF',
     &     'EPGR_ELGA','SIGM_NOEU_SIEF','SIPO_NOEU_SIEF','EPGR_ELNO',
     &     'VALE_CONT','VARI_ELNO_COQU','CRIT_ELNO_RUPT','ETOT_ELGA',
     &     'ETOT_ELNO_ELGA','ETOT_ELEM','VALE_NCOU_MAXI',
     &     'MODE_FLAMB','ENDO_ELGA','ENDO_ELNO_ELGA','INDI_LOCA_ELGA'/
C     ------------------------------------------------------------------
C                      C H A M P _ T H E R M I Q U E
C     ------------------------------------------------------------------
      DATA CHTHER/'TEMP','FLUX_ELGA_TEMP','FLUX_ELNO_TEMP',
     &     'FLUX_NOEU_TEMP','META_ELGA_TEMP','META_ELNO_TEMP',
     &     'META_NOEU_TEMP','DURT_ELGA_META','DURT_ELNO_META',
     &     'DURT_NOEU_META','HYDR_ELGA','HYDR_ELNO_ELGA',
     &     'HYDR_NOEU_ELGA','DETE_ELNO_DLTE','DETE_NOEU_DLTE',
     &     'COMPORTHER','ERTH_ELEM_TEMP','ERTH_ELNO_ELEM',
     &     'SOUR_ELGA_ELEC'/
C     ------------------------------------------------------------------
C                      C H A M P _ V A R C
C     ------------------------------------------------------------------
      DATA CHVARC/'IRRA'/
C     ------------------------------------------------------------------
C                      C H A M P _ A C O U S T I Q U E
C     ------------------------------------------------------------------
      DATA CHACOU/'PRES','PRES_ELNO_DBEL','PRES_ELNO_REEL',
     &     'PRES_ELNO_IMAG','INTE_ELNO_ACTI','INTE_ELNO_REAC',
     &     'PRES_NOEU_DBEL','PRES_NOEU_REEL','PRES_NOEU_IMAG',
     &     'INTE_NOEU_ACTI','INTE_NOEU_REAC'/
C     ------------------------------------------------------------------
C                      C H A M P _ T H E T A _ R U P T
C     ------------------------------------------------------------------
      DATA CHTHET/'THETA','GRAD_NOEU_THETA'/
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
      ELSE IF (TYPES2(1:9).EQ.'MODE_STAT') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOST')
        DO 180 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  180   CONTINUE

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPMOST,' ')
        DO 190 I = 1,NPMOST
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAMOST(I)))
  190   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPMOST)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'NOEUD_CMP','NOEU','K16',NBORDR)
        CALL UTACCE('P',NOMSD,'NUME_DDL','NUME','I',NBORDR)
        CALL UTACCE('P',NOMSD,'TYPE_DEFO','TYPE','K16',NBORDR)
        CALL UTACCE('P',NOMSD,'COEF_X','COEX','R',NBORDR)
        CALL UTACCE('P',NOMSD,'COEF_Y','COEY','R',NBORDR)
        CALL UTACCE('P',NOMSD,'COEF_Z','COEZ','R',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        GO TO 310
C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'MODE_MECA' .OR. TYPES2.EQ.'MODE_GENE'
     &        .OR. TYPES2.EQ.'MODE_MECA_C' ) THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        IF (TYPES2.EQ.'MODE_MECA') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOME')
        ELSEIF (TYPES2.EQ.'MODE_MECA_C') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOME')
        ELSE
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOGE')
        END IF
        DO 200 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  200   CONTINUE

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPMOME,' ')
        DO 210 I = 1,NPMOME
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAMOME(I)))
  210   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPMOME)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'FREQ','FREQ','R',NBORDR)
        CALL UTACCE('A',NOMSD,'NUME_MODE','NUMO','I',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        CALL UTACCE('P',NOMSD,'NORME','NORM','K24',NBORDR)
        CALL UTACCE('P',NOMSD,'METHODE','METH','K24',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_QR','ITEQ','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_BATHE','ITEB','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_ARNO','ITEA','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_JACOBI','ITEJ','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_SEPARE','ITES','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_AJUSTE','ITAJ','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_INVERSE','ITEI','I',NBORDR)
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

        CALL UTACCE('A',NOMSD,'NUME_MODE','NUMO','I',NBORDR)
        CALL UTACCE('P',NOMSD,'NORME','NORM','K24',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        CALL UTPARA(NOMSD,NPFLAM,NAFLAM,PAFLAM,NBORDR)
        GO TO 310
C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'MODE_ACOU') THEN

        NBCHAM = 1
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOAC')
        CALL JECROC(JEXNOM(NOMS2//'.DESC','PRES'))

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPMOMA,' ')
        DO 240 I = 1,NPMOMA
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PAMOMA(I)))
  240   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPMOMA)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'FREQ','FREQ','R',NBORDR)
        CALL UTACCE('A',NOMSD,'NUME_MODE','NUMO','I',NBORDR)
        CALL UTACCE('P',NOMSD,'NORME','NORM','K24',NBORDR)
        CALL UTACCE('P',NOMSD,'METHODE','METH','K24',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_QR','ITEQ','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_BATHE','ITEB','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_ARNO','ITEA','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_JACOBI','ITEJ','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_SEPARE','ITES','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_AJUSTE','ITAJ','I',NBORDR)
        CALL UTACCE('P',NOMSD,'ITER_INVERSE','ITEI','I',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        CALL UTPARA(NOMSD,NPMOMA,NAMOMA,PAMOMA,NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'BASE_MODALE') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'BAMO')
        DO 250 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  250   CONTINUE

        CALL JEECRA(NOMS2//'.NOVA','NOMMAX',NPBAMO,' ')
        DO 260 I = 1,NPBAMO
          CALL JECROC(JEXNOM(NOMS2//'.NOVA',PABAMO(I)))
  260   CONTINUE

        CALL JECREC(NOMS2//'.TAVA','G V K8','NU','CONTIG','CONSTANT',
     &              NPBAMO)
        CALL JEECRA(NOMS2//'.TAVA','LONMAX',4,' ')

        CALL UTACCE('A',NOMSD,'FREQ','FREQ','R',NBORDR)
        CALL UTACCE('A',NOMSD,'NUME_MODE','NUMO','I',NBORDR)
        CALL UTACCE('A',NOMSD,'NOEUD_CMP','NOEU','K16',NBORDR)
        CALL UTACCE('P',NOMSD,'NORME','NORM','K24',NBORDR)
        CALL UTACCE('P',NOMSD,'TYPE_DEFO','TYPE','K16',NBORDR)
        CALL UTACCE('P',NOMSD,'MODELE','MODL','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CHAMPMAT','MATE','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'CARAELEM','CARA','K8',NBORDR)
        CALL UTACCE('P',NOMSD,'EXCIT','CHAR','K24',NBORDR)
        CALL UTPARA(NOMSD,NPBAMO,NABAMO,PABAMO,NBORDR)
        GO TO 310

C     ------------------------------------------------------------------
      ELSE IF (TYPES2.EQ.'COMB_FOURIER') THEN

        NBCHAM = NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'COFO')
        DO 270 I = 1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  270   CONTINUE

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

        GO TO 310

      ELSE
        CALL UTMESS('F','RSCRSD','TYPE_RESULTAT INCONNU :'//TYPES2)
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
