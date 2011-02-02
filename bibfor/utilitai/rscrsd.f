      SUBROUTINE RSCRSD(BASE,NOMSD,TYPESD,NBORDR)
      IMPLICIT NONE
      CHARACTER*(*) BASE,NOMSD,TYPESD
      INTEGER NBORDR
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 02/02/2011   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_20
C ----------------------------------------------------------------------
C      CREATION D'UNE STRUCTURE DE DONNEES "RESULTAT-COMPOSE".
C      (SI CETTE STRUCTURE EXISTE DEJA, ON LA DETRUIT).
C     ------------------------------------------------------------------
C IN  NOMSD  : NOM DE LA STRUCTURE "RESULTAT" A CREER.
C IN  TYPESD : TYPE DE LA STRUCTURE "RESULTAT" A CREER.
C IN  NBORDR : NOMBRE MAX DE NUM. D'ORDRE.
C ----------------------------------------------------------------------
      INTEGER I,K,IBID,IRET,JORDR
      INTEGER NBCHAM,NBNOVA
      INTEGER NCMEC1,NCMEC2,NCMEC3,NCMECA,NCTHER,NCTHET,NCVARC,NCACOU
      CHARACTER*1 KBID,BAS1
      CHARACTER*16 TYPES2
      CHARACTER*19 NOMS2
      CHARACTER*32 JEXNOM,JEXNUM
C     ------------------------------------------------------------------
C                      C H A M P _ M E C A N I Q U E
C     ------------------------------------------------------------------
      PARAMETER (NCMEC1=39)
      PARAMETER (NCMEC2=49)
      PARAMETER (NCMEC3=41)
      PARAMETER (NCMECA=NCMEC1+NCMEC2+NCMEC3)
      CHARACTER*16 CHMEC1(NCMEC1)
      CHARACTER*16 CHMEC2(NCMEC2)
      CHARACTER*16 CHMEC3(NCMEC3)
      CHARACTER*16 CHMECA(NCMECA)
C     ------------------------------------------------------------------
C                      C H A M P _ T H E R M I Q U E
C     ------------------------------------------------------------------
      PARAMETER (NCTHER=17)
      CHARACTER*16 CHTHER(NCTHER)
C     ------------------------------------------------------------------
C                      C H A M P _ V A R C
C     ------------------------------------------------------------------
      PARAMETER (NCVARC=6)
      CHARACTER*16 CHVARC(NCVARC)
C     ------------------------------------------------------------------
C                      C H A M P _ A C O U S T I Q U E
C     ------------------------------------------------------------------
      PARAMETER (NCACOU=5)
      CHARACTER*16 CHACOU(NCACOU)
C     ------------------------------------------------------------------
C                      C H A M P _ T H E T A
C     ------------------------------------------------------------------
      PARAMETER (NCTHET=2)
      CHARACTER*16 CHTHET(NCTHET)
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C                      C H A M P _ M E C A N I Q U E
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHMEC1/
     & 'DEPL',            'VITE',            'ACCE',
     & 'DEPL_ABSOLU',     'VITE_ABSOLU',     'ACCE_ABSOLU',
     & 'EFGE_ELNO',  'EFGE_NOEU',  'EFCA_ELNO',
     & 'EFCA_NOEU',  'EPSI_ELGA',       'EPSI_ELNO',
     & 'EPSI_NOEU',       'EPTU_ELNO',  'SIEF_ELGA',
     & 'SIEF_ELNO',       'SIEF_NOEU',       'SIGM_ELNO',
     & 'SIGM_NOEU',  'EPTQ_ELNO',       'SITQ_ELNO',
     & 'SICA_ELNO',  'SICA_NOEU',  'SIZ1_ELGA',
     & 'SIZ2_ELGA',  'SIPO_ELNO',       'SIPO_NOEU',
     & 'SIEQ_ELGA',  'SIEQ_ELNO',  'SIEQ_NOEU',
     & 'EPEQ_ELGA',  'EPEQ_ELNO',  'EPEQ_NOEU',
     & 'ALPH0_ELGA_EPSP', 'ALPHP_ELGA_ALPH0','VARI_NON_LOCAL',
     & 'LANL_ELGA',       'SIRO_ELEM',       'FLHN_ELGA'/
C
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHMEC2/
     & 'DEGE_ELNO',       'DEGE_NOEU',       'EPOT_ELEM',
     & 'ECIN_ELEM',       'FORC_NODA',       'REAC_NODA',
     & 'ERME_ELEM',       'ERME_ELNO',       'ERME_NOEU',
     & 'ERZ1_ELEM',       'ERZ2_ELEM',       'QIRE_ELEM',
     & 'QIRE_ELNO',       'QIRE_NOEU',       'QIZ1_ELEM',
     & 'QIZ2_ELEM',       'EPSG_ELGA',       'EPSG_ELNO',
     & 'EPSG_NOEU',       'EPSP_ELGA',       'EPSP_ELNO',
     & 'EPSP_NOEU',       'VARI_ELGA',
     & 'VARI_NOEU',       'VARI_ELNO',
     & 'VATU_ELNO',  'EPSA_ELNO',       'EPSA_NOEU',
     & 'COMPORTEMENT',    'DERA_ELGA',       'DERA_ELNO',
     & 'DERA_NOEU',       'PRME_ELNO',       'SICO_ELNO',
     & 'EPME_ELNO',       'EPME_ELGA',       'EPMG_ELNO',
     & 'EPMG_ELGA',       'ENEL_ELGA',       'ENEL_ELNO',
     & 'ENEL_NOEU',       'SICO_NOEU',  'SITU_ELNO',
     & 'EPMG_NOEU',       'SING_ELEM',       'SING_ELNO',
     & 'DISS_ELGA',       'DISS_ELNO',       'DISS_NOEU'/
C
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHMEC3/
     & 'EPMQ_ELGA',  'EPMQ_ELNO',  'EPMQ_NOEU',
     & 'DEDE_ELNO',       'DEDE_NOEU',       'DESI_ELNO',
     & 'DESI_NOEU',       'PMPB_ELGA',       'PMPB_ELNO',
     & 'PMPB_NOEU',       'EPFP_ELNO',
     & 'EPFP_ELGA',       'EPFD_ELNO',       'EPFD_ELGA',
     & 'EPVC_ELNO',       'EPVC_ELGA',       'VALE_CONT',
     & 'VACO_ELNO',  'CRIT_ELNO',       'ETOT_ELGA',
     & 'ETOT_ELNO',       'ETOT_ELEM',       'SPMX_ELGA',
     & 'MODE_FLAMB',
     & 'ENDO_ELGA',       'ENDO_ELNO',       'ENDO_NOEU',
     & 'INDL_ELGA',       'VAEX_ELGA',       'VAEX_ELNO',
     & 'VAEX_NOEU',       'MODE_MECA',       'SIEF_SENO_SEGA',
     & 'COHE_ELGA',       'INDC_ELGA',       'SECO_ELGA',
     & 'VARC_ELGA',       'FERRAILLAGE',     'EPVC_NOEU',
     & 'EPFD_NOEU',       'EPFP_NOEU'/
C     ------------------------------------------------------------------
C                      C H A M P _ T H E R M I Q U E
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHTHER/
     & 'TEMP',
     & 'FLUX_ELGA',       'FLUX_ELNO',       'FLUX_NOEU',
     & 'META_ELNO',       'META_NOEU',
     & 'DURT_ELNO',       'DURT_NOEU',
     & 'HYDR_ELNO',       'HYDR_NOEU',
     & 'DETE_ELNO',       'DETE_NOEU',
     & 'SOUR_ELGA',       'COMPORTHER',
     & 'ERTH_ELEM',       'ERTH_ELNO',       'ERTH_NOEU'/
C     ------------------------------------------------------------------
C                      C H A M P _ V A R C
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHVARC/
     & 'IRRA',            'TEMP',            'HYDR_ELNO',
     & 'HYDR_NOEU',       'EPSA_ELNO',       'META_ELNO'/
C     ------------------------------------------------------------------
C                      C H A M P _ A C O U S T I Q U E
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHACOU/
     & 'PRES',            'PRAC_ELNO',       'PRAC_NOEU',
     & 'INTE_ELNO',       'INTE_NOEU'/
C     ------------------------------------------------------------------
C                      C H A M P _ T H E T A _ R U P T
C     ------------------------------------------------------------------
C      '1234567890123456','1234567890123456','1234567890123456',
      DATA CHTHET/
     & 'THETA',           'GRAD_NOEU_THETA'/
C     ------------------------------------------------------------------

      NOMS2=NOMSD
      TYPES2=TYPESD
      BAS1=BASE

C     --- SI LA SD EXISTE DEJA, ON S'ARRETE EN ERREUR F :
      CALL JEEXIN(NOMS2//'.DESC',IRET)
      CALL ASSERT(IRET.EQ.0)

C     --- CREATION DE .DESC  ET  .ORDR ---
      CALL JECREO(NOMS2//'.DESC',BAS1//' N K16')
      CALL WKVECT(NOMS2//'.ORDR',BAS1//' V I',NBORDR,JORDR)
      CALL JEECRA(NOMS2//'.ORDR','LONUTI',0,' ')

      DO 10 I=1,NCMEC1
        CHMECA(I)=CHMEC1(I)
   10 CONTINUE
      DO 20 I=1,NCMEC2
        CHMECA(I+NCMEC1)=CHMEC2(I)
   20 CONTINUE
      DO 30 I=1,NCMEC3
        CHMECA(I+NCMEC1+NCMEC2)=CHMEC3(I)
   30 CONTINUE

C     -- DECLARATION ET INITIALISATION DES PARAMETRES ET VAR. D'ACCES :
C     ------------------------------------------------------------------
      CALL UTPARA(BAS1,NOMSD,TYPES2,NBORDR)




C     ------------------------------------------------------------------
      IF (TYPES2.EQ.'EVOL_ELAS') THEN

        NBCHAM=NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVEL')
        DO 40 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
   40   CONTINUE

        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'MULT_ELAS') THEN

        NBCHAM=NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MUEL')
        DO 60 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
   60   CONTINUE
        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'FOURIER_ELAS') THEN

        NBCHAM=NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'FOEL')
        DO 80 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
   80   CONTINUE

        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'FOURIER_THER') THEN

        NBCHAM=NCTHER
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'FOTH')
        DO 90 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHTHER(I)))
   90   CONTINUE

        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'EVOL_NOLI') THEN

        NBCHAM=NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVNO')
        DO 100 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  100   CONTINUE

        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'DYNA_TRANS') THEN

        NBCHAM=NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'DYTR')
        DO 120 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  120   CONTINUE
        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'DYNA_HARMO') THEN

        NBCHAM=NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'DYHA')
        DO 140 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  140   CONTINUE
        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'HARM_GENE') THEN

        NBCHAM=NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'HAGE')
        DO 160 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  160   CONTINUE
        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'ACOU_HARMO') THEN

        NBCHAM=NCACOU
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'ACHA')
        DO 170 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHACOU(I)))
  170   CONTINUE

        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'EVOL_CHAR') THEN

        NBCHAM=6
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVCH')
        CALL JECROC(JEXNOM(NOMS2//'.DESC','PRES'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','FVOL_3D'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','FVOL_2D'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','FSUR_3D'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','FSUR_2D'))
        CALL JECROC(JEXNOM(NOMS2//'.DESC','VITE_VENT'))
        GOTO 320


C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'EVOL_THER') THEN

        NBCHAM=NCTHER
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVTH')
        DO 190 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHTHER(I)))
  190   CONTINUE
        GOTO 320


C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'EVOL_VARC') THEN

        NBCHAM=NCVARC
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'EVVA')
        DO 210 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHVARC(I)))
  210   CONTINUE
        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'MODE_MECA' .OR. TYPES2.EQ.'MODE_MECA_C' .OR.
     &        TYPES2.EQ.'MODE_GENE' .OR. TYPES2.EQ.'MODE_ACOU' .OR.
     &        TYPES2.EQ.'DYNAMIQUE' ) THEN


        IF (TYPES2.EQ.'MODE_MECA') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOME')
        ELSEIF (TYPES2.EQ.'MODE_MECA_C') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOME')
        ELSEIF (TYPES2.EQ.'MODE_GENE') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOGE')
        ELSEIF (TYPES2.EQ.'DYNAMIQUE') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'BAMO')
        ELSEIF (TYPES2.EQ.'MODE_ACOU') THEN
          CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOAC')
        ENDIF

        IF (TYPES2.EQ.'MODE_ACOU') THEN
          NBCHAM=1
          CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
          CALL JECROC(JEXNOM(NOMS2//'.DESC','PRES'))
        ELSE
          NBCHAM=NCMECA
          CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
          DO 230 I=1,NBCHAM
            CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  230     CONTINUE
        ENDIF
        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'MODE_FLAMB') THEN

        NBCHAM=NCMECA
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'MOFL')
        DO 250 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  250   CONTINUE
        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'COMB_FOURIER') THEN

        NBCHAM=NCMECA
        NBCHAM=NBCHAM+NCTHER-3
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'COFO')
        DO 270 I=1,NCMECA
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHMECA(I)))
  270   CONTINUE
        DO 280 I=1,NCTHER-3
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHTHER(I)))
  280   CONTINUE
        GOTO 320

C     ------------------------------------------------------------------
      ELSEIF (TYPES2.EQ.'THETA_GEOM') THEN

        NBCHAM=NCTHET
        CALL JEECRA(NOMS2//'.DESC','NOMMAX',NBCHAM,' ')
        CALL JEECRA(NOMS2//'.DESC','DOCU',IBID,'THET')
        DO 290 I=1,NBCHAM
          CALL JECROC(JEXNOM(NOMS2//'.DESC',CHTHET(I)))
  290   CONTINUE
        GOTO 320

      ELSE
        CALL U2MESK('F','UTILITAI4_31',1,TYPES2)
      ENDIF

C     ------------------------------------------------------------------
  320 CONTINUE



C     --- CREATION DE .TACH
C     -------------------------
      CALL JECREC(NOMS2//'.TACH',BAS1//' V K24','NU','CONTIG',
     &            'CONSTANT',NBCHAM)
      CALL JEECRA(NOMS2//'.TACH','LONMAX',NBORDR,' ')


C     -- POUR QUE LES COLLECTIONS .TACH ET .TAVA SOIENT BIEN CREEES :
C     ---------------------------------------------------------------
      DO 330,K=1,NBCHAM
        CALL JECROC(JEXNUM(NOMS2//'.TACH',K))
  330 CONTINUE
      CALL JELIRA(NOMS2//'.NOVA','NOMMAX',NBNOVA,KBID)
      DO 340,K=1,NBNOVA
        CALL JECROC(JEXNUM(NOMS2//'.TAVA',K))
  340 CONTINUE


      END
