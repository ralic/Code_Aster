      SUBROUTINE COPISD(TYPESD,BASE,SD1,SD2)
      IMPLICIT NONE
      CHARACTER*(*) TYPESD,BASE,SD1,SD2
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
C RESPONSABLE VABHHTS J.PELLET
C
C   BUT:
C   DUPLIQUER UNE STRUCTURE DE DONNEES SOUS UN AUTRE NOM.
C   (SI SD2 EXISTE DEJA, ON L'ECRASE)
C
C     IN:
C     TYPESD  : TYPE DE LA SD A DUPPLIQUER
C               ' '(INCONNU)      'CHAMP_GD'
C               'FONCTION'  (POUR FONCTIONS ET NAPPES)  
C               'CORRESP_2_MAILLA'
C               'CHAM_NO_S'       'CHAM_ELEM_S'
C               'VARI_COM'        'TABLE'
C               'RESULTAT'        'NUME_DDL'
C               'MAILLAGE'        'LIGREL'
C               'MATR_ASSE_GENE'  'MATR_ASSE'
C               'PROF_CHNO'
C     BASE     : 'G' , 'V' , ... : BASE DE CREATION DE SD2
C     SD1 (K*) : NOM DE LA SD A DUPPLIQUER
C     SD2 (K*) : NOM DE LA SD A CREER
C
C     OUT:
C     SD2 EST CREEE ET A LE MEME CONTENU QUE SD1
C
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER IRET,IRES,NMAIL,NSMAIL
      CHARACTER*1 BAS2
      CHARACTER*8 MAIL1,MAIL2
      CHARACTER*14 COM1,COM2,NU1,NU2
      CHARACTER*16 TYP2SD,CORR1,CORR2
      CHARACTER*19 CH1,CH2,SDR1,MAT1,MAT2,PRNO1,PRNO2
      CHARACTER*19 CHC1,CHC2,CNS1,CNS2,CES1,CES2,LIGRE1,LIGRE2
      CHARACTER*24 O1,O2
C
C DEB-------------------------------------------------------------------
C
      CALL JEMARQ
      BAS2 = BASE
C
C ----------------------------------------------------------------------
C     SUPRESSION DE SD2 :
      CALL DETRSD(TYPESD,SD2)
C
C ----------------------------------------------------------------------
      IF (TYPESD .EQ. ' ') THEN
C     -----------------------
C       -- TYPESD INCONNU => ON UTILISE JEDUPC => COUTEUX EN CPU
        CALL JEDUPC(' ',SD1,1,BASE,SD2,.TRUE.)
C
C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'CHAMP_GD') THEN
C     -----------------------------------
        CH1 = SD1
        CH2 = SD2
        CALL COPICH(BAS2,CH1,CH2)
C
C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'VARI_COM') THEN
C     -----------------------------------
        COM1 = SD1
        COM2 = SD2
C
        CH1 = COM1 // '.TEMP'
        CH2 = COM2 // '.TEMP'
        CALL EXISD('CHAMP_GD',CH1,IRET)
        IF (IRET .NE. 0) CALL COPICH(BAS2,CH1,CH2)
C
        CH1 = COM1 // '.HYDR'
        CH2 = COM2 // '.HYDR'
        CALL EXISD('CHAMP_GD',CH1,IRET)
        IF (IRET .NE. 0) CALL COPICH(BAS2,CH1,CH2)
C
        CH1 = COM1 // '.SECH'
        CH2 = COM2 // '.SECH'
        CALL EXISD('CHAMP_GD',CH1,IRET)
        IF (IRET .NE. 0) CALL COPICH(BAS2,CH1,CH2)
C
        CH1 = COM1 // '.IRRA'
        CH2 = COM2 // '.IRRA'
        CALL EXISD('CHAMP_GD',CH1,IRET)
        IF (IRET .NE. 0) CALL COPICH(BAS2,CH1,CH2)
C
        CH1 = COM1 // '.PHAS'
        CH2 = COM2 // '.PHAS'
        CALL EXISD('CHAMP_GD',CH1,IRET)
        IF (IRET .NE. 0) CALL COPICH(BAS2,CH1,CH2)
C
        CH1 = COM1 // '.EPAN'
        CH2 = COM2 // '.EPAN'
        CALL EXISD('CHAMP_GD',CH1,IRET)
        IF (IRET .NE. 0) CALL COPICH(BAS2,CH1,CH2)
C
        CH1 = COM1 // '.INST'
        CH2 = COM2 // '.INST'
        CALL EXISD('CHAMP_GD',CH1,IRET)
        IF (IRET .NE. 0) CALL COPICH(BAS2,CH1,CH2)
C
        CH1 = COM1 // '.CORR'
        CH2 = COM2 // '.CORR'
        CALL EXISD('CHAMP_GD',CH1,IRET)
        IF (IRET .NE. 0) CALL COPICH(BAS2,CH1,CH2)
C
        CALL JEDUPO(COM1//'.EXISTENCE',BAS2,COM2//'.EXISTENCE',.FALSE.)
C
C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'FONCTION') THEN
C     -----------------------------------
        CHC1 = SD1
        CHC2 = SD2
C
C        -- .PARA :
        O1 = CHC1 // '.PARA'
        O2 = CHC2 // '.PARA'
        CALL JEEXIN(O1,IRET)
        IF (IRET .GT. 0) CALL JEDUPO(O1,BAS2,O2,.FALSE.)
C
C        -- .PROL :
        O1 = CHC1 // '.PROL'
        O2 = CHC2 // '.PROL'
        CALL JEEXIN(O1,IRET)
        IF (IRET .GT. 0) CALL JEDUPO(O1,BAS2,O2,.FALSE.)
C
C        -- .VALE :
        O1 = CHC1 // '.VALE'
        O2 = CHC2 // '.VALE'
        CALL JEEXIN(O1,IRET)
        IF (IRET .GT. 0) CALL JEDUPO(O1,BAS2,O2,.FALSE.)
C
C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'CORRESP_2_MAILLA') THEN
C        ---------------------------------
        CORR1 = SD1
        CORR2 = SD2
        CALL JEDUPO(CORR1//'.PJEF_NO',BAS2,CORR2//'.PJEF_NO',.FALSE.)
        CALL JEDUPO(CORR1//'.PJEF_NU',BAS2,CORR2//'.PJEF_NU',.FALSE.)
        CALL JEDUPO(CORR1//'.PJEF_NB',BAS2,CORR2//'.PJEF_NB',.FALSE.)
        CALL JEDUPO(CORR1//'.PJEF_CF',BAS2,CORR2//'.PJEF_CF',.FALSE.)
        CALL JEDUPO(CORR1//'.PJEF_M1',BAS2,CORR2//'.PJEF_M1',.FALSE.)
C
C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'CHAM_NO_S') THEN
C     -----------------------------------
        CNS1 = SD1
        CNS2 = SD2
        CALL JEDUPO(CNS1//'.CNSD',BAS2,CNS2//'.CNSD',.FALSE.)
        CALL JEDUPO(CNS1//'.CNSK',BAS2,CNS2//'.CNSK',.FALSE.)
        CALL JEDUPO(CNS1//'.CNSC',BAS2,CNS2//'.CNSC',.FALSE.)
        CALL JEDUPO(CNS1//'.CNSL',BAS2,CNS2//'.CNSL',.FALSE.)
        CALL JEDUPO(CNS1//'.CNSV',BAS2,CNS2//'.CNSV',.FALSE.)
C
C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'CHAM_ELEM_S') THEN
C     -----------------------------------
        CES1 = SD1
        CES2 = SD2
        CALL JEDUPO(CES1//'.CESD',BAS2,CES2//'.CESD',.FALSE.)
        CALL JEDUPO(CES1//'.CESK',BAS2,CES2//'.CESK',.FALSE.)
        CALL JEDUPO(CES1//'.CESC',BAS2,CES2//'.CESC',.FALSE.)
        CALL JEDUPO(CES1//'.CESL',BAS2,CES2//'.CESL',.FALSE.)
        CALL JEDUPO(CES1//'.CESV',BAS2,CES2//'.CESV',.FALSE.)

C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'PROF_CHNO') THEN
C     -----------------------------------
        PRNO1 = SD1
        PRNO2 = SD2
        O1 = PRNO1 // '.DEEQ'
        O2 = PRNO2 // '.DEEQ'
        CALL JEDUPO(O1,BAS2,O2,.FALSE.)
        O1 = PRNO1 // '.NUEQ'
        O2 = PRNO2 // '.NUEQ'
        CALL JEDUPO(O1,BAS2,O2,.FALSE.)
        O1 = PRNO1 // '.PRNO'
        O2 = PRNO2 // '.PRNO'
        CALL JEDUPO(O1,BAS2,O2,.FALSE.)
        O1 = PRNO1 // '.LILI'
        O2 = PRNO2 // '.LILI'
        CALL JEDUPO(O1,BAS2,O2,.FALSE.)
        O1 = PRNO1 // '.LPRN'
        O2 = PRNO2 // '.LPRN'
        CALL JEDUPO(O1,BAS2,O2,.FALSE.)

C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'NUME_DDL') THEN
C     -----------------------------------
        NU1 = SD1
        NU2 = SD2
        O1 = NU1 // '.NUME.DEEQ'
        O2 = NU2 // '.NUME.DEEQ'
        CALL JEEXIN(O1,IRET)
        IF (IRET .GT. 0) THEN
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.NUME.DELG'
          O2 = NU2 // '.NUME.DELG'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.NUME.NEQU'
          O2 = NU2 // '.NUME.NEQU'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.NUME.NUEQ'
          O2 = NU2 // '.NUME.NUEQ'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.NUME.PRNO'
          O2 = NU2 // '.NUME.PRNO'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.NUME.LILI'
          O2 = NU2 // '.NUME.LILI'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.NUME.LPRN'
          O2 = NU2 // '.NUME.LPRN'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.NUME.REFN'
          O2 = NU2 // '.NUME.REFN'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
        ENDIF
        O1 = NU1 // '.SLCS.ABLO'
        O2 = NU2 // '.SLCS.ABLO'
        CALL JEEXIN(O1,IRET)
        IF (IRET .GT. 0) THEN
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SLCS.ADIA'
          O2 = NU2 // '.SLCS.ADIA'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SLCS.DESC'
          O2 = NU2 // '.SLCS.DESC'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SLCS.HCOL'
          O2 = NU2 // '.SLCS.HCOL'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SLCS.IABL'
          O2 = NU2 // '.SLCS.IABL'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SLCS.REFE'
          O2 = NU2 // '.SLCS.REFE'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
        ENDIF
        O1 = NU1 // '.SMOS.ABLO'
        O2 = NU2 // '.SMOS.ABLO'
        CALL JEEXIN(O1,IRET)
        IF (IRET .GT. 0) THEN
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SMOS.ADIA'
          O2 = NU2 // '.SMOS.ADIA'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SMOS.DESC'
          O2 = NU2 // '.SMOS.DESC'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SMOS.HCOL'
          O2 = NU2 // '.SMOS.HCOL'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SMOS.IABL'
          O2 = NU2 // '.SMOS.IABL'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
          O1 = NU1 // '.SMOS.REFE'
          O2 = NU2 // '.SMOS.REFE'
          CALL JEDUPO(O1,BAS2,O2,.FALSE.)
        ENDIF
C
C --------------------------------------------------------------------
      ELSE IF (TYPESD.EQ.'MATR_ASSE_GENE') THEN
C     -------------------------------------------
        MAT1=SD1
        MAT2=SD2
        CALL JEDUPO(MAT1//'.DESC',BAS2,MAT2//'.DESC',.FALSE.)
        CALL JEDUPO(MAT1//'.LIME',BAS2,MAT2//'.LIME',.FALSE.)
        CALL JEDUPO(MAT1//'.CONL',BAS2,MAT2//'.CONL',.FALSE.)
        CALL JEDUPO(MAT1//'.REFA',BAS2,MAT2//'.REFA',.FALSE.)
        CALL JEDUPO(MAT1//'.VALE',BAS2,MAT2//'.VALE',.FALSE.)
C
C --------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'MATR_ASSE') THEN
C     -----------------------------------
        MAT1 = SD1
        MAT2 = SD2
        CALL JEDUPO(MAT1//'.ABLI',BAS2,MAT2//'.ABLI',.FALSE.)
        CALL JEDUPO(MAT1//'.ALIG',BAS2,MAT2//'.ALIG',.FALSE.)
        CALL JEDUPO(MAT1//'.COND',BAS2,MAT2//'.COND',.FALSE.)
        CALL JEDUPO(MAT1//'.CONI',BAS2,MAT2//'.CONI',.FALSE.)
        CALL JEDUPO(MAT1//'.CONL',BAS2,MAT2//'.CONL',.FALSE.)
        CALL JEDUPO(MAT1//'.JDRF',BAS2,MAT2//'.JDRF',.FALSE.)
        CALL JEDUPO(MAT1//'.JDDC',BAS2,MAT2//'.JDDC',.FALSE.)
        CALL JEDUPO(MAT1//'.JDFF',BAS2,MAT2//'.JDFF',.FALSE.)
        CALL JEDUPO(MAT1//'.JDHF',BAS2,MAT2//'.JDHF',.FALSE.)
        CALL JEDUPO(MAT1//'.JDPM',BAS2,MAT2//'.JDPM',.FALSE.)
        CALL JEDUPO(MAT1//'.JDES',BAS2,MAT2//'.JDES',.FALSE.)
        CALL JEDUPO(MAT1//'.JDVL',BAS2,MAT2//'.JDVL',.FALSE.)
        CALL JEDUPO(MAT1//'.LILI',BAS2,MAT2//'.LILI',.FALSE.)
        CALL JEDUPO(MAT1//'.LIME',BAS2,MAT2//'.LIME',.FALSE.)
        CALL JEDUPO(MAT1//'.LLIG',BAS2,MAT2//'.LLIG',.FALSE.)
        CALL JEDUPO(MAT1//'.REFA',BAS2,MAT2//'.REFA',.FALSE.)
        CALL JEDUPO(MAT1//'.VALE',BAS2,MAT2//'.VALE',.FALSE.)
        CALL JEDUPO(MAT1//'.VALF',BAS2,MAT2//'.VALF',.FALSE.)
        CALL JEDUPO(MAT1//'.VALI',BAS2,MAT2//'.VALI',.FALSE.)
C
C --------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'TABLE') THEN
C     -----------------------------------
        CALL EXISD(TYPESD,SD1,IRET)
        IF (IRET .EQ. 0) THEN
          SDR1 = SD1
          CALL UTMESS('F','COPISD',
     &               'STRUCTURE DE DONNEES INEXISTANTE :'//SDR1)
        ENDIF
        CALL TBCOPI(BASE,SD1,SD2)
C
C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'RESULTAT') THEN
C     -----------------------------------
        CALL EXISD(TYPESD,SD1,IRET)
        IF (IRET .EQ. 0) THEN
          SDR1 = SD1
          CALL UTMESS('F','COPISD',
     &               'STRUCTURE DE DONNEES INEXISTANTE :'//SDR1)
        ENDIF
        CALL RSCOPI(BASE,SD1,SD2)
C
C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'LIGREL') THEN
C     -----------------------------------
        CALL EXISD(TYPESD,SD1,IRET)
        LIGRE1 = SD1
        LIGRE2 = SD2
        IF (IRET .EQ. 0) THEN
          CALL UTMESS('F','COPISD',
     &               'STRUCTURE DE DONNEES INEXISTANTE :'//LIGRE1)
        ENDIF
        CALL JEEXIN(LIGRE1//'.LGNS',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(LIGRE1//'.LGNS',BAS2,LIGRE2//'.LGNS',.FALSE.)
        ENDIF
        CALL JEDUPO(LIGRE1//'.LIEL',BAS2,LIGRE2//'.LIEL',.FALSE.)
        CALL JEDUPO(LIGRE1//'.NEMA',BAS2,LIGRE2//'.NEMA',.FALSE.)
        CALL JEDUPO(LIGRE1//'.NOMA',BAS2,LIGRE2//'.NOMA',.FALSE.)
        CALL JEDUPO(LIGRE1//'.NBNO',BAS2,LIGRE2//'.NBNO',.FALSE.)
        CALL JEEXIN(LIGRE1//'.PRNM',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(LIGRE1//'.PRNM',BAS2,LIGRE2//'.PRNM',.FALSE.)
        ENDIF
        CALL JEEXIN(LIGRE1//'.PRNS',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(LIGRE1//'.PRNS',BAS2,LIGRE2//'.PRNS',.FALSE.)
        ENDIF
C
C ----------------------------------------------------------------------
      ELSEIF (TYPESD .EQ. 'MAILLAGE') THEN
C     -----------------------------------
        CALL EXISD(TYPESD,SD1,IRET)
        MAIL1 = SD1
        MAIL2 = SD2
        IF (IRET .EQ. 0) THEN
          CALL UTMESS('F','COPISD',
     &               'STRUCTURE DE DONNEES INEXISTANTE :'//MAIL1)
        ENDIF

        CALL JEDUPO(MAIL1//'           .LTNS',BAS2,
     &              MAIL2//'           .LTNS',.FALSE.)
        CALL JEDUPO(MAIL1//'           .LTNT',BAS2,
     &              MAIL2//'           .LTNT',.FALSE.)
        CALL JEDUPO(MAIL1//'           .TITR',BAS2,
     &              MAIL2//'           .TITR',.FALSE.)
        CALL JEDUPO(MAIL1//'.ADAPTATION',BAS2,
     &              MAIL2//'.ADAPTATION',.FALSE.)

        CALL JEDUPO(MAIL1//'.DIME',BAS2,MAIL2//'.DIME',.FALSE.)
C -- NOMBRE DE MAILLES ET SUPER MAILLES
        CALL JEVEUO(MAIL1//'.DIME           ','L',IRES)
        NMAIL = ZI(IRES+3-1)
        NSMAIL = ZI(IRES+4-1)

        CALL JEDUPO(MAIL1//'.NOMNOE',BAS2,MAIL2//'.NOMNOE',.FALSE.)
C
        CALL JEDUPO(MAIL1//'.COORDO    .VALE',BAS2,
     &              MAIL2//'.COORDO    .VALE',.FALSE.)
        CALL JEDUPO(MAIL1//'.COORDO    .REFE',BAS2,
     &              MAIL2//'.COORDO    .REFE',.FALSE.)
        CALL JEDUPO(MAIL1//'.COORDO    .DESC',BAS2,
     &              MAIL2//'.COORDO    .DESC',.FALSE.)

        CALL JEEXIN(MAIL1//'.NOMGNO',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(MAIL1//'.NOMGNO',BAS2,MAIL2//'.NOMGNO',.FALSE.)
        ENDIF
        CALL JEEXIN(MAIL1//'.GROUPENO',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(MAIL1//'.GROUPENO',BAS2,
     &                MAIL2//'.GROUPENO',.FALSE.)
        ENDIF
        IF (NMAIL.NE.0) THEN
          CALL JEDUPO(MAIL1//'.NOMMAI',BAS2,MAIL2//'.NOMMAI',.FALSE.)
          CALL JEDUPO(MAIL1//'.TYPMAIL',BAS2,MAIL2//'.TYPMAIL',.FALSE.)
          CALL JEDUPO(MAIL1//'.CONNEX',BAS2,MAIL2//'.CONNEX',.FALSE.)
          CALL JEEXIN(MAIL1//'.NOMGMA',IRET)
          IF (IRET .NE. 0) THEN
            CALL JEDUPO(MAIL1//'.NOMGMA',BAS2,MAIL2//'.NOMGMA',.FALSE.)
          ENDIF
          CALL JEEXIN(MAIL1//'.GROUPEMA',IRET)
          IF (IRET .NE. 0) THEN
            CALL JEDUPO(MAIL1//'.GROUPEMA',BAS2,
     &                     MAIL2//'.GROUPEMA',.FALSE.)
          ENDIF
        ENDIF
        IF (NSMAIL.NE.0) THEN
          CALL JEDUPO(MAIL1//'.NOMACR',BAS2,MAIL2//'.NOMACR',.FALSE.)
          CALL JEDUPO(MAIL1//'.PARA_R',BAS2,MAIL2//'.PARA_R',.FALSE.)
          CALL JEDUPO(MAIL1//'.SUPMAIL',BAS2,MAIL2//'.SUPMAIL',.FALSE.)
          CALL JEEXIN(MAIL1//'.TYPL',IRET)
          IF (IRET .NE. 0) THEN
            CALL JEDUPO(MAIL1//'.TYPL',BAS2,MAIL2//'.TYPL',.FALSE.)
          ENDIF
        ENDIF
        CALL JEDUPO(MAIL1//'.TITR',BAS2,MAIL2//'.TITR',.FALSE.)
C -- CAS ABSC_CURV
        CALL JEEXIN(MAIL1//'.ABS_CURV  .DESC',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(MAIL1//'.ABS_CURV  .DESC',BAS2,
     &                MAIL2//'.ABS_CURV  .DESC',.FALSE.)
        ENDIF
        CALL JEEXIN(MAIL1//'.ABS_CURV  .LIMA',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(MAIL1//'.ABS_CURV  .LIMA',BAS2,
     &                MAIL2//'.ABS_CURV  .LIMA',.FALSE.)
        ENDIF
        CALL JEEXIN(MAIL1//'.ABS_CURV  .NOLI',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(MAIL1//'.ABS_CURV  .NOLI',BAS2,
     &                MAIL2//'.ABS_CURV  .NOLI',.FALSE.)
        ENDIF
        CALL JEEXIN(MAIL1//'.ABS_CURV  .NOMA',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(MAIL1//'.ABS_CURV  .NOMA',BAS2,
     &                MAIL2//'.ABS_CURV  .NOMA',.FALSE.)
        ENDIF
        CALL JEEXIN(MAIL1//'.ABS_CURV  .VALE',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(MAIL1//'.ABS_CURV  .VALE',BAS2,
     &                MAIL2//'.ABS_CURV  .VALE',.FALSE.)
        ENDIF
C -- CAS DU FORMAT MED
        CALL JEEXIN(MAIL1//'           .FORM',IRET)
        IF (IRET .NE. 0) THEN
          CALL JEDUPO(MAIL1//'           .FORM',BAS2,
     &                MAIL2//'           .FORM',.FALSE.)
        ENDIF
C
C ----------------------------------------------------------------------
      ELSE
        TYP2SD = TYPESD
        CALL UTMESS('F',' COPISD ','TYPE DE SD. INCONNU : '//TYP2SD)
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA
      END
