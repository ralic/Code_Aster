      SUBROUTINE OP0112 ()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ECHANGE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GREFFET N.GREFFET
C TOLE CRP_4
C ======================================================================
C
C     COMMANDE:  MODI_CHAR_YACS
C  RECUPERATION DES EFFORTS DE SATURNE VIA YACS POUR COUPLAGE IFS
C
C ======================================================================
      IMPLICIT NONE
C     ------------------------------------------------------------------
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
C
      INTEGER          NBVAL, ICMP, IBID, IDECAL, INO2
      INTEGER          INO1, II, JJ, ICMPG, IOCC, IMA, NBNOG1, NBMAG1
      INTEGER          JNOMO, JCHNSK, JCHNSD, JCHNSC, JCHNSV, JCHNSL
      INTEGER          JACONO, JACONB, JACONU, JACOCF, JFLAN1, IALIN2
      INTEGER          JNCMP, JVALV, JLIGR, JALIM1, JCXMA1
      INTEGER          JFORC2, ILENGT
      INTEGER          NBCMPG, NBNO2, NBNO1, NBOCC
      INTEGER          DDLFOR(3)
      CHARACTER*1      KBID
      CHARACTER*8      CHARG, MODELE, MA, MA1, MA2
      CHARACTER*16     CORRES, NOMGMA, NOMGNO, NCMPGD(10), NOMCMP(3)
      CHARACTER*19     CHNOS, CARTE
      CHARACTER*24     LIEL, GRPMA, GRPNO
C ======================================================================
C ======================================================================
C     COUPLAGE =>
      INTEGER*4          LENVAR,CPITER,NUMPA4,NBNO4,TAILLE,IBID4
      PARAMETER (LENVAR = 144)
      CHARACTER*(LENVAR) NOMVAR
      PARAMETER (CPITER= 41)
      INTEGER            ICOMPO,NUMPAS,IADR,IFM,NIV
      REAL*8             TI,TF,DT
      CHARACTER*24       AYACS
      INTEGER      IARG
C     COUPLAGE <=

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV (IFM, NIV)
C
C     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
C     ------------------------------------------------------------
      AYACS='&ADR_YACS'
C
C     RECUPERATION DE L'ADRESSE YACS
C     ------------------------------
      CALL JEVEUO(AYACS,'L',IADR)
      ICOMPO=ZI(IADR)

C     ! ========================== !
C     ! RECUPERATION DES MOTS-CLES !
C     ! ========================== !
      CALL GETVR8(' ','INST',0,IARG,1,TF,IBID)
      CALL GETVR8(' ','PAS',0,IARG,1,DT,IBID)
      CALL GETVIS(' ','NUME_ORDRE_YACS',0,IARG,1,NUMPAS,IBID)
      NUMPA4 = NUMPAS
      CALL GETVID(' ', 'CHAR_MECA', 1,IARG, 1, CHARG,  IBID)
      CALL GETVID(' ', 'MATR_PROJECTION',     1,IARG, 1, CORRES, IBID)
      CALL GETVTX(' ', 'NOM_CMP_IFS', 1,IARG, 3, NOMCMP, IBID)
      CALL GETFAC('VIS_A_VIS',NBOCC)
      IF (NBOCC.LT.1) THEN
        CALL U2MESS('F','COUPLAGEIFS_5')
      ENDIF
      NCMPGD(1)  = 'FX'
      NCMPGD(2)  = 'FY'
      NCMPGD(3)  = 'FZ'
      NCMPGD(4)  = 'MX'
      NCMPGD(5)  = 'MY'
      NCMPGD(6)  = 'MZ'
      NCMPGD(7)  = 'REP'
      NCMPGD(8)  = 'ALPHA'
      NCMPGD(9)  = 'BETA'
      NCMPGD(10) = 'GAMMA'
      NBCMPG     = 10

C     ! =============================== !
C     ! RECUPERATION DU NOM DU MAILLAGE !
C     ! =============================== !
      CALL JEVEUO(CHARG//'.CHME.MODEL.NOMO','L',JNOMO)
      MODELE = ZK8(JNOMO)
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,MA,IBID)

C      ! ========================= !
C      ! CREATION D'UN CHAMPS_NO_S !
C      ! ========================= !
      CHNOS='&&OP0112.CHNOS'
      CALL CNSCRE(MA, 'FORC_R', NBCMPG, NCMPGD, 'V', CHNOS)
      CALL JEVEUO(CHNOS//'.CNSK', 'L', JCHNSK)
      CALL JEVEUO(CHNOS//'.CNSD', 'L', JCHNSD)
      CALL JEVEUO(CHNOS//'.CNSC', 'L', JCHNSC)
      CALL JEVEUO(CHNOS//'.CNSV', 'E', JCHNSV)
      CALL JEVEUO(CHNOS//'.CNSL', 'E', JCHNSL)

C     ! ======================================== !
C     ! RECUPERATION DES POINTEURS DE PROJECTION !
C     ! ======================================== !
C      CALL JEVEUO(CORRES//'.PJEF_NO','L',JACONO)
      CALL JEVEUO(CORRES//'.PJXX_K1','L',JACONO)
      CALL JEVEUO(CORRES//'.PJEF_NB','L',JACONB)
      CALL JEVEUO(CORRES//'.PJEF_NU','L',JACONU)
      CALL JEVEUO(CORRES//'.PJEF_CF','L',JACOCF)
C     ! ================= !
C     ! NOM DES MAILLAGES !
C     ! ================= !
      MA1 = ZK24(JACONO-1+1)(1:8)
      MA2 = ZK24(JACONO-1+2)(1:8)

C     ! ======================== !
C     ! VERIFICATION ELEMENTAIRE !
C     ! ======================== !
      IF (MA.NE.MA1) CALL U2MESS('F','COUPLAGEIFS_6')

C     ! ======================================= !
C     ! RECUPERATIONS DES DONNEES DU MAILLAGE 1 !
C     ! ======================================= !
      DO 10 ICMP = 1, 3
        DDLFOR(ICMP) = 0
 10   CONTINUE
      DO 20 ICMP = 1, 3
        IF (NOMCMP(ICMP).EQ.'FX') DDLFOR(1) = 1
        IF (NOMCMP(ICMP).EQ.'FY') DDLFOR(2) = 1
        IF (NOMCMP(ICMP).EQ.'FZ') DDLFOR(3) = 1
 20   CONTINUE

C     ! ================================================= !
C     ! RECUPERATIONS DES NOMBRES DE NOEUDS DES MAILLAGES !
C     ! ================================================= !
      CALL DISMOI('F','NB_NO_MAILLA',MA1,'MAILLAGE',NBNO1,KBID,IBID)
      CALL DISMOI('F','NB_NO_MAILLA',MA2,'MAILLAGE',NBNO2,KBID,IBID)
      NBNO4 = NBNO2

C     ! ===================================== !
C     ! RECUPERATION DES FORCES DU MAILLAGE 2 !
C     ! ===================================== !
      CALL WKVECT('&&OP0112.FORCE2',     'V V R',3*NBNO2,JFORC2)
      NOMVAR = 'FORAST'
      TI = TF
      CALL CPLDB(ICOMPO,CPITER,TI,TF,NUMPA4,NOMVAR,
     &  3*NBNO4,TAILLE,ZR(JFORC2),IBID4)

C     ! ====================================== !
C     ! LISTE DES NOEUDS DU MAILLAGE 1 COUPLES !
C     ! ====================================== !
      CALL WKVECT('&&OP0112.FLAGN1', 'V V I', NBNO1, JFLAN1)
      DO 30 INO1 = 1, NBNO1
        ZI(JFLAN1-1+INO1) = 0
        DO 40 ICMP = 1, NBCMPG
          ZR(JCHNSV-1+NBCMPG*(INO1-1)+ICMP) = 0.D0
 40     CONTINUE
 30   CONTINUE
      GRPMA = MA1//'.GROUPEMA'
      DO 50 IOCC = 1, NBOCC
C        CALL GETVID('VIS_A_VIS','GROUP_MA_1',IOCC,IARG,1,NOMGMA,IBID)
        CALL GETVTX('VIS_A_VIS','GROUP_MA_1',IOCC,IARG,1,NOMGMA,IBID)
        CALL JELIRA(JEXNOM(GRPMA,NOMGMA),'LONMAX',NBMAG1,KBID)
        CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'L',JALIM1)
        DO 60 II = 1, NBMAG1
          IMA = ZI(JALIM1-1+II)
          CALL JELIRA(JEXNUM(MA1//'.CONNEX', IMA),'LONMAX',NBNOG1,KBID)
          CALL JEVEUO(JEXNUM(MA1//'.CONNEX', IMA),'L',JCXMA1)
          DO 70 JJ = 1, NBNOG1
            INO1 = ZI(JCXMA1-1+JJ)
            ZI(JFLAN1-1+INO1) = 1
 70       CONTINUE
 60     CONTINUE
 50   CONTINUE

C     ! =============================================== !
C     ! PROJECTIONS DES FORCES ENTRE LES DEUX MAILLAGES !
C     ! =============================================== !
      IDECAL = 0
      ILENGT = 0
      GRPNO = MA2//'.GROUPENO'
      DO 80 IOCC = 1,NBOCC
C        CALL GETVID('VIS_A_VIS','GROUP_NO_2',IOCC,IARG,1,NOMGNO,IBID)
        CALL GETVTX('VIS_A_VIS','GROUP_NO_2',IOCC,IARG,1,NOMGNO,IBID)
        CALL JELIRA(JEXNOM(GRPNO,NOMGNO),'LONMAX',NBNO2,KBID)
        CALL JEVEUO(JEXNOM(GRPNO,NOMGNO),'L',IALIN2)
        DO 90 JJ = 1, NBNO2
          INO2 = ZI(IALIN2-1+JJ)
          DO 100 II = 1, ZI(JACONB-1+ILENGT+JJ)
            INO1 = ZI(JACONU-1+IDECAL+II)
            DO 110 ICMP = 1, 3
              IF (DDLFOR(ICMP).EQ.1) THEN
                ICMPG = NBCMPG*(INO1-1)+ICMP
                ZR(JCHNSV-1+ICMPG) = ZR(JCHNSV-1+ICMPG)
     &                             + ZR(JFORC2-1+3*(INO2-1)+ICMP)
     &                             * ZR(JACOCF-1+IDECAL+II)
              ENDIF
 110        CONTINUE
 100      CONTINUE
          IDECAL = IDECAL + ZI(JACONB-1+ILENGT+JJ)
 90     CONTINUE
        ILENGT = ILENGT + NBNO2
 80   CONTINUE


C     ! ===================================== !
C     ! TRANSFORMATION DU CHAMP_NO_S EN CARTE !
C     ! ===================================== !
      CARTE = CHARG//'.CHME.FORNO'
      LIEL  = CHARG//'.CHME.LIGRE.LIEL'
      CALL JELIRA(LIEL, 'NUTIOC', NBVAL, KBID)
      IF (NBVAL.NE.1) CALL U2MESS('F','COUPLAGEIFS_7')
      CALL DETRSD('CARTE',CARTE)
      CALL ALCART('G', CARTE, MA, 'FORC_R')
      CALL JEVEUO(CARTE//'.NCMP', 'E', JNCMP)
      CALL JEVEUO(CARTE//'.VALV', 'E', JVALV)
      CALL JEVEUO(JEXNUM(LIEL,1), 'L', JLIGR)
      DO 120 ICMP = 1, NBCMPG
        ZK8(JNCMP-1+ICMP) = NCMPGD(ICMP)
 120  CONTINUE
      IDECAL = 0
      DO 130 INO1 = 1,NBNO1
        IF (ZI(JFLAN1-1+INO1).EQ.1) THEN
          IDECAL = IDECAL + 1
          DO 140 ICMP = 1, 3
            ZR(JVALV-1+ICMP) = ZR(JCHNSV-1+NBCMPG*(INO1-1)+ICMP)
 140      CONTINUE
          DO 150 ICMP = 4, NBCMPG
            ZR(JVALV-1+ICMP) = 0.D0
 150      CONTINUE
          II = ZI(JLIGR-1+IDECAL)
          CALL NOCART(CARTE, -3, ' ', 'NUM', 1, ' ', II, LIEL, NBCMPG)
        ENDIF
 130  CONTINUE

C     ! ======================== !
C     ! LIBERATION DE LA MEMOIRE !
C     ! ======================== !
      CALL JEDETR(CHNOS)
      CALL JEDETR('&&OP0112.NOGRMA')
      CALL JEDETR('&&OP0112.FORCE2')
      CALL JEDETR('&&OP0112.FLAGN1')
      CALL JEDEMA()

C=======================================================================
      END
