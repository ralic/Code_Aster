      SUBROUTINE OP0111 ()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ECHANGE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C =====================================================================
C
C     COMMANDE:  ENV_CINE_YACS
C  ENVOI DES CHAMPS CINEMATIQUES A SATURNE VIA YACS
C
C =====================================================================
      IMPLICIT   NONE
C     ------------------------------------------------------------------
      INCLUDE 'jeveux.h'
C
C 0.3. ==> VARIABLES LOCALES
C
C
C     ------------------------------------------------------------------
      INTEGER          IBID, ICODE, ICMP, INO1, INO2, II, IDECAL
      INTEGER          IRESU, IETIN, IDEPL, IVITE, IACCE, ICMPG, IOCC
      INTEGER          JDEPSK, JDEPSD, JDEPSC, JDEPSV, JDEPSL, JDEPL
      INTEGER          JVITSK, JVITSD, JVITSC, JVITSV, JVITSL, JVITE
      INTEGER          JACCSK, JACCSD, JACCSC, JACCSV, JACCSL, JACCE
      INTEGER          JACONO, JACONB, JACONU, JACOCF, IALIN2
      INTEGER          JJ, ILENGT
      INTEGER          NORDRE,  NBNO1, NBCMP, NBNO2, NBOCC
      INTEGER          DDLDEP(3), DDLVIT(3), DDLACC(3)
      INTEGER          IFM, NIV
      CHARACTER*1      KBID
      CHARACTER*8      MA, MA1, MA2
      CHARACTER*16     CORRES, NOMGNO, VALK(2)
      CHARACTER*19     RESU
      CHARACTER*19     CHDEPL, CHDEPS, CHVITE, CHVITS, CHACCE, CHACCS
      CHARACTER*24     GRPNO
C     COUPLAGE =>
      INTEGER*4          LENVAR,CPITER,NUMPA4,IBID4,IDIM
      PARAMETER (LENVAR = 144)
      CHARACTER*(LENVAR) NOMVAR
      PARAMETER (CPITER= 41)
      INTEGER            ICOMPO,NUMPAS,IADR
      REAL*8             TF,DT
      CHARACTER*24       AYACS   
      INTEGER      IARG
C     COUPLAGE <=
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------

C     ! ========================== !
C     ! RECUPERATION DES MOTS-CLES !
C     ! ========================== !
      CALL GETVID(' ', 'MATR_PROJECTION',   1,IARG, 1, CORRES, IBID)
      CALL GETVR8(' ','INST',0,IARG,1,TF,IBID)
      CALL GETVR8(' ','PAS',0,IARG,1,DT,IBID)
      CALL GETVIS(' ','NUME_ORDRE_YACS',0,IARG,1,NUMPAS,IBID)
      NUMPA4 = NUMPAS
      CALL GETFAC('VIS_A_VIS',NBOCC)
      CALL GETFAC('RESULTAT', IRESU)
      CALL GETFAC('ETAT_INIT',IETIN)
      IF (IRESU+IETIN.GT.1) CALL U2MESS('F','COUPLAGEIFS_1')
      IF (NBOCC.LT.1)       CALL U2MESS('F','COUPLAGEIFS_2')
      IDEPL = 0
      IVITE = 0
      IACCE = 0
      IF (IRESU.EQ.1) THEN
        CALL GETVID('RESULTAT', 'RESU',       1,IARG, 1, RESU,   IBID)
        CALL GETVIS('RESULTAT', 'NUME_ORDRE', 1,IARG, 1, NORDRE, IBID)
        IF (NIV.EQ.2) THEN
          VALK(1) = 'OP0111'
          VALK(2) = 'NUME_ORDRE'      
          CALL U2MESG('I+','COUPLAGEIFS_8',2,VALK,1,NORDRE,0,0.D0)
        ENDIF
        CALL RSEXCH(RESU, 'DEPL', NORDRE, CHDEPL, ICODE)
        CALL RSEXCH(RESU, 'VITE', NORDRE, CHVITE, ICODE)
        CALL RSEXCH(RESU, 'ACCE', NORDRE, CHACCE, ICODE)
        IDEPL = 1
        IVITE = 1
        IACCE = 1
      ELSE IF(IETIN.EQ.1) THEN
        CALL GETVID('ETAT_INIT', 'DEPL', 1,IARG, 1, CHDEPL, IDEPL)
        CALL GETVID('ETAT_INIT', 'VITE', 1,IARG, 1, CHVITE, IVITE)
        CALL GETVID('ETAT_INIT', 'ACCE', 1,IARG, 1, CHACCE, IACCE)
        IF (IDEPL.GT.1.OR.IVITE.GT.1.OR.IACCE.GT.1) THEN
          CALL U2MESS('F','COUPLAGEIFS_3')
        ENDIF
      ENDIF
C     ! ================================================ !
C     ! TRANSFORMATION DES CHAMPS EN CHAMPS NOEUD SIMPLE !
C     ! ================================================ !
      NBNO1 = 0
      NBCMP = 0
      IF (IDEPL.EQ.1) THEN
        CHDEPS = '&&IRGMCN.DEPL'
        CALL CNOCNS(CHDEPL, 'V', CHDEPS)
        CALL JEVEUO(CHDEPS//'.CNSK', 'L', JDEPSK)
        CALL JEVEUO(CHDEPS//'.CNSD', 'L', JDEPSD)
        CALL JEVEUO(CHDEPS//'.CNSC', 'L', JDEPSC)
        CALL JEVEUO(CHDEPS//'.CNSV', 'L', JDEPSV)
        CALL JEVEUO(CHDEPS//'.CNSL', 'L', JDEPSL)
        MA    = ZK8(JDEPSK)
        NBNO1 = ZI(JDEPSD-1+1)
        NBCMP = ZI(JDEPSD-1+2)
      ENDIF
      IF (IVITE.EQ.1) THEN
        CHVITS = '&&IRGMCN.VITE'
        CALL CNOCNS(CHVITE, 'V', CHVITS)
        CALL JEVEUO(CHVITS//'.CNSK', 'L', JVITSK)
        CALL JEVEUO(CHVITS//'.CNSD', 'L', JVITSD)
        CALL JEVEUO(CHVITS//'.CNSC', 'L', JVITSC)
        CALL JEVEUO(CHVITS//'.CNSV', 'L', JVITSV)
        CALL JEVEUO(CHVITS//'.CNSL', 'L', JVITSL)
        MA    = ZK8(JVITSK)
        NBNO1 = ZI(JVITSD-1+1)
        NBCMP = ZI(JVITSD-1+2)
      ENDIF
      IF (IACCE.EQ.1) THEN
        CHACCS = '&&IRGMCN.ACCE'
        CALL CNOCNS(CHACCE, 'V', CHACCS)
        CALL JEVEUO(CHACCS//'.CNSK', 'L', JACCSK)
        CALL JEVEUO(CHACCS//'.CNSD', 'L', JACCSD)
        CALL JEVEUO(CHACCS//'.CNSC', 'L', JACCSC)
        CALL JEVEUO(CHACCS//'.CNSV', 'L', JACCSV)
        CALL JEVEUO(CHACCS//'.CNSL', 'L', JACCSL)
        MA    = ZK8(JACCSK)
        NBNO1 = ZI(JACCSD-1+1)
        NBCMP = ZI(JACCSD-1+2)
      ENDIF
C     ! ========================================== !
C     ! APPEL DES POINTEURS DE CORRESP_2_MAILLAGES !
C     ! ========================================== !
C      CALL JEVEUO(CORRES//'.PJEF_NO', 'L', JACONO)
      CALL JEVEUO(CORRES//'.PJXX_K1', 'L', JACONO)
      CALL JEVEUO(CORRES//'.PJEF_NB', 'L', JACONB)
      CALL JEVEUO(CORRES//'.PJEF_NU', 'L', JACONU)
      CALL JEVEUO(CORRES//'.PJEF_CF', 'L', JACOCF)
C     ! ===================================================== !
C     ! VERIFICATIONS ELEMENTAIRES SUR LES NOMS DES MAILLAGES !
C     ! ===================================================== !
      MA1 = ZK24(JACONO-1+1)(1:8)
      MA2 = ZK24(JACONO-1+2)(1:8)
      IF (NBNO1.GT.0) THEN
        IF (MA.NE.MA1) CALL U2MESS('F','COUPLAGEIFS_4')
      ENDIF
C     ! ======================================= !
C     ! RECUPERATIONS DES DONNEES DU MAILLAGE 1 !
C     ! ======================================= !
      DO 10 ICMP = 1, 3
        DDLDEP(ICMP) = 0
        DDLVIT(ICMP) = 0
        DDLACC(ICMP) = 0
 10   CONTINUE
      DO 20 ICMP = 1, NBCMP
        IF (IDEPL.EQ.1) THEN
          IF (ZK8(JDEPSC-1+ICMP).EQ.'DX') DDLDEP(1) = ICMP
          IF (ZK8(JDEPSC-1+ICMP).EQ.'DY') DDLDEP(2) = ICMP
          IF (ZK8(JDEPSC-1+ICMP).EQ.'DZ') DDLDEP(3) = ICMP
        ENDIF
        IF (IVITE.EQ.1) THEN
          IF (ZK8(JVITSC-1+ICMP).EQ.'DX') DDLVIT(1) = ICMP
          IF (ZK8(JVITSC-1+ICMP).EQ.'DY') DDLVIT(2) = ICMP
          IF (ZK8(JVITSC-1+ICMP).EQ.'DZ') DDLVIT(3) = ICMP
        ENDIF
        IF (IACCE.EQ.1) THEN
          IF (ZK8(JACCSC-1+ICMP).EQ.'DX') DDLACC(1) = ICMP
          IF (ZK8(JACCSC-1+ICMP).EQ.'DY') DDLACC(2) = ICMP
          IF (ZK8(JACCSC-1+ICMP).EQ.'DZ') DDLACC(3) = ICMP
        ENDIF
 20   CONTINUE
C     ! ======================================= !
C     ! RECUPERATIONS DES DONNEES DU MAILLAGE 2 !
C     ! ======================================= !
      CALL DISMOI('F','NB_NO_MAILLA',MA2,'MAILLAGE',NBNO2,KBID,IBID)
C     ! ===================================================== !
C     ! PROJECTIONS DES DEPLACEMENTS ENTRE LES DEUX MAILLAGES !
C     ! ===================================================== !
      CALL WKVECT('&&OP0111.DEPL','V V R', 3*NBNO2, JDEPL)
      CALL WKVECT('&&OP0111.VITE','V V R', 3*NBNO2, JVITE)
      CALL WKVECT('&&OP0111.ACCE','V V R', 3*NBNO2, JACCE)
      DO 30 INO2 = 1, NBNO2
        DO 40 ICMP = 1, 3
          ZR(JDEPL-1+3*(INO2-1)+ICMP) = 0.D0
          ZR(JVITE-1+3*(INO2-1)+ICMP) = 0.D0
          ZR(JACCE-1+3*(INO2-1)+ICMP) = 0.D0
 40     CONTINUE
 30   CONTINUE
C     Condition if pour le cas ETAT_INIT == None
      IF (NBNO1.GT.0) THEN
        IDECAL = 0
        ILENGT = 0
        GRPNO  = MA2//'.GROUPENO'
        DO 50 IOCC = 1, NBOCC
          CALL GETVTX('VIS_A_VIS','GROUP_NO_2',IOCC,IARG,1,NOMGNO,IBID)
          CALL JELIRA(JEXNOM(GRPNO,NOMGNO),'LONMAX',NBNO2,KBID)
          CALL JEVEUO(JEXNOM(GRPNO,NOMGNO),'L',IALIN2)
          DO 60 JJ = 1, NBNO2
            INO2 = ZI(IALIN2-1+JJ)
            DO 70  II = 1, ZI(JACONB-1+ILENGT+JJ)
              INO1 = ZI(JACONU-1+IDECAL+II)
              DO 80 ICMP = 1, 3
                IF (DDLDEP(ICMP).NE.0) THEN
                  ICMPG = DDLDEP(ICMP)+NBCMP*(INO1-1)
                  IF (ZL(JDEPSL-1+ICMPG)) THEN
                    ZR(JDEPL-1+3*(INO2-1)+ICMP) = 
     &                       + ZR(JDEPL-1+3*(INO2-1)+ICMP)
     &                       + ZR(JDEPSV-1+ICMPG)*ZR(JACOCF-1+IDECAL+II)
                  ENDIF
                ENDIF
                IF (DDLVIT(ICMP).NE.0) THEN
                  ICMPG = DDLVIT(ICMP)+NBCMP*(INO1-1)
                  IF (ZL(JVITSL-1+ICMPG)) THEN
                    ZR(JVITE-1+3*(INO2-1)+ICMP) = 
     &                       + ZR(JVITE-1+3*(INO2-1)+ICMP)
     &                       + ZR(JVITSV-1+ICMPG)*ZR(JACOCF-1+IDECAL+II)
                  ENDIF
                ENDIF
                IF (DDLACC(ICMP).NE.0) THEN
                  ICMPG = DDLACC(ICMP)+NBCMP*(INO1-1)
                  IF (ZL(JACCSL-1+ICMPG)) THEN
                    ZR(JACCE-1+3*(INO2-1)+ICMP) = 
     &                       + ZR(JACCE-1+3*(INO2-1)+ICMP)
     &                       + ZR(JACCSV-1+ICMPG)*ZR(JACOCF-1+IDECAL+II)
                  ENDIF
                ENDIF
 80           CONTINUE
 70         CONTINUE
            IDECAL = IDECAL + ZI(JACONB-1+ILENGT+JJ)
 60       CONTINUE
          ILENGT = ILENGT + NBNO2
 50     CONTINUE
      ENDIF
C     ! ================================ !
C     ! ENVOI DES GRANDEURS CINEMATIQUES !
C     ! ================================ !
      IDIM = 3 * NBNO2
      NOMVAR = 'DEPAST'
      CALL CPEDB(ICOMPO,CPITER,
     & TF,NUMPA4,NOMVAR,IDIM,ZR(JDEPL),IBID4)
      NOMVAR = 'VITAST'
      CALL CPEDB(ICOMPO,CPITER,
     & TF,NUMPA4,NOMVAR,IDIM,ZR(JVITE),IBID4)
C  DEBUG
C      IF (TF .GT. 0.29) THEN
C        WRITE (6,*) 'OP0111 : DEPAST=',
C     &     ZR(JDEPL),ZR(JDEPL+IDIM-25),ZR(JDEPL+IDIM-4)
C        WRITE (6,*) 'OP0111 : VITAST=',
C     &     ZR(JVITE),ZR(JVITE+IDIM-25),ZR(JVITE+IDIM-4)
C      ENDIF
C  FIN DEBUG
C
C     ! ======================== !
C     ! LIBERATION DE LA MEMOIRE !
C     ! ======================== !
      CALL JEDETR('&&OP0111.DEPL')
      CALL JEDETR('&&OP0111.VITE')
      CALL JEDETR('&&OP0111.ACCE')
      CALL JEDEMA()
C     ------------------------------------------------------------------
      END
