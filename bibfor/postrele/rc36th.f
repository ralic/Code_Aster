      SUBROUTINE RC36TH ( NOMA, NBMA, LISTMA ,CHTH, IOCS, NBTHS, LITHS)
      IMPLICIT   NONE        
      INTEGER             NBMA, LISTMA(*), IOCS, NBTHS, LITHS(*)
      CHARACTER*8         NOMA
      CHARACTER*24        CHTH(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/10/2008   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C     RECUPERATION DES DONNEES DE "RESU_THER"
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NBRETH, NBCMP, ITHS, ITH, IRET, N1, N2, IOCC, IERD,
     &             INO, IAD, IN, IMA, IM, JMAIL, JNOEU, NBMAIL, NBMAT,
     &             NBTOU, NBNOEU, JCESD, JCESL, JCESV, NBPT, DECAL, I,
     &             IPT, JCONX1, JCONX2, ICMP, IT1, VALI(4), JINST, 
     &             JTHER, JMOYE, JABSC, NBABSC, NBINST, IBID
      PARAMETER   ( NBCMP = 2 )
      REAL*8       INST, EPSI, VMOY, TA, TINT, TEXT, VALE(2), PREC(2)
      COMPLEX*16   CBID
      LOGICAL      EXIST
      CHARACTER*8  K8B, NOMGD, LICMP(NBCMP), TBTHER, TBMOYE, KIOC,
     &             CRIT(2)
      CHARACTER*16 MOTCLF, MOTCLS(2), TYPMCS(2), MOTCLN(2), TYPMCN(2)
      CHARACTER*16 NOPARA(2)
      CHARACTER*19 CHAMS0
      CHARACTER*24 INSTAN,ABSCUR, MESMAI,MESNOE, NOJVTH,NOJVMY, VALK(7) 
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'RESU_THER'
      CALL GETFAC ( MOTCLF, NBRETH )
C
      EPSI    = 1.0D-06
      PREC(1) = 1.0D-06
      PREC(2) = 1.0D-06
      CRIT(1) = 'RELATIF'
      CRIT(2) = 'RELATIF'
      INSTAN  = '&&RC36TH.INSTANT'
      ABSCUR  = '&&RC36TH.ABSC_CURV'
C
      NOMGD = 'RCCM_K'
      LICMP(1) = 'TB_TEMP'
      LICMP(2) = 'TB_MOYE'
C
      MESMAI = 'RC36TH.MES_MAILLES'
      MOTCLS(1) = 'GROUP_MA'
      MOTCLS(2) = 'MAILLE'
      TYPMCS(1) = 'GROUP_MA'
      TYPMCS(2) = 'MAILLE'
      MESNOE = 'RC36TH.MES_NOEUDS'
      MOTCLN(1) = 'GROUP_NO'
      MOTCLN(2) = 'NOEUD'
      TYPMCN(1) = 'GROUP_NO'
      TYPMCN(2) = 'NOEUD'
C
C --- POUR CHAQUE SITUATION, UN CHAMELEM ELNO EST CREE
C
C     PAS DE SURCHARGE AUTORISEE POUR REMPLIR CE CHAMP
C
      CALL CODENT ( IOCS , 'D0', K8B)
      CHAMS0 = 'RC36TH.CHAM'//K8B
      CALL JEEXIN ( CHAMS0, IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL CESCRE ( 'V', CHAMS0, 'ELNO', NOMA, NOMGD,NBCMP,LICMP,
     &         -1, -1, -NBCMP )
         CHTH(IOCS) = CHAMS0
      ELSE
         CALL U2MESS('F','POSTRCCM_19')
      ENDIF
C
      CALL JEVEUO (CHAMS0//'.CESD', 'L', JCESD )
      CALL JEVEUO (CHAMS0//'.CESL', 'E', JCESL )
      CALL JEVEUO (CHAMS0//'.CESV', 'E', JCESV )
C
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMAT,K8B,IERD)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
C
      DO 10, IT1 = 1, NBTHS, 1
C
         DO 12, IOCC = 1, NBRETH, 1
C
            CALL GETVIS ( MOTCLF, 'NUME_RESU_THER', IOCC,1,1,ITH,N1)
C
            ITHS = LITHS(IT1)
            IF (ITH.EQ.ITHS) GOTO 14
12       CONTINUE
         VALI(1) = IOCC
         VALI(2) = ITHS
         CALL U2MESI('F', 'POSTRCCM_23',2,VALI)
14       CONTINUE

         CALL GETVID ( MOTCLF,'TABL_RESU_THER' ,IOCC,1,1,TBTHER,N1)
C        ON VERIFIE L'ORDRE DES NOEUDS DANS LA TABLE
         CALL RCVERI(TBTHER)
C
         CALL TBEXIP ( TBTHER, 'INST', EXIST, K8B )
         IF ( .NOT. EXIST ) THEN
            VALK(1) = TBMOYE
            VALK(2) = 'INST'
            CALL U2MESK('F', 'POSTRCCM_1',2,VALK)
         ENDIF
         CALL TBEXIP ( TBTHER, 'ABSC_CURV', EXIST, K8B )
         IF ( .NOT. EXIST ) THEN
            VALK(1) = TBTHER
            VALK(2) = 'ABSC_CURV'
            CALL U2MESG('F', 'POSTRCCM_1',2,VALK,0,0,0,0.D0)
         ENDIF
         CALL TBEXV1 ( TBTHER, 'INST', INSTAN, 'V', NBINST, K8B )
         CALL JEVEUO ( INSTAN, 'L', JINST )
         CALL TBEXV1 ( TBTHER, 'ABSC_CURV', ABSCUR, 'V', NBABSC, K8B)
         CALL JEVEUO ( ABSCUR, 'L', JABSC )
C
         CALL GETVID ( MOTCLF,'TABL_MOYE_THER' ,IOCC,1,1,TBMOYE,N1)
C
         CALL TBEXIP ( TBMOYE, 'INST', EXIST, K8B )
         IF ( .NOT. EXIST ) THEN
            VALK(1) = TBMOYE
            VALK(2) = 'INST'
            CALL U2MESK('F', 'POSTRCCM_1',2,VALK)
         ENDIF
C
         EXIST = .FALSE.
         CALL CODENT ( IOCC , 'D0', KIOC )
         NOJVMY = '&&RC36TH_MOYENE_'//KIOC
         CALL JEEXIN ( NOJVMY , IRET )
         IF ( IRET .EQ. 0 ) THEN
            CALL WKVECT ( NOJVMY, 'V V R', 2*NBINST, JMOYE )
         ELSE
            EXIST = .TRUE.
         ENDIF
         NOJVTH = '&&RC36TH_TEMPER_'//KIOC
         IF ( IRET .EQ. 0 ) THEN
            CALL WKVECT ( NOJVTH, 'V V R', 2*NBINST, JTHER )
         ELSE
            EXIST = .TRUE.
         ENDIF
         IF ( EXIST ) GOTO 22
C
         DO 20 I = 1 , NBINST
            INST = ZR(JINST+I-1)
C
C --------- ON RECUPERE TEMP_INT, TEMP_EXT
C
            NOPARA(1) = 'INST'
            NOPARA(2) = 'ABSC_CURV'
            VALE(1) = INST
            VALE(2) = ZR(JABSC)
            CALL TBLIVA ( TBTHER, 2, NOPARA, IBID, VALE, CBID, K8B,
     &                    CRIT, PREC, 'TEMP',
     &                    K8B, IBID, TINT, CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               VALK (1) = TBTHER
               VALK (2) = 'TEMP'
               VALK (3) = NOPARA(1)
               VALK (4) = NOPARA(2)
               CALL U2MESG('F', 'POSTRCCM_2',4,VALK,0,0,2,VALE)
            ENDIF
            VALE(2) = ZR(JABSC+NBABSC-1)
            CALL TBLIVA ( TBTHER, 2, NOPARA, IBID, VALE, CBID, K8B,
     &                    CRIT, PREC, 'TEMP',
     &                    K8B, IBID, TEXT, CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               VALK (1) = TBTHER
               VALK (2) = 'TEMP'
               VALK (3) = NOPARA(1)
               VALK (4) = NOPARA(2)
               CALL U2MESG('F', 'POSTRCCM_2',4,VALK,0,0,2,VALE)
            ENDIF
            ZR(JTHER-1+2*(I-1)+1) = TINT
            ZR(JTHER-1+2*(I-1)+2) = TEXT
C
C --------- ON RECUPERE LES MOYENNES
C
            NOPARA(1) = 'INST'
            NOPARA(2) = 'QUANTITE'
            CALL TBLIVA ( TBMOYE, 2, NOPARA, IBID, INST, CBID,
     &                    'MOMENT_0', 'RELATIF', EPSI, 'TEMP', K8B,
     &                    IBID, TA, CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               VALK (1) = TBMOYE
               VALK (2) = 'TEMP'
               VALK (3) = NOPARA(1)
               VALK (4) = NOPARA(2)
               VALK (5) = 'MOMENT_0'
               CALL U2MESG('F', 'POSTRCCM_16',5,VALK,0,0,1,INST)
            ENDIF
            CALL TBLIVA ( TBMOYE, 2, NOPARA, IBID, INST, CBID,
     &                    'MOMENT_1', 'RELATIF', EPSI, 'TEMP', K8B,
     &                    IBID, VMOY, CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               VALK (1) = TBMOYE
               VALK (2) = 'TEMP'
               VALK (3) = NOPARA(1)
               VALK (4) = NOPARA(2)
               VALK (5) = 'MOMENT_1'
               CALL U2MESG('F', 'POSTRCCM_16',5,VALK,0,0,1,INST)
            ENDIF
            ZR(JMOYE-1+2*(I-1)+1) = TA
            ZR(JMOYE-1+2*(I-1)+2) = VMOY
 20      CONTINUE
 22      CONTINUE
C
         CALL GETVTX ( MOTCLF, 'TOUT', IOCC, 1, 1, K8B, NBTOU )

         IF ( NBTOU .NE. 0 ) THEN
            NBMAIL = NBMAT
            CALL WKVECT ( MESMAI, 'V V I' , NBMAIL, JMAIL )
            DO 30 IMA = 1 , NBMAIL
               ZI(JMAIL+IMA-1) = IMA
 30         CONTINUE
         ELSE
            CALL RELIEM ( ' ', NOMA, 'NU_MAILLE', MOTCLF, IOCC, 2,
     &                                 MOTCLS, TYPMCS, MESMAI, NBMAIL )
            CALL JEVEUO ( MESMAI, 'L', JMAIL )
         ENDIF
C
         CALL GETVID ( MOTCLF, 'GROUP_NO', IOCC,1,0, K8B, N1)
         CALL GETVID ( MOTCLF, 'NOEUD'   , IOCC,1,0, K8B, N2)
         IF ( N1+N2 .NE. 0 ) THEN
            CALL RELIEM ( ' ', NOMA, 'NU_NOEUD', MOTCLF, IOCC, 2,
     &                                MOTCLN, TYPMCN, MESNOE, NBNOEU )
            CALL JEVEUO ( MESNOE, 'L', JNOEU )
         ELSE
            NBNOEU = 0
         ENDIF

         IF ( NBNOEU .EQ. 0 ) THEN
            DO 100  IM = 1 , NBMAIL
               IMA = ZI(JMAIL+IM-1)
               NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
               DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
               DO 110,IPT = 1,NBPT
                  ICMP = 1
                  IAD = DECAL + (IPT-1)*NBCMP + ICMP
                  IF (.NOT.ZL(JCESL-1+IAD)) THEN
                     ZL  (JCESL-1+IAD) = .TRUE.
                     ZK24(JCESV-1+IAD) = NOJVTH
                  ELSE
                    VALI(1) = IOCS
                    VALI(2) = IMA
                    CALL U2MESI('F','POSTRCCM_24',2,VALI)
                  ENDIF
                  ICMP = 2
                  IAD = DECAL + (IPT-1)*NBCMP + ICMP
                  ZL  (JCESL-1+IAD) = .TRUE.
                  ZK24(JCESV-1+IAD) = NOJVMY
 110           CONTINUE
 100        CONTINUE
         ELSE
            DO 200  IM = 1 , NBMAIL
               IMA = ZI(JMAIL+IM-1)
               NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
               DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
               DO 210,IPT = 1,NBPT
                  INO = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+IPT-1)
                  DO 220,IN = 1,NBNOEU
                     IF ( ZI(JNOEU+IN-1) .EQ. INO ) THEN
                        ICMP = 1
                        IAD = DECAL + (IPT-1)*NBCMP + ICMP
                        IF (.NOT.ZL(JCESL-1+IAD)) THEN
                           ZL  (JCESL-1+IAD) = .TRUE.
                           ZK24(JCESV-1+IAD) = NOJVTH
                        ELSE
                           VALI(1) = IOCS
                           VALI(2) = IMA
                           VALI(3) = INO
                           CALL U2MESI('F','POSTRCCM_25',3,VALI)
                        ENDIF
                        ICMP = 2
                        IAD = DECAL + (IPT-1)*NBCMP + ICMP
                        ZL  (JCESL-1+IAD) = .TRUE.
                        ZK24(JCESV-1+IAD) = NOJVMY
                        GOTO 210
                     ENDIF
 220              CONTINUE
 210           CONTINUE
 200        CONTINUE
         ENDIF
C
         CALL JEDETR ( INSTAN )
         CALL JEDETR ( ABSCUR )
         CALL JEDETR ( MESMAI )
         CALL JEDETR ( MESNOE )
C
 10   CONTINUE
C
C --- VERIF QUE TOUTES LES MAILLES ANALYSEES SONT AFFECTEES
C
      DO 300 IM = 1 , NBMA
         IMA = LISTMA(IM)
         NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
         DECAL= ZI(JCESD-1+5+4*(IMA-1)+4)
         DO 310,IPT = 1,NBPT
            DO 320 ICMP=1,2
               IAD = DECAL + (IPT-1)*NBCMP + ICMP
               IF (.NOT.ZL(JCESL-1+IAD)) THEN
                  VALI(1) = IOCS
                  VALI(2) = IMA
                  CALL U2MESI('F', 'POSTRCCM_24',2,VALI)
               ENDIF
 320        CONTINUE
 310     CONTINUE
 300  CONTINUE
C
      CALL JEDEMA( )
      END
