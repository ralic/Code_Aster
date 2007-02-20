      SUBROUTINE RCSP01 ( NBM, ADRM, IPT, SP3, SP4, SP5, ALPHAA, ALPHAB,
     &                    NBTH, IOCS, SP6 )
      IMPLICIT   NONE
      INTEGER             NBM, ADRM(*), IPT, NBTH, IOCS
      REAL*8              SP3, SP4, SP5, ALPHAA, ALPHAB, SP6
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     CALCUL DU SP
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      INTEGER VALI(3)
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      REAL*8 VALR(2)
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      JCHTH, IAD, ICMP, NBCMP, DECAL, JCESD, JCESV,
     &             JCESL, NBINST, JINST, I, IBID, IRET, JABSC, NBABSC
      REAL*8       INST, PREC(2), TINT, TEXT, TMOY(2), VMOY, TA, TB,
     &             TAB, DT1, DT2, TERM1, TERM2, DT1MAX, DT2MAX, TABMAX,
     &             VALE(2)
      COMPLEX*16   CBID
      LOGICAL      EXIST
      CHARACTER*8  K8B, TBTHER(2), TBMOYE(2), CRIT(2)
      CHARACTER*16 NOPARA(2)
      CHARACTER*24 NOMOBJ, ABSCUR, CHTEMP
      CHARACTER*24 VALK(2)
C
C DEB ------------------------------------------------------------------
C
      SP6 = 0.D0
      IF ( NBTH .EQ. 0 ) GOTO 9999
C
      NOPARA(1) = 'INST'
      NOPARA(2) = 'QUANTITE'
      PREC(1) = 1.0D-06
      PREC(2) = 1.0D-06
      CRIT(1) = 'RELATIF'
      CRIT(2) = 'RELATIF'
C
      CALL JEVEUO ( '&&RC3600.CHAM_THER', 'L', JCHTH )
C
      CHTEMP = ZK24(JCHTH+IOCS-1)
C
      CALL JEVEUO ( CHTEMP(1:19)//'.CESD', 'L', JCESD )
      CALL JEVEUO ( CHTEMP(1:19)//'.CESV', 'L', JCESV )
      CALL JEVEUO ( CHTEMP(1:19)//'.CESL', 'L', JCESL )
C
      NBCMP = ZI(JCESD-1+2)
      DECAL = ZI(JCESD-1+5+4*(ADRM(1)-1)+4)
C
      ICMP = 1
      IAD = DECAL + (IPT-1)*NBCMP + ICMP
      IF (.NOT.ZL(JCESL-1+IAD)) THEN
            VALI (1) = IOCS
            VALI (2) = ADRM(1)
            VALI (3) = 0
         CALL U2MESG('F', 'POSTRELE1_41',0,' ',3,VALI,0,0.D0)
      ENDIF
      TBTHER(1) = ZK8(JCESV-1+IAD)
      ICMP = 2
      IAD = DECAL + (IPT-1)*NBCMP + ICMP
      IF (.NOT.ZL(JCESL-1+IAD)) THEN
            VALI (1) = IOCS
            VALI (2) = ADRM(1)
            VALI (3) = 0
         CALL U2MESG('F', 'POSTRELE1_42',0,' ',3,VALI,0,0.D0)
      ENDIF
      TBMOYE(1) = ZK8(JCESV-1+IAD)
C
      IF ( NBM .GT. 1 ) THEN
         DECAL = ZI(JCESD-1+5+4*(ADRM(2)-1)+4)
         ICMP = 1
         IAD = DECAL + (IPT-1)*NBCMP + ICMP
         IF (.NOT.ZL(JCESL-1+IAD)) THEN
            VALI (1) = IOCS
            VALI (2) = ADRM(2)
            VALI (3) = 0
            CALL U2MESG('F', 'POSTRELE1_43',0,' ',3,VALI,0,0.D0)
         ENDIF
         TBTHER(2) = ZK8(JCESV-1+IAD)
         ICMP = 2
         IAD = DECAL + (IPT-1)*NBCMP + ICMP
         IF (.NOT.ZL(JCESL-1+IAD)) THEN
            VALI (1) = IOCS
            VALI (2) = ADRM(2)
            VALI (3) = 0
            CALL U2MESG('F', 'POSTRELE1_44',0,' ',3,VALI,0,0.D0)
         ENDIF
         TBMOYE(2) = ZK8(JCESV-1+IAD)
      ENDIF
C
C --- ON RECUPERE LES INSTANTS DANS UNE TABLE
C
      CALL TBEXIP ( TBTHER(1), 'INST', EXIST, K8B )
      IF ( .NOT. EXIST ) THEN
         CALL U2MESS('F','POSTRELE_44')
      ENDIF
      CALL TBEXIP ( TBMOYE(1), 'INST', EXIST, K8B )
      IF ( .NOT. EXIST ) THEN
         CALL U2MESS('F','POSTRELE_43')
      ENDIF
      NOMOBJ = '&&RCSP01.INSTANT'
      CALL TBEXV1 ( TBMOYE(1), 'INST', NOMOBJ, 'V', NBINST, K8B )
      CALL JEVEUO ( NOMOBJ, 'L', JINST )
C
C --- ON RECUPERE L'ABSC_CURV DANS LA TABLE TABL_RESU_THER
C
      CALL TBEXIP ( TBTHER(1), 'ABSC_CURV', EXIST, K8B )
      IF ( .NOT. EXIST ) THEN
         CALL U2MESS('F','POSTRELE_45')
      ENDIF
      ABSCUR = '&&RCSP01.ABSC_CURV'
      CALL TBEXV1 ( TBTHER(1), 'ABSC_CURV', ABSCUR, 'V', NBABSC, K8B)
      CALL JEVEUO ( ABSCUR, 'L', JABSC )
C
C --- ON BOUCLE SUR LES INSTANTS :
C
      DT1MAX = 0.D0
      DT2MAX = 0.D0
      TABMAX = 0.D0
C
      DO 10 I = 1 , NBINST
C
         INST = ZR(JINST+I-1)
C
C ------ ON RECUPERE TEMP_INT, TEMP_EXT
C
         NOPARA(1) = 'INST'
         NOPARA(2) = 'ABSC_CURV'
         VALE(1) = INST
         VALE(2) = ZR(JABSC)
C
         CALL TBLIVA ( TBTHER(1), 2, NOPARA, IBID, VALE, CBID, K8B,
     &                 CRIT, PREC, 'TEMP',
     &                 K8B, IBID, TINT, CBID, K8B, IRET )
         IF (IRET.NE.0) THEN
            VALK (1) = TBTHER(1)
            VALR (1) = INST
            VALR (2) = VALE(2)
            CALL U2MESG('F', 'POSTRELE1_45',1,VALK,0,0,2,VALR)
         ENDIF
C
         VALE(2) = ZR(JABSC+NBABSC-1)
C
         CALL TBLIVA ( TBTHER(1), 2, NOPARA, IBID, VALE, CBID, K8B,
     &                 CRIT, PREC, 'TEMP',
     &                 K8B, IBID, TEXT, CBID, K8B, IRET )
         IF (IRET.NE.0) THEN
            VALK (1) = TBTHER(1)
            VALR (1) = INST
            VALR (2) = VALE(2)
            CALL U2MESG('F', 'POSTRELE1_45',1,VALK,0,0,2,VALR)
         ENDIF
C
C ------ ON RECUPERE LES MOYENNES
C
         NOPARA(1) = 'INST'
         NOPARA(2) = 'QUANTITE'
C
         CALL TBLIVA ( TBMOYE(1), 2, NOPARA, IBID, INST, CBID,
     &              'MOMENT_0', CRIT, PREC, 'TEMP',
     &              K8B, IBID, TMOY(1), CBID, K8B, IRET )
         IF (IRET.NE.0) THEN
            VALK (1) = TBMOYE(1)
            VALK (2) = 'MOMENT_0'
            VALR (1) = INST
            CALL U2MESG('F', 'POSTRELE1_47',2,VALK,0,0,1,VALR)
         ENDIF
         IF ( NBM .GT. 1 ) THEN
            CALL TBLIVA ( TBMOYE(2), 2, NOPARA, IBID, INST, CBID,
     &                 'MOMENT_0', CRIT, PREC, 'TEMP',
     &                 K8B, IBID, TMOY(2), CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
            VALK (1) = TBMOYE(2)
            VALK (2) = 'MOMENT_0'
            VALR (1) = INST
            CALL U2MESG('F', 'POSTRELE1_48',2,VALK,0,0,1,VALR)
            ENDIF
         ENDIF
         CALL TBLIVA ( TBMOYE(1), 2, NOPARA, IBID, INST, CBID,
     &              'MOMENT_1', CRIT, PREC, 'TEMP',
     &              K8B, IBID, VMOY, CBID, K8B, IRET )
         IF (IRET.NE.0) THEN
            VALK (1) = TBMOYE(1)
            VALK (2) = 'MOMENT_1'
            VALR (1) = INST
            VALI (1) = IOCS
            CALL U2MESG('F', 'POSTRELE1_49',2,VALK,1,VALI,1,VALR)
         ENDIF
C
C ------ DT1: AMPLITUDE DE LA VARIATION ENTRE LES 2 ETATS STABILISES
C             DE LA DIFFERENCE DE TEMPERATURE ENTRE LES PAROIS
C             INTERNE ET EXTERNE
C
         DT1 = VMOY
C
C ------ DT2: PARTIE NON LINEAIRE DE LA DISTRIBUTION DANS L'EPAISSEUR
C             DE PAROI DE L'AMPLITUDE DE VARIATION DE LA TEMPERATURE
C             ENTRE LES 2 ETATS STABILISES
C
         TERM1 = ABS(TEXT-TMOY(1)) - ABS(0.5D0*DT1)
         TERM2 = ABS(TINT-TMOY(1)) - ABS(0.5D0*DT1)
         DT2 = MAX( TERM1, TERM2, 0.D0 )
C
C ------ TA : AMPLITUDE DE VARIATION ENTRE LES 2 ETATS STABILISES
C             DES TEMPERATURES MOYENNES A GAUCHE D'UNE DISCONTINUITE
C
         TA = TMOY(1)
C
C ------ TB : AMPLITUDE DE VARIATION ENTRE LES 2 ETATS STABILISES
C             DES TEMPERATURES MOYENNES A DROITE D'UNE DISCONTINUITE
C
         IF ( NBM .GT. 1 ) THEN
            TB = TMOY(2)
         ELSE
            TB = TMOY(1)
         ENDIF
C
         DT1MAX = MAX ( DT1MAX, ABS( DT1 ) )
C
         DT2MAX = MAX ( DT2MAX, ABS( DT2 ) )
C
         IF ( NBM .GT. 1 ) THEN
            TAB =  ( ALPHAA * TA )  - ( ALPHAB * TB )
            TABMAX = MAX ( TABMAX, ABS( TAB ) )
         ENDIF
C
 10   CONTINUE
C
      SP6 = SP6 + ( SP3 * DT1MAX )
      SP6 = SP6 + ( SP5 * DT2MAX )
      IF ( NBM .GT. 1 ) SP6 = SP6 + ( SP4 * TABMAX )
C
      CALL JEDETR ( NOMOBJ )
      CALL JEDETR ( ABSCUR )
C
C
 9999 CONTINUE
C
      END
