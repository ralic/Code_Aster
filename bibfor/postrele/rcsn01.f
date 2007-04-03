      SUBROUTINE RCSN01 ( NBM, ADRM, IPT, SN3, SN4, ALPHAA, ALPHAB,
     &                    NBTH, IOCS,SN6 )
      IMPLICIT   NONE
      INTEGER             NBM, ADRM(*), IPT, NBTH, IOCS
      REAL*8              SN3, SN4, ALPHAA, ALPHAB, SN6
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/04/2007   AUTEUR VIVAN L.VIVAN 
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
C     CALCUL DU SN
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      INTEGER VALI(3)
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      REAL*8 VALR
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
     &             JCESL, NBINST, JINST, I, IBID, IRET, ITH
      REAL*8       INST, EPSI, TMOY(2), VMOY, TA, TB, TAB,
     &             DT1, DT1MAX, TABMAX
      COMPLEX*16   CBID
      LOGICAL      EXIST
      CHARACTER*8  K8B, TBMOYE(2)
      CHARACTER*16 NOPARA(2)
      CHARACTER*24 NOMOBJ, CHTEMP
      CHARACTER*24 VALK(7)
C
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      SN6 = 0.D0
      IF ( NBTH .EQ. 0 ) GOTO 9999
C
      NOPARA(1) = 'INST'
      NOPARA(2) = 'QUANTITE'
      EPSI = 1.0D-06
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
         ICMP = 2
         IAD = DECAL + (IPT-1)*NBCMP + ICMP
         IF (.NOT.ZL(JCESL-1+IAD)) THEN
            VALI (1) = IOCS
            VALI (2) = ADRM(1)
            CALL U2MESG('F','POSTRCCM_15',1,'RESU_THER',2,VALI,0,0.D0)
         ENDIF
         TBMOYE(1) = ZK8(JCESV-1+IAD)
C
         IF ( NBM .GT. 1 ) THEN
            DECAL = ZI(JCESD-1+5+4*(ADRM(2)-1)+4)
            ICMP = 2
            IAD = DECAL + (IPT-1)*NBCMP + ICMP
            IF (.NOT.ZL(JCESL-1+IAD)) THEN
               VALI (1) = IOCS
               VALI (2) = ADRM(2)
               CALL U2MESG('F','POSTRCCM_15',1,'RESU_THER_MOYE',
     +                                       2,VALI,0,0.D0)
            ENDIF
            TBMOYE(2) = ZK8(JCESV-1+IAD)
         ENDIF
C
C ------ ON RECUPERE LES INSTANTS DANS UNE TABLE
C
         CALL TBEXIP ( TBMOYE(1), 'INST', EXIST, K8B )
         IF ( .NOT. EXIST ) THEN
            VALK(1) = TBMOYE(1)
            VALK(2) = 'INST'
            CALL U2MESG('F', 'POSTRCCM_1',2,VALK,0,0,0,0.D0)
         ENDIF
         NOMOBJ = '&&RCSN01.INSTANT'
         CALL TBEXV1 ( TBMOYE(1), 'INST', NOMOBJ, 'V', NBINST, K8B )
         CALL JEVEUO ( NOMOBJ, 'L', JINST )
C
C ------ ON BOUCLE SUR LES INSTANTS :
C
         DT1MAX = 0.D0
         TABMAX = 0.D0
C
         DO 10 I = 1 , NBINST
C
            INST = ZR(JINST+I-1)
C
C --------- ON RECUPERE LES MOYENNES
C
            CALL TBLIVA ( TBMOYE(1), 2, NOPARA, IBID, INST, CBID,
     &                    'MOMENT_0', 'RELATIF', EPSI, 'TEMP', K8B,
     &                    IBID, TMOY(1), CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               VALK (1) = TBMOYE(1)
               VALK (2) = 'TEMP'
               VALK (3) = NOPARA(1)
               VALK (4) = NOPARA(2)
               VALK (5) = 'MOMENT_0'
               CALL U2MESG('F', 'POSTRCCM_16',5,VALK,0,0,1,INST)
            ENDIF
            IF ( NBM .GT. 1 ) THEN
               CALL TBLIVA ( TBMOYE(2), 2, NOPARA, IBID, INST, CBID,
     &                    'MOMENT_0', 'RELATIF', EPSI, 'TEMP', K8B,
     &                    IBID, TMOY(2), CBID, K8B, IRET )
               IF (IRET.NE.0) THEN
                  VALK (1) = TBMOYE(2)
                  VALK (2) = 'TEMP'
                  VALK (3) = NOPARA(1)
                  VALK (4) = NOPARA(2)
                  VALK (5) = 'MOMENT_0'
                  CALL U2MESG('F', 'POSTRCCM_16',5,VALK,0,0,1,INST)
               ENDIF
            ENDIF
            CALL TBLIVA ( TBMOYE(1), 2, NOPARA, IBID, INST, CBID,
     &                    'MOMENT_1', 'RELATIF', EPSI, 'TEMP', K8B,
     &                    IBID, VMOY, CBID, K8B, IRET )
            IF (IRET.NE.0) THEN
               VALK (1) = TBMOYE(1)
               VALK (2) = 'TEMP'
               VALK (3) = NOPARA(1)
               VALK (4) = NOPARA(2)
               VALK (5) = 'MOMENT_1'
               CALL U2MESG('F', 'POSTRCCM_16',5,VALK,0,0,1,INST)
            ENDIF
C
C --------- DT1: AMPLITUDE DE LA VARIATION ENTRE LES 2 ETATS STABILISES
C                DE LA DIFFERENCE DE TEMPERATURE ENTRE LES PAROIS
C                INTERNE ET EXTERNE
C
            DT1 = VMOY
C
C --------- TA : AMPLITUDE DE VARIATION ENTRE LES 2 ETATS STABILISES
C                DES TEMPERATURES MOYENNES A GAUCHE D'UNE DISCONTINUITE
C
            TA = TMOY(1)
C
C --------- TB : AMPLITUDE DE VARIATION ENTRE LES 2 ETATS STABILISES
C                DES TEMPERATURES MOYENNES A DROITE D'UNE DISCONTINUITE
C
            IF ( NBM .GT. 1 ) THEN
               TB = TMOY(2)
            ELSE
               TB = TMOY(1)
            ENDIF
C
            DT1MAX = MAX ( DT1MAX, ABS( DT1 ) )
C
            IF ( NBM .GT. 1 ) THEN
               TAB =  ( ALPHAA * TA )  - ( ALPHAB * TB )
               TABMAX = MAX ( TABMAX, ABS( TAB ) )
            ENDIF
C
 10   CONTINUE
C
      SN6 = SN6 + ( SN3 * DT1MAX )
      IF ( NBM .GT. 1 ) SN6 = SN6 + ( SN4 * TABMAX )
C
      CALL JEDETR ( NOMOBJ )
C
 9999 CONTINUE
C
      CALL JEDEMA( )
      END
