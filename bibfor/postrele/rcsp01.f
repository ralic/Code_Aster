      SUBROUTINE RCSP01 ( NBM, ADRM, IPT, SP3, SP4, SP5, ALPHAA, ALPHAB,
     &                    NBTH, IOCS, SP6 )
      IMPLICIT   NONE
      INTEGER             NBM, ADRM(*), IPT, NBTH, IOCS
      REAL*8              SP3, SP4, SP5, ALPHAA, ALPHAB, SP6
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
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
      INTEGER      JCHTH, IAD, ICMP, NBCMP, DECAL, JCESD, JCESV, JCESL,
     &             NBINST, I, JMOYE, JMOY2, JTHER, VALI(2)
      REAL*8       TINT, TEXT, TA, TB, TAB, DT1, DT2, 
     &             TERM1, TERM2, DT1MAX, DT2MAX, TABMAX
      CHARACTER*8  K8B
      CHARACTER*24 CHTEMP, VALK(7)
C
C DEB ------------------------------------------------------------------
C
      SP6 = 0.D0
      IF ( NBTH .EQ. 0 ) GOTO 9999
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
         VALI(1) = IOCS
         VALI(2) = ADRM(1)
         CALL U2MESG('F','POSTRCCM_15',1,'RESU_THER',2,VALI,0,0.D0)
      ENDIF
      CALL JEVEUO ( ZK24(JCESV-1+IAD),'L', JTHER )
      CALL JELIRA ( ZK24(JCESV-1+IAD),'LONMAX', NBINST, K8B )
      NBINST = NBINST / 2
C
      ICMP = 2
      IAD = DECAL + (IPT-1)*NBCMP + ICMP
      IF (.NOT.ZL(JCESL-1+IAD)) THEN
         VALI(1) = IOCS
         VALI(2) = ADRM(1)
         CALL U2MESG('F','POSTRCCM_15',1,'RESU_THER_MOYE',2,VALI,
     +                                                    0,0.D0)
      ENDIF
      CALL JEVEUO ( ZK24(JCESV-1+IAD),'L', JMOYE )
C
      IF ( NBM .GT. 1 ) THEN
         DECAL = ZI(JCESD-1+5+4*(ADRM(2)-1)+4)
         ICMP = 2
         IAD = DECAL + (IPT-1)*NBCMP + ICMP
         IF (.NOT.ZL(JCESL-1+IAD)) THEN
            VALI(1) = IOCS
            VALI(2) = ADRM(2)
            CALL U2MESG('F','POSTRCCM_15',1,'RESU_THER_MOYE',2,VALI,
     +                                                       0,0.D0)
         ENDIF
         CALL JEVEUO ( ZK24(JCESV-1+IAD),'L', JMOY2 )
      ENDIF
C
C --- ON BOUCLE SUR LES INSTANTS :
C
      DT1MAX = 0.D0
      DT2MAX = 0.D0
      TABMAX = 0.D0
C
      DO 10 I = 1 , NBINST
C
C ------ TEMP_INT, TEMP_EXT
C
         TINT = ZR(JTHER-1+2*(I-1)+1)
         TEXT = ZR(JTHER-1+2*(I-1)+2)
C
C ------ DT1: AMPLITUDE DE LA VARIATION ENTRE LES 2 ETATS STABILISES
C             DE LA DIFFERENCE DE TEMPERATURE ENTRE LES PAROIS
C             INTERNE ET EXTERNE
C
         DT1 = ZR(JMOYE-1+2*(I-1)+2)
C
C ------ TA : AMPLITUDE DE VARIATION ENTRE LES 2 ETATS STABILISES
C             DES TEMPERATURES MOYENNES A GAUCHE D'UNE DISCONTINUITE
C
         TA = ZR(JMOYE-1+2*(I-1)+1)
C
C ------ DT2: PARTIE NON LINEAIRE DE LA DISTRIBUTION DANS L'EPAISSEUR
C             DE PAROI DE L'AMPLITUDE DE VARIATION DE LA TEMPERATURE
C             ENTRE LES 2 ETATS STABILISES
C
         TERM1 = ABS(TEXT-TA) - ABS(0.5D0*DT1)
         TERM2 = ABS(TINT-TA) - ABS(0.5D0*DT1)
         DT2 = MAX( TERM1, TERM2, 0.D0 )
C
         DT1MAX = MAX ( DT1MAX, ABS( DT1 ) )
C
         DT2MAX = MAX ( DT2MAX, ABS( DT2 ) )
C
         IF ( NBM .GT. 1 ) THEN
            TB = ZR(JMOY2-1+2*(I-1)+1)
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
 9999 CONTINUE
C
      END
