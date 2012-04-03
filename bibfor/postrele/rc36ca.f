      SUBROUTINE RC36CA ( CARAEL, NOMA, NBMA, LISTMA, CHCARA )
      IMPLICIT   NONE
      INTEGER             NBMA, LISTMA(*)
      CHARACTER*8         CARAEL, NOMA
      CHARACTER*24        CHCARA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/04/2012   AUTEUR SELLENET N.SELLENET 
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
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C
C     TRAITEMENT DU CARA_ELEM
C     ON A BESOIN DES INERTIES (CARGENPO): IY1, IZ1, IY2, IZ2
C                 DU DIAMETRE EXTERIEUR (CARGEOPO) : R1, R2
C                 DE L'EPAISSEUR (CARGEOPO) : EP1, EP2
C
C IN  : CARAEL : CARA_ELEM UTILISATEUR
C IN  : NOMA   : MAILLAGE
C IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
C IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
C OUT : CHCARA : CHAM_ELEM DE TYPE ELNO DE CARACTERISTIQUES ELEMENTAIRES
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NBCMP, IRET,
     +             IM, IMA, NBPT, DECAL, IPT, ICMP, IAD, IADC, NCMP
      INTEGER      JCESD, JCESV, JCESL, JCESDC, JCESVC, JCESLC
      INTEGER      JCESD1, JCESV1, JCESL1, JCESD2, JCESV2, JCESL2
      REAL*8       VC
      CHARACTER*8  NOMGD
      CHARACTER*16 NOCMP(4)
      CHARACTER*19 K19B, CES1, CES2
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      NOMGD = 'RCCM_R'
      NBCMP = 4
      NOCMP(1) = 'IY'
      NOCMP(2) = 'IZ'
      NOCMP(3) = 'D'
      NOCMP(4) = 'EP'
C
      CALL RC36ZZ ( NOMA, NOMGD, NBCMP, NOCMP, NBMA, LISTMA, CHCARA )
C
      CALL JEVEUO ( CHCARA(1:19)//'.CESD', 'E', JCESD )
      CALL JEVEUO ( CHCARA(1:19)//'.CESV', 'E', JCESV )
      CALL JEVEUO ( CHCARA(1:19)//'.CESL', 'E', JCESL )
C
      K19B = CARAEL//'.CARGENPO'
      CES1  = '&&RC36CA.CARGENPO'
      CALL CARCES ( K19B, 'ELNO', ' ', 'V', CES1, 'A', IRET )
C
      NBCMP = 4
      NOCMP(1) = 'IY1'
      NOCMP(2) = 'IZ1'
      NOCMP(3) = 'IY2'
      NOCMP(4) = 'IZ2'
      CALL CESRED ( CES1, NBMA, LISTMA, NBCMP, NOCMP, 'V', CES1 )
C
      CALL JEVEUO ( CES1//'.CESD', 'L', JCESD1 )
      CALL JEVEUO ( CES1//'.CESV', 'L', JCESV1 )
      CALL JEVEUO ( CES1//'.CESL', 'L', JCESL1 )
C
      K19B = CARAEL//'.CARGEOPO'
      CES2  = '&&RC36CA.CARGEOPO'
      CALL CARCES ( K19B, 'ELNO', ' ', 'V', CES2, 'A', IRET )
C
      NBCMP = 4
      NOCMP(1) = 'R1'
      NOCMP(2) = 'EP1'
      NOCMP(3) = 'R2'
      NOCMP(4) = 'EP2'
      CALL CESRED ( CES2, NBMA, LISTMA, NBCMP, NOCMP, 'V', CES2 )
C
      CALL JEVEUO ( CES2//'.CESD', 'L', JCESD2 )
      CALL JEVEUO ( CES2//'.CESV', 'L', JCESV2 )
      CALL JEVEUO ( CES2//'.CESL', 'L', JCESL2 )
C
      DO 100  IM = 1, NBMA
         IMA = LISTMA(IM)
         NBPT = ZI(JCESD-1+5+4*(IMA-1)+1)
         NCMP = ZI(JCESD-1+5+4*(IMA-1)+3)
         CALL ASSERT(NCMP.EQ.4 .AND. NBPT.EQ.2)
         DO 110 IPT = 1, NBPT
           DO 120 ICMP = 1, NCMP
C            POINT 1 : CMPS 1 ET 2 - POINT 2 : CMPS 3 ET 4
             IF (ICMP.LE.2) THEN
               IF (IPT.EQ.1) THEN
                 DECAL = 0
               ELSE
                 DECAL = 2
               ENDIF
             ELSE
               IF (IPT.EQ.1) THEN
                 DECAL = -2
               ELSE
                 DECAL = 0
               ENDIF
             ENDIF
C            CMPS IY/IZ DANS CHAMP 1 - CMPS D/EP DANS CHAMP 2
             IF (ICMP.LE.2) THEN
                JCESDC = JCESD1
                JCESVC = JCESV1
                JCESLC = JCESL1
             ELSE
                JCESDC = JCESD2
                JCESVC = JCESV2
                JCESLC = JCESL2
             ENDIF
             CALL CESEXI('S',JCESDC,JCESLC,IMA,IPT,1,ICMP+DECAL,IADC)
             CALL ASSERT(IADC.GT.0)
             VC = ZR(JCESVC-1+IADC)
C            PASSAGE R A D : X2 (CMP 3)
             IF (ICMP.EQ.3) THEN
               VC = 2.D0 * VC
             ENDIF
             CALL CESEXI('S',JCESD,JCESL,IMA,IPT,1,ICMP,IAD)
             IF (IAD.LT.0) THEN
               IAD = -IAD
             ENDIF
             ZR(JCESV-1+IAD) = VC
             ZL(JCESL-1+IAD) = .TRUE.
 120       CONTINUE
 110     CONTINUE
 100  CONTINUE
C
      CALL DETRSD ( 'CHAM_ELEM_S', CES1 )
      CALL DETRSD ( 'CHAM_ELEM_S', CES2 )
C
      CALL JEDEMA( )
      END
