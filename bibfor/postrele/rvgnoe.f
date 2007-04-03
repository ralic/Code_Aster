      SUBROUTINE RVGNOE ( MCF, IOCC, NMAILA, NLSTND, NBTROU, LINOEU )
      IMPLICIT NONE
      INTEGER             IOCC, NBTROU, LINOEU(*)
      CHARACTER*(*)       MCF
      CHARACTER*8                    NMAILA
      CHARACTER*24                           NLSTND
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/04/2007   AUTEUR VIVAN L.VIVAN 
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
C     ------------------------------------------------------------------
C     SAISIE DES NOEUDS DE L' OCCURENCE IOCC DE ACTION
C     CAS OU LE LIEU EST UNE LISTE DE GROUP_NO ET/OU NOEUD
C     ------------------------------------------------------------------
C IN   IOCC   : I : NUMERO DE L' OCCURENCE TRAITEE
C IN   NMAILA : K : NOM DU MAILLAGE CONTENANT LES GROUPES ET LES NOEUDS
C JXIN NLSTND : K : NOM OJB S V I <-- NUMERO DES NOEUDS
C     ------------------------------------------------------------------
C     CONSTRUCTION DE LA LISTE NLSTND :
C         LA LISTE ARGUMENT DE NOEUD
C         LES NOEUD DES GROUPES DE NOEUD DANS L' ORDRE DE LA LISTE
C         ARGUMENT DE GROUP_NO
C         SI 2 NOEUD CONSECUTIF DANS CETTE CONSTRUCTION SONT IDENTIQUES
C         ON N' EN GARDE QU' UN
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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NBRGPN,NBNEUD,ANEUD,AGRPN,ALNDTP,ALSTND,AGNEUD
      INTEGER      I,J,K,LIBRE,NUMND,NBTND,N1,NBN,IRET,IERA
      INTEGER      ASGTU,I1,I2,JVALE,NY
      REAL*8       VECTY(3), TOLE
      CHARACTER*1  K1B
      CHARACTER*8  COURBE, CRIT, NOMGRN
      CHARACTER*15 NREPND
      CHARACTER*17 NREPGN
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
C
      NBTND = 0
      NREPGN = NMAILA//'.GROUPENO'
      NREPND = NMAILA//'.NOMNOE'
      LIBRE = 1
C
C --- RECUPERATION DES ENTITES
C
      CALL GETVEM(NMAILA,'GROUP_NO',MCF,'GROUP_NO',
     &                                  IOCC,1,0,ZK8,NBRGPN)
      CALL GETVEM(NMAILA,'NOEUD',MCF,'NOEUD',
     &                                  IOCC,1,0,ZK8,NBNEUD)
      NBRGPN = -NBRGPN
      NBNEUD = -NBNEUD
      IF ( NBRGPN .NE. 0 ) THEN
         CALL WKVECT('&OP0051.NOM.GRP.ND','V V K8',NBRGPN,AGRPN)
         CALL GETVEM(NMAILA,'GROUP_NO',MCF,'GROUP_NO',
     &                                    IOCC,1,NBRGPN,ZK8(AGRPN),N1)
         DO 10, I = 1, NBRGPN, 1
            CALL JELIRA(JEXNOM(NREPGN,ZK8(AGRPN+I-1)),'LONMAX',N1,K1B)
            NBTND = NBTND + N1
10       CONTINUE
      ENDIF
      IF ( NBNEUD .NE. 0 ) THEN
         CALL WKVECT('&OP0051.NOM.NOEUD','V V K8',NBNEUD,ANEUD)
         CALL GETVEM(NMAILA,'NOEUD',MCF,'NOEUD',
     &                                   IOCC,1,NBNEUD,ZK8(ANEUD),N1)
         NBTND = NBTND + NBNEUD
      ENDIF
C
      CALL WKVECT('&OP0051.LIST.ND.TEMP','V V I',NBTND,ALNDTP)
      DO 20, I = 1, NBTND, 1
         ZI(ALNDTP + I-1) = 0
20    CONTINUE
C
      IF ( NBNEUD .NE. 0 ) THEN
         DO 200, I = 1, NBNEUD, 1
            CALL JENONU(JEXNOM(NREPND,ZK8(ANEUD + I-1)),NUMND)
            ZI(ALNDTP + I-1) = NUMND
200      CONTINUE
      ENDIF
      LIBRE = NBNEUD + 1
      IF ( NBRGPN .NE. 0 ) THEN
         DO 100, I = 1, NBRGPN, 1
            NOMGRN = ZK8(AGRPN + I-1)
            CALL JELIRA(JEXNOM(NREPGN,NOMGRN),'LONMAX',NBN,K1B)
            CALL JEVEUO(JEXNOM(NREPGN,NOMGRN),'L',AGNEUD)
            DO 110, J = 1, NBN, 1
               ZI(ALNDTP + LIBRE-1 + J-1) = ZI(AGNEUD + J-1)
110         CONTINUE
            LIBRE = LIBRE + NBN
100      CONTINUE
      ENDIF
C
      NBTND = 0
      IF ( NBTROU .EQ. 0 ) THEN
         DO 250, I = 1, LIBRE-1, 1
            NBTND = NBTND + MIN(1,ABS(ZI(ALNDTP+I-1)-ZI(ALNDTP+I)))
 250     CONTINUE
      ELSE
         DO 252, I = 1, NBTROU, 1
            DO 254, J = 1, LIBRE, 1
               IF ( LINOEU(I) .EQ. ZI(ALNDTP+J-1) ) THEN
                  NBTND = NBTND + 1
                  GOTO 252
               ENDIF
 254        CONTINUE
 252     CONTINUE
      ENDIF
C
      CALL WKVECT ( NLSTND, 'V V I', NBTND, ALSTND )
      NBTND = LIBRE - 1
      IF ( NBTROU .EQ. 0 ) THEN
         LIBRE = 2
         NUMND = ZI(ALNDTP)
         ZI(ALSTND) = NUMND
         DO 300, I = 2, NBTND, 1
            IF ( NUMND .NE. ZI(ALNDTP + I-1) ) THEN
               NUMND = ZI(ALNDTP + I-1)
               ZI(ALSTND + LIBRE-1) = NUMND
               LIBRE = LIBRE + 1
            ENDIF
 300     CONTINUE
      ELSE
         LIBRE = 1
         DO 302, I = 1, NBTND, 1
            NUMND = ZI(ALNDTP+I-1)
            DO 304, J = 1, NBTROU, 1
               IF ( LINOEU(J) .EQ. NUMND ) THEN
                  DO 306, K = 1, LIBRE-1, 1
                     IF ( NUMND .EQ. ZI(ALSTND+K-1) ) GOTO 302
 306              CONTINUE
                  ZI(ALSTND + LIBRE-1) = NUMND
                  LIBRE = LIBRE + 1
               ENDIF
 304        CONTINUE
 302     CONTINUE
      ENDIF
C
C --- CAS PARTICULIER
C
      CALL GETVR8 ( 'ACTION', 'VECT_Y', IOCC,1,3, VECTY, NY )
      IF (NY.NE.0) THEN
C        VERIFICATIONS PRELIMINAIRES
        IF ((NBNEUD.GE.2.AND.NBRGPN.EQ.0).OR.
     &      (NBNEUD.EQ.0.AND.NBRGPN.EQ.1)) THEN
          IERA = 0
          IF(NBRGPN.EQ.1) THEN
            NOMGRN = ZK8(AGRPN + 1-1)
            CALL JELIRA(JEXNOM(NREPGN,NOMGRN),'LONMAX',NBN,K1B)
            IF(NBN.LT.2)  CALL U2MESS('F','POSTRELE_21')
          ENDIF
        ELSE
          CALL U2MESS('F','POSTRELE_22')
        ENDIF
        CALL JEEXIN('&&YAPAS '//'S1   '//'.DESC',N1)
        IF ( N1 .NE. 0 ) CALL JEDETR('&&YAPAS '//'S1   '//'.DESC')
        COURBE='&&YAPAS'
        CALL WKVECT(COURBE//'S1   '//'.DESC','V V R',6,ASGTU)
C       ORIGINE
        I1=ZI(ALSTND-1+1)
C       EXTREMITE
        I2=ZI(ALSTND-1+LIBRE-1)
        CALL JEVEUO ( NMAILA//'.COORDO    .VALE','L', JVALE )
C       TOLERANCE
        CALL GETVTX ( MCF, 'CRITERE'  , IOCC,1,1, CRIT  , N1 )
        CALL GETVR8 ( MCF, 'PRECISION', IOCC,1,1, TOLE  , N1 )
C       VERIFICATION QUE LES POINTS SONT ALIGNES
        CALL OREINO(NMAILA,ZI(ALSTND),LIBRE-1,I1,I2,ZR(JVALE),CRIT,
     &              TOLE,IERA,IRET)
        IF (IRET.NE.0) CALL U2MESS('F','POSTRELE_60')
        ZR(ASGTU-1+1)=ZR(JVALE-1+3*(I1-1)+1)
        ZR(ASGTU-1+2)=ZR(JVALE-1+3*(I1-1)+2)
        ZR(ASGTU-1+3)=ZR(JVALE-1+3*(I1-1)+3)
        ZR(ASGTU-1+4)=ZR(JVALE-1+3*(I2-1)+1)
        ZR(ASGTU-1+5)=ZR(JVALE-1+3*(I2-1)+2)
        ZR(ASGTU-1+6)=ZR(JVALE-1+3*(I2-1)+3)
      ENDIF
C
      CALL JEEXIN('&OP0051.NOM.NOEUD',N1)
      IF ( N1 .NE. 0 ) CALL JEDETR('&OP0051.NOM.NOEUD')
      CALL JEEXIN('&OP0051.NOM.GRP.ND',N1)
      IF ( N1 .NE. 0 ) CALL JEDETR('&OP0051.NOM.GRP.ND')
      CALL JEDETR('&OP0051.LIST.ND.TEMP')
C
      CALL JEDEMA()
      END
