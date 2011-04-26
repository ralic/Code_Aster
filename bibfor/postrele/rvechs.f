      SUBROUTINE RVECHS(SSCH19,NBCP,NBCO,NBSP,MA,VLC,FOR,FEX,
     &                  RSOR,RSEX,N,PTADR,VAL,NBNDF,CLOCF)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*19 SSCH19
      INTEGER     NBCP,NBCO,NBSP,N,PTADR,FOR(*),FEX(*)
      INTEGER     MA(*),VLC(*),NBNDF(6,*),CLOCF(6,4,*)
      REAL*8      RSOR(*),RSEX(*),VAL(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     EVALUATION DE CMP LE LONG D' UN SGT_3D EN UN MORCEAUX
C     ------------------------------------------------------------------
C IN  SSCH19 : K : NOM DU SOUS_CHAM_GD
C IN  NBCP   : I : NOMBRE DE CMP
C IN  NBOP   : I : NOMBRE DE COUCHE
C IN  NBSP   : I : NOMBRE DE SOUS-POINT
C IN  MA     : I : TABLE DES MAILLES 3D RENCONTREES
C IN  VLC    : I : POINTEUR DES SGT_ELEM SUR LES MAILLES 3D RENCONTREES
C IN  FOR    : I : TABLES FACES CONTENANT ORIGINES   DES SGT_ELEM
C IN  FEX    : I : TABLES FACES CONTENANT EXTREMITES DES SGT_ELEM
C IN  RSOR   : R : TABLES CO_REF DES ORIGINES   DES SGT_ELEM
C IN  RSEX   : R : TABLES CO_REF DES EXTREMITES DES SGT_ELEM
C IN  N      : I : NOMBRE DE SGT_ELEM
C VAR PTADR  : I : POINTEUR SUR LE PREMIER ELEMENT LIBRE DE VAL
C OUT VAL    : R : TABLEAU DES VALEUR DES CMPS (TOUS MORCEAUX)
C IN  NBNDF  : I : TABLE(1..6,1..3)
C            :   :    T(I,J) = NB_ND DE LA FACE I DU TYPE DE MAILLE J
C IN  CLOCF  : I : TABLE(1..6,1..4,1..3)
C            :   :    T(I,J,K) = NUM_LOC (DANS CONNEC_MAILLE)
C            :   :    ND_LOC I DE LA FACE J DE LA MAILLE DE TYPE K
C     ------------------------------------------------------------------
C
      CHARACTER*32 JEXNUM,JEXATR
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*24 NCONEC,NPADR,NVALE
      CHARACTER*8  NMAILA,KTYPM
      CHARACTER*4  DOCU
C
      INTEGER APNCO,APNSP,APNBN,ACONEC,VLCCNC
      INTEGER M,FACE(2),I,J,NBPT,AVALE,APADR,ITYPM,NBMA,AUX,IATYMA
      REAL*8  CREF(2,2)
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
      CALL WKVECT('&&RVECHS.TABLE.AUX','V V R',4*NBCO*NBSP*NBCP,AUX)
      CALL JELIRA(SSCH19//'.VALE','DOCU',I,DOCU)
      IF ( DOCU .EQ. 'CHNO' ) THEN
         APNCO = 0
         APNSP = 0
         APNBN = 0
      ELSE
         CALL JEVEUO(SSCH19//'.PNCO','L',APNCO)
         CALL JEVEUO(SSCH19//'.PNSP','L',APNSP)
         CALL JEVEUO(SSCH19//'.PNBN','L',APNBN)
      ENDIF
      NPADR = SSCH19//'.PADR'
      NVALE = SSCH19//'.VALE'
      CALL JEVEUO(NPADR,'L',APADR)
      CALL JEVEUO(NVALE,'L',AVALE)
      CALL JEVEUO(SSCH19//'.NOMA','L',I)
      NMAILA = ZK8(I)
      NCONEC = NMAILA//'.CONNEX         '
      CALL JEVEUO(JEXNUM(NCONEC,1),'L',ACONEC)
      CALL JEVEUO(JEXATR(NCONEC,'LONCUM'),'L',VLCCNC)
      IF ( DOCU .EQ. 'CHNO' ) THEN
         NBMA = 1
         NBPT = 1
         DO 100, I = 1, N, 1
            M = MA(VLC(I))
            CALL JEVEUO(NMAILA//'.TYPMAIL','L',IATYMA)
            J=IATYMA-1+M
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(J)),KTYPM)
            FACE(1)   = FOR(I)
            CREF(1,1) = RSOR(2*(I-1)+1)
            CREF(2,1) = RSOR(2*(I-1)+2)
            IF ( KTYPM(1:5) .EQ. 'TETRA' ) THEN
               ITYPM = 1
            ELSE IF ( KTYPM(1:5) .EQ. 'PENTA' ) THEN
               ITYPM = 2
            ELSE IF ( KTYPM(1:4) .EQ. 'HEXA' ) THEN
               ITYPM = 3
            ELSE
               CALL U2MESK('F','POSTRELE_18',1,KTYPM)
            ENDIF
            CALL RVCHN3(ZR(AVALE),ZI(APADR),MA(VLC(I)),ITYPM,NBPT,
     &                  NBCP,FACE,CREF,NBNDF,CLOCF,ZI(ACONEC),
     &                  ZI(VLCCNC),VAL,PTADR,ZR(AUX))
100      CONTINUE
         M = MA(VLC(N))
         CALL JEVEUO(NMAILA//'.TYPMAIL','L',IATYMA)
         J=IATYMA-1+M
         CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(J)),KTYPM)
         FACE(1)   = FEX(N)
         CREF(1,1) = RSEX(2*(N-1)+1)
         CREF(2,1) = RSEX(2*(N-1)+2)
         IF ( KTYPM(1:5) .EQ. 'TETRA' ) THEN
            ITYPM = 1
         ELSE IF ( KTYPM(1:5) .EQ. 'PENTA' ) THEN
            ITYPM = 2
         ELSE IF ( KTYPM(1:4) .EQ. 'HEXA' ) THEN
            ITYPM = 3
         ELSE
            CALL U2MESK('F','POSTRELE_18',1,KTYPM)
         ENDIF
         CALL RVCHN3(ZR(AVALE),ZI(APADR),MA(VLC(N)),ITYPM,NBPT,NBCP,
     &               FACE,CREF,NBNDF,CLOCF,ZI(ACONEC),
     &               ZI(VLCCNC),VAL,PTADR,ZR(AUX))
      ELSE
         NBPT = 2
         DO 200, I = 1, N, 1
            M    = MA(VLC(I))
            NBMA = VLC(I+1) - VLC(I)
            CALL JEVEUO(NMAILA//'.TYPMAIL','L',IATYMA)
            J=IATYMA-1+M
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(J)),KTYPM)
            FACE(1)   = FOR(I)
            FACE(2)   = FEX(I)
            CREF(1,1) = RSOR(2*(I-1)+1)
            CREF(2,1) = RSOR(2*(I-1)+2)
            CREF(1,2) = RSEX(2*(I-1)+1)
            CREF(2,2) = RSEX(2*(I-1)+2)
            IF ( KTYPM(1:5) .EQ. 'TETRA' ) THEN
               ITYPM = 1
            ELSE IF ( KTYPM(1:5) .EQ. 'PENTA' ) THEN
               ITYPM = 2
            ELSE IF ( KTYPM(1:4) .EQ. 'HEXA' ) THEN
               ITYPM = 3
            ELSE
               CALL U2MESK('F','POSTRELE_18',1,KTYPM)
            ENDIF
            CALL RVCHL3(ZR(AVALE),ZI(APADR),ZI(APNSP),
     &                   ZI(APNBN),MA(VLC(I)),NBMA,ITYPM,NBCO,NBSP,NBPT,
     &                   NBCP,FACE,CREF,NBNDF,CLOCF,ZI(ACONEC),
     &                   ZI(VLCCNC),VAL,PTADR,ZR(AUX))
200      CONTINUE
      ENDIF
      CALL JEDETR('&&RVECHS.TABLE.AUX')
      CALL JEDEMA()
      END
