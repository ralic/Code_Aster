      SUBROUTINE CNONUA ( NX,CHNO, LNO, NUAGE )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER NX
      CHARACTER*(*)       CHNO, LNO, NUAGE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     PASSAGE D'UNE SD CHAM_NO A UNE SD NUAGE
C
C IN  NX     : DIMENSION D'ESPACE DU NUAGE (1,2 OU 3)
C IN  CHNO   : NOM DE LA SD CHAM_NO
C IN  LNO    : LISTE DES NOEUDS A PRENDRE EN COMPTE
C OUT NUAGE  : SD NUAGE PRODUITE
C     ------------------------------------------------------------------
      INTEGER       GD, NBEC,JDESC,NUM,NCMPMX,IAD,NEC,KCOMP
      INTEGER IAEC,JREFE,NP,IE,KCOOR,JLNO,I,IBID,KVALE,ITYPE
      INTEGER NC,IEC,ICMP,IANUEQ,IAPRNO,J,INO,NCMP,ICOMPT
      INTEGER JNUAI,JNUAX,JNUAV,JNUAL,IVAL,K,IEQ
      CHARACTER*4   TYPE
      CHARACTER*8   K8B, NOMA, NOMGD
      CHARACTER*19  KCHNO, KLNO, KNUAGE, NONU
      LOGICAL       EXISDG, LNUAL, PREM
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      KCHNO  = CHNO
      KLNO   = LNO
      KNUAGE = NUAGE
      LNUAL  = .FALSE.
C
      CALL JEVEUO ( KCHNO//'.DESC', 'L', JDESC )
      GD  = ZI(JDESC-1+1)
      NUM = ZI(JDESC-1+2)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K8B)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',IAD)
      CALL JENUNO(JEXNUM('&CATA.GD.NOMGD',GD),NOMGD)
      NEC = NBEC(GD)
      CALL WKVECT ( '&&CNONUA.NOMCMP', 'V V I', NCMPMX, KCOMP )
      CALL WKVECT ( '&&CNONUA.ENT_COD','V V I', NEC,    IAEC  )
C
      CALL JEVEUO ( KCHNO//'.REFE', 'L', JREFE )
      NOMA = ZK24(JREFE-1+1) (1:8)
      NONU = ZK24(JREFE-1+2) (1:19)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NP,K8B,IE)
      CALL JEVEUO (NOMA//'.COORDO    .VALE', 'L', KCOOR )

      IF ( KLNO .NE. ' ' ) THEN
         CALL JELIRA ( KLNO, 'LONUTI', NP, K8B )
         CALL JEVEUO ( KLNO, 'L', JLNO)
      ELSE
         CALL WKVECT ( '&&CNONUA.NOEUD', 'V V I', NP, JLNO )
         DO 10 I = 1 , NP
            ZI(JLNO+I-1) = I
 10      CONTINUE
      ENDIF
C
      CALL JELIRA ( KCHNO//'.VALE', 'TYPE' ,IBID, TYPE )
      CALL JEVEUO ( KCHNO//'.VALE', 'L', KVALE )
      IF ( TYPE(1:1) .EQ. 'R' ) THEN
         ITYPE = 1
      ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
         ITYPE = 2
      ELSE
         CALL U2MESS('F','UTILITAI_36')
      ENDIF


C     --SI LE CHAMP EST A REPRESENTATION CONSTANTE ---
C     ---------------------------------------------------
      IF ( NUM .LT. 0 ) THEN
         NC = -NUM
         DO 100 IEC = 1 , NEC
            ZI(IAEC+IEC-1) = ZI(JDESC-1+2+IEC)
 100     CONTINUE
         DO 102 ICMP = 1 , NCMPMX
            IF ( EXISDG(ZI(IAEC) , ICMP ) )  ZI(KCOMP+ICMP-1) = ICMP
 102     CONTINUE
      ELSE


C     --- SI LE CHAMP EST DECRIT PAR 1 "PRNO" ---
C     ---------------------------------------------------
         PREM = .TRUE.
         CALL JEVEUO(NONU//'.NUEQ','L',IANUEQ)
         CALL JENONU(JEXNOM(NONU//'.LILI','&MAILLA'),IBID)
         CALL JEVEUO(JEXNUM(NONU//'.PRNO',IBID),'L',IAPRNO)
         DO 110 J = 1 , NP
            INO = ZI(JLNO+J-1)
            NCMP = ZI(IAPRNO-1+ (INO-1)*(NEC+2)+2 )
            IF ( NCMP .EQ. 0 ) GOTO 110
            DO 112 IEC = 1 , NEC
               ZI(IAEC+IEC-1) = ZI(IAPRNO-1+ (INO-1)*(NEC+2)+2+IEC )
 112        CONTINUE
            ICOMPT = 0
            DO 120 ICMP = 1 , NCMPMX
               IF ( EXISDG(ZI(IAEC) , ICMP ) ) THEN
                  ICOMPT = ICOMPT + 1
                  ZI(KCOMP+ICMP-1) = ICMP
               ENDIF
 120        CONTINUE
            IF ( PREM ) THEN
               NC = ICOMPT
               PREM = .FALSE.
            ELSE
               IF ( NC .NE. ICOMPT ) THEN
                  NC = MAX( NC , ICOMPT )
                  LNUAL = .TRUE.
               ENDIF
            ENDIF
 110     CONTINUE
      ENDIF
C
      CALL CRENUA ( NUAGE, NOMGD, NP, NX, NC, LNUAL )
C
C     --- .NUAI ---
C
      CALL JEVEUO ( KNUAGE//'.NUAI','E',JNUAI)
      ZI(JNUAI  ) = NP
      ZI(JNUAI+1) = NX
      ZI(JNUAI+2) = NC
      ZI(JNUAI+3) = GD
      ZI(JNUAI+4) = ITYPE
      ICMP = 0
      DO 30 I = 1 , NCMPMX
         IF ( ZI(KCOMP+I-1) .NE. 0 ) THEN
            ICMP = ICMP + 1
            ZI(JNUAI+5+ICMP-1) = ZI(KCOMP+I-1)
         ENDIF
 30   CONTINUE
C
C     --- .NUAX ---
C
      CALL JEVEUO ( KNUAGE//'.NUAX','E',JNUAX)
      DO 40 I = 1 , NP
         DO 42 J = 1 , NX
           ZR(JNUAX-1+NX*(I-1)+J) = ZR(KCOOR-1+3*(I-1)+J)
 42      CONTINUE
 40   CONTINUE
C
C     --- .NUAV ---
C
      CALL JEVEUO ( KNUAGE//'.NUAV','E',JNUAV)
C
C     --- .NUAL ---
C
      IF ( LNUAL ) CALL JEVEUO ( KNUAGE//'.NUAL','E',JNUAL)
C
C     --SI LE CHAMP EST A REPRESENTATION CONSTANTE ---
C
      IF ( NUM .LT. 0 ) THEN
         NCMP = -NUM
         DO 202 J = 1 , NP
            INO = ZI(JLNO+J-1)
            IVAL = NCMP * ( INO - 1 )
            ICOMPT = 0
            DO 204 ICMP = 1 , NCMPMX
               IF ( EXISDG(ZI(IAEC) , ICMP ) ) THEN
                  ICOMPT = ICOMPT + 1
                  K = NC*(J-1) + ICOMPT
                  IF (ITYPE.EQ.1) THEN
                     ZR(JNUAV+K-1) = ZR(KVALE-1+IVAL+ICMP)
                  ELSE
                     ZC(JNUAV+K-1) = ZC(KVALE-1+IVAL+ICMP)
                  ENDIF
               ENDIF
 204        CONTINUE
 202     CONTINUE
      ELSE
C
C     --- SI LE CHAMP EST DECRIT PAR 1 "PRNO" ---
C
         CALL JEVEUO(NONU//'.NUEQ','L',IANUEQ)
         CALL JENONU(JEXNOM(NONU//'.LILI','&MAILLA'),IBID)
         CALL JEVEUO(JEXNUM(NONU//'.PRNO',IBID),'L',IAPRNO)
         DO 210 J = 1 , NP
            INO = ZI(JLNO+J-1)
            IVAL = ZI(IAPRNO-1+ (INO-1)*(NEC+2)+1 )
            NCMP = ZI(IAPRNO-1+ (INO-1)*(NEC+2)+2 )
            IF ( NCMP .EQ. 0 ) GOTO 210
            DO 212 IEC = 1 , NEC
               ZI(IAEC+IEC-1) = ZI(IAPRNO-1+ (INO-1)*(NEC+2)+2+IEC )
 212        CONTINUE
            ICOMPT = 0
            DO 220 ICMP = 1 , NCMPMX
               IF ( EXISDG(ZI(IAEC) , ICMP ) ) THEN
                  ICOMPT = ICOMPT + 1
                  IEQ = ZI(IANUEQ-1+IVAL-1+ICOMPT)
                  K = NC*(J-1) + ICOMPT
                  IF ( LNUAL ) ZL(JNUAL+K-1) = .TRUE.
                  IF (ITYPE.EQ.1) THEN
                     ZR(JNUAV+K-1) = ZR(KVALE-1+IEQ)
                  ELSE
                     ZC(JNUAV+K-1) = ZC(KVALE-1+IEQ)
                  ENDIF
               ENDIF
 220        CONTINUE
 210     CONTINUE
      ENDIF
C
      CALL JEDETR ( '&&CNONUA.NOMCMP')
      CALL JEDETR ( '&&CNONUA.ENT_COD')
      CALL JEDETR ( '&&CNONUA.NOEUD')
      CALL JEDEMA()
      END
