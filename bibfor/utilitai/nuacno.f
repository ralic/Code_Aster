      SUBROUTINE NUACNO ( NUAGE, LMA, LNO, CHNO )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       NUAGE, LMA, LNO, CHNO
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 14/01/98   AUTEUR VABHHTS J.PELLET 
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
C     PASSAGE D'UNE UNE SD NUAGE A SD CHAM_NO
C
C IN  NUAGE  : NOM DE LA SD NUAGE
C IN  LMA    : LISTE DES MAILLES A PRENDRE EN COMPTE
C IN  LNO    : LISTE DES NOEUDS A PRENDRE EN COMPTE
C IN  CHNO   : NOM DE LA SD CHAM_NO
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       GD, NBEC
      CHARACTER*4   TYPE
      CHARACTER*8   K8B, NOMA, NOMGD
      CHARACTER*19  KCHNO, KLMA, KLNO, KNUAGE, NONU
      LOGICAL       EXISDG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      KNUAGE = NUAGE
      KLNO   = LNO
      KCHNO  = CHNO
C
      CALL JEVEUO ( KCHNO//'.DESC', 'L', JDESC )
      GD  = ZI(JDESC-1+1)
      NUM = ZI(JDESC-1+2)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K8B)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',IAD)
      CALL JENUNO(JEXNUM('&CATA.GD.NOMGD',GD),NOMGD)
      NEC = NBEC(GD)
      CALL WKVECT ( '&&NUACNO.NOMCMP', 'V V I', NCMPMX, KCOMP )
      CALL WKVECT ( '&&NUACNO.ENT_COD','V V I', NEC,    IAEC  )
C
      CALL JEVEUO ( KCHNO//'.REFE', 'L', JREFE )
      NOMA = ZK24(JREFE-1+1) (1:8)
      NONU = ZK24(JREFE-1+2) (1:19)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NP,K8B,IE)
C
      IF ( KLNO .NE. ' ' ) THEN
         CALL JELIRA ( KLNO//'.LSNO', 'LONUTI', NP, K8B )
         CALL JEVEUO ( KLNO//'.LSNO', 'L', JLNO)
      ELSE
         CALL WKVECT ( '&&NUACNO.NOEUD', 'V V I', NP, JLNO )
         DO 10 I = 1 , NP
            ZI(JLNO+I-1) = I
 10      CONTINUE
      ENDIF
C
      CALL JELIRA ( KCHNO//'.VALE', 'TYPE' ,IBID, TYPE )
      CALL JEVEUO ( KCHNO//'.VALE', 'E', KVALE )
      IF ( TYPE(1:1) .EQ. 'R' ) THEN
         ITYPE = 1
      ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
         ITYPE = 2
      ELSE
         CALL UTMESS('F','NUACNO','ON NE TRAITE QUE DES CHAM_NO '//
     +                         'REELS OU COMPLEXES. VRAIMENT DESOLE !')
      ENDIF
C
      CALL JEVEUO ( KNUAGE//'.NUAV','L',JNUAV)
      CALL JEVEUO ( KNUAGE//'.NUAI','L',JNUAI)
      NC = ZI(JNUAI+2)
C
C     --SI LE CHAMP EST A REPRESENTATION CONSTANTE ---
C
      IF ( NUM .LT. 0 ) THEN
         NCMP = -NUM
         DO 200 IEC = 1 , NEC
            ZI(IAEC+IEC-1) = ZI(JDESC-1+2+IEC)
 200     CONTINUE
         DO 202 J = 1 , NP
            INO = ZI(JLNO+J-1)
            IVAL = NCMP * ( INO - 1 )
            ICOMPT = 0
            DO 204 ICMP = 1 , NCMPMX
               IF ( EXISDG(ZI(IAEC) , ICMP ) ) THEN
                  ICOMPT = ICOMPT + 1
                  K = NC*(J-1) + ICOMPT
                  IF (ITYPE.EQ.1) THEN
                     ZR(KVALE-1+IVAL+ICMP) = ZR(JNUAV+K-1)
                  ELSE
                     ZC(KVALE-1+IVAL+ICMP) = ZC(JNUAV+K-1)
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
                  IF (ITYPE.EQ.1) THEN
                     ZR(KVALE-1+IEQ) = ZR(JNUAV+K-1)
                  ELSE
                     ZC(KVALE-1+IEQ) = ZC(JNUAV+K-1)
                  ENDIF
                ENDIF
 220        CONTINUE
 210     CONTINUE
      ENDIF
C
      CALL JEDETR ( '&&NUACNO.NOMCMP')
      CALL JEDETR ( '&&NUACNO.ENT_COD')
      CALL JEDETR ( '&&NUACNO.NOEUD')
      CALL JEDEMA()
      END
