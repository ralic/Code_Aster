      SUBROUTINE RVECHN(SSCH19,SDLIEU,SDEVAL)
      IMPLICIT NONE
C
      CHARACTER*19 SSCH19,SDLIEU,SDEVAL
C
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
C     OPERATION D' EXTRACTION DU POST-TRAITEMENT SUR UNE LISTE DE NOEUDS
C     ------------------------------------------------------------------
C IN  SSCH19 : K : NOM DU SOUS CHAMP DE GRANDEUR
C IN  SDLIEU : K : NOM DE LA SD REPRESENTANT LE LIEU
C OUT SDEVAL : K : NOM DE LA SD SOUS_CHAMP_GD PRODUITES
C            :   :(DESCRIPTION : CF RVPSTE)
C     ------------------------------------------------------------------
C
      CHARACTER*32 JEXNOM,JEXNUM
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
      CHARACTER*24 INVALE,INPADR,INPCMP,INNOMA,INNUGD,INPNCO,INPNSP
      CHARACTER*24 OUVALE,OUPADR,OUPCMP,OUERRE,OUNOMA,OUPNBN,OUNUGD,
     +             OUPNB2
      CHARACTER*24 NREFE,NABSC,NDESC,NNUMND,NINDIR,NNMAIL,OUPNCO,OUPNSP
      CHARACTER*24 VALK
      CHARACTER*19 SDEMNO
      CHARACTER*15 NREPMA
      CHARACTER*8  MAILLA
      CHARACTER*4  DOCU
C
      INTEGER AIPADR,AIPCMP,IOCER,J,ANMAIL,ANUMA,AIPNCO,AIPNSP,AOPNCO
      INTEGER AOPNBN,AOVALE,AOPADR,AOPCMP,AOERRE,AINUGD,AIVALE,AOPNSP
      INTEGER AREFE,ADESC,NBCMP,I,IBID,ANUMND,ACMPGD,LPT,AOPNB2
      INTEGER NBMPST,NBNPST,NBOCER,N,M,ADRIN,ADROU,NBM,NUMM
      INTEGER NBTCMP,SDNUND,SDVACP,AINDIR,PT,NSP,NCO,LMC,LCC,LSC,LMS
      INTEGER INDI1, INDI2
      INTEGER VALI,ILONG,K,L,LNC,NCOM,NSPM
      REAL*8  R8VIDE
C
      LOGICAL TROUVE
      CHARACTER*1 K1BID
C
      CHARACTER*1 CBID
      DATA CBID/' '/
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
      NNUMND  = '&&RVECHN.NUM.NOEUD.LISTE'
      SDEMNO  = '&&RVECHN.SDEMNO   '
      INVALE  = SSCH19//'.VALE'
      INPADR  = SSCH19//'.PADR'
      INPCMP  = SSCH19//'.PCMP'
      INPNCO  = SSCH19//'.PNCO'
      INPNSP  = SSCH19//'.PNSP'
      INNOMA  = SSCH19//'.NOMA'
      INNUGD  = SSCH19//'.NUGD'
      OUVALE  = SDEVAL//'.VALE'
      OUPNBN  = SDEVAL//'.PNBN'
      OUPNB2  = SDEVAL//'.PNB2'
      OUPNCO  = SDEVAL//'.PNCO'
      OUPNSP  = SDEVAL//'.PNSP'
      OUPADR  = SDEVAL//'.PADR'
      OUPCMP  = SDEVAL//'.PCMP'
      OUNOMA  = SDEVAL//'.NOMA'
      OUNUGD  = SDEVAL//'.NUGD'
      OUERRE  = SDEVAL//'.ERRE'
      NABSC   = SDLIEU//'.ABSC'
      NREFE   = SDLIEU//'.REFE'
      NDESC   = SDLIEU//'.DESC'
      CALL JELIRA(INVALE,'DOCU',IBID,DOCU)
      CALL JEVEUO(NREFE,'L',AREFE)
      CALL JEVEUO(NDESC,'L',ADESC)
      CALL JELIRA(JEXNUM(NABSC,1),'LONMAX',NBNPST,K1BID)
      NBMPST = NBNPST
      CALL JELIRA(INPCMP,'LONMAX',NBTCMP,K1BID)
      CALL JEVEUO(INPCMP,'L',AIPCMP)
      CALL WKVECT(OUPCMP,'V V I',NBTCMP,AOPCMP)
C
      NBCMP = 0
      DO 20, I = 1, NBTCMP, 1
         NBCMP            = NBCMP +  MIN(ZI(AIPCMP + I-1),1)
         ZI(AOPCMP + I-1) = ZI(AIPCMP + I-1)
20    CONTINUE
      CALL WKVECT(OUNOMA,'V V K8',1,ADROU)
      CALL JEVEUO(INNOMA,'L',ADRIN)
      MAILLA     = ZK8(ADRIN)
      ZK8(ADROU) = MAILLA
      CALL WKVECT(OUNUGD,'V V I',1,ADROU)
      CALL JEVEUO(INNUGD,'L',AINUGD)
      ZI(ADROU) = ZI(AINUGD)
      NBOCER = NBMPST
C
      CALL JECREC(OUERRE,'V V I','NU','DISPERSE','VARIABLE',NBOCER)
      DO 30, IOCER = 1, NBOCER, 1
         CALL JECROC(JEXNUM(OUERRE,IOCER))
         CALL JEECRA(JEXNUM(OUERRE,IOCER),'LONMAX',NBCMP,' ')
         CALL JEVEUO(JEXNUM(OUERRE,IOCER),'E',AOERRE)
         DO 31, I = 1, NBCMP, 1
            ZI(AOERRE + I-1) = 0
31       CONTINUE
30    CONTINUE
C
      CALL WKVECT(NNUMND,'V V I',NBNPST,ANUMND)
      DO 50, I = 1, NBNPST, 1
         CALL JENONU(JEXNOM(MAILLA//'.NOMNOE',ZK8(ADESC + I-1)),
     +               ZI(ANUMND + I-1))
50    CONTINUE
      CALL WKVECT(OUPADR,'V V I',NBNPST,AOPADR)
      CALL JEVEUO(INVALE,'L',AIVALE)
      CALL JEVEUO(INPADR,'L',AIPADR)
C
      IF ( DOCU .EQ. 'CHNO' ) THEN
         CALL WKVECT(OUVALE,'V V R',NBCMP*NBNPST,AOVALE)
         ZI(AOPADR + 1-1) = 1
         DO 100, I = 1, NBNPST-1, 1
            ZI(AOPADR + I+1-1) = ZI(AOPADR + I-1) + NBCMP
100      CONTINUE
         DO 110, I = 1, NBNPST, 1
            ADRIN = ZI(AIPADR + ZI(ANUMND + I-1)-1)
            ADROU = ZI(AOPADR + I-1)
            DO 120, J = 1, NBCMP, 1
               ZR(AOVALE + ADROU + J-2) = ZR(AIVALE + ADRIN + J-2)
120         CONTINUE
110      CONTINUE
C
      ELSE IF ( DOCU .EQ. 'CHLM' ) THEN
         NINDIR = '&&RVECHN.TABLE.INDIR'
         CALL TREMNO(ZK8(ADESC + 1-1),SSCH19,SDEMNO)
         CALL WKVECT(OUPNBN,'V V I',NBNPST,AOPNBN)
         CALL WKVECT(OUPNB2,'V V I',NBNPST,AOPNB2)
         CALL WKVECT(OUPNCO,'V V I',NBNPST,AOPNCO)
         CALL WKVECT(OUPNSP,'V V I',NBNPST,AOPNSP)
         CALL WKVECT(NINDIR,'V V I',NBNPST,AINDIR)
         CALL JEVEUO(SDEMNO//'.NUND','L',SDNUND)
         CALL JELIRA(SDEMNO//'.NUND','LONMAX',LPT, CBID)
         CALL JEVEUO(INPNCO,'L',AIPNCO)
         CALL JEVEUO(INPNSP,'L',AIPNSP)
         NNMAIL = SDEVAL//'.MAIL'
         NREPMA = MAILLA//'.NOMMAI'
         DO 200, I = 1, NBNPST, 1
            PT     = 1
            TROUVE = .FALSE.
            N = ZI(ANUMND + I-1)
210         CONTINUE
            IF ( (.NOT. TROUVE).AND. (PT .LE. LPT) ) THEN
               IF ( ZI(SDNUND + PT-1) .EQ. N ) THEN
                  TROUVE = .TRUE.
                  CALL JELIRA(JEXNUM(SDEMNO//'.NUMA',PT),'LONMAX',NBM,
     &                        CBID)
                  ZI(AINDIR + I-1) = PT
                  ZI(AOPNB2 + I-1) = NBM
                  ZI(AOPNBN + I-1) = 1
               ENDIF
               PT = PT + 1
               GOTO 210
            ENDIF
            IF (.NOT. TROUVE) THEN
               VALI = N
               VALK = ZK8(ADESC+I-1)
               CALL U2MESG('F', 'POSTRELE_40',1,VALK,1,VALI,0,0.D0)
            ENDIF
            CALL JEVEUO(JEXNUM(SDEMNO//'.NUMA',PT-1),'L',ANUMA)
            NSP = ZI(AIPNSP + ZI(ANUMA)-1)
            NCO = ZI(AIPNCO + ZI(ANUMA)-1)
            DO 205, J = 2, NBM, 1
               NSP = MIN(NSP,ZI(AIPNSP + ZI(ANUMA + J-1)-1))
               NCO = MIN(NCO,ZI(AIPNCO + ZI(ANUMA + J-1)-1))
205         CONTINUE
            ZI(AOPNSP + I-1) = NSP
            ZI(AOPNCO + I-1) = NCO
200      CONTINUE
C
         ZI(AOPADR + 1-1) = 1
         DO 240, I = 1, NBNPST-1, 1
            ZI(AOPADR + I+1-1) = ZI(AOPADR+I-1) + NBCMP*ZI(AOPNBN+I-1)*
     +                           ZI(AOPNCO+I-1)*ZI(AOPNSP+I-1)
240      CONTINUE
         ILONG = ZI(AOPADR+NBNPST-1) + NBCMP*ZI(AOPNBN+NBNPST-1)*
     +           ZI(AOPNCO+NBNPST-1)*ZI(AOPNSP+NBNPST-1) - 1
         CALL WKVECT(OUVALE,'V V R',ILONG,AOVALE)
         CALL JECREC(NNMAIL,'V V K8','NU','DISPERSE','VARIABLE',NBNPST)
         DO 427, I = 1, NBNPST, 1
            L = ZI(AOPNB2 + I-1)
            CALL JECROC(JEXNUM(NNMAIL,I))
            CALL JEECRA(JEXNUM(NNMAIL,I),'LONMAX',L,' ')
            CALL JEVEUO(JEXNUM(NNMAIL,I),'E',ANMAIL)
            CALL JEVEUO(JEXNUM(SDEMNO//'.NUMA',ZI(AINDIR + I-1)),
     +                  'L',ANUMA)
            DO 428, J = 1, L, 1
               CALL JENUNO(JEXNUM(NREPMA,ZI(ANUMA+J-1)),ZK8(ANMAIL+J-1))
428         CONTINUE
427      CONTINUE
         CALL JEDETR(SDEMNO//'.VACP')
         CALL JEDETR(SDEMNO//'.NUMA')
         CALL JEDETR(SDEMNO//'.NOCP')
         CALL JEDETR(SDEMNO//'.NUCP')
         CALL JEDETR(SDEMNO//'.NUND')
         CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',ZI(AINUGD)),'L',ACMPGD)
         DO 220, I =1, NBTCMP, 1
C
            PT = ZI(AOPCMP + I-1)
            IF ( PT .GT. 0 ) THEN
               CALL TREMNO(ZK8(ACMPGD + I-1),SSCH19,SDEMNO)
               DO 221, J = 1, NBNPST, 1
                  CALL JEVEUO(JEXNUM(SDEMNO//'.VACP',ZI(AINDIR + J-1)),
     +                       'L',SDVACP)
                  CALL JEVEUO(JEXNUM(SDEMNO//'.NUMA',ZI(AINDIR + J-1)),
     +                       'L',ANUMA)
                  NSP = ZI(AOPNSP + J-1)
                  NCO = ZI(AOPNCO + J-1)
                  NBM = ZI(AOPNB2 + J-1)
                  LNC = ZI(AOPADR + J-1)
                  LMC = NBCMP*NSP
C*+*                  LCC = LMC*NBM
                  LCC = LMC
                  LMS = 0
                  DO 222, M = 1, NBM, 1
                     NUMM = ZI(ANUMA + M-1)
                     NSPM = ZI(AIPNSP + NUMM-1)
                     NCOM = ZI(AIPNCO + NUMM-1)
                     DO 223, K = 1, NCO, 1
                        LSC  = (K-1)*LCC
                        DO 224, L = 1, NSP, 1
                           INDI1 = LNC-1 + LSC + (L-1)*NBCMP + PT-1
                           INDI2 = LMS + (K-1)*NSPM + L-1
                           IF (ZR(SDVACP+INDI2).EQ.R8VIDE()) GOTO 224
                           ZR(AOVALE+INDI1) = ZR(AOVALE+INDI1) +
     +                                        ZR(SDVACP+INDI2)
224                     CONTINUE
C
223                  CONTINUE
                     LMS = LMS + NSPM*NCOM
222               CONTINUE
C
                  IF ( NBM .GT. 1 ) THEN
                  DO 233, K = 1, NCO, 1
                     LSC  = (K-1)*LCC
                     DO 234, L = 1, NSP, 1
                        INDI1 = LNC-1 + LSC + (L-1)*NBCMP + PT-1
                        ZR(AOVALE+INDI1) = ZR(AOVALE+INDI1) / NBM
234                  CONTINUE
233               CONTINUE
                  ENDIF
C
221            CONTINUE
C
               CALL JEDETR(SDEMNO//'.VACP')
               CALL JEDETR(SDEMNO//'.NUMA')
               CALL JEDETR(SDEMNO//'.NOCP')
               CALL JEDETR(SDEMNO//'.NUCP')
               CALL JEDETR(SDEMNO//'.NUND')
            ENDIF
220      CONTINUE
         CALL JEDETR(NINDIR)
      ELSE
      ENDIF
      CALL JEECRA(OUVALE,'DOCU',IBID,DOCU)
      CALL JEDETR(NNUMND)
      CALL JEDETR(OUPNB2)
      CALL JEDEMA()
      END
