      SUBROUTINE CCCHUC(RESUIN, RESUOU, CHIN,   NCHOUT, CRIT,
     &                  NF,     NFOR,   LISORD, NBORDR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 22/04/2013   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      NBORDR,NCHOUT,NF
      CHARACTER*8  RESUIN,RESUOU,NFOR(NF)
      CHARACTER*16 CHIN,CRIT
      CHARACTER*19 LISORD
C RESPONSABLE COURTOIS M.COURTOIS
C ----------------------------------------------------------------------
C  CALC_CHAMP - TRAITEMENT DE CHAM_UTIL - CRITERE
C  -    -                     --   -      -
C ----------------------------------------------------------------------
C IN  :
C   RESUIN K8    NOM DE LA SD IN
C   RESUOU K8    NOM DE LA SD OUT
C   CHIN   K16   NOM DU CHAMP EN ENTREE
C   CHOUT  K16   NOM DU CHAMP EN SORTIE
C   CRIT   K16   NOM DU CRITERE A CALCULER (UTILISE SI NF=0)
C   NF     I     NOMBRE DE FORMULES
C   NFOR   K8(*) NOMS DES FORMULES
C   LISORD K19   NOM DE LA LISTE DES NUMEROS D'ORDRE
C   NBORDR I     NOMBRE DE NUMEROS D'ORDRE
C ----------------------------------------------------------------------
      INTEGER      JORDR,I,IRET,IMA,IORDR,IPT,ISP,ICMP,NBAJ
      INTEGER      JCHSD,JCHSL,JCHSV,JCHSC,JCHRD,JCHRL,JCHRV,JVAL
      INTEGER      JVRES,IAD,IBID,JCMP,JLIMA,JLAST,ICHK,IV
      INTEGER      NBMA,NBPT,NBSP,NBCMP,NBCMPP,NBCMPR
      INTEGER      VALI(3)
      LOGICAL      IDEM
      REAL*8       RBID
      CHARACTER*2  CNUM
      CHARACTER*4  TYPCES
      CHARACTER*8  MA,MODEL,NOMGD
      CHARACTER*16 CHOUT,TYPS,VALK(3)
      CHARACTER*19 CHS,CHR,WKIN,LIGREL
      CHARACTER*19 WKOUT,WKCMP,WLIMA,WLAST
      CHARACTER*24 CHPS,CHRES,NOOJB
      CHARACTER*8  NOMPRO
      DATA NOMPRO /'&&CCCHUC'/
C     ----- FIN  DECLARATIONS ------------------------------------------
C
      CALL JEMARQ()
      TYPCES = '    '
      NOMGD = '        '
      CHOUT = '                '
      CHS = NOMPRO//'.CHSIN'
      CHR = NOMPRO//'.CHSOUT'
      CHRES = NOMPRO//'.CHRES'
      WKIN = NOMPRO//'.VALIN'
      WKCMP = NOMPRO//'.CMPS'
      WKOUT = NOMPRO//'.VALRES'
      WLIMA = NOMPRO//'.WLIMA'
      WLAST = NOMPRO//'.WLAST'
      LIGREL = 'NOT_INIT'
      CALL CODENT(NCHOUT,'D0',CNUM)
      CALL ASSERT((NF.EQ.0 .AND. CRIT.NE.' ')
     &        .OR.(NF.NE.0 .AND. CRIT.EQ.' '))
C
C     RECUPERATION DE LA LISTE DE NUMEROS D'ORDRE
      CALL JEVEUO(LISORD,'L',JORDR)
C
C --- BOUCLE SUR LES NUMEROS D'ORDRE
      DO 10 I = 1,NBORDR
        IORDR = ZI(JORDR-1+I)
C       TEST L'EXISTENCE DANS RESUIN OU RESUOU
        CALL RSEXCH(' ',RESUIN,CHIN,IORDR,CHPS,IRET)
        IF (IRET.NE.0) THEN
          IF (RESUIN.EQ.RESUOU) THEN
            VALK(1) = CHIN
            VALK(2) = RESUIN
            VALI(1) = IORDR
            CALL U2MESG('F','CHAMPS_6',2,VALK,1,VALI,0,RBID)
          ELSE
            CALL RSEXCH(' ',RESUOU,CHIN,IORDR,CHPS,IRET)
            IF (IRET.NE.0) THEN
              VALK(1) = CHIN
              VALK(2) = RESUIN
              VALK(3) = RESUOU
              VALI(1) = IORDR
              CALL U2MESG('F','CHAMPS_9',3,VALK,1,VALI,0,RBID)
            ENDIF
          ENDIF
        ENDIF
        IF (I.EQ.1) THEN
          CALL DISMOI('F','NOM_GD',CHPS,'CHAMP',IBID,NOMGD,IRET)
          CALL DISMOI('F','TYPE_CHAMP',CHPS,'CHAMP',IBID,TYPCES,IRET)
          CALL ASSERT(TYPCES.NE.'CART' .AND. TYPCES.NE.'RESL')
          CHOUT = 'UT'//CNUM//'_'//TYPCES
        ENDIF
        CALL DISMOI('F','NOM_MAILLA',CHPS,'CHAMP',IBID,MA,IRET)
        IF (NF.EQ.0) THEN
          CALL CCCHCI(CRIT,'NBCMP',NBCMPR)
        ELSE
          NBCMPR = NF
        ENDIF
C
C       CHAM_NO_S OU CHAM_ELEM_S ?
        IF (TYPCES.EQ.'NOEU') THEN
C
C ------- TRAITEMENT DES CHAM_ELEM
          TYPS = 'CHAM_NO_S'
C
          CALL CNOCNS(CHPS,'V',CHS)
          CALL JEVEUO(CHS//'.CNSD','L',JCHSD)
          CALL JEVEUO(CHS//'.CNSC','L',JCHSC)
          CALL JEVEUO(CHS//'.CNSV','L',JCHSV)
          CALL JEVEUO(CHS//'.CNSL','L',JCHSL)
C
C         CREATION DU CHAM_NO_S RESULTAT
          CALL WKVECT(WKCMP,'V V K8',NBCMPR,JCMP)
          DO 100 ICMP=1,NBCMPR
            CALL CODENT(ICMP, 'G', CNUM)
            ZK8(JCMP-1+ICMP) = 'X'//CNUM
 100      CONTINUE
          CALL CNSCRE(MA,'NEUT_R',NBCMPR,ZK8(JCMP),'V',CHR)
          CALL JEDETR(WKCMP)

          CALL JEVEUO(CHR//'.CNSD','E',JCHRD)
          CALL JEVEUO(CHR//'.CNSL','E',JCHRL)
          CALL JEVEUO(CHR//'.CNSV','E',JCHRV)
C         VECTEURS DE TRAVAIL DES VALEURS PAR COMPOSANTE
          NBCMP = ZI(JCHSD-1+2)
          CALL WKVECT(WKIN,'V V R',NBCMP,JVAL)
          CALL WKVECT(WKCMP,'V V K8',NBCMP,JCMP)
          CALL WKVECT(WKOUT,'V V R',NBCMPR,JVRES)
C
          NBPT = ZI(JCHSD-1+1)
          NBAJ = 0
          DO 110 IPT = 1,NBPT
            CALL JEUNDF(WKIN)
            CALL JEUNDF(WKCMP)
            IV = 0
            DO 112 ICMP = 1,NBCMP
              IF (ZL(JCHSL-1+(IPT-1)*NBCMP+ICMP)) THEN
                IV = IV + 1
                ZR(JVAL-1+IV) = ZR(JCHSV-1+(IPT-1)*NBCMP+ICMP)
                ZK8(JCMP-1+IV) = ZK8(JCHSC-1+ICMP)
              ENDIF
 112        CONTINUE
C
            IF (NF.EQ.0) THEN
              CALL CCCHCR(CRIT,NOMGD,IV,ZR(JVAL),ZK8(JCMP),
     &                    NBCMPR,ZR(JVRES),ICHK)
            ELSE
              CALL CCCHCF(NFOR,IV,ZR(JVAL),ZK8(JCMP),NBCMPR,
     &                    ZR(JVRES),ICHK)
            ENDIF
            IF (ICHK.NE.0) THEN
              GOTO 110
            ENDIF
C
            NBAJ = NBAJ + 1
            DO 114 ICMP = 1,NBCMPR
                ZL(JCHRL-1+(IPT-1)*NBCMPR+ICMP) = .TRUE.
                ZR(JCHRV-1+(IPT-1)*NBCMPR+ICMP) = ZR(JVRES-1+ICMP)
 114        CONTINUE
 110      CONTINUE

          VALI(1) = IORDR
          VALI(2) = NBAJ
          VALI(3) = NBPT
          CALL U2MESI('I','CHAMPS_10',3,VALI)
C
C         STOCKAGE DU CHAMP
          CALL RSEXCH(' ',RESUOU,CHOUT,IORDR,CHRES,IRET)
          IF (IRET.NE.100) THEN
             VALK(1) = CHOUT
             VALK(2) = RESUOU
             CALL U2MESK('F', 'CHAMPS_14', 2, VALK)
          ENDIF
          CALL CNSCNO(CHR,' ','UNUSED','G',CHRES,'F',IRET)
          CALL ASSERT(IRET.EQ.0)
          CALL RSNOCH(RESUOU,CHOUT,IORDR)
C
          CALL DETRSD(TYPS,CHS)
          CALL DETRSD(TYPS,CHR)
          CALL JEDETR(WKCMP)
          CALL JEDETR(WKIN)
          CALL JEDETR(WKOUT)
C
C ------- ENDIF CHAM_NO
        ELSE
C
C ------- TRAITEMENT DES CHAM_ELEM
          TYPS = 'CHAM_ELEM_S'
C
          IF (I.EQ.1) THEN
            CALL DISMOI('F','NOM_LIGREL',CHPS,'CHAMP',IBID,LIGREL,IRET)
          ENDIF
C
          CALL CELCES(CHPS,'V',CHS)
          CALL JEVEUO(CHS//'.CESD','L',JCHSD)
          CALL JEVEUO(CHS//'.CESL','L',JCHSL)
          CALL JEVEUO(CHS//'.CESV','L',JCHSV)
          CALL JEVEUO(CHS//'.CESC','L',JCHSC)
C
C         CREATION DU CHAM_ELEM_S RESULTAT
          CALL CESCRM('V',CHR,TYPCES,'NEUT_R',NBCMPR,' ',CHS)

          CALL JEVEUO(CHR//'.CESD','E',JCHRD)
          CALL JEVEUO(CHR//'.CESL','E',JCHRL)
          CALL JEVEUO(CHR//'.CESV','E',JCHRV)
C         VECTEURS DE TRAVAIL DES VALEURS PAR COMPOSANTE
          NBCMP = ZI(JCHSD-1+2)
          CALL WKVECT(WKIN,'V V R',NBCMP,JVAL)
          CALL WKVECT(WKCMP,'V V K8',NBCMP,JCMP)
          CALL WKVECT(WKOUT,'V V R',NBCMPR,JVRES)
C
          NBMA = ZI(JCHSD-1+1)
          CALL WKVECT(WLIMA,'V V I',NBMA,JLIMA)
          NBAJ = 0
          DO 200 IMA = 1,NBMA
            ICHK = -1
            NBPT = ZI(JCHSD-1+5+4*(IMA-1)+1)
            NBSP = ZI(JCHSD-1+5+4*(IMA-1)+2)
            NBCMPP = ZI(JCHSD-1+5+4*(IMA-1)+3)
            DO 210 IPT = 1,NBPT
              DO 212 ISP = 1,NBSP
                CALL JEUNDF(WKIN)
                CALL JEUNDF(WKCMP)
                IV = 0
                DO 214 ICMP = 1,NBCMPP
                  CALL CESEXI('S',JCHSD,JCHSL,IMA,IPT,ISP,ICMP,IAD)
                  IF (IAD.GT.0) THEN
                    IV = IV + 1
                    ZR(JVAL-1+IV) = ZR(JCHSV-1+IAD)
                    ZK8(JCMP-1+IV) = ZK8(JCHSC-1+ICMP)
                  ENDIF
 214            CONTINUE
C
                IF (NF.EQ.0) THEN
                  CALL CCCHCR(CRIT,NOMGD,IV,ZR(JVAL),ZK8(JCMP),
     &                        NBCMPR,ZR(JVRES),ICHK)
                ELSE
                  CALL CCCHCF(NFOR,IV,ZR(JVAL),ZK8(JCMP),NBCMPR,
     &                        ZR(JVRES),ICHK)
                ENDIF
                IF (ICHK.NE.0) THEN
                  GOTO 200
                ENDIF
C
                DO 216 ICMP = 1,NBCMPR
                  CALL CESEXI('S',JCHRD,JCHRL,IMA,IPT,ISP,ICMP,IAD)
                  IAD = -IAD
                  ZL(JCHRL-1+IAD) = .TRUE.
                  ZR(JCHRV-1+IAD) = ZR(JVRES-1+ICMP)
 216            CONTINUE
 212          CONTINUE
 210        CONTINUE
            IF (ICHK.EQ.0) THEN
C             ON CONSERVE LA MAILLE SI LE CRITERE A PU ETRE CALCULE SUR
C             TOUS SES POINTS
              NBAJ = NBAJ + 1
              ZI(JLIMA-1+NBAJ) = IMA
            ENDIF
 200      CONTINUE

          VALI(1) = IORDR
          VALI(2) = NBAJ
          VALI(3) = NBMA
          CALL U2MESI('I','CHAMPS_8',3,VALI)
C
C         FAUT-IL CREER UN NOUVEAU LIGREL ?
          IDEM = .TRUE.
          CALL JEEXIN(WLAST, IRET)
          IF (IRET.EQ.0) THEN
C           ON STOCKE EN INDICE 1 LE NOMBRE DE MAILLES DU LIGREL
            CALL WKVECT(WLAST,'V V I',NBMA+1,JLAST)
            IF (NBAJ.NE.NBMA) THEN
              IDEM = .FALSE.
            ENDIF
          ELSE
            IF (ZI(JLAST-1+1).NE.NBAJ)
     &        GOTO 51
            DO 50 IMA=1,NBAJ
              IF (ZI(JLAST-1+IMA+1).NE.ZI(JLIMA-1+IMA)) THEN
                IDEM = .FALSE.
                GOTO 51
              ENDIF
 50         CONTINUE
 51         CONTINUE
          ENDIF
          ZI(JLAST-1+1) = NBAJ
          DO 52 IMA=1,NBAJ
            ZI(JLAST-1+IMA+1) = ZI(JLIMA-1+IMA)
 52       CONTINUE
C
          IF (.NOT.IDEM) THEN
C           SI PAS LES MEMES MAILLES, ON CREE UN NOUVEAU LIGREL
            CALL DISMOI('F','NOM_MODELE',CHPS,'CHAMP',IBID,MODEL,IBID)
            NOOJB='12345678.LIGR000000.NBNO'
            CALL GNOMSD(' ',NOOJB,14,19)
            LIGREL=NOOJB(1:19)
            CALL EXLIM1(ZI(JLIMA),NBAJ,MODEL,'G',LIGREL)
          ENDIF
C
C         STOCKAGE DU CHAMP
          CALL RSEXCH(' ',RESUOU,CHOUT,IORDR,CHRES,IRET)
          IF (IRET.NE.100) THEN
             VALK(1) = CHOUT
             VALK(2) = RESUOU
             CALL U2MESK('F', 'CHAMPS_14', 2, VALK)
          ENDIF
          CALL CESCEL(CHR,LIGREL,' ',' ','NAN',IBID,'G',CHRES,'F',IRET)
          CALL ASSERT(IRET.EQ.0)
          CALL RSNOCH(RESUOU,CHOUT,IORDR)
C
          CALL DETRSD(TYPS,CHS)
          CALL DETRSD(TYPS,CHR)
          CALL JEDETR(WKCMP)
          CALL JEDETR(WKIN)
          CALL JEDETR(WKOUT)
          CALL JEDETR(WLIMA)
C
C ------- ENDIF CHAM_ELEM
        ENDIF
C
 10   CONTINUE
C
      CALL JEDETR(WLAST)
C
      CALL JEDEMA()
C
      END
