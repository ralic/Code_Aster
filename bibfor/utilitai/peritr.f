      SUBROUTINE PERITR(RESU,MODELE,CARA,NCHAR,LCHAR,NH,NBOCC)
      IMPLICIT NONE
      INTEGER NCHAR,NH,NBOCC
      CHARACTER*(*) RESU,MODELE,CARA,LCHAR(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     OPERATEUR   POST_ELEM
C     TRAITEMENT DU MOT CLE-FACTEUR "RICE_TRACEY"
C     ------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER NBPARR,NBPARD,NUMA,LONG,MXVALE
      INTEGER IFM,ND,NR,NIV,I,NI,NP,NQ,N1,N2,IRET,JORD,JINS
      INTEGER IORD,IAINST,IBID,LVALE,NBIN,IOCC,NT,NM,NC
      INTEGER NG,KK,NBGRMA,JGR,IG,NBMA,JAD,NBMAIL,JMA,IM,NUME,IER
      INTEGER NUMORD,NUMOMU,NBORDR
      PARAMETER (MXVALE=5,NBPARR=6,NBPARD=4)
      REAL*8 R8B,PREC,INST,RSR0,VOLU,NUMEMA,TRIAX,LNRSR0
      REAL*8 VR(5),RTVAL(2),VALER(3)
      CHARACTER*8 K8B,NOMA,RESUL,CRIT,NOMAIL,NOMMAI,LPAIN(7),
     &            LPAOUT(2),TYPARR(NBPARR),TYPARD(NBPARD),VALEK(2)
     &            ,TABCMP(5)
      CHARACTER*16 TYPRES,OPTION,OPTIO2,OPTCAL(2),TOPTCA(2),
     &             NOPARR(NBPARR),NOPARD(NBPARD),TABTYP(3)
      CHARACTER*19 CHELEM,KNUM,KINS,VARNUL
      CHARACTER*24 CHGEOM,CHCARA(18),CHHARM,LIGREL,LCHIN(7)
      CHARACTER*24 MLGGMA,MLGNMA,COMPOR
      CHARACTER*24 LCHOUT(2),CONTG,VARIPG,VARIMG,DEPLA,SSOUP
      CHARACTER*1 K1BID
      COMPLEX*16 C16B
      INTEGER      IARG

      DATA NOPARR/'NUME_ORDRE','INST','LIEU','ENTITE',
     &     'TX_CROIS_CAVITES','VOLUME_CONCERNE'/
      DATA TYPARR/'I','R','K8','K8','R','R'/
      DATA NOPARD/'LIEU','ENTITE','TX_CROIS_CAVITES','VOLUME_CONCERNE'/
      DATA TYPARD/'K8','K8','R','R'/
C      DATA VARIMG /'&&PERITR.VARIMR'/
      DATA VARNUL/'&&PERITR.VARNUL'/
      DATA TABTYP/'NOEU#DEPL_R','NOEU#TEMP_R','ELEM#ENER_R'/
      DATA TABCMP/'TRIAX','RSR0','VOLU','NUMEMA','DEPSEQ'/
C     ------------------------------------------------------------------
      CALL JEMARQ()

C --- RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)

      INST = 0.D0
      CALL GETVID(' ','CHAM_GD',1,IARG,1,CONTG,ND)
      IF(ND.NE.0)THEN
         CALL CHPVE2(CONTG,3,TABTYP,IER)
      ENDIF
      CALL GETVID(' ','RESULTAT',1,IARG,1,RESUL,NR)
      CALL GETVR8(' ','INST',1,IARG,1,INST,NI)
      CALL GETVTX('RICE_TRACEY','OPTION',1,IARG,1,OPTCAL(1),NP)
      CALL GETVTX('RICE_TRACEY','LOCAL',1,IARG,1,OPTCAL(2),NQ)
      IF (NBOCC.GT.1) THEN
        DO 10 I = 2,NBOCC
          CALL GETVTX('RICE_TRACEY','OPTION',I,IARG,1,TOPTCA(1),N1)
          CALL GETVTX('RICE_TRACEY','LOCAL',I,IARG,1,TOPTCA(2),N2)
          IF ((TOPTCA(1).NE.OPTCAL(1)) .OR.
     &        (TOPTCA(2).NE.OPTCAL(2))) CALL U2MESS('F','UTILITAI3_83')
   10   CONTINUE
      END IF

      OPTION = 'RICE_TRACEY'
      CALL MECHAM(OPTION,MODELE,NCHAR,LCHAR,CARA,NH,CHGEOM,CHCARA,
     &            CHHARM,IRET)
      IF (IRET.NE.0) GO TO 110
      NOMA = CHGEOM(1:8)
      MLGNMA = NOMA//'.NOMMAI'
      MLGGMA = NOMA//'.GROUPEMA'

C      NOMLIG = '&&PERITR'
C      CALL EXLIMA ( 'RICE_TRACEY', 'V', MODELE, NOMLIG, LIGREL )
C     IL FAUT FAIRE LE CALCUL SUR TOUT LE MODELE

      LIGREL = MODELE//'.MODELE'

      KNUM = '&&PERITR.NUME_ORDRE'
      KINS = '&&PERITR.INSTANT'
      IF (ND.NE.0) THEN
        NBORDR = 1
        CALL WKVECT(KNUM,'V V I',NBORDR,JORD)
        ZI(JORD) = 1
        CALL WKVECT(KINS,'V V R',NBORDR,JINS)
        ZR(JINS) = INST
        CALL TBCRSD(RESU,'G')
        CALL TBAJPA(RESU,NBPARD,NOPARD,TYPARD)
      ELSE
        CALL GETTCO(RESUL,TYPRES)
        IF (TYPRES(1:9).NE.'EVOL_NOLI') THEN
          CALL U2MESS('F','UTILITAI3_84')
        END IF
        CALL GETVR8(' ','PRECISION',1,IARG,1,PREC,NP)
        CALL GETVTX(' ','CRITERE',1,IARG,1,CRIT,NC)
        CALL RSUTNU(RESUL,' ',0,KNUM,NBORDR,PREC,CRIT,IRET)
        IF (IRET.NE.0) GO TO 100
        CALL JEVEUO(KNUM,'L',JORD)
C        --- ON RECUPERE LES INSTANTS ---
        CALL WKVECT(KINS,'V V R',NBORDR,JINS)
        CALL JENONU(JEXNOM(RESUL//'           .NOVA','INST'),IRET)
        IF (IRET.NE.0) THEN
          DO 20 IORD = 1,NBORDR
            NUMORD = ZI(JORD+IORD-1)
            CALL RSADPA(RESUL,'L',1,'INST',NUMORD,0,IAINST,K8B)
            ZR(JINS+IORD-1) = ZR(IAINST)
   20     CONTINUE
        END IF
        CALL TBCRSD(RESU,'G')
        CALL TBAJPA(RESU,NBPARR,NOPARR,TYPARR)
      END IF

C     --- INITIALISATIONS DES CHAMPS ---

      LNRSR0 = 0.D0
C      VARIPG = '&&PERITR.VARIPG'
      CALL MECACT('V','&&PERITR.SDRMR','MAILLA',NOMA,'NEUT_R',1,'X1',
     &            IBID,0.D0,C16B,K8B)

      CALL WKVECT('&&PERITR.TRAV1','V V R',MXVALE,LVALE)
      DO 90 IORD = 1,NBORDR
        CALL JEMARQ()
        CALL JERECU('V')
        NUMORD = ZI(JORD+IORD-1)
        INST = ZR(JINS+IORD-1)
        VALER(1) = INST

        CALL RSEXCH(RESUL,'COMPORTEMENT',NUMORD,COMPOR,IRET)
        IF (NR.NE.0) THEN
          CALL RSEXCH(RESUL,'SIEF_ELGA',NUMORD,CONTG,IRET)
          IF (IRET.GT.0) THEN
            CALL U2MESS('F','UTILITAI3_85')
          END IF
          CALL RSEXCH(RESUL,'VARI_ELGA',NUMORD,VARIPG,IRET)
          IF (IRET.GT.0) THEN
            CALL U2MESS('F','UTILITAI3_86')
          END IF
          IF (IORD.GE.2) THEN
            NUMOMU = ZI(JORD+IORD-2)
            CALL RSEXCH(RESUL,'VARI_ELGA',NUMOMU,VARIMG,IRET)
            IF (IRET.GT.0) THEN
              CALL U2MESS('F','UTILITAI3_86')
            END IF
          ELSE
            CALL COPISD('CHAMP_GD','V',VARIPG,VARNUL)
            CALL JELIRA(VARNUL//'.CELV','LONUTI',LONG,K1BID)
            CALL JERAZO(VARNUL//'.CELV',LONG,1)
          END IF
          CALL RSEXCH(RESUL,'DEPL',NUMORD,DEPLA,IRET)
          IF (IRET.GT.0) THEN
            CALL U2MESS('F','UTILITAI3_87')
          END IF
        END IF

C        --- AFFECTATION D'UNE CARTE CONSTANTE SUR LE MAILLAGE :
C            OPTION DE CALCUL RICE_TRACEY ---

        SSOUP = OPTCAL(1)//OPTCAL(2)
        CALL MECACT('V','&&PERITR.CH.SOUSOP','MAILLA',NOMA,'NEUT_K24',1,
     &              'Z1',IBID,R8B,C16B,SSOUP)

        OPTIO2 = 'RICE_TRACEY'
        CHELEM = '&&PERITR.RITR'
        NBIN = 7
        LCHIN(1) = CHGEOM
        LPAIN(1) = 'PGEOMER'
        LCHIN(2) = CONTG
        LPAIN(2) = 'PCONTPR'
        IF (IORD.GE.2) THEN
          LCHIN(3) = VARIMG
        ELSE
          LCHIN(3) = VARNUL
        END IF
        LPAIN(3) = 'PVARIMR'
        LCHIN(4) = VARIPG
        LPAIN(4) = 'PVARIPR'
        LCHIN(5) = '&&PERITR.SDRMR'
        LPAIN(5) = 'PSDRMR'
        LCHIN(6) = '&&PERITR.CH.SOUSOP'
        LPAIN(6) = 'PSOUSOP'
        LCHIN(7) = COMPOR
        LPAIN(7) = 'PCOMPOR'
        LCHOUT(1) = CHELEM
        LPAOUT(1) = 'PRICTRA'
        LCHOUT(2) = '&&PERITR.SDRPR'
        LPAOUT(2) = 'PSDRPR'
        CALL CALCUL('S',OPTIO2,LIGREL,NBIN,LCHIN,LPAIN,2,LCHOUT,LPAOUT,
     &              'V','OUI')

        DO 80 IOCC = 1,NBOCC
          CALL GETVTX(OPTION(1:11),'TOUT',IOCC,IARG,0,K8B,NT)
          CALL GETVEM(NOMA,'MAILLE',OPTION(1:11),'MAILLE',IOCC,IARG,0,
     &                K8B,
     &                NM)
          CALL GETVEM(NOMA,'GROUP_MA',OPTION(1:11),'GROUP_MA',IOCC,IARG,
     &                0,
     &                K8B,NG)

          IF (NT.NE.0) THEN
            IF (OPTCAL(2).EQ.'OUI') THEN
              CALL MEMAX('MAX',CHELEM,'RSR0',MXVALE,TABCMP,VR,0,IBID)
              DO 30 KK = 1,MXVALE
                ZR(LVALE+KK-1) = VR(KK)
   30         CONTINUE
            ELSE IF (OPTCAL(2).EQ.'NON') THEN
              CALL MEMOY(CHELEM,1,CHELEM,3,VR,0,IBID)
              ZR(LVALE) = VR(1)
              ZR(LVALE+2) = VR(2)
              TRIAX = ZR(LVALE)
              CALL MEMOY(CHELEM,5,CHELEM,3,VR,0,IBID)
              ZR(LVALE+4) = VR(1)
              LNRSR0 = LNRSR0 + 0.283D0*SIGN(1.D0,TRIAX)*
     &                 EXP(1.5D0*ABS(TRIAX))*ZR(LVALE+4)
              ZR(LVALE+1) = EXP(LNRSR0)
              ZR(LVALE+3) = 0.D0
            END IF
            RSR0 = ZR(LVALE+1)
            VOLU = ZR(LVALE+2)
            NUMEMA = ZR(LVALE+3)
            IF (OPTCAL(2).EQ.'OUI') THEN
              NUMA = NINT(NUMEMA)
              CALL JENUNO(JEXNUM(MLGNMA,NUMA),NOMAIL)
              VALEK(1) = NOMAIL
              VALEK(2) = 'MAILLE'
            ELSE
              VALEK(1) = NOMA
              VALEK(2) = 'TOUT'
            END IF
            RTVAL(1) = RSR0
            RTVAL(2) = VOLU
            IF (NR.NE.0) THEN
              VALER(2) = RTVAL(1)
              VALER(3) = RTVAL(2)
              CALL TBAJLI(RESU,NBPARR,NOPARR,NUMORD,VALER,C16B,VALEK,0)
            ELSE
              CALL TBAJLI(RESU,NBPARD,NOPARD,NUMORD,RTVAL,C16B,VALEK,0)
            END IF
          END IF

          IF (NG.NE.0) THEN
            NBGRMA = -NG
            CALL WKVECT('&&PERITR_GROUPM','V V K8',NBGRMA,JGR)
            CALL GETVEM(NOMA,'GROUP_MA',OPTION(1:11),'GROUP_MA',IOCC,
     &                  IARG,
     &                  NBGRMA,ZK8(JGR),NG)
            DO 50 IG = 1,NBGRMA
              NOMMAI = ZK8(JGR+IG-1)
              CALL JEEXIN(JEXNOM(MLGGMA,NOMMAI),IRET)
              IF (IRET.EQ.0) THEN
                CALL U2MESK('A','UTILITAI3_46',1,NOMMAI)
                GO TO 50
              END IF
              CALL JELIRA(JEXNOM(MLGGMA,NOMMAI),'LONUTI',NBMA,K8B)
              IF (NBMA.EQ.0) THEN
                CALL U2MESK('A','UTILITAI3_47',1,NOMMAI)
                GO TO 50
              END IF
              CALL JEVEUO(JEXNOM(MLGGMA,NOMMAI),'L',JAD)
              IF (OPTCAL(2).EQ.'OUI') THEN
                CALL MEMAX('MAX',CHELEM,'RSR0',MXVALE,TABCMP,
     &                            VR,NBMA,ZI(JAD))
                DO 40 KK = 1,MXVALE
                  ZR(LVALE+KK-1) = VR(KK)
   40           CONTINUE
              ELSE IF (OPTCAL(2).EQ.'NON') THEN
                CALL MEMOY(CHELEM,1,CHELEM,3,VR,NBMA,ZI(JAD))
                ZR(LVALE) = VR(1)
                ZR(LVALE+2) = VR(2)
                TRIAX = ZR(LVALE)
                CALL MEMOY(CHELEM,5,CHELEM,3,VR,NBMA,ZI(JAD))
                ZR(LVALE+4) = VR(1)
                LNRSR0 = LNRSR0 + 0.283D0*SIGN(1.D0,TRIAX)*
     &                   EXP(1.5D0*ABS(TRIAX))*ZR(LVALE+4)
                ZR(LVALE+1) = EXP(LNRSR0)
                ZR(LVALE+3) = 0.D0
              END IF
              RSR0 = ZR(LVALE+1)
              VOLU = ZR(LVALE+2)
              NUMEMA = ZR(LVALE+3)
              IF (OPTCAL(2).EQ.'OUI') THEN
                NUMA = NINT(NUMEMA)
                CALL JENUNO(JEXNUM(MLGNMA,NUMA),NOMAIL)
                VALEK(1) = NOMAIL
                VALEK(2) = 'MAILLE'
              ELSE
                VALEK(1) = NOMA
                VALEK(2) = 'TOUT'
              END IF
              RTVAL(1) = RSR0
              RTVAL(2) = VOLU
              IF (NR.NE.0) THEN
                VALER(2) = RTVAL(1)
                VALER(3) = RTVAL(2)
                CALL TBAJLI(RESU,NBPARR,NOPARR,NUMORD,VALER,C16B,VALEK,
     &                      0)
              ELSE
                CALL TBAJLI(RESU,NBPARD,NOPARD,NUMORD,RTVAL,C16B,VALEK,
     &                      0)
              END IF
   50       CONTINUE
            CALL JEDETR('&&PERITR_GROUPM')
          END IF

          IF (NM.NE.0) THEN
            NBMAIL = -NM
            CALL WKVECT('&&PERITR_MAILLE','V V K8',NBMAIL,JMA)
            CALL GETVEM(NOMA,'MAILLE',OPTION(1:11),'MAILLE',IOCC,IARG,
     &                  NBMAIL,ZK8(JMA),NM)
            DO 70 IM = 1,NBMAIL
              NOMMAI = ZK8(JMA+IM-1)
              CALL JEEXIN(JEXNOM(MLGNMA,NOMMAI),IRET)
              IF (IRET.EQ.0) THEN
                CALL U2MESK('A','UTILITAI3_49',1,NOMMAI)
                GO TO 70
              END IF
              CALL JENONU(JEXNOM(MLGNMA,NOMMAI),NUME)
              IF (OPTCAL(2).EQ.'OUI') THEN
                CALL MEMAX('MAX',CHELEM,'RSR0',MXVALE,TABCMP,VR,1,NUME)
                DO 60 KK = 1,MXVALE
                  ZR(LVALE+KK-1) = VR(KK)
   60           CONTINUE
              ELSE IF (OPTCAL(2).EQ.'NON') THEN
                CALL MEMOY(CHELEM,1,CHELEM,3,VR,1,NUME)
                ZR(LVALE) = VR(1)
                ZR(LVALE+2) = VR(2)
                TRIAX = ZR(LVALE)
                CALL MEMOY(CHELEM,5,CHELEM,3,VR,1,NUME)
                ZR(LVALE+4) = VR(1)
                LNRSR0 = LNRSR0 + 0.283D0*SIGN(1.D0,TRIAX)*
     &                   EXP(1.5D0*ABS(TRIAX))*ZR(LVALE+4)
                ZR(LVALE+1) = EXP(LNRSR0)
                ZR(LVALE+3) = 0.D0
              END IF
              RSR0 = ZR(LVALE+1)
              VOLU = ZR(LVALE+2)
              NUMEMA = ZR(LVALE+3)
              IF (OPTCAL(2).EQ.'OUI') THEN
                NUMA = NINT(NUMEMA)
                CALL JENUNO(JEXNUM(MLGNMA,NUMA),NOMAIL)
                VALEK(1) = NOMAIL
                VALEK(2) = 'MAILLE'
              ELSE
                VALEK(1) = NOMA
                VALEK(2) = 'TOUT'
              END IF
              RTVAL(1) = RSR0
              RTVAL(2) = VOLU
              IF (NR.NE.0) THEN
                VALER(2) = RTVAL(1)
                VALER(3) = RTVAL(2)
                CALL TBAJLI(RESU,NBPARR,NOPARR,NUMORD,VALER,C16B,VALEK,
     &                      0)
              ELSE
                CALL TBAJLI(RESU,NBPARD,NOPARD,NUMORD,RTVAL,C16B,VALEK,
     &                      0)
              END IF
   70       CONTINUE
            CALL JEDETR('&&PERITR_MAILLE')
          END IF
   80   CONTINUE
        CALL COPISD('CHAMP_GD','V','&&PERITR.SDRPR','&&PERITR.SDRMR')
        CALL JEDETR('&&PERITR.PAR')
        CALL JEDETR('&&PERITR.EPSP')
        CALL JEDETR('&&PERITR.CH.SOUSOP')
        CALL JEDETR(CHELEM)
        CALL JEDEMA()
   90 CONTINUE

  100 CONTINUE
      CALL JEDETR(KNUM)
      CALL JEDETR(KINS)
      CALL JEDETC('V','&&PERITR',1)
  110 CONTINUE
      CALL JEDEMA()
      END
