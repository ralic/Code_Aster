      SUBROUTINE CELCES(CELZ,BASEZ,CESZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 16/06/2009   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE PELLET J.PELLET
C A_UTIL
      IMPLICIT NONE
      CHARACTER*(*) CELZ,CESZ,BASEZ
C ------------------------------------------------------------------
C BUT : TRANSFORMER UN CHAM_ELEM (CELZ) EN CHAM_ELEM_S (CESZ)
C       LES ELEMENTS DONT LA MAILLE SUPPORT EST TARDIVE SONT
C       IGNORES.
C ------------------------------------------------------------------
C     ARGUMENTS:
C CELZ    IN/JXIN  K19 : SD CHAM_ELEM A TRANSFORMER
C BASEZ   IN       K1  : BASE DE CREATION POUR CESZ : G/V/L
C CESZ    IN/JXOUT K19 : SD CHAM_ELEM_S A CREER
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C     ------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      CHARACTER*1 KBID,BASE
      CHARACTER*3 TSCA
      CHARACTER*4 TYPCES,KMPIC
      CHARACTER*8 MA,NOMGD,LICMP(503)
      CHARACTER*19 CEL,CES,LIGREL
      LOGICAL EXISDG,DIFF
      INTEGER NEC,GD,NCMPMX,IBID,NBMA,JCMPGD,JNUCMP,JCELV,JCELD
      INTEGER IADG,ICMP,NCMP1,JCESL,JCESV
      INTEGER IEQ,ICMP1,IGR,IEL,IALIEL,ILLIEL,NUMAIL
      INTEGER JCELK,NBPT,NBGR,IMOLO,JMOLO,K
      INTEGER IPT,NBELEM,NUMA,IAD,JNBPT,JNBSPT,JNBCMP
      INTEGER NPTMX,NBEL,NCMPM,NBSPT,NCDYN,NCDYMX,LGCATA
      INTEGER ICO,ADIEL,ISPT,JCESD,JLPT,JLCUPT,CUMU

      NUMAIL(IGR,IEL) = ZI(IALIEL-1+ZI(ILLIEL+IGR-1)+IEL-1)
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CEL = CELZ
      CES = CESZ
      BASE = BASEZ

C     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :

C     -- SI CES EXISTE DEJA, ON LE DETRUIT :
      CALL DETRSD('CHAM_ELEM_S',CES)


C     1- CREATION DU CHAM_ELEM_S VIERGE :
C     -------------------------------------------


C     1.1 CALCUL DE MA,NOMGD,LIGREL,GD,NEC,TSCA,NCMPMX,NBMA :
C     --------------------------------------------------------
      CALL DISMOI('F','NOM_MAILLA',CEL,'CHAM_ELEM',IBID,MA,IBID)
      CALL DISMOI('F','NOM_GD',CEL,'CHAM_ELEM',IBID,NOMGD,IBID)
      CALL DISMOI('F','NOM_LIGREL',CEL,'CHAM_ELEM',IBID,LIGREL,IBID)

C     -- SI CEL N'EST PAS MPI_COMPLET, ON LE COMPLETE :
      CALL DISMOI('F','MPI_COMPLET',CEL,'CHAM_ELEM',IBID,KMPIC,IBID)
      CALL ASSERT((KMPIC.EQ.'OUI').OR.(KMPIC.EQ.'NON'))
      IF (KMPIC.EQ.'NON')  CALL SDMPIC('CHAM_ELEM',CEL)

      CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMA,KBID,IBID)

      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,KBID,IBID)
      CALL DISMOI('F','NB_CMP_MAX',NOMGD,'GRANDEUR',NCMPMX,KBID,IBID)
      CALL DISMOI('F','NUM_GD',NOMGD,'GRANDEUR',GD,KBID,IBID)
      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',JCMPGD)


C     1.2 RECUPERATION DES OBJETS DU CHAM_ELEM ET DU LIGREL :
C     -------------------------------------------------------
      CALL JEVEUO(CEL//'.CELK','L',JCELK)
      CALL JEVEUO(CEL//'.CELV','L',JCELV)
      CALL JEVEUO(CEL//'.CELD','L',JCELD)
      CALL JEVEUO(LIGREL//'.LIEL','L',IALIEL)
      CALL JEVEUO(JEXATR(LIGREL//'.LIEL','LONCUM'),'L',ILLIEL)
      NBGR = ZI(JCELD-1+2)


C     1.3  CALCUL DE  NCMP1, LICMP(NCMP1)
C          LICMP CONTIENT LES CMPS PRESENTES DANS LE CATALOGUE
C          DES TYPE_ELEM. ELLES SONT DANS L'ORDRE DU CATALOGUE
C          DES GRANDEURS

C          CALCUL DE  .TMP_NUCMP :
C          CET OBJET PERMET D'OBTENIR LA CORRESPONDANCE :
C          ICMP (DANS CATALOGUE GRANDEURS) -> NUCMP (DANS LICMP)
C     ----------------------------------------------------------

C     1.3.1 : ON "COCHE" LES CMPS PRESENTES SUR LES ELEMENTS:
C     -------------------------------------------------------
      CALL WKVECT('&&CELCES.TMP_NUCMP','V V I',NCMPMX,JNUCMP)
      DO 30 IGR = 1,NBGR
        IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
        IF (IMOLO.EQ.0) GO TO 30
        CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
        CALL ASSERT(ZI(JMOLO-1+1).LE.3)
        CALL ASSERT(ZI(JMOLO-1+2).EQ.GD)
        DIFF = (ZI(JMOLO-1+4).GT.10000)
        NBPT = MOD(ZI(JMOLO-1+4),10000)

        DO 20,IPT = 1,NBPT
          K = 1
          IF (DIFF) K = IPT
          IADG = JMOLO - 1 + 4 + (K-1)*NEC + 1
          DO 10,ICMP = 1,NCMPMX
            IF (EXISDG(ZI(IADG),ICMP)) THEN
              ZI(JNUCMP-1+ICMP) = 1
            END IF
   10     CONTINUE
   20   CONTINUE

   30 CONTINUE


C     1.3.2 : ON NUMEROTE DES CMPS DE LICMP :
C     ----------------------------------------
      NCMP1 = 0
      DO 40,ICMP = 1,NCMPMX
        IF (ZI(JNUCMP-1+ICMP).EQ.1) THEN
          NCMP1 = NCMP1 + 1
          ZI(JNUCMP-1+ICMP) = NCMP1
          CALL ASSERT(NCMP1.LE.503)
          LICMP(NCMP1) = ZK8(JCMPGD-1+ICMP)
        END IF
   40 CONTINUE


C     1.4 CALCUL DE  NBPT(IMA), NBSPT(IMA), NBCMP(IMA)
C         CALCUL DE  NPTMX : MAXIMUM DU NOMBRE DE POINTS
C         CALCUL DE  NCDYMX : MAXIMUM DU NOMBRE DE VARI_*
C     ---------------------------------------------------------
      CALL WKVECT('&&CELCES.NBPT','V V I',NBMA,JNBPT)
      CALL WKVECT('&&CELCES.NBSPT','V V I',NBMA,JNBSPT)
      DO 50,NUMA = 1,NBMA
        ZI(JNBSPT-1+NUMA) = 1
   50 CONTINUE
      CALL WKVECT('&&CELCES.NBCMP','V V I',NBMA,JNBCMP)
      NPTMX = 0
      NCDYMX = 0

      DO 90,IGR = 1,NBGR
        NBEL = NBELEM(LIGREL,IGR)
        IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
        IF (IMOLO.EQ.0) GO TO 90

        CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
        CALL ASSERT(ZI(JMOLO-1+1).LE.3)
        CALL ASSERT(ZI(JMOLO-1+2).EQ.GD)
        DIFF = (ZI(JMOLO-1+4).GT.10000)
        NBPT = MOD(ZI(JMOLO-1+4),10000)
        NPTMX = MAX(NPTMX,NBPT)


C       -- CALCUL DE NCMPM : NUMERO MAX DES CMPS PORTEES
C          PAR LES ELEMENTS DU GREL
        NCMPM = 0
        DO 70,IPT = 1,NBPT
          K = 1
          IF (DIFF) K = IPT
          IADG = JMOLO - 1 + 4 + (K-1)*NEC + 1
          DO 60,ICMP = 1,NCMPMX
            IF (EXISDG(ZI(IADG),ICMP)) THEN
              NCMPM = MAX(NCMPM,ICMP)
            END IF
   60     CONTINUE
   70   CONTINUE


        DO 80,IEL = 1,NBEL
          NUMA = NUMAIL(IGR,IEL)
          IF (NUMA.LT.0) GO TO 80

C         -- NOMBRE DE POINTS:
          ZI(JNBPT-1+NUMA) = NBPT

C         -- NOMBRE DE SOUS-POINTS:
          NBSPT = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+1)
          ZI(JNBSPT-1+NUMA) = NBSPT

C         -- NOMBRE DE CMPS:
          NCDYN = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+2)
          NCDYN = MAX(NCDYN,1)
          NCDYMX = MAX(NCDYMX,NCDYN)
          IF (NOMGD(1:5).EQ.'VARI_') THEN
            ZI(JNBCMP-1+NUMA) = NCDYN
          ELSE
            ZI(JNBCMP-1+NUMA) = ZI(JNUCMP-1+NCMPM)
          END IF

   80   CONTINUE
   90 CONTINUE
      CALL ASSERT(NPTMX.NE.0)



C     1.6 ALLOCATION DE CES :
C     -------------------------------------------
      CALL DISMOI('F','TYPE_CHAMP',CEL,'CHAM_ELEM',IBID,TYPCES,IBID)
      IF (NOMGD(1:5).EQ.'VARI_') NCMP1 = -NCDYMX
      CALL CESCRE(BASE,CES,TYPCES,MA,NOMGD,NCMP1,LICMP,ZI(JNBPT),
     &            ZI(JNBSPT),ZI(JNBCMP))

C======================================================================

C     2- REMPLISSAGE DE CES.CESL ET CES.CESV :
C     -------------------------------------------
      CALL JEVEUO(CES//'.CESD','E',JCESD)
      CALL JEVEUO(CES//'.CESL','E',JCESL)
      CALL JEVEUO(CES//'.CESV','E',JCESV)


      IF (NOMGD(1:5).NE.'VARI_') THEN
C     ----------------------------
        CALL WKVECT('&&CELCES.LONG_PT','V V I',NPTMX,JLPT)
        CALL WKVECT('&&CELCES.LONG_PT_CUMU','V V I',NPTMX,JLCUPT)
        DO 170,IGR = 1,NBGR
          IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
          IF (IMOLO.EQ.0) GO TO 170


          CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
          DIFF = (ZI(JMOLO-1+4).GT.10000)
          NBPT = MOD(ZI(JMOLO-1+4),10000)
          NBEL = NBELEM(LIGREL,IGR)

C         -- CALCUL DU NOMBRE DE CMPS POUR CHAQUE POINT
C            ET DU CUMUL SUR LES POINTS PRECEDENTS :
          DO 110,IPT = 1,NBPT
            ICO = 0
            K = 1
            IF (DIFF) K = IPT
            IADG = JMOLO - 1 + 4 + (K-1)*NEC + 1
            DO 100,ICMP = 1,NCMPMX
              IF (EXISDG(ZI(IADG),ICMP)) ICO = ICO + 1
  100       CONTINUE
            ZI(JLPT-1+IPT) = ICO
  110     CONTINUE

          CUMU = 0
          DO 120,IPT = 1,NBPT
            ZI(JLCUPT-1+IPT) = CUMU
            CUMU = CUMU + ZI(JLPT-1+IPT)
  120     CONTINUE


          DO 160,IPT = 1,NBPT
            K = 1
            IF (DIFF) K = IPT
            IADG = JMOLO - 1 + 4 + (K-1)*NEC + 1
            ICO = 0
            DO 150,ICMP = 1,NCMPMX
              IF (EXISDG(ZI(IADG),ICMP)) THEN
                ICO = ICO + 1
                ICMP1 = ZI(JNUCMP-1+ICMP)

                DO 140,IEL = 1,NBEL
                  NUMA = NUMAIL(IGR,IEL)
                  IF (NUMA.LT.0) GO TO 140

                  NBSPT = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+1)
                  ADIEL = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+4)

                  DO 130,ISPT = 1,NBSPT
                    CALL CESEXI('S',JCESD,JCESL,NUMA,IPT,ISPT,ICMP1,IAD)
                    IAD = ABS(IAD)
                    ZL(JCESL-1+IAD) = .TRUE.

                    IEQ = ADIEL - 1 + NBSPT*ZI(JLCUPT-1+IPT) +
     &                    (ISPT-1)*ZI(JLPT-1+IPT) + ICO

                    IF (TSCA.EQ.'R') THEN
                      ZR(JCESV-1+IAD) = ZR(JCELV-1+IEQ)
                    ELSE IF (TSCA.EQ.'I') THEN
                      ZI(JCESV-1+IAD) = ZI(JCELV-1+IEQ)
                    ELSE IF (TSCA.EQ.'C') THEN
                      ZC(JCESV-1+IAD) = ZC(JCELV-1+IEQ)
                    ELSE IF (TSCA.EQ.'L') THEN
                      ZL(JCESV-1+IAD) = ZL(JCELV-1+IEQ)
                    ELSE IF (TSCA.EQ.'K8') THEN
                      ZK8(JCESV-1+IAD) = ZK8(JCELV-1+IEQ)
                    ELSE IF (TSCA.EQ.'K16') THEN
                      ZK16(JCESV-1+IAD) = ZK16(JCELV-1+IEQ)
                    ELSE IF (TSCA.EQ.'K24') THEN
                      ZK24(JCESV-1+IAD) = ZK24(JCELV-1+IEQ)
                    ELSE
                      CALL ASSERT(.FALSE.)
                    END IF
  130             CONTINUE
  140           CONTINUE
              END IF
  150       CONTINUE
  160     CONTINUE
  170   CONTINUE


      ELSE
C       -- CAS DE VARI_* :
C       -------------------
        DO 220,IGR = 1,NBGR
          IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
          IF (IMOLO.EQ.0) GO TO 220


          LGCATA = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+3)
          CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
          DIFF = (ZI(JMOLO-1+4).GT.10000)
          CALL ASSERT(.NOT.DIFF)
          NBPT = MOD(ZI(JMOLO-1+4),10000)
          CALL ASSERT(NBPT.EQ.LGCATA)
          NBEL = NBELEM(LIGREL,IGR)

          DO 210,IEL = 1,NBEL
            NUMA = NUMAIL(IGR,IEL)
            IF (NUMA.LT.0) GO TO 210

            NBSPT = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+1)
            NCDYN = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+2)
            ADIEL = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+4)
            DO 200,IPT = 1,NBPT
              DO 190,ISPT = 1,NBSPT
                DO 180,ICMP = 1,NCDYN

                  CALL CESEXI('S',JCESD,JCESL,NUMA,IPT,ISPT,ICMP,IAD)
                  IAD = ABS(IAD)
                  ZL(JCESL-1+IAD) = .TRUE.

                  IEQ = ADIEL - 1 + ((IPT-1)*NBSPT+ISPT-1)*NCDYN + ICMP

                  IF (TSCA.EQ.'R') THEN
                    ZR(JCESV-1+IAD) = ZR(JCELV-1+IEQ)
                  ELSE
                    CALL ASSERT(.FALSE.)
                  END IF
  180           CONTINUE
  190         CONTINUE
  200       CONTINUE
  210     CONTINUE
  220   CONTINUE
      END IF


      CALL JEDETR('&&CELCES.TMP_NUCMP')
      CALL JEDETR('&&CELCES.NBPT')
      CALL JEDETR('&&CELCES.NBSPT')
      CALL JEDETR('&&CELCES.NBCMP')
      CALL JEDETR('&&CELCES.LONG_PT')
      CALL JEDETR('&&CELCES.LONG_PT_CUMU')
      CALL JEDEMA()
      END
