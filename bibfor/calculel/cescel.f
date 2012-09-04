      SUBROUTINE CESCEL(CESZ,LIGREZ,OPTINI,NOMPAZ,PROLZ,NNCP,BASEZ,CELZ,
     &                  KSTOP,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 04/09/2012   AUTEUR PELLET J.PELLET 
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

C RESPONSABLE PELLET J.PELLET
C TOLE CRP_20
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) CESZ,CELZ,BASEZ,LIGREZ,OPTINI,NOMPAZ,PROLZ
      CHARACTER*1 KSTOP
      INTEGER NNCP,IRET
C ------------------------------------------------------------------
C BUT : TRANSFORMER UN CHAM_ELEM_S (CESZ) EN CHAM_ELEM (CELZ)
C ------------------------------------------------------------------
C ARGUMENTS:
C ==========
C CESZ    IN/JXIN  K19 : SD CHAM_ELEM_S A TRANSFORMER
C LIGREZ  IN/JXIN  K19 : SD LIGREL QUI SERA ASSOCIE A CELZ
C OPTINI  IN       K16 : OPTION QUI SERA ASSOCIEE A CELZ
C         SI OPTINI=' ' : ON PREND OPTINI='TOU_INI_ELNO'
C                         OU OPTINI='TOU_INI_ELGA'
C                         SELON LE TYPE DE CESZ
C NOMPAZ  IN       K8  : NOM DU PARAMETRE "IN" OU "OUT" DANS OPTINI
C         SI NOMPAZ=' ' : ON CHERCHE LE BON CANDIDAT DANS LES
C                         PARAMETRES "IN" ET "OUT" DE OPTINI
C         ATTENTION : SI NOMPAZ=' ' ET QU'IL EXISTE PLUSIEURS
C                     PARAMETRES ASSOCIES A LA MEME GRANDEUR
C                     CELA CONDUIRA A UNE ERREUR <F>
C                     => IL VAUT MIEUX FOURNIR NOMPAZ !
C PROLZ   IN       K3  :
C    /'NON' : ERREUR <F> SI IL EXISTE DES
C             DES VALEURS DE CEL QUI NE SONT PAS AFFECTEES PAR CES.
C             => ON N'INVENTE AUCUNE VALEUR
C    /'OUI' : LE CHAM_ELEM CEL EST PROLONGE
C             PAR DES VALEURS NULLES LA OU CES N'EST PAS DEFINI.
C             SI LA GRANDEUR EST NEUT_F, ON MET LA FONCTION "&FOZERO"
C    /'CHL' : (UTILISE PAR CHLIGR)
C             PROLONGE PAR "ZERO" LES MAILLES DE CEL QUI NE SONT
C             PAS DU TOUT AFFECTEES DANS CES (NOUVELLES MAILLES)
C             ARRETE EN ERREUR <F> SI DES MAILLES DE CEL PORTENT
C             DES CMPS INCONNUES DANS CES
C    /'NAN' : LE CHAM_ELEM CEL EST PROLONGE
C             PAR DES VALEURS "NOT A NUMBER" LA OU CES N'EST PAS DEFINI.
C NNCP   OUT       I   : NOMBRE DE VALEURS DE CESZ NON RECOPIEES
C                        DANS CELZ
C BASEZ   IN       K1  : BASE DE CREATION POUR CELZ : G/V/L
C CELZ    IN/JXOUT K19 : SD CHAM_ELEM A CREER
C KSTOP   IN       K1  : COMPORTEMENT EN CAS DE PROBLEME :
C              / 'A' : ON EMET UNE ALARME ET ON REND IRET > 0
C              / 'F' : ON EMET UNE ERREUR FATALE
C              / ' ' : ON N'EMET PAS DE MESSAGE
C IRET    OUT       I  : CODE DE RETOUR :
C              / 0 : OK
C              / 1 : LE CHAM_ELEM N'A PAS PU ETRE CREE

C-----------------------------------------------------------------------

      LOGICAL DBG
C     ------------------------------------------------------------------
      INTEGER ICMP,NEC,JCESK,JCESD,JCESV,JCESL,GD
      INTEGER IBID,JNUCM2,JNUCM1,JCESC,I
      INTEGER NCMPMX,NCMP1,JCMPGD,ICMP1,K,IOPT,IADG
      INTEGER JCELV,INDIK8,NEQ,NBVCES,JCOPI,NBVCOP,NBVACO
      INTEGER IGR,IEL,IALIEL,ILLIEL,JCELD,NBGR,IMOLO,JMOLO
      INTEGER NBPT,ICO,IPT,NUMA,IAD,IEQ,NUMAIL,NBELEM,IAD2
      INTEGER JDCELD,JDCELL,JDCELV,IMA,NBMA,NBSPT,ISPT,ICMPMX
      INTEGER ADIEL,JLPT,JLCUPT,LGCATA,NCDYN,CUMU,NBEL,NPTMX
      INTEGER NBSP,NBCMP,ISP,NBPT2,VALI(2),ISNNEM,INAN
      LOGICAL EXISDG,DIFF,PROL,PROL2
      CHARACTER*1 BASE,KBID
      CHARACTER*8 MA,NOMGD,NOMCMP,NOMPAR,NOMMA,LICMP(2),NOPAR2
      CHARACTER*3 TSCA,KNAN
      CHARACTER*4 TYPCES
      CHARACTER*16 OPTION
      CHARACTER*19 CES,CEL,LIGREL,DCEL
      CHARACTER*24 VALK(5),MESSAG
      CHARACTER*3 PROL0
      REAL*8 R8NNEM,RNAN

      NUMAIL(IGR,IEL) = ZI(IALIEL-1+ZI(ILLIEL+IGR-1)+IEL-1)
C     ------------------------------------------------------------------
      CALL JEMARQ()

      DBG=.TRUE.
      DBG=.FALSE.

      BASE = BASEZ
      CES = CESZ
      CEL = CELZ
      OPTION = OPTINI
      NOMPAR = NOMPAZ
      LIGREL = LIGREZ
      PROL0 = PROLZ

      DO 1, I=1,3
        VALK(I)=' '
 1    CONTINUE
      VALK(4) = CEL
      VALK(5) = CES



C     PROL : AUTORISATION DE PROLONGER (MEME UNE CMP ISOLEE)
C     PROL2: AUTORISATION DE PROLONGER UNE MAILLE ENTIEREMENT VIERGE
      IF (PROL0.EQ.'OUI') THEN
        PROL = .TRUE.
        PROL2 = .TRUE.

      ELSEIF (PROL0.EQ.'NON') THEN
        PROL = .FALSE.
        PROL2 = .FALSE.

      ELSEIF (PROL0.EQ.'CHL') THEN
        PROL = .FALSE.
        PROL2 = .TRUE.

      ELSEIF (PROL0.EQ.'NAN') THEN
        PROL = .TRUE.
        PROL2 = .TRUE.

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF


C     -- SI CEL EXISTE DEJA, ON LE DETRUIT :
      CALL DETRSD('CHAM_ELEM',CEL)

      CALL JEVEUO(CES//'.CESK','L',JCESK)
      CALL JEVEUO(CES//'.CESD','L',JCESD)
      CALL JEVEUO(CES//'.CESC','L',JCESC)
      CALL JEVEUO(CES//'.CESV','L',JCESV)
      CALL JEVEUO(CES//'.CESL','L',JCESL)
C     -- OBJET .COPI TEMPORAIRE POUR VERIFIER QUE TOUTES LES
C        COMPOSANTES DE CES ONT ETE RECOPIEES
      CALL JELIRA(CES//'.CESV','LONMAX',NBVCES,KBID)
      CALL WKVECT('&&CESCEL.COPI','V V I',NBVCES,JCOPI)

      MA = ZK8(JCESK-1+1)
      NOMGD = ZK8(JCESK-1+2)
      TYPCES = ZK8(JCESK-1+3)
      IF (NOMGD.EQ.'VAR2_R') NOMGD = 'VARI_R'

      NBMA = ZI(JCESD-1+1)
      NCMP1 = ZI(JCESD-1+2)

      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,KBID,IBID)
      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)
      CALL DISMOI('F','NB_CMP_MAX',NOMGD,'GRANDEUR',NCMPMX,KBID,IBID)
      CALL DISMOI('F','NUM_GD',NOMGD,'GRANDEUR',GD,KBID,IBID)


C     1- REMPLISSAGE DE .NUCM2 ET .NUCM1 (SI NOMGD /='VARI_R'):
C     -----------------------------------------------------------------
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'L',JCMPGD)
      IF (NOMGD.NE.'VARI_R') THEN
        CALL WKVECT('&&CESCEL.NUCM1','V V I',NCMP1,JNUCM1)
        CALL WKVECT('&&CESCEL.NUCM2','V V I',NCMPMX,JNUCM2)

        DO 10,ICMP1 = 1,NCMP1
          NOMCMP = ZK8(JCESC-1+ICMP1)
          ICMP = INDIK8(ZK8(JCMPGD),NOMCMP,1,NCMPMX)
          IF (ICMP.EQ.0) THEN
            VALK(1) = NOMCMP
            VALK(2) = NOMGD
            VALK(3) = CEL
            MESSAG = 'CALCULEL_52'
            GOTO 240

          ENDIF
          ZI(JNUCM2-1+ICMP) = ICMP1
          ZI(JNUCM1-1+ICMP1) = ICMP
   10   CONTINUE
      ENDIF


C     -- ALLOCATION ET REMPLISSAGE DE 2 PETITS VECTEURS D'INDIRECTION
C       ENTRE LES CMPS (SI VARI_R) :
C     ----------------------------------------------------------------
      IF (NOMGD.EQ.'VARI_R') THEN
        NCMPMX = 0
        DO 20,ICMP1 = 1,NCMP1
          NOMCMP = ZK8(JCESC-1+ICMP1)
          READ (NOMCMP(2:8),'(I7)') ICMP
          NCMPMX = MAX(NCMPMX,ICMP)
   20   CONTINUE
        CALL ASSERT(NCMPMX.GT.0)
        CALL WKVECT('&&CESCEL.NUCM1','V V I',NCMP1,JNUCM1)
        CALL WKVECT('&&CESCEL.NUCM2','V V I',NCMPMX,JNUCM2)
        DO 30,ICMP1 = 1,NCMP1
          NOMCMP = ZK8(JCESC-1+ICMP1)
          READ (NOMCMP(2:8),'(I7)') ICMP
          ZI(JNUCM2-1+ICMP) = ICMP1
          ZI(JNUCM1-1+ICMP1) = ICMP
   30   CONTINUE
      ENDIF



C     2- ON ALLOUE LE CHAM_ELEM CEL "VIERGE"
C     =========================================

C     2.1 DETERMINATION DE OPTION SI NECESSAIRE :
C     -------------------------------------------
      IF (OPTION.EQ.' ') THEN
        IF (TYPCES.EQ.'ELNO') THEN
          OPTION = 'TOU_INI_ELNO'

        ELSEIF (TYPCES.EQ.'ELGA') THEN
          OPTION = 'TOU_INI_ELGA'

        ELSEIF (TYPCES.EQ.'ELEM') THEN
          OPTION = 'TOU_INI_ELEM'

        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ENDIF
      CALL JENONU(JEXNOM('&CATA.OP.NOMOPT',OPTION),IOPT)


      IF (IOPT.EQ.0) THEN
        VALK(1) = OPTINI
        MESSAG = 'CALCULEL_53'
        GOTO 240

      ENDIF


C     2.2 DETERMINATION DE NOMPAR SI NECESSAIRE :
C     -------------------------------------------
      IF (NOMPAR.EQ.' ') NOMPAR = NOPAR2(OPTION,NOMGD,'INOUT')


C     2.3 CREATION DE DCEL :
C     ----------------------------------------------
      LICMP(1) = 'NPG_DYN'
      LICMP(2) = 'NCMP_DYN'
      DCEL = '&&CESCEL.DCEL'
      CALL CESCRE('V',DCEL,'ELEM',MA,'DCEL_I',2,LICMP,-1,-1,-2)
      CALL JEVEUO(DCEL//'.CESD','L',JDCELD)
      CALL JEVEUO(DCEL//'.CESV','E',JDCELV)
      CALL JEVEUO(DCEL//'.CESL','E',JDCELL)
      DO 70,IMA = 1,NBMA
C       -- NBRE DE SOUS-POINTS :
        CALL CESEXI('C',JDCELD,JDCELL,IMA,1,1,1,IAD)
        CALL ASSERT(IAD.LT.0)
        ZL(JDCELL-1-IAD) = .TRUE.
        ZI(JDCELV-1-IAD) = ZI(JCESD-1+5+4* (IMA-1)+2)

C       -- NBRE DE CMPS "DYNAMIQUES" (POUR VARI_R) :
        CALL CESEXI('C',JDCELD,JDCELL,IMA,1,1,2,IAD)
        CALL ASSERT(IAD.LT.0)
        ZL(JDCELL-1-IAD) = .TRUE.
        IF (NOMGD.EQ.'VARI_R') THEN
          NBPT = ZI(JCESD-1+5+4* (IMA-1)+1)
          NBSP = ZI(JCESD-1+5+4* (IMA-1)+2)
          NBCMP = ZI(JCESD-1+5+4* (IMA-1)+3)
          ICMPMX = 0
          DO 60,ICMP1 = 1,NBCMP
            ICMP = ZI(JNUCM1-1+ICMP1)
            DO 50,IPT = 1,NBPT
              DO 40,ISP = 1,NBSP
                CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,ICMP1,IAD2)
                IF (IAD2.GT.0) ICMPMX = ICMP
   40         CONTINUE
   50       CONTINUE
   60     CONTINUE
          ZI(JDCELV-1-IAD) = ICMPMX

        ELSE
          ZI(JDCELV-1-IAD) = 0
        ENDIF
   70 CONTINUE


C     2.4 ALLOCATION DU CHAM_ELEM :
C     ----------------------------------------------
      CALL ALCHML(LIGREL,OPTION,NOMPAR,BASE,CEL,IRET,DCEL)
      IF (IRET.EQ.1) THEN
        VALK(1) = NOMPAR
        VALK(2) = OPTION
        VALK(3) = LIGREL
        MESSAG = 'CALCULEL_54'
        GOTO 240

      ENDIF

C     3- ON REMPLIT LE .CELV :
C     ===================================================
      CALL JEVEUO(CEL//'.CELV','E',JCELV)
      CALL JELIRA(CEL//'.CELV','LONMAX',NEQ,KBID)
      CALL JEVEUO(CEL//'.CELD','L',JCELD)
      NBGR = ZI(JCELD-1+2)
      CALL JEVEUO(LIGREL//'.LIEL','L',IALIEL)
      CALL JEVEUO(JEXATR(LIGREL//'.LIEL','LONCUM'),'L',ILLIEL)


C     3.1 ON INITIALISE CELV AVEC "NAN" SI NECESSAIRE :
C     -------------------------------------------------
      IF (PROL0.EQ.'NAN') THEN
        RNAN = R8NNEM()
        INAN = ISNNEM()
        KNAN = '???'
        IF (TSCA.EQ.'R') THEN
          DO 80,IEQ = 1,NEQ
            ZR(JCELV-1+IEQ) = RNAN
   80     CONTINUE
        ELSEIF (TSCA.EQ.'C') THEN
          DO 81,IEQ = 1,NEQ
            ZC(JCELV-1+IEQ) = DCMPLX(RNAN,RNAN)
   81     CONTINUE
        ELSEIF (TSCA.EQ.'I') THEN
          DO 82,IEQ = 1,NEQ
            ZI(JCELV-1+IEQ) = INAN
   82     CONTINUE
        ELSEIF (TSCA.EQ.'K8') THEN
          DO 83,IEQ = 1,NEQ
            ZK8(JCELV-1+IEQ) = KNAN
   83     CONTINUE
        ELSEIF (TSCA.EQ.'K24') THEN
          DO 84,IEQ = 1,NEQ
            ZK24(JCELV-1+IEQ) = KNAN
   84     CONTINUE
          CALL ASSERT(.FALSE.)
        ENDIF
      ENDIF


C     3.2 ON INITIALISE CELV AVEC "&FOZERO" SI NEUT_F :
C     -----------------------------------------------------
      IF (PROL0.EQ.'OUI'. AND. NOMGD.EQ.'NEUT_F') THEN
        CALL ASSERT (TSCA.EQ.'K8')
        DO 85,IEQ = 1,NEQ
          ZK8(JCELV-1+IEQ) = '&FOZERO'
   85   CONTINUE
      ENDIF


C     3.2 CAS NOMGD /= 'VARI_R' :
C     ---------------------------------------------------
      IF (NOMGD.NE.'VARI_R') THEN

C       3.2.1 ALLOCATION DE 2 VECTEURS DE TRAVAIL :
        NPTMX = ZI(JCESD-1+3)
        DO 90,IGR = 1,NBGR
          IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
          IF (IMOLO.EQ.0) GOTO 90
          CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
          NBPT = MOD(ZI(JMOLO-1+4),10000)
          NPTMX = MAX(NPTMX,NBPT)
   90   CONTINUE

        CALL WKVECT('&&CESCEL.LONG_PT','V V I',NPTMX,JLPT)
        CALL WKVECT('&&CESCEL.LONG_PT_CUMU','V V I',NPTMX,JLCUPT)

C       3.2.2 BOUCLE SUR LES GREL DU LIGREL
        DO 170,IGR = 1,NBGR
          IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
          IF (IMOLO.EQ.0) GOTO 170

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
            ICO = 0
            K = 1
            IF (DIFF) K = IPT
            IADG = JMOLO - 1 + 4 + (K-1)*NEC + 1
            DO 150,ICMP = 1,NCMPMX
              IF (EXISDG(ZI(IADG),ICMP)) THEN
                ICO = ICO + 1
                ICMP1 = ZI(JNUCM2-1+ICMP)
                IF (ICMP1.EQ.0) THEN
                  IF (PROL) THEN
                    GOTO 150

                  ELSE
                    NOMCMP = ZK8(JCMPGD-1+ICMP)
                    MESSAG = 'CALCULEL_55'
                    GOTO 240

                  ENDIF
                ENDIF

                DO 140,IEL = 1,NBEL
                  NUMA = NUMAIL(IGR,IEL)

C                 -- QUE FAIRE SI LA MAILLE EST TARDIVE ?
                  IF (NUMA.LT.0) THEN
                    IF (PROL2) THEN
                      GOTO 140

                    ELSE
                      MESSAG = 'CALCULEL_56'
                      GOTO 240

                    ENDIF
                  ENDIF

                  NBPT2 = ZI(JCESD-1+5+4* (NUMA-1)+1)
                  IF (NBPT.NE.NBPT2) THEN
                    IF ((NBPT2.EQ.0) .AND. PROL2) THEN
                      GOTO 140

                    ELSE
                      CALL JENUNO(JEXNUM(MA//'.NOMMAI',NUMA),NOMMA)
                      VALK(1) = NOMMA
                      VALK(2) = NOMGD
                      VALI(1)=NBPT
                      VALI(2)=NBPT2
                      MESSAG = 'CALCULEL_57'
                      GOTO 240

                    ENDIF
                  ENDIF


                  NBSPT = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+1)
                  NBSPT = MAX(NBSPT,1)
                  ADIEL = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+4)
                  DO 130,ISPT = 1,NBSPT


                    CALL CESEXI('C',JCESD,JCESL,NUMA,IPT,ISPT,ICMP1,IAD)
                    IF (IAD.LE.0) THEN
                      IF (PROL) THEN
                        GOTO 130

                      ELSE
                        NOMCMP = ZK8(JCMPGD-1+ICMP)
                        CALL JENUNO(JEXNUM(MA//'.NOMMAI',NUMA),NOMMA)
                        VALK(1) = NOMCMP
                        VALK(2) = NOMMA
                        MESSAG = 'CALCULEL_58'
                        GOTO 240

                      ENDIF
                    ENDIF

                    IEQ = ADIEL - 1 + NBSPT*ZI(JLCUPT-1+IPT) +
     &                    (ISPT-1)*ZI(JLPT-1+IPT) + ICO
                    IF (TSCA.EQ.'R') THEN
                      ZR(JCELV-1+IEQ) = ZR(JCESV-1+IAD)

                    ELSEIF (TSCA.EQ.'I') THEN
                      ZI(JCELV-1+IEQ) = ZI(JCESV-1+IAD)

                    ELSEIF (TSCA.EQ.'C') THEN
                      ZC(JCELV-1+IEQ) = ZC(JCESV-1+IAD)

                    ELSEIF (TSCA.EQ.'L') THEN
                      ZL(JCELV-1+IEQ) = ZL(JCESV-1+IAD)

                    ELSEIF (TSCA.EQ.'K8') THEN
                      ZK8(JCELV-1+IEQ) = ZK8(JCESV-1+IAD)

                    ELSEIF (TSCA.EQ.'K16') THEN
                      ZK16(JCELV-1+IEQ) = ZK16(JCESV-1+IAD)

                    ELSEIF (TSCA.EQ.'K24') THEN
                      ZK24(JCELV-1+IEQ) = ZK24(JCESV-1+IAD)

                    ELSE
                      CALL ASSERT(.FALSE.)
                    ENDIF
                    ZI(JCOPI-1+IAD) = 1
  130             CONTINUE
  140           CONTINUE
              ENDIF
  150       CONTINUE
  160     CONTINUE
  170   CONTINUE


C     3.3 CAS NOMGD == 'VARI_R' :
C     ---------------------------------------------------
      ELSE
        DO 220,IGR = 1,NBGR
          IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
          IF (IMOLO.EQ.0) GOTO 220

          CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
          DIFF = (ZI(JMOLO-1+4).GT.10000)
C         CAS (ZI(JMOLO-1+4).GT.10000) RESTE A PROGRAMMER
          CALL ASSERT(.NOT.DIFF)
          NBPT = MOD(ZI(JMOLO-1+4),10000)
          LGCATA = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+3)
          CALL ASSERT(NBPT.EQ.LGCATA)
          NBEL = NBELEM(LIGREL,IGR)


          DO 210,IEL = 1,NBEL

            NBSPT = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+1)
            NBSPT = MAX(NBSPT,1)
            NCDYN = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+2)
            ADIEL = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+4)
            NUMA = NUMAIL(IGR,IEL)

C           -- QUE FAIRE SI LA MAILLE EST TARDIVE ?
            IF (NUMA.LT.0) THEN
              IF (PROL2) GOTO 210
              MESSAG = 'CALCULEL_56'
              GOTO 240

            ENDIF

            NBPT2 = ZI(JCESD-1+5+4* (NUMA-1)+1)
            IF (NBPT.NE.NBPT2) THEN
              IF ((NBPT2.EQ.0) .AND. PROL2) THEN
                GOTO 210

              ELSE
                CALL JENUNO(JEXNUM(MA//'.NOMMAI',NUMA),NOMMA)
                VALK(1) = NOMMA
                VALK(2) = NOMGD
                VALI(1)=NBPT
                VALI(2)=NBPT2
                MESSAG = 'CALCULEL_57'
                GOTO 240

              ENDIF
            ENDIF

            DO 200,IPT = 1,NBPT
              DO 190,ISPT = 1,NBSPT
                DO 180,ICMP = 1,NCDYN
                  ICMP1 = ZI(JNUCM2-1+ICMP)
                  IF (ICMP1.EQ.0) GOTO 180
                  CALL CESEXI('C',JCESD,JCESL,NUMA,IPT,ISPT,ICMP1,IAD)
                  IF (IAD.LE.0) THEN
                    IF (PROL) THEN
                      GOTO 180

                    ELSE
                      NOMCMP = 'V'
                      CALL CODENT(ICMP,'G',NOMCMP(2:8))
                      CALL JENUNO(JEXNUM(MA//'.NOMMAI',NUMA),NOMMA)
                      VALK(1) = NOMCMP
                      VALK(2) = NOMMA
                      MESSAG = 'CALCULEL_58'
                      GOTO 240

                    ENDIF
                  ENDIF

                  IEQ = ADIEL - 1 + ((IPT-1)*NBSPT+ISPT-1)*NCDYN + ICMP
                  IF (TSCA.EQ.'R') THEN
                    ZR(JCELV-1+IEQ) = ZR(JCESV-1+IAD)

                  ELSEIF (TSCA.EQ.'I') THEN
                    ZI(JCELV-1+IEQ) = ZI(JCESV-1+IAD)

                  ELSEIF (TSCA.EQ.'C') THEN
                    ZC(JCELV-1+IEQ) = ZC(JCESV-1+IAD)

                  ELSEIF (TSCA.EQ.'L') THEN
                    ZL(JCELV-1+IEQ) = ZL(JCESV-1+IAD)

                  ELSEIF (TSCA.EQ.'K8') THEN
                    ZK8(JCELV-1+IEQ) = ZK8(JCESV-1+IAD)

                  ELSE
                    CALL ASSERT(.FALSE.)
                  ENDIF
                  ZI(JCOPI-1+IAD) = 1
  180           CONTINUE
  190         CONTINUE
  200       CONTINUE
  210     CONTINUE
  220   CONTINUE
      ENDIF


C     -- CALCUL DU NOMBRE DE CMPS NON RECOPIEES (NNCP):
C     ------------------------------------------------------
      NBVCOP = 0
      NBVACO = 0
      DO 230,IAD = 1,NBVCES
        IF (ZL(JCESL-1+IAD)) NBVACO = NBVACO + 1
        IF (ZI(JCOPI-1+IAD).EQ.1) NBVCOP = NBVCOP + 1
  230 CONTINUE
      NNCP = NBVACO - NBVCOP
      IRET = 0
      GOTO 250


C     -- MESSAGES D'ERREUR:
C     ---------------------
  240 CONTINUE
      IRET = 1
      CALL ASSERT(KSTOP.EQ.'F' .OR. KSTOP.EQ.'A' .OR. KSTOP.EQ.' ')
      CALL DETRSD('CHAMP',CEL)
      IF (KSTOP.EQ.' ') GOTO 250


      IF (MESSAG.EQ.'CALCULEL_52') THEN
        CALL U2MESK(KSTOP,'CALCULEL_52',4,VALK)
      ELSEIF (MESSAG.EQ.'CALCULEL_53') THEN
        CALL U2MESK(KSTOP,'CALCULEL_53',4,VALK)
      ELSEIF (MESSAG.EQ.'CALCULEL_54') THEN
        CALL U2MESK(KSTOP,'CALCULEL_54',4,VALK)
      ELSEIF (MESSAG.EQ.'CALCULEL_55') THEN
        VALK(1) = NOMCMP
        CALL U2MESK(KSTOP,'CALCULEL_55',4,VALK)
      ELSEIF (MESSAG.EQ.'CALCULEL_56') THEN
        CALL U2MESK(KSTOP,'CALCULEL_56',4,VALK)
      ELSEIF (MESSAG.EQ.'CALCULEL_57') THEN
        CALL U2MESG(KSTOP,'CALCULEL_57',5,VALK,2,VALI,0,0.D0)
      ELSEIF (MESSAG.EQ.'CALCULEL_58') THEN
        CALL U2MESK(KSTOP,'CALCULEL_58',4,VALK)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF



  250 CONTINUE
      IF (DBG) THEN
        CALL CHEKSD(CEL,'SD_CHAM_ELEM',IRET)
        CALL ASSERT(IRET.EQ.0)
      ENDIF

      CALL DETRSD('CHAM_ELEM_S',DCEL)
      CALL JEDETR('&&CESCEL.COPI')
      CALL JEDETR('&&CESCEL.NUCM1')
      CALL JEDETR('&&CESCEL.NUCM2')
      CALL JEDETR('&&CESCEL.LONG_PT')
      CALL JEDETR('&&CESCEL.LONG_PT_CUMU')

      CALL JEDEMA()
      END
