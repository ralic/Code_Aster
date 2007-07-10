      SUBROUTINE MANOPG(LIGREZ,OPTIOZ,PARAMZ,MNOGAZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE VABHHTS J.PELLET
C A_UTIL
      IMPLICIT NONE
      CHARACTER*(*) LIGREZ,MNOGAZ,OPTIOZ,PARAMZ
C ------------------------------------------------------------------
C BUT: CREER LE CHAM_ELEM_S MNOGAZ QUI CONTIENDRA LA MATRICE
C      DE PASSAGE NOEUDS -> POINTS DE GAUSS POUR LES ELEMENTS
C      DU LIGREL ET POUR LA FAMILLE (OPTIOZ/PARAMZ)
C ------------------------------------------------------------------
C     ARGUMENTS:
C LIGREZ  IN/JXIN  K19 : LIGREL
C OPTIOZ,PARAMZ  IN  K* : OPTION ET PARAMETRE PERMETTANT DE DETERMINER
C                         LA FAMILLE DE PG UTILISEE.
C MNOGAZ  IN/JXOUT K19 : CHAM_ELEM_S (VARI_R) DE TYPE 'ELEM'
C ------------------------------------------------------------------
C REMARQUES :
C  MNOGAZ(IMA) EST UN VECTEUR V DE REELS DE DIMENSION 2 + NBNO*NBPG
C    V(1) : NBNO
C    V(2) : NBPG
C    V(2+NBNO*(IPG-1)+INO) : MATRICE DE PASSAGE (IPG,INO)

C  ATTENTION :
C     1) LES MAILLES TARDIVES SONT IGNOREES.
C     2) POUR ECONOMISER LE VOLUME DE MNOGAZ, ON UTILISE LE FAIT QUE
C        LES MATRICES DE PASSAGE DES ELEMENTS D'UN MEME GREL SONT
C        IDENTIQUES CAR ELLES NE DEPENDENT QUE DE L'ELREFA.
C
C        ON UTILISE LA CONVENTION :
C          SI MNOGAZ(IMA,1) > 0 : LA MAILLE IMA EST LA 1ERE D'UN GREL
C              SA MATRICE EST STOCKEE ET ELLE SERT DE REFERENCE POUR
C              LES AUTRES
C          SI MNOGAZ(IMA,1) < 0 : LA MAILLE IMA N'EST PAS LA 1ERE
C              D'UN GREL. MNOGAZ(IMA,1)= -IMAREF
C              IMAREF EST LA MAILLE DE REFERENCE POUR IMA
C
C-----------------------------------------------------------------------

C---- COMMUNS NORMALISES  JEVEUX
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
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER NBPGMX,NBNOMX,NBFAMX,NBFLMX
      PARAMETER (NBPGMX=27,NBNOMX=27,NBFAMX=20,NBFLMX=20)
      INTEGER IBID,NBMA,IMA,JCESD,JCESL,JCESV,IAD,JNBPG
      INTEGER ILCNX1,NBPGF(NBFAMX),K,JFPGL,JPNLFP
      INTEGER NEC,KFPG,NDIM,NNO,NNOS,NBFPG,INDIK8,NPG,KP,INO
      INTEGER JCELD,NBGREL,NEL,NBELEM,NUTE,TYPELE,IMOLO,JMOLO
      INTEGER IERD,IFAM,NUMAIL,IGR,IEL,JMAREF
      INTEGER JNBNO,JDIME,IRET,NCPMAX,NBFAM,KFAM,NBPGT,IAD0
      INTEGER NBLFPG,JNOLFP,NUFLPG,INDK32,NUFGPG,JLIEL,JLIEL1
      CHARACTER*1 KBID
      CHARACTER*8 MA, FAPG(NBFAMX),NOMGD,FAMIL,ELREFE,PARAM
      CHARACTER*8 LIELRF(NBFLMX),LIFAPG(NBFLMX)
      CHARACTER*16 PHENO,OPTION,NOMTE,NOFPG
      CHARACTER*19 MNOGA,LIGREL,CELMOD,LIGRE1
      CHARACTER*24 OBNBPG ,OBNBNO, OBDIME
      CHARACTER*32 NOFLPG
      REAL*8 XNO(3*NBNOMX),XPG(3*NBPGMX),VOL,FF(NBNOMX)
      REAL*8 POIPG(NBNOMX)
C     ------------------------------------------------------------------
      CALL JEMARQ()
      MNOGA=MNOGAZ
      LIGREL=LIGREZ
      OPTION=OPTIOZ
      PARAM=PARAMZ

      OBNBPG = '&&MANOPG.NBPG'
      OBNBNO = '&&MANOPG.NBNO'
      OBDIME = '&&MANOPG.DIME'

      CALL DISMOI('F','NOM_MAILLA',LIGREL,'LIGREL',IBID,MA,IBID)
      CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMA,KBID,IBID)
      CALL DISMOI('F','PHENOMENE',LIGREL,'LIGREL',IBID,PHENO,IBID)
      CALL JEVEUO(JEXATR(MA//'.CONNEX','LONCUM'),'L',ILCNX1)

      CALL JEVEUO('&CATA.TE.PNLOCFPG','L',JPNLFP)
      CALL JELIRA('&CATA.TE.NOLOCFPG','LONMAX',NBLFPG,KBID)
      CALL JEVEUO('&CATA.TE.NOLOCFPG','L',JNOLFP)


C     0. ON FABRIQUE UN "FAUX" LIGREL (LIGRE1) N'AYANT QU'UN SEUL
C        ELEMENT PAR GREL POUR DIMINUER LA TAILLE DE MNOGA
C     ------------------------------------------------------------
C     0.1 ALLOCATION D'UN OBJET DONNANT POUR CHAQUE MAILLE SA MAILLE
C         DE REFERENCE
      CALL WKVECT('&&MANOPG.MAILREF','V V I',NBMA,JMAREF)

      LIGRE1='&&MANOPG.LIGRE1'
      CALL JELIRA(LIGREL//'.LIEL','NMAXOC',NBGREL,KBID)
      CALL ASSERT(NBGREL.GT.0)
      CALL JECREC(LIGRE1//'.LIEL','V V I','NU','CONTIG','VARIABLE',
     &            NBGREL)
      CALL JEECRA(LIGRE1//'.LIEL','LONT',2*NBGREL,' ')
      DO 881, IGR=1,NBGREL
        NEL = NBELEM(LIGREL,IGR)
        CALL ASSERT(NEL.GE.1)
        CALL JECROC(JEXNUM(LIGRE1//'.LIEL',IGR))
        CALL JEECRA(JEXNUM(LIGRE1//'.LIEL',IGR),'LONMAX',2,KBID)
        CALL JEVEUO(JEXNUM(LIGRE1//'.LIEL',IGR),'E',JLIEL1)
        CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',IGR),'L',JLIEL)
        ZI(JLIEL1-1+1)=ZI(JLIEL-1+1)
        ZI(JLIEL1-1+2)=ZI(JLIEL-1+NEL+1)
        DO 882, IEL=1,NEL
           IMA=ZI(JLIEL-1+IEL)
           IF (IMA.LT.0) GO TO 882
           IF (IEL.EQ.1) THEN
              ZI(JMAREF-1+IMA)=+ZI(JLIEL-1+1)
           ELSE
              ZI(JMAREF-1+IMA)=-ZI(JLIEL-1+1)
           ENDIF
882     CONTINUE
881   CONTINUE
      CALL JEDUPO(LIGREL//'.LGRF','V',LIGRE1//'.LGRF',.FALSE.)
      CALL JEDUPO(LIGREL//'.NBNO','V',LIGRE1//'.NBNO',.FALSE.)



C     1. ON RECUPERE LES NOMBRES DE POINTS DE GAUSS ET DE NOEUDS :
C     ------------------------------------------------------------
      CALL NBPTCA(LIGRE1,OPTION,PARAM,OBNBPG,OBNBNO)
      CALL JEVEUO(OBNBPG,'L',JNBPG)
      CALL JEVEUO(OBNBNO,'L',JNBNO)


C     2. ALLOCATION DU CHAM_ELEM_S MNOGA :
C     ---------------------------------------------------------------
      CALL WKVECT(OBDIME,'V V I',NBMA,JDIME)
      NCPMAX=0
      DO 77, IMA=1,NBMA
        ZI(JDIME-1+IMA)= ZI(JNBPG-1+IMA)*ZI(JNBNO-1+IMA) +2
        NCPMAX=MAX(NCPMAX,ZI(JDIME-1+IMA))
77    CONTINUE
      CALL CESCRE('V',MNOGA,'ELEM',MA,'VARI_R',-NCPMAX,' ',
     &             -1,-1,ZI(JDIME))


C     3. ALLOCATION D'UN CHAMP MODELE POUR DETERMINER LES FAMILLES
C        DE POINTS DE GAUSS UTILISEES.
C     ---------------------------------------------------------------
      CELMOD='&&MANOPG.CELMOD'
      CALL ALCHML(LIGRE1,OPTION,PARAM,'V',CELMOD,IRET,' ')
      CALL JEVEUO(CELMOD//'.CELD','L',JCELD)


C     4. REMPLISSAGE DE MNOGA :
C     ---------------------------------------------------------------
      CALL JEVEUO(MNOGA//'.CESD','L',JCESD)
      CALL JEVEUO(MNOGA//'.CESL','E',JCESL)
      CALL JEVEUO(MNOGA//'.CESV','E',JCESV)


      DO 1, IGR=1,NBGREL
        CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',IGR),'L',JLIEL)
        NEL = NBELEM(LIGREL,IGR)
        NUTE = TYPELE(LIGREL,IGR)
        CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',NUTE),NOMTE)
        IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
        IF (IMOLO.EQ.0) GO TO 1

        CALL JEVEUO(JEXNUM(LIGRE1//'.LIEL',IGR),'L',JLIEL1)


C       4.1 DETERMINATION DE LA LISTE DES FAMILLES DE PG :
C       -----------------------------------------------------------
        CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
        CALL DISMOI('F','NOM_GD',CELMOD,'CHAMP',IBID,NOMGD,IERD)
        CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,KBID,IERD)
        KFPG = ZI(JMOLO-1+4+NEC+1)

C       -- FAMILLE "LISTE"
        IF (KFPG.LT.0) THEN
C          FAMILLE "LISTE" :
           CALL JELIRA(JEXNUM('&CATA.TE.FPG_LISTE',-KFPG),'LONMAX',
     &                 NBFAM,KBID)
           NBFAM=NBFAM-1
           CALL ASSERT(NBFAM.LE.NBFLMX)
           CALL JEVEUO(JEXNUM('&CATA.TE.FPG_LISTE',-KFPG),'L',JFPGL)
           ELREFE=ZK8(JFPGL-1+NBFAM+1)
           DO 18,K=1,NBFAM
              LIELRF(K)=ELREFE
              NOFLPG = NOMTE//ELREFE//ZK8(JFPGL-1+K)
              NUFLPG = INDK32(ZK32(JPNLFP),NOFLPG,1,NBLFPG)
              NUFGPG = ZI(JNOLFP-1+NUFLPG)
              CALL JENUNO(JEXNUM('&CATA.TM.NOFPG',NUFGPG),NOFPG)
              CALL ASSERT(ELREFE.EQ.NOFPG(1:8))
              LIFAPG(K)=NOFPG(9:16)
18         CONTINUE

C       -- FAMILLE "ORDINAIRE"
        ELSE
           NBFAM=1
           CALL JENUNO(JEXNUM('&CATA.TM.NOFPG',KFPG),NOFPG)
           LIELRF(1)=NOFPG(1:8)
           LIFAPG(1)=NOFPG(9:16)
        END IF


C       4.2 BOUCLE SUR LA/LES FAMILLE(S) :
C       ------------------------------------------
        NBPGT=0
        DO 2, KFAM=1,NBFAM
          ELREFE=LIELRF(KFAM)
          FAMIL=LIFAPG(KFAM)

C         4.2.1 APPEL AUX ROUTINES ELRACA ET ELRFVF :
C         ------------------------------------------
          CALL ELRACA(ELREFE,NDIM,NNO,NNOS,NBFPG,FAPG,NBPGF,XNO,VOL)
          CALL ASSERT(NBFPG.LE.NBFAMX)
          CALL ASSERT(NNO.LE.NBNOMX)
          IFAM = INDIK8(FAPG,FAMIL,1,NBFPG)
          CALL ASSERT(IFAM.GT.0)
          CALL ELRAGA(ELREFE,FAMIL,NDIM,NPG,XPG,POIPG)
          CALL ASSERT(NPG.LE.NBPGMX)

C         4.2.2 ECRITURE DANS MANOPG :
C         ------------------------------------------
          DO 3,IEL = 1,NEL
            IMA=ZI(JLIEL-1+IEL)
            IF (IMA.LT.0) GOTO 3

            CALL CESEXI('C',JCESD,JCESL,IMA,1,1,1,IAD0)
            IAD0=ABS(IAD0)
            CALL ASSERT(IAD0.GT.0)

C           -- SI CE N'EST PAS UNE MAILLE DE REFERENCE :
            IF (ZI(JMAREF-1+IMA).LT.0) THEN
               ZL(JCESL-1+IAD0-1+1) = .TRUE.
               ZR(JCESV-1+IAD0-1+1) = ZI(JMAREF-1+IMA)
               GOTO 3
            ENDIF
            CALL ASSERT(ZI(JMAREF-1+IMA).EQ.IMA)


C           -- LES 2 PREMIERES CMPS : NNO ET NPG :
            IF (KFAM.EQ.1) THEN
              ZL(JCESL-1+IAD0-1+1) = .TRUE.
              ZL(JCESL-1+IAD0-1+2) = .TRUE.
              ZR(JCESV-1+IAD0-1+1) = NNO
              ZR(JCESV-1+IAD0-1+2) = NPG
            ELSE
              CALL ASSERT(NINT(ZR(JCESV-1+IAD0-1+1)).EQ.NNO)
              ZR(JCESV-1+IAD0-1+2) = ZR(JCESV-1+IAD0-1+2) + NPG
            END IF

            CALL CESEXI('C',JCESD,JCESL,IMA,1,1,2+NNO*(NBPGT+NPG),IAD)
            CALL ASSERT(IAD.LT.0)
            IAD=IAD0+2+NBPGT*NNO

C           -- LES NNO*NPG AUTRES CMPS :
            DO 20 KP = 1,NPG
              CALL ELRFVF(ELREFE,XPG(NDIM*(KP-1)+1),NBNOMX,FF,NNO)
              DO 10 INO = 1,NNO
                ZL(JCESL-1+IAD-1+NNO*(KP-1)+INO) = .TRUE.
                ZR(JCESV-1+IAD-1+NNO*(KP-1)+INO) = FF(INO)
   10         CONTINUE
   20       CONTINUE

   3      CONTINUE
          NBPGT=NBPGT+NPG
   2    CONTINUE
   1  CONTINUE


      CALL DETRSD('CHAMP',CELMOD)
      CALL DETRSD('LIGREL',LIGRE1)
      CALL JEDETR(OBNBPG)
      CALL JEDETR(OBNBNO)
      CALL JEDETR(OBDIME)
      CALL JEDETR('&&MANOPG.MAILREF')

      CALL JEDEMA()
      END
