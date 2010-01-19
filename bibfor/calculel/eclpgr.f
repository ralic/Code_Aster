      SUBROUTINE ECLPGR()
      IMPLICIT   NONE
C MODIF CALCULEL  DATE 19/01/2010   AUTEUR PELLET J.PELLET 
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
C   - TRAITEMENT DU MOT CLE CREA_RESU/ECLA_PG

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------------------

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
      CHARACTER*24 ZK24,NOOJB
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR

C --- FIN DECLARATIONS NORMALISEES JEVEUX -----------------------------

C ---------------------------------------------------------------------
C     VARIABLES NECESSAIRES A L'APPEL DE ECLATY :
C     ON COMPREND LE SENS DE CES VARIABLES EN REGARDANT ECLATY
      INTEGER MXNBN2,MXNBPG,MXNBPI,MXNBTE
      INTEGER VALI
C     MXNBN2 : MAX DU NOMBRE DE NOEUDS D'UN SOUS-ELEMENT (HEXA8)
      PARAMETER (MXNBN2=8)
C     MXNBPG : MAX DU NOMBRE DE POINTS DE GAUSS D'UN TYPE_ELEM
      PARAMETER (MXNBPG=27)
C     MXNBPI : MAX DU NOMBRE DE POINT_I (HEXA A 27 POINTS DE GAUSS)
C     MXNBPI = 4X4X4
      PARAMETER (MXNBPI=64)
C     MXNBTE : MAX DU NOMBRE DE TERMES DE LA C.L. DEFINISSANT 1 POINT_I
C              AU PLUS LES 8 SOMMETS D'UN HEXA8
      PARAMETER (MXNBTE=8)
      INTEGER CONNX(MXNBN2,MXNBPG),NSOMM1(MXNBPI,MXNBTE)
      INTEGER NTERM1(MXNBPI),NBNO2(MXNBPG),KK
      REAL*8 CSOMM1(MXNBPI,MXNBTE),PREC
      INTEGER TYMA(MXNBPG)
C ---------------------------------------------------------------------
      LOGICAL EXISDG
      INTEGER NUMA,JNOFPG,NUMAIL,IRET1
      INTEGER K,NBGREL,TE,TYPELE,NPG1,NPOINI,IDECA2
      INTEGER IGR,IAMACO,ILMACO,IALIEL,ILLIEL,NBELEM,JCNSL2
      INTEGER IBID,NBPG,INO,IRET,I,NBGR,INOGL,IAINS1,IAINS2
      INTEGER NUMGLM,IPG,IAMOL1,JCELV1,JCNSV2,NBSY,MXCMP,NP,NC
      INTEGER IMA,NBELGR,NBNOMA,JVAL2,NBNO,NDDL,IDDL,ADIEL,MXSY,ISY
      INTEGER IIPG,JCELD1,JCELK1,MOLOC1,NCMPMX,NBORDR,IORDR,JORDR
      INTEGER ICH,IRET2,ICO
      PARAMETER (MXSY=5,MXCMP=100)
      INTEGER NUDDL(MXCMP),IACMP,NCMPM1,ICOEF1,IEL
      CHARACTER*8   MO1,MA1,MA2,KBID,RESU,EVO1,NOMG1,NOMG2,CRIT,
     &              ELREFA, FAPG
      CHARACTER*16 TYPRES,NOMCMD,NOMTE,NOMSY1,NOMSY2,LICHAM(MXSY)
      CHARACTER*16 TYPRE2 ,OPTIO,PARAM
      CHARACTER*19 LIGREL,CH1,CH2S,CH2,PRCHNO
      CHARACTER*24 NOMFPG
      CHARACTER*24 VALK(2)
C     FONCTIONS FORMULES :
C     NBNOMA(IMA)=NOMBRE DE NOEUDS DE LA MAILLE IMA
      NBNOMA(IMA) = ZI(ILMACO-1+IMA+1) - ZI(ILMACO-1+IMA)
C     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
C                     IMA ETANT UNE MAILLE DU MAILLAGE.
      NUMGLM(IMA,INO) = ZI(IAMACO-1+ZI(ILMACO+IMA-1)+INO-1)
      NUMAIL(IGR,IEL) = ZI(IALIEL-1+ZI(ILLIEL+IGR-1)+IEL-1)
C DEB -----------------------------------------------------------------
      CALL JEMARQ()


      CALL GETRES(RESU,TYPRE2,NOMCMD)
      CALL GETVID('ECLA_PG','RESU_INIT',1,1,1,EVO1,IBID)
      CALL GETTCO(EVO1,TYPRES)
      IF (TYPRES.NE.TYPRE2) CALL U2MESS('F','CALCULEL2_37')

      CALL GETVID('ECLA_PG','MAILLAGE',1,1,1,MA2,IBID)
      CALL GETVID('ECLA_PG','MODELE_INIT',1,1,1,MO1,IBID)
      CALL GETVTX('ECLA_PG','NOM_CHAM',1,1,MXSY,LICHAM,NBSY)

      CALL DISMOI('F','NOM_MAILLA',MO1,'MODELE',IBID,MA1,IBID)
      CALL JEVEUO(MA2//'.CONNEX','L',IAMACO)
      CALL JEVEUO(JEXATR(MA2//'.CONNEX','LONCUM'),'L',ILMACO)

      CALL EXLIMA('ECLA_PG','V',MO1,LIGREL)
      CALL JEVEUO(LIGREL//'.LIEL','L',IALIEL)
      CALL JEVEUO(JEXATR(LIGREL//'.LIEL','LONCUM'),'L',ILLIEL)

      NOMFPG = '&&ECLPGR.NOMFPG'

C     -- CREATION DE LA SD RESULTAT : RESU
C     ------------------------------------
      CALL GETVR8('ECLA_PG','PRECISION',1,1,1,PREC,NP)
      CALL GETVTX('ECLA_PG','CRITERE',1,1,1,CRIT,NC)
      CALL RSUTNU(EVO1,'ECLA_PG',1,'&&ECLPGR.NUME_ORDRE',NBORDR,PREC,
     &            CRIT,IRET)
      IF (NBORDR.EQ.0) CALL U2MESS('F','CALCULEL2_38')
      CALL JEVEUO('&&ECLPGR.NUME_ORDRE','L',JORDR)

      IF (RESU.NE.EVO1) CALL RSCRSD('G',RESU,TYPRES,NBORDR)



C     -- ON CALCULE LES CHAM_NO RESULTATS :
C     --------------------------------------
      DO 90,ISY = 1,NBSY

C       -- ON SUPPOSE QUE TOUS LES INSTANTS ONT LE MEME PROFIL :
C          PRCHNO
        NOOJB='12345678.00000.NUME.PRNO'
        CALL GNOMSD ( NOOJB,10,14)
        PRCHNO=NOOJB(1:19)

        NOMSY1 = LICHAM(ISY)
        IF(NOMSY1(6:9).NE.'ELGA') CALL U2MESS('F','CALCULEL2_41')

        ICO=0
        DO 80,I = 1,NBORDR
          IORDR = ZI(JORDR+I-1)

C         -- DETERMINATION DE NOMSY1,NOMSY2,NOMG1,NOMG2
C         ---------------------------------------------------
          CALL RSEXCH(EVO1,NOMSY1,IORDR,CH1,IRET)
          IF (IRET.GT.0) GO TO 80

          CALL DISMOI('F','NOM_GD',CH1,'CHAMP',IBID,NOMG1,IBID)
          IF (NOMG1(5:6).NE.'_R') CALL U2MESS('F','CALCULEL2_39')
          NOMG2 = NOMG1
          IF (NOMG1.EQ.'VARI_R') NOMG2 = 'VAR2_R'

C      -- ON STOCKE LE CHAMP CREE SOUS LE MEME NOM QUE LE
C      -- CHAMP D'ORIGINE :
C         NOMSY2=NOMG1(1:4)//'_ELGA'
          NOMSY2 = NOMSY1

          CALL DISMOI('F','NB_CMP_MAX',NOMG2,'GRANDEUR',NCMPMX,KBID,
     &                IBID)
          CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMG2),'L',IACMP)


C         -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
          CALL CELVER(CH1,'NBSPT_1','COOL',KK)
          IF (KK.EQ.1) THEN
            CALL U2MESK('I','CALCULEL2_40',1,NOMSY1)
            CALL CELCEL('PAS_DE_SP',CH1,'V','&&ECLPGR.CH1')
            CH1= '&&ECLPGR.CH1'
          END IF

C        PROJECTION SUR LE LIGREL REDUIT
          CALL JEVEUO(CH1//'.CELK','L',JCELK1)
          IF (ZK24(JCELK1-1+3) (1:4).NE.'ELGA') CALL U2MESS('F',
     &       'CALCULEL2_41')

          OPTIO=ZK24(JCELK1-1+2)
          PARAM=ZK24(JCELK1-1+6)
          CALL CHLIGR(CH1,LIGREL,OPTIO,PARAM,'V','&&ECLPGR.CHR1')
          CH1 = '&&ECLPGR.CHR1'
          CALL JEEXIN(CH1//'.CELD',IBID)
          IF (IBID.EQ.0) THEN
             VALK(1) = NOMSY2
             VALI = IORDR
             CALL U2MESG('A', 'CALCULEL5_83',1,VALK,1,VALI,0,0.D0)
             GO TO 80
          ENDIF

          CALL JEVEUO(CH1//'.CELV','L',JCELV1)
          CALL JEVEUO(CH1//'.CELD','L',JCELD1)


          CALL RSEXCH(RESU,NOMSY2,IORDR,CH2,IRET)


C       -- CREATION D'UN CHAM_NO_S : CH2S
C       -----------------------------------------------------
          ICO=ICO+1
C         -- LA 1ERE FOIS, ON CREE CH2S :
          IF (ICO.EQ.1) THEN
            CH2S = '&&ECLPGR.CH2S'
            NCMPM1 = NCMPMX
            CALL CNSCRE(MA2,NOMG2,NCMPM1,ZK8(IACMP),'V',CH2S)
          ELSE
            CALL JEUNDF(CH2S//'.CNSV')
            CALL JEUNDF(CH2S//'.CNSL')
          END IF
          CALL JEVEUO(CH2S//'.CNSV','E',JCNSV2)
          CALL JEVEUO(CH2S//'.CNSL','E',JCNSL2)



C       -- REMPLISSAGE DU CHAM_NO :
C       ---------------------------
C         -- ON NE CALCULE NOMFPG QUE POUR LE 1ER NUME_ORDRE :
C         -- ON VERIFIE QUE LES CHAMPS ONT TOUS LA MEME FAMILLE DE
C            DE POINTS DE GAUSS
          IF (ICO.EQ.1) THEN
             DO 130 ICH=1,NBSY
               CALL CELFPG ( CH1, NOMFPG, ICH, IRET2 )
               IF (IRET2.EQ.1) THEN
                  VALK(1) = MO1
                  VALK(2) = LICHAM(1)
                  CALL U2MESK('I','CALCULEL2_33', 2 ,VALK)
               ENDIF
130          CONTINUE
             CALL JEEXIN ( NOMFPG, IRET1 )
             CALL ASSERT(IRET1.GT.0)
             CALL JEVEUO ( NOMFPG, 'L', JNOFPG )
          ENDIF

          IPG = 0
          NBGR = NBGREL(LIGREL)
          DO 70,IGR = 1,NBGR
            MOLOC1 = ZI(JCELD1-1+ZI(JCELD1-1+4+IGR)+2)
            IF (MOLOC1.EQ.0) GO TO 70
            ICOEF1 = MAX(1,ZI(JCELD1-1+4))

            CALL ASSERT(.NOT.((NOMG1.NE.'VARI_R').AND.(ICOEF1.GT.1)))

            CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',MOLOC1),'L',IAMOL1)
            CALL ASSERT(ZI(IAMOL1-1+1).EQ.3)
            NBPG = ZI(IAMOL1-1+4)

            NUMA = NUMAIL(IGR,1)
            ELREFA = ZK16(JNOFPG-1+NUMA)(1:8)
            FAPG   = ZK16(JNOFPG-1+NUMA)(9:16)
            IF ( FAPG .EQ. ' ' ) GOTO 70

C           -- ON VERIFIE QUE C'EST UN CHAMP "ELGA/IDEN" :
C           ----------------------------------------------
            CALL ASSERT(.NOT.((NBPG.LT.0).OR.(NBPG.GT.10000)))

C           -- ON ECLATE LE TYPE_ELEM :
C           ---------------------------
            TE = TYPELE(LIGREL,IGR)
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',TE),NOMTE)
            CALL ECLATY ( NOMTE, ELREFA, FAPG, NPG1, NPOINI,
     &                    NTERM1, NSOMM1, CSOMM1, TYMA, NBNO2, CONNX,
     &                    MXNBN2, MXNBPG, MXNBPI, MXNBTE )
            IF (NPG1.NE.0) THEN
              IF (NBPG.NE.NPG1) THEN
                 VALK(1) =  NOMTE
                 VALK(2) = NOMSY1
                 CALL U2MESK('F','CALCULEL2_42', 2 ,VALK)
              ENDIF
            ELSE
C            -- ON IGNORE LES AUTRES ELEMENTS :
              NBPG = 0
              GO TO 70
            END IF
            NBELGR = NBELEM(LIGREL,IGR)

C            -- QUELLES SONT LES CMPS PORTEES PAR LES POINTS DE GAUSS ?
C            ----------------------------------------------------------
            IF (NOMG1.EQ.'VARI_R') THEN
              NDDL = ICOEF1
              DO 10,K = 1,NDDL
                NUDDL(K) = K
   10         CONTINUE
            ELSE
              NDDL = 0
              DO 20,K = 1,NCMPM1
                IF (EXISDG(ZI(IAMOL1-1+4+1),K)) THEN
                  NDDL = NDDL + 1
                  NUDDL(NDDL) = K
                END IF
   20         CONTINUE
            END IF


C          -- BOUCLE SUR TOUS LES POINTS DE GAUSS DU GREL :
C          ------------------------------------------------
            DO 60,IEL = 1,NBELGR
              IF (NOMG1.EQ.'VARI_R') NDDL = ZI(JCELD1-1+
     &            ZI(JCELD1-1+4+IGR)+4+4* (IEL-1)+2)

              DO 50,IIPG = 1,NBPG
                IPG = IPG + 1
C            -- AU POINT DE GAUSS IPG CORRESPOND LA MAILLE NUMERO IPG
C               DANS MA2.
                IMA = IPG
                NBNO = NBNOMA(IMA)
                IF (NBNO.GT.27) CALL U2MESS('F','CALCULEL2_43')
                DO 40,INO = 1,NBNO
                  INOGL = NUMGLM(IMA,INO)
                  DO 30,IDDL = 1,NDDL
                    IDECA2 = NCMPM1* (INOGL-1) + NUDDL(IDDL)
                    JVAL2 = JCNSV2 - 1 + IDECA2
                    ZL(JCNSL2-1+IDECA2) = .TRUE.
                    ADIEL = ZI(JCELD1-1+ZI(JCELD1-1+4+IGR)+4+4* (IEL-1)+
     &                      4)
                    ZR(JVAL2) = ZR(JCELV1-1+ADIEL-1+NDDL* (IIPG-1)+IDDL)
   30             CONTINUE
   40           CONTINUE
   50         CONTINUE
   60       CONTINUE
   70     CONTINUE


C         -- ON TRANSFORME CH2S  EN VRAI CHAM_NO :
C         ----------------------------------------
          CALL CNSCNO(CH2S,PRCHNO,'NON','G',CH2,'F',IBID)

          CALL RSNOCH(RESU,NOMSY2,IORDR,' ')
          CALL JELIBE(CH1//'.CELV')
          CALL JELIBE(CH1//'.CELD')
          CALL JELIBE(CH1//'.CELK')
          CALL DETRSD('CHAMP_GD','&&ECLPGR.CH1')
          CALL DETRSD('CHAMP_GD','&&ECLPGR.CHR1')
   80   CONTINUE
        IF (ICO.EQ.0) CALL U2MESK('A','CALCULEL2_14',1,NOMSY1)
        CALL JEDETR ( NOMFPG )
        CALL DETRSD('CHAM_NO_S',CH2S)
   90 CONTINUE


C       -- ON RECOPIE LE PARAMETRE "INST" :
C       -----------------------------------
      DO 100,I = 1,NBORDR
        IORDR = ZI(JORDR+I-1)
        CALL RSADPA(EVO1,'L',1,'INST',IORDR,0,IAINS1,KBID)
        CALL RSADPA(RESU,'E',1,'INST',IORDR,0,IAINS2,KBID)
        ZR(IAINS2) = ZR(IAINS1)
  100 CONTINUE

      CALL JEDETC('V','&&ECLPGR',1)

  110 CONTINUE
      CALL JEDEMA()
      END
