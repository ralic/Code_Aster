      SUBROUTINE OP0150(IER)
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/01/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20 CRS_512
C     LECTURE D'UN RESULTAT SUR FICHIER EXTERNE AU FORMAT
C         - UNV (IDEAS)
C         - ENSIGHT
C         - MED

C     -----------------------------------------------------------------

      IMPLICIT NONE

C 0.1. ==> ARGUMENTS

      INTEGER IER

C 0.2. ==> COMMUNS
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)

      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX --------------------------

C 0.3. ==> VARIABLES LOCALES

      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='OP0150')
      INTEGER NTYMAX
      INTEGER VALI(2)
      PARAMETER (NTYMAX=53)
      INTEGER NNOMAX
      PARAMETER (NNOMAX=27)
      INTEGER LXLGUT
      INTEGER NDIM,TYPGEO(NTYMAX),LETYPE
      INTEGER NBTYP,NNOTYP(NTYMAX)
      INTEGER RENUMD(NTYMAX),NUANOM(NTYMAX,NNOMAX)
      INTEGER MODNUM(NTYMAX),NUMNOA(NTYMAX,NNOMAX)
      INTEGER NTO,NNU,JLIST,NBORDR,NBNOCH,NVAR
      INTEGER NBVARI,JNUME,NP,ICH,NIS,NPAS
      INTEGER IRET,NFOR,NBTITR,JTITR,LL
      INTEGER I,LONG,IO,NSCAL,NVECT,NFLAG
      INTEGER IPAS,I1,I2,NBNO,INOPR,NCARLU
      INTEGER I21,NLIG,IPRES,NBGR,NFACHA
      INTEGER IDEC,JCELD,NBELGR,IEL,IMA,LIEL
      INTEGER INO,NNO,II,IADNO,IAD,JCELV
      INTEGER LORDR,IORD,NBORLU,NC,INDIIS
      INTEGER IBID,NBV,NBTROU,NSTAR,J,IREST
      INTEGER TE,TYPELE,NBGREL,NFIC,NBELEM,IGR
      INTEGER MFICH,N1,PRECIS,JINST,ITPS,NBPASE
      INTEGER LNOMA,IFM,NIVINF,ULISOP,JREFE,JNUOM
      INTEGER NBMA,JNBPGM,JNBPMM
      REAL*8 EPSI,R8B
      CHARACTER*1 KBID
      CHARACTER*3 PROLZ
      CHARACTER*4 ACCE
      CHARACTER*8 RESU,NOMA,NOMO,TYPCHA,CHMAT,CARAEL
      CHARACTER*8 K8B,CRIT,CHAINE,MODELE,BASENO
      CHARACTER*8 LPAIN(1),LPAOUT(1),K8BID,BLAN8
      CHARACTER*8 NOMTYP(NTYMAX),PARAM
      CHARACTER*10 ACCES
      CHARACTER*13 INPSCO
      CHARACTER*16 NOMCMD,CONCEP,TYPRES,FICH
      CHARACTER*16 DIR,NOMTE,LINOCH(100),FORM,NOCH,K16NOM
      CHARACTER*19 LISTR8,LISTIS,NOMCH,LIGRMO,LIGREL,INFCHA
      CHARACTER*19 CHPRES,PREFIX,CHANOM,PCHN1,LISCHA,LISCH2
      CHARACTER*24 LCHIN(1),LCHOUT(1),CHAMP
      CHARACTER*24 VALK(2)
      CHARACTER*24 NOLIEL,NOMPRN, NOOBJ,FOMULT,K24B,COMPOR,CARCRI
      CHARACTER*80 K80B,FIRES,FIPRES,FIGEOM,FIC80B
      CHARACTER*24 CHGEOM,OPTION,CONNEX
      COMPLEX*16 CBID
      REAL*8 RBID
      CHARACTER*32 NOCHMD,NOMAMD
      CHARACTER*200 NOFIMD
      CHARACTER*255 KFIC
      INTEGER TYPENT,TYPGOM
      INTEGER EDNOEU
      PARAMETER (EDNOEU=3)
      INTEGER EDMAIL
      PARAMETER (EDMAIL=0)
      INTEGER EDNOMA
      PARAMETER (EDNOMA=4)
      INTEGER EDNONO
      PARAMETER (EDNONO=-1)
      INTEGER TYPNOE
      PARAMETER (TYPNOE=0)
      CHARACTER*1 SAUX01
      CHARACTER*8 SAUX08

      CHARACTER*8 NOMGD
      INTEGER NUMPT,NUMORD,INUM, N2, IORDR, JPARA
      INTEGER NBCMPV,IAUX,NPAS0,ITPS0,N3

      INTEGER IINST
      REAL*8 INST

      CHARACTER*24 NCMPVA,NCMPVM
      CHARACTER*7 LCMPVA
      PARAMETER (LCMPVA='NOM_CMP')
      CHARACTER*11 LCMPVM
      PARAMETER (LCMPVM='NOM_CMP_MED')
      INTEGER JCMPVA,JCMPVM,IU99,IU98,IU97,ULNUME,NEXCI

      CHARACTER*72 REP
      CHARACTER*32 K32B

      LOGICAL EXISTM,IDENSD,MATCST,COECST

C     -----------------------------------------------------------------

      CALL JEMARQ()

      LISCHA = '&&'//NOMPRO//'.LISCHA    '
      BLAN8  = '        '
      CALL GETRES(RESU,CONCEP,NOMCMD)
      CALL GETVTX(' ','TYPE_RESU',0,1,1,TYPRES,N1)
      CALL ASSERT(TYPRES.EQ.CONCEP)
      CALL GETVTX(' ','NOM_FICHIER',0,1,1,FICH,NFIC)
      NOMPRN = RESU//'.PRFCN00000.PRNO'

      CALL INFMAJ
      CALL INFNIV(IFM,NIVINF)


C     --- FORMAT ---
      CALL GETVTX(' ','FORMAT',0,1,1,FORM,NFOR)
      CALL GETVIS(' ','UNITE',0,1,1,MFICH,N1)
      IF ((N1.GT.0) .AND. (FORM.NE.'MED')) THEN
        K16NOM = ' '
        IF (ULISOP(MFICH,K16NOM).EQ.0) THEN
          CALL ULOPEN(MFICH,' ',' ','NEW','O')
        END IF
      END IF
      CALL GETVTX(' ','NOM_FICHIER',0,1,1,FICH,N1)


C     ---  LISTE DES CHAMPS A LIRE ---
      CALL GETFAC('FORMAT_MED',N1)
      IF (N1.GT.0) THEN
        NBNOCH = N1
        IF (NBNOCH.GT.100) THEN
          NBNOCH = -NBNOCH
        ELSE
          DO 10,I = 1,NBNOCH
            CALL GETVTX('FORMAT_MED','NOM_CHAM',I,1,1,LINOCH(I),N1)
   10     CONTINUE
        END IF
      ELSE
        CALL GETVTX(' ','NOM_CHAM',0,1,100,LINOCH,NBNOCH)
      END IF
      IF (NBNOCH.LT.0) CALL U2MESS('F','UTILITAI2_86')


C     --- NOMBRE DE VARIABLES INTERNES A LIRE ---
      CALL GETVIS(' ','NB_VARI',0,1,1,NBVARI,NVAR)


C     --- MAILLAGE ---
      CALL GETVID(' ','MAILLAGE',0,1,1,NOMA,NBV)


C     --- MODELE ---
      CALL GETVID(' ','MODELE',0,1,1,NOMO,NBV)
      IF (NBV.NE.0) THEN
        LIGREL = NOMO//'.MODELE'
        CALL JEVEUO(LIGREL//'.LGRF','L',LNOMA)
        NOMA = ZK8(LNOMA)
      END IF


C     --- QUELS SONT LES INSTANTS A RELIRE ---
      NNU=0
      NIS=0
      CALL GETVTX(' ','TOUT_ORDRE',0,1,1,K8B,NTO)
      IF (NTO.NE.0) THEN
        ACCES = 'TOUT_ORDRE'
        NBORDR = 100
        IINST=0
        GO TO 20
      END IF

      CALL GETVIS(' ','NUME_ORDRE',0,1,0,IBID,NNU)
      IF (NNU.NE.0) THEN
        ACCES = 'NUME_ORDRE'
        LISTIS = '&&'//NOMPRO
        NBORDR = -NNU
        CALL WKVECT(LISTIS//'.VALE','V V I',NBORDR,JNUME)
        CALL GETVIS(' ','NUME_ORDRE',0,1,NBORDR,ZI(JNUME),N1)
        IINST=0
        GO TO 20
      END IF

      CALL GETVID(' ','LIST_ORDRE',0,1,1,LISTIS,NNU)
      IF (NNU.NE.0) THEN
        ACCES = 'LIST_ORDRE'
        IINST=0
        CALL JEVEUO(LISTIS//'.VALE','L',JNUME)
        CALL JELIRA(LISTIS//'.VALE','LONMAX',NBORDR,K8B)
        GO TO 20
      END IF

      CALL GETVR8(' ','INST',0,1,0,RBID,NIS)
      IF (NIS.NE.0) THEN
        ACCES = 'INST'
        LISTR8 = '&&'//NOMPRO
        NBORDR = -NIS
        IINST=1
        CALL WKVECT(LISTR8//'.VALE','V V R',NBORDR,JLIST)
        CALL GETVR8(' ','INST',0,1,NBORDR,ZR(JLIST),N1)
        GO TO 20
      END IF

      CALL GETVID(' ','LIST_INST',0,1,1,LISTR8,NIS)
      IF (NIS.NE.0) THEN
        ACCES = 'LIST_INST'
        IINST=1
        CALL JEVEUO(LISTR8//'.VALE','L',JLIST)
        CALL JELIRA(LISTR8//'.VALE','LONMAX',NBORDR,K8B)
        GO TO 20
      END IF

      CALL GETVR8(' ','FREQ',0,1,0,RBID,NIS)
      IF (NIS.NE.0) THEN
        ACCES = 'FREQ'
        LISTR8 = '&&'//NOMPRO
        NBORDR = -NIS
        IINST=1
        CALL WKVECT(LISTR8//'.VALE','V V R',NBORDR,JLIST)
        CALL GETVR8(' ','FREQ',0,1,NBORDR,ZR(JLIST),N1)
        GO TO 20
      END IF

      CALL GETVID(' ','LIST_FREQ',0,1,1,LISTR8,NIS)
      IF (NIS.NE.0) THEN
        ACCES = 'LIST_FREQ'
        IINST=1
        CALL JEVEUO(LISTR8//'.VALE','L',JLIST)
        CALL JELIRA(LISTR8//'.VALE','LONMAX',NBORDR,K8B)
        GO TO 20
      END IF

   20 CONTINUE
      NBORLU = NBORDR

C     --- LECTURE DE LA PRECISION ET DU CRITERE ---

      CALL GETVR8(' ','PRECISION',0,1,1,EPSI,NP)
      CALL GETVTX(' ','CRITERE',0,1,1,CRIT,NC)
      PRECIS = 0
      IF (NP.NE.0) PRECIS = 1

C     --- NOMBRE DE VARIABLES INTERNES A LIRE ---

      CALL GETVIS(' ','NB_VARI',0,1,1,NBVARI,NVAR)

C     --- CREATION DE LA STRUCTURE DE DONNEES RESULTAT ---

      CALL RSCRSD('G',RESU,TYPRES,NBORDR)

C- ON VERIFIE SI LE CHAMP DEMANDE EST COMPATIBLE AVEC LE TYPE DE RESUTAT

      DO 30 ICH = 1,NBNOCH
        CALL RSUTCH(RESU,LINOCH(ICH),1,NOMCH,IRET)
        IF (IRET.NE.0) THEN
          VALK (1) = TYPRES
          VALK (2) = LINOCH(ICH)
          CALL U2MESG('F','UTILITAI8_24',2,VALK,0,0,0,0.D0)
        END IF
   30 CONTINUE



      IF (FORM.EQ.'IDEAS') THEN
C     =========================

        CALL JEEXIN(NOMA//'           .TITR',IRET)
        IF (IRET.EQ.0) THEN
          CALL U2MESS('A','UTILITAI2_87')
        ELSE
          CALL JEVEUO(NOMA//'           .TITR','L',JTITR)
          CALL JELIRA(NOMA//'           .TITR','LONMAX',NBTITR,K8B)
          IF (NBTITR.GE.1) THEN
            IF (ZK80(JTITR) (10:31).NE.'AUTEUR=INTERFACE_IDEAS') THEN
              CALL U2MESS('A','UTILITAI2_87')
            END IF
          ELSE
            CALL U2MESS('A','UTILITAI2_87')
          END IF
        END IF

C     --- LECTURE

        CALL LRIDEA(RESU,TYPRES,LINOCH,NBNOCH,NOMCMD,LISTR8,LISTIS,
     &              PRECIS,CRIT,EPSI,ACCES,MFICH,NOMA,LIGREL,NBVARI)

      ELSE IF (FORM.EQ.'IDEAS_DS58') THEN
C     ================================

        CALL JEEXIN(NOMA//'           .TITR',IRET)
        IF (IRET.EQ.0) THEN
          CALL U2MESS('A','UTILITAI2_87')
        ELSE
          CALL JEVEUO(NOMA//'           .TITR','L',JTITR)
          CALL JELIRA(NOMA//'           .TITR','LONMAX',NBTITR,K8B)
          IF (NBTITR.GE.1) THEN
            IF (ZK80(JTITR) (10:31).NE.'AUTEUR=INTERFACE_IDEAS') THEN
              CALL U2MESS('A','UTILITAI2_87')
            END IF
          ELSE
            CALL U2MESS('A','UTILITAI2_87')
          END IF
        END IF

C     --- LECTURE

        CALL LECT58(MFICH,RESU,NOMA,TYPRES,ACCES,LISTR8,LISTIS,PRECIS,
     &              CRIT,EPSI)

C     --- LECTURE

      ELSE IF (FORM.EQ.'ENSIGHT') THEN
C     ================================

        CALL JEEXIN(LIGREL//'.LGRF',IRET)
        IF (IRET.EQ.0) THEN
          CALL U2MESS('F','UTILITAI2_88')
        ELSE
          CALL DISMOI('F','DIM_GEOM',NOMO,'MODELE',NDIM,K8BID,IER)
        END IF

        IF (NBNOCH.NE.1 .OR. LINOCH(1).NE.'PRES') THEN
          CALL U2MESS('F','UTILITAI2_89')
        END IF

        CALL GETVTX(' ','NOM_FICHIER',0,1,1,FICH,NFIC)
        LL = LEN(FICH)
        DO 40 I = 1,LL
          IF (FICH(I:I).NE.' ') GO TO 40
          LONG = I - 1
          GO TO 50
   40   CONTINUE
   50   CONTINUE
C              1234567890123456
        DIR = 'DONNEES_ENSIGHT/'
        FIC80B = FICH
        FIRES = DIR//FICH(1:LONG)
        IU99 = ULNUME ()
        CALL ULOPEN(IU99,FIRES,' ','OLD','O')

C  LECTURE DU FICHIER RESULTS (PAS DE TEMPS
C                              ET FICHIERS GEOM ET PRES)
        READ (IU99,'(3I8)',ERR=290,END=290,IOSTAT=IO) NSCAL,NVECT,
     &    NFLAG
        READ (IU99,'(1I8)',ERR=290,END=290,IOSTAT=IO) NPAS

        CALL WKVECT('&&'//NOMPRO//'.INST','V V R',NPAS,IPAS)
        READ (IU99,'(6(E12.5))',ERR=290,END=290,
     &    IOSTAT=IO) (ZR(IPAS-1+I),I=1,NPAS)
        I1 = 0
        I2 = 0
        IF (NFLAG.GT.0) THEN
          READ (IU99,'(2I8)',ERR=290,END=290,IOSTAT=IO) I1,I2
        ELSE
          IF (NPAS.NE.1) THEN
            CALL U2MESS('F','UTILITAI2_90')
          END IF
        END IF
        READ (IU99,'(A80)',ERR=290,END=290,IOSTAT=IO) FIGEOM
        LONG = LXLGUT(FIGEOM)
        FIGEOM(1:16+LONG) = DIR//FIGEOM(1:LONG)
        READ (IU99,'(A80)',ERR=290,END=290,IOSTAT=IO) FIPRES
        CALL ULOPEN(-IU99,' ',' ',' ',' ')
        LONG = LXLGUT(FIGEOM)
        FIPRES(1:16+LONG) = DIR//FIPRES(1:LONG)

        FIC80B = FIGEOM
        IU98 = ULNUME ()
        CALL ULOPEN(IU98,FIGEOM,' ','OLD','O')
C  LECTURE DU FICHIER GEOM (NUMEROS DE NOEUDS)
        READ (IU98,'(A80)',ERR=290,END=290,IOSTAT=IO) K80B
        READ (IU98,'(A80)',ERR=290,END=290,IOSTAT=IO) K80B
        READ (IU98,'(1I8)',ERR=290,END=290,IOSTAT=IO) NBNO
        CALL WKVECT('&&'//NOMPRO//'.NUMNOEU','V V I',NBNO,INOPR)
        DO 60 I = 1,NBNO
          READ (IU98,'(1I8)',ERR=290,IOSTAT=IO) ZI(INOPR-1+I)
   60   CONTINUE
        CALL ULOPEN(-IU98,' ',' ',' ',' ')

        NSTAR = 0
        NCARLU = 0
        LL = LEN(FIPRES)
        DO 70 I = 1,LL
          IF (FIPRES(I:I).EQ.'*') THEN
            NSTAR = NSTAR + 1
          END IF
          IF (NSTAR.GT.0 .AND. FIPRES(I:I).EQ.' ') GO TO 80
          NCARLU = NCARLU + 1
   70   CONTINUE
   80   CONTINUE
        LL = NCARLU
        IF (NSTAR.GE.8) THEN
          VALI (1) = NSTAR
          CALL U2MESG('F','UTILITAI8_25',0,' ',1,VALI,0,0.D0)
        END IF

        CHGEOM = NOMA//'.COORDO'

        LPAIN(1) = 'PGEOMER'
        LCHIN(1) = CHGEOM

        LPAOUT(1) = 'PPRES_R'
        CHPRES = '&&CHPRES'
        LCHOUT(1) = CHPRES
        LIGRMO = NOMO//'.MODELE'
        OPTION = 'TOU_INI_ELNO'
        CALL CALCUL('S',OPTION,LIGRMO,1,LCHIN,LPAIN,1,LCHOUT,LPAOUT,'V')


C       -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
        CALL CELVER(CHPRES,'NBVARI_CST','STOP',IBID)
        CALL CELVER(CHPRES,'NBSPT_1','STOP',IBID)

        CALL JEVEUO(CHPRES//'.CELD','L',JCELD)
        CALL JEVEUO(CHPRES//'.CELV','E',JCELV)

C  BOUCLE SUR LES PAS DE TEMPS ET LECTURE DES PRESSIONS
C  ----------------------------------------------------

        DO 220 ITPS = 1,NPAS

          I21 = I1 + (ITPS-1)*I2
          CALL CODENT(I21,'D0',CHAINE)
          IF (NSTAR.GT.0) THEN
            FIPRES = FIPRES(1:LL-NSTAR)//CHAINE(9-NSTAR:8)
          END IF
          FIC80B = FIPRES
          IU97 = ULNUME ()
          CALL ULOPEN(IU97,FIPRES,' ','OLD','O')
          READ (IU97,'(A80)',ERR=290,END=290,IOSTAT=IO) K80B
          CALL WKVECT('&&'//NOMPRO//'.PRES.'//CHAINE,'V V R',NBNO,IPRES)
          NLIG = NBNO/6
          DO 90 I = 1,NLIG
            READ (IU97,'(6(E12.5))') (ZR(IPRES-1+6* (I-1)+J),J=1,6)
   90     CONTINUE
          IREST = NBNO - 6*NLIG
          IF (IREST.GT.0) THEN
            READ (IU97,'(6(E12.5))',ERR=290,END=290,
     &        IOSTAT=IO) (ZR(IPRES-1+6*NLIG+J),J=1,IREST)
          END IF
          CALL ULOPEN (-IU97,' ',' ',' ',' ')

C  REMPLISSAGE DU .VALE DU CHAM_ELEM DE PRES_R

          CONNEX = NOMA//'.CONNEX'
          NOLIEL = LIGRMO//'.LIEL'
          NBGR = NBGREL(LIGRMO)

          IF (NDIM.GE.3) THEN
            NFACHA = 0
            DO 150 IGR = 1,NBGR
              IDEC = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+8)
              TE = TYPELE(LIGRMO,IGR)
              CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',TE),NOMTE)

              IF (NOMTE.EQ.'MECA_FACE3' .OR. NOMTE.EQ.'MECA_FACE4' .OR.
     &            NOMTE.EQ.'MECA_FACE6' .OR. NOMTE.EQ.'MECA_FACE8' .OR.
     &            NOMTE.EQ.'MECA_FACE9' .OR. NOMTE.EQ.'MEQ4QU4' .OR.
     &            NOMTE.EQ.'MEDSQU4' .OR. NOMTE.EQ.'MEDSTR3' ) THEN
                NBELGR = NBELEM(LIGRMO,IGR)
                CALL JEVEUO(JEXNUM(NOLIEL,IGR),'L',LIEL)
                DO 140 IEL = 1,NBELGR
                  IMA = ZI(LIEL-1+IEL)
                  CALL JEVEUO(JEXNUM(CONNEX,IMA),'L',IADNO)
                  CALL JELIRA(JEXNUM(CONNEX,IMA),'LONMAX',NNO,KBID)
                  DO 100 INO = 1,NNO
                    II = INDIIS(ZI(INOPR),ZI(IADNO-1+INO),1,NBNO)
                    IF (II.EQ.0) GO TO 120
  100             CONTINUE

C   LA MAILLE IMA EST CHARGEE EN PRESSION

                  NFACHA = NFACHA + 1
                  IAD = JCELV - 1 + IDEC - 1 + NNO* (IEL-1)
                  DO 110 I = 1,NNO
                    II = INDIIS(ZI(INOPR),ZI(IADNO-1+I),1,NBNO)
                    ZR(IAD+I) = ZR(IPRES-1+II)
  110             CONTINUE
                  GO TO 140
  120             CONTINUE

C   LA MAILLE IMA N'EST PAS CHARGEE EN PRESSION

                  IAD = JCELV - 1 + IDEC - 1 + NNO* (IEL-1)
                  DO 130 I = 1,NNO
                    ZR(IAD+I) = 0.0D0
  130             CONTINUE

  140           CONTINUE
              ELSE
                CALL U2MESK('A','UTILITAI2_91',1,NOMTE)
              END IF
  150       CONTINUE
          END IF

          IF (NDIM.EQ.2 .OR. (NDIM.GE.3.AND.NFACHA.EQ.0)) THEN
            DO 210 IGR = 1,NBGR
              IDEC = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+8)
              TE = TYPELE(LIGRMO,IGR)
              CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',TE),NOMTE)

              IF (NOMTE.EQ.'MEPLSE2' .OR. NOMTE.EQ.'MEAXSE2' .OR.
     &            NOMTE.EQ.'MEPLSE3' .OR. NOMTE.EQ.'MEAXSE3') THEN
                NBELGR = NBELEM(LIGRMO,IGR)
                CALL JEVEUO(JEXNUM(NOLIEL,IGR),'L',LIEL)
                DO 200 IEL = 1,NBELGR
                  IMA = ZI(LIEL-1+IEL)
                  CALL JEVEUO(JEXNUM(CONNEX,IMA),'L',IADNO)
                  CALL JELIRA(JEXNUM(CONNEX,IMA),'LONMAX',NNO,KBID)
                  DO 160 INO = 1,NNO
                    II = INDIIS(ZI(INOPR),ZI(IADNO-1+INO),1,NBNO)
                    IF (II.EQ.0) GO TO 180
  160             CONTINUE

C   LA MAILLE IMA EST CHARGEE EN PRESSION

                  IAD = JCELV - 1 + IDEC - 1 + 2*NNO* (IEL-1)
                  DO 170 I = 1,NNO
                    II = INDIIS(ZI(INOPR),ZI(IADNO-1+I),1,NBNO)
                    ZR(IAD+2*I-1) = ZR(IPRES-1+II)
                    ZR(IAD+2*I) = 0.0D0
  170             CONTINUE
                  GO TO 200
  180             CONTINUE

C   LA MAILLE IMA N'EST PAS CHARGEE EN PRESSION

                  IAD = JCELV - 1 + IDEC - 1 + 2*NNO* (IEL-1)
                  DO 190 I = 1,2*NNO
                    ZR(IAD+I) = 0.0D0
  190             CONTINUE

  200           CONTINUE
              END IF
  210       CONTINUE

          END IF

          CALL RSEXCH(RESU,LINOCH(1),ITPS,NOMCH,IRET)
          IF (IRET.EQ.100) THEN
          ELSE IF (IRET.EQ.110) THEN
            CALL RSAGSD(RESU,0)
            CALL RSEXCH(RESU,LINOCH(1),ITPS,NOMCH,IRET)
          ELSE
            VALK (1) = RESU
            VALK (2) = NOMCH
            VALI (1) = ITPS
            VALI (2) = IRET
            CALL U2MESG('F','UTILITAI8_26',2,VALK,2,VALI,0,0.D0)
          END IF
          CALL COPISD('CHAMP_GD','G',CHPRES,NOMCH)
          CALL RSNOCH(RESU,LINOCH(1),ITPS,' ')
          CALL RSADPA(RESU,'E',1,'INST',ITPS,0,JINST,K8B)
          ZR(JINST) = ZR(IPAS-1+ITPS)

          CALL JEDETR('&&'//NOMPRO//'.PRES.'//CHAINE)

  220   CONTINUE



      ELSE IF (FORM.EQ.'MED') THEN
C     =============================

        IF ((TYPRES.EQ.'DYNA_TRANS')  .OR.
     &      (TYPRES.EQ.'DYNA_HARMO')  .OR.
     &      (TYPRES.EQ.'HARM_GENE')   .OR.
     &      (TYPRES.EQ.'MODE_MECA')   .OR.
     &      (TYPRES.EQ.'MODE_MECA_C')) THEN
          CALL U2MESK('F','UTILITAI5_40',1,TYPRES)
        ENDIF

C       ON VERIFIE QUE LE PHENOMENE DU MODELE FOURNI EST COHERENT AVEC
C       LA SD RESULTAT
        CALL GETVID(' ','MODELE',0,1,1,NOMO,NBV)
        IF (NBV.EQ.1) THEN
           CALL LRVEMO(NOMO)
        ENDIF


        DO 260 I = 1,NBNOCH
          OPTION = ' '
          PARAM  = ' '
          CALL GETVTX('FORMAT_MED','NOM_CHAM_MED',I,1,1,NOCHMD,N1)

          IF (N1.EQ.0) THEN
            CALL U2MESS('F','MED_94')
          END IF

          CALL GETVTX('FORMAT_MED','NOM_CHAM',I,1,1,NOCH,N1)
          IF ((TYPRES(1:9).EQ.'EVOL_THER') .AND.
     &        (NOCH(1:4).NE.'TEMP')) THEN
            CALL U2MESS('F','UTILITAI2_93')
          END IF
          IF (NOCH.EQ.'TEMP') THEN
            NOMGD = 'TEMP_R  '
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'DEPL') THEN
            NOMGD = 'DEPL_R  '
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'VITE') THEN
            NOMGD = 'DEPL_R  '
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'ACCE') THEN
            NOMGD = 'DEPL_R  '
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'SIEF_ELNO') THEN
            NOMGD = 'SIEF_R'
            TYPCHA = 'ELNO'
            OPTION = 'SIEF_ELNO_ELGA'
            PARAM  = 'PSIEFNOR'
          ELSE IF (NOCH.EQ.'SIEF_ELNO_ELGA') THEN
            NOMGD = 'SIEF_R'
            TYPCHA = 'ELNO'
            OPTION = 'SIEF_ELNO_ELGA'
            PARAM  = 'PSIEFNOR'
          ELSE IF (NOCH.EQ.'SIEF_NOEU') THEN
            NOMGD = 'SIEF_R'
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'SIEF_ELGA') THEN
            NOMGD = 'SIEF_R'
            TYPCHA = 'ELGA'
            OPTION = 'RAPH_MECA'
            PARAM  = 'PCONTPR'
          ELSE IF (NOCH.EQ.'SIEF_ELGA_DEPL') THEN
            NOMGD = 'SIEF_R'
            TYPCHA = 'ELGA'
            OPTION = 'SIEF_ELGA_DEPL'
            PARAM  = 'PCONTPR'
          ELSE IF (NOCH.EQ.'EPSI_ELNO_DEPL') THEN
            NOMGD = 'EPSI_R'
            TYPCHA = 'ELNO'
            OPTION ='EPSI_ELNO_DEPL'
            PARAM='PDEFORR'
          ELSE IF (NOCH.EQ.'EPSI_ELNO_TUYO') THEN
            NOMGD = 'EPSI_R'
            TYPCHA = 'ELNO'
            OPTION ='EPSI_ELNO_TUYO'
            PARAM='PDEFONO'
          ELSE IF (NOCH.EQ.'EQUI_ELGA_EPME') THEN
            NOMGD  = 'EPSI_R'
            TYPCHA = 'ELGA'
            OPTION = 'EQUI_ELGA_EPME'
            PARAM  = 'PDEFOEQ'
          ELSE IF (NOCH.EQ.'EPSI_NOEU') THEN
            NOMGD = 'EPSI_R'
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'EPSI_ELGA') THEN
            NOMGD = 'EPSI_R'
            TYPCHA = 'ELGA'
            OPTION = 'EPSI_ELGA_DEPL'
            PARAM  = 'PDEFORR'
          ELSE IF (NOCH.EQ.'EPSI_ELGA_DEPL') THEN
            NOMGD = 'EPSI_R'
            TYPCHA = 'ELGA'
            OPTION = 'EPSI_ELGA_DEPL'
            PARAM  = 'PDEFORR'
          ELSE IF (NOCH.EQ.'EPSA_ELNO') THEN
            NOMGD = 'EPSI_R'
            TYPCHA = 'ELNO'
            OPTION ='EPSI_ELNO_DEPL'
            PARAM='PDEFORR'
          ELSE IF (NOCH.EQ.'EPSA_NOEU') THEN
            NOMGD = 'EPSI_R'
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'VARI_ELNO') THEN
            NOMGD = 'VARI_R'
            TYPCHA = 'ELNO'
            OPTION='VARI_ELNO_ELGA'
            PARAM='PVARINR'
          ELSE IF (NOCH.EQ.'VARI_ELNO_ELGA') THEN
            NOMGD = 'VARI_R'
            TYPCHA = 'ELNO'
            OPTION='VARI_ELNO_ELGA'
            PARAM='PVARINR'
          ELSE IF (NOCH.EQ.'VARI_NOEU') THEN
            NOMGD = 'VARI_R'
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'VARI_ELGA') THEN
            NOMGD = 'VARI_R'
            TYPCHA = 'ELGA'
            OPTION = 'RAPH_MECA'
            PARAM  = 'PVARIPR'
          ELSE IF (NOCH.EQ.'EQUI_ELNO_SIGM') THEN
            NOMGD = 'SIEF_R'
            TYPCHA = 'ELNO'
          ELSE IF (NOCH.EQ.'EQUI_NOEU_SIGM') THEN
            NOMGD = 'SIEF_R'
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'EQUI_ELGA_SIGM') THEN
            NOMGD = 'SIEF_R'
            TYPCHA = 'ELGA'
            OPTION = 'EQUI_ELGA_SIGM'
            PARAM  = 'PCONTEQ'
          ELSE IF (NOCH.EQ.'EQUI_ELGA_EPSI') THEN
            NOMGD = 'EPSI_R'
            TYPCHA = 'ELGA'
            OPTION = 'EQUI_ELGA_EPSI'
            PARAM  = 'PDEFOEQ'
          ELSE IF (NOCH.EQ.'PRES') THEN
            NOMGD = 'PRES_R  '
            TYPCHA = 'ELEM'
          ELSE IF (NOCH.EQ.'IRRA') THEN
            NOMGD = 'IRRA_R  '
            TYPCHA = 'NOEU'
          ELSE IF (NOCH.EQ.'EPSG_ELGA_DEPL') THEN
            NOMGD  = 'EPSI_R'
            TYPCHA = 'ELGA'
            OPTION = 'EPSG_ELGA_DEPL'
            PARAM  = 'PDEFORR'
          ELSE IF (NOCH.EQ.'EPME_ELNO_DEPL') THEN
            NOMGD  = 'EPSI_R'
            TYPCHA = 'ELNO'
          ELSE IF (NOCH.EQ.'EPSP_ELNO') THEN
            NOMGD  = 'EPSI_R'
            TYPCHA = 'ELNO'
            OPTION = 'EPSP_ELNO'
            PARAM  = 'PDEFOPL'
          ELSE
            CALL U2MESK('F','UTILITAI2_94',1,NOCH)
          END IF

C         VERIFICATION DE LA PRESENCE DE 'PARAM' ET 'OPTION'
C         POUR LES CHAMPS ELGA
          IF(NOCH(6:9).EQ.'ELGA') THEN
            CALL ASSERT(OPTION.NE.' ')
            CALL ASSERT(PARAM .NE.' ')
          ENDIF

C          ==> NOM DES COMPOSANTES VOULUES

          NCMPVA = '&&'//NOMPRO//'.'//LCMPVA
          NCMPVM = '&&'//NOMPRO//'.'//LCMPVM

C         --- NOM_CMP ASTER ?
          NBCMPV=0
          CALL GETVTX('FORMAT_MED',LCMPVA,I,1,0,REP,IAUX)
          IF (IAUX.LT.0) THEN
            NBCMPV = -IAUX
          ENDIF

C         --- NOM_CMP MED ?
          CALL GETVTX('FORMAT_MED',LCMPVM,I,1,0,REP,IAUX)
          IF (-IAUX.NE.NBCMPV) THEN
             VALK(1) = LCMPVA
             VALK(2) = LCMPVM
             CALL U2MESK('F','UTILITAI2_95', 2 ,VALK)
          ENDIF

C         --- LECTURE DES NOMS DE COMPOSANTES ASSOCIEES DEUX A DEUX
          IF (NBCMPV.GT.0) THEN
            CALL WKVECT(NCMPVA,'V V K8',NBCMPV,JCMPVA)
            CALL GETVTX('FORMAT_MED',LCMPVA,I,1,NBCMPV,ZK8(JCMPVA),
     &                  IAUX)
            CALL WKVECT(NCMPVM,'V V K16',NBCMPV,JCMPVM)
            CALL GETVTX('FORMAT_MED',LCMPVM,I,1,NBCMPV,ZK16(JCMPVM),
     &                  IAUX)
          ENDIF

C         --- PROLONGEMENT PAR ZERO OU NOT A NUMBER

          CALL GETVTX(' ', 'PROL_ZERO', 0, 1, 1, PROLZ, IAUX)
          IF (PROLZ .NE. 'OUI') THEN
             PROLZ = 'NAN'
          ENDIF

C         --- NOM DU FICHIER MED
          CALL ULISOG(MFICH, KFIC, SAUX01)
          IF ( KFIC(1:1).EQ.' ' ) THEN
            CALL CODENT ( MFICH, 'G', SAUX08 )
            NOFIMD = 'fort.'//SAUX08
          ELSE
            NOFIMD = KFIC(1:200)
          ENDIF
C
          IF ( NIVINF.GT.1 ) THEN
            WRITE (IFM,*) '<',NOMPRO,'> NOM DU FICHIER MED : ',NOFIMD
          ENDIF
C                   12   345678   90123456789
          PREFIX = '&&'//NOMPRO//'.MED'
          CALL JEDETC('V',PREFIX,1)

C     -- RECUPERATION DU NOMBRE DE PAS DE TEMPS DANS LE CHAMP
C     --------------------------------------------

          IF (TYPCHA(1:2).EQ.'NO') THEN
            TYPENT = EDNOEU
            TYPGOM = TYPNOE
            CALL MDCHIN(NOFIMD,NOCHMD,TYPENT,TYPGOM,PREFIX,NPAS,IRET)
            IF (NPAS.EQ.0) THEN
              CALL U2MESK('A','MED_95',1,NOCHMD)
              GO TO 240
            END IF
            CALL JEVEUO(PREFIX//'.INST','L',IPAS)
            CALL JEVEUO(PREFIX//'.NUME','L',INUM)


          ELSE IF (TYPCHA(1:2).EQ.'EL') THEN
            CALL MDEXPM(NOFIMD,NOMAMD,EXISTM,NDIM,IRET)
            CALL LRMTYP(NBTYP,NOMTYP,NNOTYP,TYPGEO,RENUMD,
     &              MODNUM, NUANOM, NUMNOA )

            IF(TYPCHA(1:4).EQ.'ELNO')THEN
              TYPENT = EDNOMA
            ELSE
              TYPENT = EDMAIL
            ENDIF

            DO 230,LETYPE = 1,NBTYP
              IAUX = RENUMD(LETYPE)
              TYPGOM = TYPGEO(IAUX)
              CALL MDCHIN(NOFIMD,NOCHMD,TYPENT,TYPGOM,PREFIX,NPAS,IRET)

              IF (NPAS.NE.0) THEN
                CALL JEVEUO(PREFIX//'.INST','L',IPAS)
                CALL JEVEUO(PREFIX//'.NUME','L',INUM)
                GO TO 240
              END IF
  230       CONTINUE

C           CAS PARTICULIER: LECTURE DU FICHIER MED DONT L'ENTITE
C           DES CHAMPS ELNO EST ENCORE 'MED_MAILLE'
            IF(TYPCHA(1:4).EQ.'ELNO')THEN
              TYPENT = EDMAIL
              CALL U2MESK('A','MED_53',1,NOCHMD)
              DO 231,LETYPE = 1,NBTYP
               IAUX = RENUMD(LETYPE)
               TYPGOM = TYPGEO(IAUX)
               CALL MDCHIN(NOFIMD,NOCHMD,TYPENT,TYPGOM,PREFIX,NPAS,IRET)
               IF (NPAS.NE.0) THEN
                  CALL JEVEUO(PREFIX//'.INST','L',IPAS)
                  CALL JEVEUO(PREFIX//'.NUME','L',INUM)
                  GO TO 240
               END IF
  231        CONTINUE
           END IF

          END IF
  240     CONTINUE

C
C
         IF(ACCES.NE.'TOUT_ORDRE')THEN
             NPAS0=NBORDR
         ELSE
             NPAS0=NPAS
         ENDIF

C        DETERMINATION DES NUMEROS D'ORDRE MED : ZI(JNUOM)
         IF(NNU.NE.0)THEN
            CALL WKVECT('&&OP0150_NUMORD_MED','V V I',NPAS,JNUOM)
            DO 242 J=1,NPAS
              ZI(JNUOM+J-1)=ZI(INUM+2*J-1)
 242        CONTINUE
         ENDIF

         CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',
     &                                               NBMA,K8B,IRET)
         CALL WKVECT('&&OP0150_NBPG_MAILLE','V V I',NBMA,JNBPGM)
         CALL WKVECT('&&OP0150_NBPG_MED','V V I',NBMA,JNBPMM)



C     -- BOUCLE SUR LES PAS DE TEMPS
C     --------------------------------------------

          DO 250 ITPS = 1,NPAS0
            CHANOM = '&&OP0150.TEMPOR'
            K32B = '                                '
C
            IF(NNU.NE.0)THEN
              NUMORD = ZI(JNUME+ITPS-1)
              ITPS0=INDIIS(ZI(JNUOM),NUMORD,1,NPAS)
              IF(ITPS0.EQ.0)THEN
                CALL U2MESG('A','MED_87',1,RESU,1,NUMORD,0,R8B)
                GOTO 250
              ENDIF
              NUMPT=ZI(INUM+2*ITPS0-2)
            ELSEIF(NTO.NE.0)THEN
              NUMORD = ZI(INUM+2*ITPS-1)
              NUMPT  = ZI(INUM+2*ITPS-2)
            ELSEIF(NIS.NE.0)THEN
              INST = ZR(JLIST+ITPS-1)
            ENDIF
C
            CALL LRCHME(CHANOM,NOCHMD,K32B,NOMA,TYPCHA,NOMGD,TYPENT,
     &                  NBCMPV,NCMPVA,NCMPVM,PROLZ,
     &                  IINST,NUMPT,NUMORD,INST,CRIT,EPSI,
     &                  MFICH,LIGREL,OPTION,PARAM,ZI(JNBPGM),ZI(JNBPMM),
     &                  IRET)


C        -- POUR LES CHAM_NO :
C        -- POUR ECONOMISER L'ESPACE, ON ESSAYE DE PARTAGER LE PROF_CHNO
C           DU CHAMP CREE AVEC LE PROF_CHNO PRECEDENT :
            IF (TYPCHA.EQ.'NOEU') THEN
               CALL DISMOI('F','PROF_CHNO',CHANOM,'CHAM_NO',IBID,
     &                     PCHN1,IER)
               IF (.NOT.IDENSD('PROF_CHNO',NOMPRN(1:19),PCHN1) )  THEN
                  CALL GNOMSD ( NOMPRN,15,19 )
                  CALL COPISD ( 'PROF_CHNO', 'G', PCHN1, NOMPRN )
               END IF
               CALL JEVEUO ( CHANOM//'.REFE', 'E', JREFE )
               ZK24(JREFE+1) = NOMPRN(1:19)
               CALL DETRSD ( 'PROF_CHNO', PCHN1)
            END IF

            IF (NUMORD.EQ.EDNONO) THEN
              NUMORD = NUMPT
            END IF

            CALL RSEXCH(RESU,LINOCH(I),NUMORD,NOMCH,IRET)
            IF (IRET.EQ.100) THEN
            ELSE IF (IRET.EQ.110) THEN
              CALL RSAGSD(RESU,0)
              CALL RSEXCH(RESU,LINOCH(I),NUMORD,NOMCH,IRET)
            ELSE
              VALK (1) = RESU
              VALK (2) = CHANOM
              VALI (1) = ITPS
              VALI (2) = IRET
              CALL U2MESG('F','UTILITAI8_27',2,VALK,2,VALI,0,0.D0)
            END IF
            CALL COPISD('CHAMP_GD','G',CHANOM,NOMCH)
            CALL RSNOCH(RESU,LINOCH(I),NUMORD,' ')
            CALL RSADPA(RESU,'E',1,'INST',NUMORD,0,JINST,K8B)
            IF(NIS.NE.0)THEN
               ZR(JINST) = INST
            ELSEIF(NNU.NE.0)THEN
               ZR(JINST) = ZR(IPAS-1+ITPS0)
            ELSEIF(NTO.NE.0)THEN
               ZR(JINST) = ZR(IPAS-1+ITPS)
            ENDIF
            CALL DETRSD('CHAMP_GD',CHANOM)
  250     CONTINUE
          CALL JEDETR('&&OP0150_NBPG_MAILLE')
          CALL JEDETR('&&OP0150_NBPG_MED')
          CALL JEDETR(NCMPVA)
          CALL JEDETR(NCMPVM)
          CALL JEDETR('&&OP0150_NUMORD_MED')
  260   CONTINUE


      ELSE
        CALL ASSERT(.FALSE.)
      END IF

C - STOCKAGE EVENTUEL : MODELE, CHAM_MATER, CARA_ELEM, EXCIT
C   --------------------------------------------------------
       CALL GETVID(' ','CHAM_MATER',0,1,0,K8B,N1)
       CALL GETVID(' ','CARA_ELEM',0,1,0,K8B,N2)
       CALL GETVID(' ','MODELE',0,1,0,K8B,N3)

       IF ( ( (N1.EQ.0) .OR. (N2.EQ.0) .OR. (N3.EQ.0) ) .AND.
     &  ( (TYPRES.EQ.'EVOL_CHAR') .OR. (TYPRES.EQ.'HARM_GENE') ) ) THEN
         CALL U2MESK('A','UTILITAI5_93',1,TYPRES)
         GOTO 35
       ENDIF

       CALL RSORAC(RESU,'LONUTI',IBID,RBID,K8B,CBID,EPSI,CRIT,NBORDR,1,
     &            NBTROU)
       IF (NBORDR.LE.0)  CALL U2MESS('F','UTILITAI2_97')
       CALL WKVECT('&&'//NOMPRO//'.NUME_ORDR','V V I',NBORDR,LORDR)
       CALL RSORAC(RESU,'TOUT_ORDRE',IBID,RBID,K8B,CBID,EPSI,CRIT,
     &            ZI(LORDR),NBORDR,NBTROU)

       IF(N1.NE.0)THEN
         CALL GETVID(' ','CHAM_MATER',0,1,1,CHMAT,N1)
         DO 31 I=1,NBORDR
            IORDR=ZI(LORDR+I-1)
            CALL RSADPA(RESU,'E',1,'CHAMPMAT',IORDR,0,JPARA,K8B)
            ZK8(JPARA)=CHMAT
 31      CONTINUE
       ENDIF
       IF(N2.NE.0)THEN
         CALL GETVID(' ','CARA_ELEM',0,1,1,CARAEL,N2)
         DO 32 I=1,NBORDR
            IORDR=ZI(LORDR+I-1)
            CALL RSADPA(RESU,'E',1,'CARAELEM',IORDR,0,JPARA,K8B)
            ZK8(JPARA)=CARAEL
 32      CONTINUE
       ENDIF
       IF(N3.NE.0)THEN
         CALL GETVID(' ','MODELE',0,1,1,MODELE,N3)
         IF(TYPRES(1:9).EQ.'EVOL_NOLI')THEN
           CALL NMDORC(MODELE,COMPOR,CARCRI)
           DO 320 I=1,NBORDR
             IORDR=ZI(LORDR+I-1)
             CALL RSEXCH(RESU,'COMPORTEMENT',IORDR,CHAMP,IRET)
             IF (IRET.LE.100) THEN
               CALL COPISD('CHAMP_GD','G',COMPOR(1:19),CHAMP(1:19))
               CALL RSNOCH(RESU,'COMPORTEMENT',IORDR,' ')
             END IF
 320       CONTINUE
         ENDIF
         DO 33 I=1,NBORDR
            IORDR=ZI(LORDR+I-1)
            CALL RSADPA(RESU,'E',1,'MODELE',IORDR,0,JPARA,K8B)
            ZK8(JPARA)=MODELE
 33      CONTINUE
       ENDIF
       CALL GETFAC('EXCIT',NEXCI)
       IF(NEXCI.GT.0)THEN
         IF(TYPRES(1:4).EQ.'DYNA'.OR.TYPRES(1:4).EQ.'MODE')THEN
           CALL U2MESK('A','UTILITAI5_94',1,TYPRES)
           GOTO 35
         ENDIF
         NOOBJ ='12345678'//'.1234'//'.EXCIT.INFC'
         CALL GNOMSD(NOOBJ,10,13)
         LISCH2 = NOOBJ(1:19)
         IAUX = 1
         BASENO='&&'//NOMPRO
         INPSCO='&&'//NOMPRO//'_PSCO'
         CALL PSLECT ( ' ', IBID, BASENO, RESU, IAUX,
     &                NBPASE, INPSCO, IRET )
         IF( TYPRES.EQ.'EVOL_ELAS' .OR.
     &       TYPRES.EQ.'EVOL_NOLI' )THEN
            CALL NMDOME ( K24B, K24B, K24B, LISCHA,
     &                    NBPASE, INPSCO ,BLAN8,IBID)
         ELSEIF(TYPRES.EQ.'EVOL_THER')THEN
            INFCHA = '&&'//NOMPRO//'_INFCHA    '
            CALL NTDOTH ( K24B, K24B, K24B, FOMULT, MATCST,
     &           COECST, INFCHA, NBPASE, INPSCO,BLAN8,IBID)
         ENDIF
         DO 34 I=1,NBORDR
            IORDR=ZI(LORDR+I-1)
            CALL RSADPA(RESU,'E',1,'EXCIT',IORDR,0,JPARA,K8B)
            ZK24(JPARA)=LISCH2
 34      CONTINUE
         CALL COPISD(' ','G',LISCHA,LISCH2)
       ENDIF
 35    CONTINUE

C     -- QUELQUES VERIFS APRES LA LECTURE :
C     --------------------------------------------
      CALL RSORAC(RESU,'LONUTI',IBID,RBID,K8B,CBID,EPSI,CRIT,NBORDR,1,
     &            NBTROU)
      IF (NBORDR.LE.0) THEN
        CALL U2MESS('F','UTILITAI2_97')
      END IF
      CALL JEDETR('&&'//NOMPRO//'.NUME_ORDR')
      CALL WKVECT('&&'//NOMPRO//'.NUME_ORDR','V V I',NBORDR,LORDR)
      CALL RSORAC(RESU,'TOUT_ORDRE',IBID,RBID,K8B,CBID,EPSI,CRIT,
     &            ZI(LORDR),NBORDR,NBTROU)
      ACCE = 'INST'
      CALL RSEXPA(RESU,0,'FREQ',IRET)
      IF (IRET.GT.0) ACCE = 'FREQ'


C     -- MESSAGE D'INFORMATION SUR CE QU'ON A LU :
C     --------------------------------------------
      IF (NIVINF.GE.2) THEN
        WRITE (IFM,*) ' LECTURE DES CHAMPS:'
        DO 270 ICH = 1,NBNOCH
          WRITE (IFM,*) '    CHAMP : ',LINOCH(ICH)
  270   CONTINUE

        IF (NIVINF.GE.2) THEN
          DO 280 IORD = 1,NBORDR
            CALL RSADPA(RESU,'L',1,ACCE,ZI(LORDR+IORD-1),0,JINST,K8B)
            WRITE (IFM,*) '    NUMERO D''ORDRE : ',ZI(LORDR+IORD-1),
     &        '    '//ACCES//' : ',ZR(JINST)
  280     CONTINUE
        END IF
      END IF


      IF (NTO.EQ.0) THEN
        IF (NBORDR.NE.NBORLU .AND. FORM(1:3).NE.'MED') THEN
          CALL U2MESS('F','UTILITAI2_98')
        END IF
      END IF


      CALL TITRE


C     -- CREATION D'UN .REFD VIDE SI NECESSAIRE :
C     ---------------------------------------------------
      IF( TYPRES.EQ.'HARM_GENE'  .OR.
     &    TYPRES.EQ.'DYNA_TRANS' .OR.
     &    TYPRES.EQ.'DYNA_HARMO' .OR.
     &    TYPRES(1:9).EQ.'MODE_MECA' )THEN
         CALL AJREFD(' ',RESU,'FORCE')
      ENDIF


      GO TO 300

  290 CONTINUE


C     -- MESSAGE D'ERREUR DE LECTURE :
C     --------------------------------------------
      IF (IO.LT.0) THEN
        CALL U2MESG('F+','UTILITAI8_28',0,' ',0,0,0,0.D0)
      ELSE IF (IO.GT.0) THEN
        CALL U2MESG('F+','UTILITAI8_29',0,' ',0,0,0,0.D0)
      END IF
      VALK (1) = FIC80B(1:24)
      CALL U2MESG('F','UTILITAI8_30',1,VALK,0,0,0,0.D0)

  300 CONTINUE
      CALL JEDEMA()
      END
