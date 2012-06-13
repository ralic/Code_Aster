      SUBROUTINE IRCERS(IFI,LIGREL,NBGREL,LONGR,NCMPMX,VALE,NOMGD,
     &   NOMCMP,TITR,NOMEL,LOC,CELD,NBNOMA,PERMUT,MAXNOD,TYPMA,
     &   NOMSD,NOMSYM,IR,NBMAT,NUMMAI,LMASU,NCMPU,NUCMP,NBCMP,NCMPS,
     &   NOCMPL)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'jeveux.h'
      INTEGER       IFI, LIGREL(*), NBGREL, LONGR(*), NCMPMX,
     &              CELD(*), NCMPU, NUCMP(*), NBNOMA(*), TYPMA(*),
     &              PERMUT(MAXNOD,*), NBMAT, NUMMAI(*),NBCMP,NCMPS(*)
      CHARACTER*(*) NOMGD,NOMCMP(*),NOMEL(*),LOC,TITR,NOMSYM,NOMSD,
     &              NOCMPL(*)
      REAL*8        VALE(*)
      LOGICAL       LMASU
C--------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_21
C TOLE CRP_20
C        ECRITURE D'UN CHAMELEM SUR FICHIER UNIVERSEL, DATASET TYPE 56
C                                                                OU 57
C        A VALEURS REELLES
C  ENTREE:
C     IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
C     LIGREL: LIGREL COMPLET
C     NBGREL: NOMBRE DE GRELS
C     LONGR : POINTEUR DE LONGUEUR DE LIGREL
C     NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
C     VALE  : VALEURS DU CHAM_NO
C     NOMGD : NOM DE LA GRANDEUR: SIEF_R, EPSI_R,...
C     NOMCMP: NOMS DES CMP
C     TITR  : 1 LIGNE  DE TITRE
C     NOMEL : NOMS DES MAILLES SUPPORTS DES ELEMENTS
C     LOC   : LOCALISATION DES VALEURS (ELNO =>57, ELGA=>56,ELEM=>56)
C     CELD  : DESCRIPTEUR DU CHAM_ELEM (MODES LOCAUX,ADRESSES->.CELV)
C     NBNOMA: NOMBRE DE NOEUDS DE CHAQUE MAILLE
C     PERMUT: TABLEAU DES PERMUTATIONS DES NOEUDS DE CHAQUE TYPE-MA
C     MAXNOD: NOMBRE MAXI DE NOEUDS POUR LES DIFF. TYPE_MAILLES
C     TYPMA : TYPE_MAILLES
C     IR    : NUMERO D'ORDRE DU CHAMP
C     NBMAT : NOMBRE DE MAILLES A IMPRIMER
C     NUMMAI: NUMEROS DES MAILLES A IMPRIMER
C     LMASU : INDIQUE SI MAILLAGE ISSU DE SUPERTAB  .TRUE.
C     NBCMP : NOMBRE DE COMPOSANTES A IMPRIMER
C
      INTEGER DIGDEL
      LOGICAL EXISDG
C     ------------------------------------------------------------------
      CHARACTER*3  TOTO
      CHARACTER*4  NOMGS
      CHARACTER*8  NOCMP, KTYPE
      CHARACTER*24 NOMST
      CHARACTER*80 ENTETE(10),TITRE,TEXTE
      INTEGER      NBCHS,NBCMPT,ENTIER,NBSPT,NNOE,ILONG,IMODEL
      INTEGER      IMPRE,IENTE,IMPEL
      LOGICAL      AFAIRE,LCMP,LNOCEN
C
C  --- INITIALISATIONS ----
C
      CALL JEMARQ()
C
      CALL WKVECT('&&IRCERS.NOMGDS','V V K8',NCMPMX,INOGDS)
      CALL WKVECT('&&IRCERS.NOMCHS','V V K8',NCMPMX,INOCHS)
      CALL WKVECT('&&IRCERS.NBCMPS','V V I' ,NCMPMX,IBCMPS)
      CALL WKVECT('&&IRCERS.IPCMPS','V V I' ,NCMPMX*NCMPMX,ICMPS)
      CALL WKVECT('&&IRCERS.LTABL' ,'V V L' ,NCMPMX,ITABL)
      CALL JEVEUO ('&CATA.TE.MODELOC', 'L', IMODEL )
      CALL JEVEUO (JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',ILONG)
C
      NOMST= '&&IRECRI.SOUS_TITRE.TITR'
      CALL JEVEUO(NOMST,'L',JTITR)
      TITRE = ZK80(JTITR)
      DO 1 I=1,NCMPMX
        ZL(ITABL-1+I)=.FALSE.
 1    CONTINUE
      LNOCEN=.FALSE.
      LCMP=.FALSE.
C
C  --- RECHERCHE DES GRANDEURS SUPERTAB ----
C
      CALL IRGAGS(NCMPMX,NOMCMP,NOMSYM,NBCHS,ZK8(INOCHS),
     &                 ZI(IBCMPS),ZK8(INOGDS),ZI(ICMPS))


C     NOM DE LA GRANDEUR SUPERTAB
      IF (NBCMP.NE.0)THEN
         LCMP=.TRUE.
         IF(NOMGD.NE.'VARI_R')THEN
            DO 897 I=1,NBCHS
               DO 898 J=1,ZI(IBCMPS+I-1)
                  IF(NCMPS(1).EQ.ZI(ICMPS-1+(I-1)*NCMPMX+J)) GOTO 899
 898           CONTINUE
 897        CONTINUE
 899        CONTINUE
            NOMGS=ZK8(INOGDS-1+I)
         ENDIF
      ENDIF
C --- DETERMINATION DU NOMBRE MAXIMUM DE SOUS-POINTS ---
      ICOMAX = 0
      DO 8 IGRE=1,NBGREL
         ICOEF=MAX(1,CELD(4))
         IF (ICOEF.GT.ICOMAX) ICOMAX=ICOEF
   8  CONTINUE
      ICOMM = 6
      IF ( NCMPU .EQ. 0 ) THEN
         ICMAX0 = ICOMAX
      ELSE
         ICMAX0 = NCMPU
      ENDIF
C
C -- ALLOCATION DES TABLEAUX DE TRAVAIL ---
C
      IF (LOC.EQ.'ELNO') THEN
        CALL JEDETR('&&IRCERS.VALNOE')
        CALL WKVECT('&&IRCERS.VALNOE','V V R',NCMPMX*ICOMM,IRVN)
      ELSEIF (LOC.EQ.'ELGA'.OR.LOC.EQ.'ELEM') THEN
        CALL JEDETR('&&IRCERS.VALGAU')
        CALL WKVECT('&&IRCERS.VALGAU','V V R',NCMPMX*ICOMM,IRVG)
      ENDIF
C
C ---- BOUCLE SUR LES DIVERSES GRANDEURS SUPERTAB ----
      IF (NBCMP.NE.0 .AND. NOMGD.NE.'VARI_R') NBCHS=1
      DO 10 ICHS = 1,NBCHS
        IF( ICHS.GT.1) THEN
         AFAIRE = .FALSE.
         DO 2 ICP=1,ZI(IBCMPS-1+ICHS)
           AFAIRE= (AFAIRE.OR.ZL(ITABL-1+(ZI(ICMPS-1
     &                 +(ICHS-1)*NCMPMX+ICP))))
 2       CONTINUE
         IF(.NOT. AFAIRE) GO TO 10
        ENDIF
C
        CALL JEDETR('&&IRCERS.SOUS_PT')
        CALL WKVECT('&&IRCERS.SOUS_PT','V V I',ICOMAX,JSPT)
C
C --- GROUPEMENT DES VARIABLES SCALAIRES 6 PAR 6 ----
C  ---  DETERMINATION DU NOMBRE DE DATASETS SUPERTAB A IMPRIMER --
C
        IF(ZI(IBCMPS-1+ICHS).EQ.1.AND.ICOMAX.GT.1) THEN
          DO 42 I=1,ICMAX0
            ZI(JSPT-1+I)=6
42        CONTINUE
          ILIG=ICMAX0/6
          IRES=ICMAX0-ILIG*6
          IF(IRES.EQ.0) THEN
            NBDATS=ILIG
          ELSE
            NBDATS=ILIG+1
            ZI(JSPT-1+NBDATS)=IRES
          ENDIF
          NBCMPT=6
          NOMGS='VARI'
        ELSEIF(NBCMP.NE.0 .AND. NOMGD.NE.'VARI_R')THEN
          CALL WKVECT('&&IRCERS.SCMP_DATS','V V I',NBCMP,INDATS)
          ILIG=NBCMP/6
          IRES=NBCMP-ILIG*6
          NI=0
          ZI(INDATS)=NI
          IF(IRES.EQ.0) THEN
            NBDATS=ILIG
            DO 901 I=1,NBDATS
               ZI(IBCMPS+I-1)=6
               NI=NI+6
               ZI(INDATS+I)=NI
 901        CONTINUE
          ELSE
            NBDATS=ILIG+1
            DO 902 I=1,NBDATS-1
               ZI(IBCMPS+I-1)=6
               NI=NI+6
               ZI(INDATS+I)=NI
 902        CONTINUE
            ZI(IBCMPS+NBDATS-1)=IRES
            ZI(INDATS+NBDATS)=NI+IRES
          ENDIF
          NBCMPT=6
        ELSE
          NBDATS=ICOMAX
          DO 3 I=1,NBDATS
            ZI(JSPT-1+I)=1
3         CONTINUE
          NBCMPT=ZI(IBCMPS-1+ICHS)
          NOMGS=ZK8(INOGDS-1+ICHS)
        ENDIF


C
C --- ECRITURE DE L'ENTETE SUPERTAB ----
C
        CALL ECRTES(NOMSD,TITR,NOMGS,IR,LOC,NBCMPT,2,ENTETE,LCMP)

C
C --- POSITION DES COMPOSANTES SELECTIONNEES ---
C
        IF(NBCMP.NE.0 .AND. NOMGD.NE.'VARI_R')THEN
           K=0
           L=0
           LL=0
           CALL WKVECT('&&IRCERS.PERM','V V I',NBCMP*NBGREL,IPERM)
           CALL WKVECT('&&IRCERS.NBCMPS_GREL','V V I',NBGREL,INBCG)
           CALL WKVECT('&&IRCERS.POS','V V I',NBCMP*NBGREL,IPOS)
           CALL WKVECT('&&IRCERS.NBCMPT_GREL','V V I',NBGREL,INBCTG)
           CALL WKVECT('&&IRCERS.SNBCPS','V V I',NBGREL+1,ISNBCS)
           CALL WKVECT('&&IRCERS.CMP_GREL','V V I',NBCMP*NBGREL,INPER)
           ZI(ISNBCS)=0
           DO 904 IGREL=1,NBGREL
              MODE=CELD(CELD(4+IGREL)+2)
              IPOIN1=LONGR(IGREL)
              IPOIN2=LONGR(IGREL+1)
              NBELGR=IPOIN2-IPOIN1-1
              IF (MODE.EQ.0) GO TO 904
              JMOD = IMODEL+ZI(ILONG-1+MODE)-1
              NEC = NBEC (ZI(JMOD-1+2))
              CALL JEDETR('&&IRCERS.ENT_COD')
              CALL WKVECT('&&IRCERS.ENT_COD','V V I',NEC,IAEC)
              CALL DGMODE(MODE,IMODEL,ILONG,NEC,ZI(IAEC))
              NCMPG=0
C             POSITIONS DES COMPOSANTES SELECTIONNEES PRESENTES
C             DANS LE  GREL PARMI LES COMPOSANTES SELECTIONNEES
              DO 905 ICMPL=1,NBCMP
                 IF (EXISDG(ZI(IAEC),NCMPS(ICMPL))) THEN
                    ZI(INPER+K)=NCMPS(ICMPL)
                    K=K+1
                    NCMPG=NCMPG+1
                 ENDIF
 905          CONTINUE
C             SOMME DES COMPOSANTES SELECTIONNEES PAR GREL
              ZI(ISNBCS+IGREL)=K
C             NOMBRE DE COMPOSANTES SELECTIONNEES PRESENTES
C             DANS LE GREL
              ZI(INBCG+IGREL-1)=NCMPG
              IF(IGREL.EQ.NBGREL .AND. K.LT.NBCMP)
     &            CALL U2MESS('F','PREPOST_83')
              NCMPP=0
              DO 906 I=1,NCMPMX
                 IF (EXISDG(ZI(IAEC),I)) THEN
                    NCMPP=NCMPP+1
C                   POSITIONS DES COMPOSANTES SELECTIONNEES PRESENTES
C                   DANS LE GREL PARMI LES COMPOSANTES DU GREL
                    DO 915 J=1,ZI(INBCG+IGREL-1)
                       IF(I.EQ.ZI(INPER+J-1+ZI(ISNBCS+IGREL-1)))THEN
                          ZI(IPOS+L)=NCMPP
                          L=L+1
                       ENDIF
 915                CONTINUE
C                   POSITION DES COMPOSANTES SELECTIONNEES
C                   DANS LES DATASETS
                    DO 789 J=1,NBCMP
                       IF(I.EQ.NCMPS(J))THEN
                          ZI(IPERM+LL)=J
                          LL=LL+1
                       ENDIF
 789               CONTINUE
                 ENDIF
 906          CONTINUE
C             NOMBRE DE COMPOSANTES DANS LE GREL
              ZI(INBCTG+IGREL-1)=NCMPP
 904       CONTINUE
        ENDIF

C
C --- IMPRESSION DES DATASETS SUPERTAB ---
C

        DO 11 IDA =1,NBDATS

          IENTE = 1
          IMPRE = 0
          IFIN  = 1
          IDEBU = 1
          ENTETE(4) = ' '
          TEXTE = ' '
C
C --- ECRITURE DANS L'ENTETE SUPERTAB DES NOMS DE COMPOSANTES---
C
          IF(NBCMP.NE.0 .AND. NOMGD.NE.'VARI_R')THEN
             DO 907 ICP=1,ZI(IBCMPS+IDA-1)
                NOCMP=NOCMPL(ZI(INDATS+IDA-1)+ICP)
                IUTIL=LXLGUT(NOCMP)
                IFIN = IDEBU+IUTIL
                TEXTE(IDEBU:IFIN)=NOCMP(1:IUTIL)//' '
                IDEBU = IFIN + 1
 907         CONTINUE
             TEXTE(IFIN+2:IFIN+7)= '('//LOC//')'
          ELSE
             NBSPT=ZI(JSPT-1+IDA)
             DO 5 ICP=1,ZI(IBCMPS-1+ICHS)
                IF (ZI(IBCMPS-1+ICHS).EQ.1.AND.ICOMAX.GT.1) THEN
                   DO 6 ISPT=1,NBSPT
                      IF ( NCMPU .EQ. 0 ) THEN
                         ENTIER=(IDA-1)*6+ISPT
                      ELSE
                         ENTIER=NUCMP((IDA-1)*6+ISPT)
                      ENDIF
                      NOCMP = NOMCMP(ZI(ICMPS-1+(ICHS-1)*NCMPMX+ICP))
                      IUTIL=LXLGUT(NOCMP)
                      CALL CODENT (ENTIER,'G',TOTO)
                      IFIN = IDEBU+IUTIL+3
                      TEXTE(IDEBU:IFIN)=
     &                     NOCMP(1:IUTIL) //'_'//TOTO//' '
                      IDEBU = IFIN + 1
 6                 CONTINUE
                ELSE
                   NOCMP = NOMCMP(ZI(ICMPS-1+(ICHS-1)*NCMPMX+ICP))
                   IUTIL=LXLGUT(NOCMP)
                   IFIN = IDEBU+IUTIL
                   TEXTE(IDEBU:IFIN)=NOCMP(1:IUTIL)//' '
                   IDEBU = IFIN + 1
                ENDIF
 5           CONTINUE
             TEXTE(IFIN+2:IFIN+7)= '('//LOC
             IDERN = IFIN+7
             IF(ZI(IBCMPS-1+ICHS).GT.1.AND.ICOMAX.GT.1) THEN
                CALL CODENT(IDA,'G',TOTO)
                TEXTE(IFIN+8:IFIN+14)= 'SPT_'//TOTO
                IDERN = IFIN + 14
             ENDIF
             TEXTE(IDERN:IDERN+1)= ')'
          ENDIF

          IUTIL = LXLGUT(TEXTE)
          JMAX = LXLGUT(TITRE)
          JMAX  = MIN(JMAX,(80-IUTIL-3))
          ENTETE(4)= TITRE(1:JMAX)//' - '//TEXTE(1:IUTIL)

          DO 12 IGREL=1,NBGREL
            MODE=CELD(CELD(4+IGREL)+2)
            IPOIN1=LONGR(IGREL)
            IPOIN2=LONGR(IGREL+1)
            NBELGR=IPOIN2-IPOIN1-1
            IF (MODE.EQ.0) GO TO 12
            JMOD = IMODEL+ZI(ILONG-1+MODE)-1
            NEC = NBEC (ZI(JMOD-1+2))
            CALL JEDETR('&&IRCERS.ENT_COD')
            CALL WKVECT('&&IRCERS.ENT_COD','V V I',NEC,IAEC)
            CALL DGMODE(MODE,IMODEL,ILONG,NEC,ZI(IAEC))
            IAD=CELD(CELD(4+IGREL)+8)
            NSCAL = DIGDEL(MODE)

            IF(NBCMP.NE.0 .AND. NOMGD.NE.'VARI_R')THEN
               NSCA=NSCAL
               IF(ZI(INBCG+IGREL-1).NE.0)THEN
                  GOTO 62
               ELSE
                  GOTO 12
               ENDIF
            ENDIF

            ICOEF=MAX(1,CELD(4))
            IF(ZI(IBCMPS-1+ICHS).EQ.1.AND.ICOMAX.GT.1) THEN
              IF ( NCMPU .EQ. 0 ) THEN
                ICO = (IDA-1)*6+1
              ELSE
                ICO = 1
              ENDIF
            ELSE
              ICO=IDA
            ENDIF
            IF (ICOEF.LT.ICO) GO TO 12
            NSCA = NSCAL*ICOEF
            NCMPP=0
            DO 23 I=1,NCMPMX
              IF (EXISDG(ZI(IAEC),I)) THEN
               NCMPP=NCMPP+1
               IF (ICHS.EQ.1) ZL(ITABL-1+I)=.TRUE.
              ENDIF
   23       CONTINUE
            DO 61 I=1,ZI(IBCMPS-1+ICHS)
              IF (EXISDG(ZI(IAEC),ZI(ICMPS-1+(ICHS-1)
     &                *NCMPMX+I))) GOTO 62
  61        CONTINUE
            GOTO 12
  62        CONTINUE
            DO 13 IELG=1,NBELGR
              IEL=LIGREL(IPOIN1+IELG-1)
              IF (IEL.LE.0) GO TO 13
C
C --- IMPRESSION DU CHAMELEM SUR UNE LISTE DE MAILLES ---
C
              IF(NBMAT.NE.0) THEN
                DO 14 IMAI=1,NBMAT
                  IF(IEL.EQ.NUMMAI(IMAI))  GO TO 15
  14            CONTINUE
                GO TO 13
              ENDIF
  15        CONTINUE
            IMPEL = 1
C
C           RECHERCHE DE L'ADRESSE DANS VALE DU DEBUT DES VALEURS
C
            IACHML = IAD + NSCA * (IELG-1)

C
C    --- CHAMELEM AUX NOEUDS ET AU POINTS DE GAUSS
C
C       - ELEMENTS NON DISPONIBLES DANS IDEAS
C
          ITYPE  = TYPMA(IEL)
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPE),KTYPE)
          IF ( KTYPE .EQ. 'PYRAM5' ) THEN
            CALL U2MESS('A','PREPOST_84')
          ELSE IF ( KTYPE .EQ. 'PYRAM13' ) THEN
            CALL U2MESS('A','PREPOST_85')
          ELSE
C
C    --- CHAMELEM AUX NOEUDS ---
C
            IF (LOC.EQ.'ELNO') THEN

               IF(NBCMP.NE.0 .AND. NOMGD.NE.'VARI_R')THEN

                  NPCALC=NSCAL/ZI(INBCTG+IGREL-1)
                  NNOE   = NBNOMA(IEL)
                  ITYPE  = TYPMA(IEL)
                  CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPE),KTYPE)
                  IF ( KTYPE .EQ. 'HEXA27' ) THEN
                     NNOE = NNOE - 7
                     LNOCEN=.TRUE.
                  ELSE IF ( KTYPE .EQ. 'TRIA7' ) THEN
                     NNOE = NNOE - 1
                     LNOCEN=.TRUE.
                  ELSE IF ( KTYPE .EQ. 'PENTA18' ) THEN
                     NNOE = NNOE - 3
                     LNOCEN=.TRUE.
                  ELSEIF ( KTYPE .EQ. 'QUAD9' ) THEN
                     NNOE = NNOE - 1
                     LNOCEN=.TRUE.
                  ELSEIF ( KTYPE .EQ. 'SEG4' ) THEN
                     NNOE = NNOE - 2
                     CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG2'),ITSEG2)
                     ITYPE=ITSEG2
                  ENDIF
                  NBCOU  = NPCALC / NNOE

                  DO 516 INOS = 1,NNOE
                     INOA=0
                     DO 528 IAST = 1,NNOE
                        ISUP=PERMUT(IAST,ITYPE)
                        IF (INOS.EQ.ISUP) THEN
                           INOA=IAST
                           GO TO 529
                        END IF
 528                 CONTINUE

 529                 CONTINUE
                     CALL ASSERT(INOA.NE.0)

                     DO 561 ICOU=1,NBCOU
                        JJ=IACHML-1+ZI(INBCTG+IGREL-1)*(INOA-1)+
     &                       (ICOU-1)*ZI(INBCTG+IGREL-1)*NNOE

                        DO 521 I=1,NBCMPT
                           ZR(IRVN-1+I)=0.D0
 521                     CONTINUE

                        DO 509 ICM=1,ZI(INBCG+IGREL-1)
                           J=ZI(IPERM+ZI(ISNBCS+IGREL-1)+ICM-1)
                           IF (J.LE.6*IDA .AND. J.GE.(6*IDA-5))THEN
                              JT=J-6*(IDA-1)
                              IC=ZI(IPOS+ZI(ISNBCS+IGREL-1)+ICM-1)
                              ZR(IRVN-1+JT)= VALE(JJ+IC)
                           ENDIF
 509                    CONTINUE

                        IF(IENTE.EQ.1) THEN
                           WRITE(IFI,'(A80)') (ENTETE(I),I=1,10)
                           IENTE=0
                        ENDIF
                        IF(IMPEL.EQ.1)THEN
                        IF(LMASU) THEN
                           CALL LXLIIS(NOMEL(IEL)(2:8),IES,IER)
                        ELSE
                           IES=IEL
                        ENDIF
                        WRITE (IFI,'(4I10,5X,A,A)')
     &                IES,1,NNOE,NBCMPT,'% MAILLE ',NOMEL(IEL)
                        IMPEL=0
                     ENDIF
                        WRITE (IFI,'(6(1PE13.5E3))') (ZR(IRVN-1+I),
     &                       I=1,NBCMPT)

 561                 CONTINUE
 516               CONTINUE

               ELSE

               NPCALC = NSCAL / NCMPP
               NNOE   = NBNOMA(IEL)
               ITYPE  = TYPMA(IEL)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPE),KTYPE)
               IF ( KTYPE .EQ. 'HEXA27' ) THEN
                  NNOE = NNOE - 7
                  LNOCEN=.TRUE.
               ELSE IF ( KTYPE .EQ. 'TRIA7' ) THEN
                  NNOE = NNOE - 1
                  LNOCEN=.TRUE.
               ELSE IF ( KTYPE .EQ. 'PENTA18' ) THEN
                  NNOE = NNOE - 3
                  LNOCEN=.TRUE.
               ELSEIF ( KTYPE .EQ. 'QUAD9' ) THEN
                  NNOE = NNOE - 1
                  LNOCEN=.TRUE.
               ELSEIF ( KTYPE .EQ. 'SEG4' ) THEN
                  NNOE = NNOE - 2
                  CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG2'),ITSEG2)
                  ITYPE=ITSEG2
               ENDIF
               NBCOU  = NPCALC / NNOE

               DO 16 INOS = 1,NNOE
                  INOA=0
                  DO 28 IAST = 1,NNOE
                     ISUP=PERMUT(IAST,ITYPE)
                     IF (INOS.EQ.ISUP) THEN
                        INOA=IAST
                        GO TO 29
                     END IF
   28             CONTINUE
   29             CONTINUE
                  CALL ASSERT(INOA.NE.0)
                  DO 161 ICOU=1,NBCOU
                   J=IACHML-1+NCMPP*ICOEF*(INOA-1)+
     &                        (ICOU-1)*NCMPP*ICOEF*NNOE+NCMPP*(ICO-1)
                     DO 21 I=1,NBCMPT
                        ZR(IRVN-1+I)=0.D0
   21                CONTINUE
                     IC=0
                     DO 22 ICMP=1,NCMPMX
                        IF (EXISDG(ZI(IAEC),ICMP)) THEN
                           IC=IC+1
                           DO 43 ICMS=1,ZI(IBCMPS-1+ICHS)
                           ICMSUP = ZI(ICMPS-1+(ICHS-1)*NCMPMX+ICMS)
                           IF(ICMP.EQ.ICMSUP) THEN
                             IMPRE=1
                             DO 26 ISP=1,ZI(JSPT-1+IDA)
                              ZR(IRVN-1+ICMS-1+ISP)=
     &                                         VALE(J+IC+NCMPP*(ISP-1))
   26                        CONTINUE
                             GOTO 22
                           ENDIF
   43                      CONTINUE
                        END IF
   22                CONTINUE
                     IF (IMPRE.EQ.1) THEN
                       IF(IENTE.EQ.1) THEN
                        WRITE(IFI,'(A80)') (ENTETE(I),I=1,10)
                        IENTE=0
                       ENDIF
                       IF(IMPEL.EQ.1) THEN
                          IF(LMASU) THEN
                            CALL LXLIIS(NOMEL(IEL)(2:8),IES,IER)
                          ELSE
                            IES=IEL
                          ENDIF
                          WRITE (IFI,'(4I10,5X,A,A)')
     &                   IES,1,NNOE,NBCMPT,'% MAILLE ',NOMEL(IEL)
                       IMPEL=0
                       ENDIF
                       WRITE (IFI,'(6(1PE13.5E3))') (ZR(IRVN-1+I),
     &                                          I=1,NBCMPT)
                     ENDIF
  161             CONTINUE
   16          CONTINUE
            ENDIF
C
C  --- CHAMELEM AUX POINTS DE GAUSS---
C
            ELSE IF (LOC.EQ.'ELGA'.OR.LOC.EQ.'ELEM') THEN

               IF(NBCMP.NE.0 .AND. NOMGD.NE.'VARI_R')THEN
                  NBPG=NSCAL/ZI(INBCTG+IGREL-1)

                  DO 908 I=1,NBCMPT
                     ZR(IRVG-1+I)=0.D0
 908              CONTINUE

                  DO 909 ICM=1,ZI(INBCG+IGREL-1)
                     J=ZI(IPERM+ZI(ISNBCS+IGREL-1)+ICM-1)

                     IF (J.LE.6*IDA .AND. J.GE.(6*IDA-5))THEN
                     JT=J-6*(IDA-1)

                     DO 910 IPG=1,NBPG
                        JJ=ZI(INBCTG+IGREL-1)*(IPG-1)+
     &                        ZI(IPOS+ZI(ISNBCS+IGREL-1)+ICM-1)
                        ZR(IRVG-1+JT)=ZR(IRVG-1+JT)+VALE(IACHML-1+JJ)
 910                 CONTINUE
                     ZR(IRVG-1+JT)=ZR(IRVG-1+JT)/NBPG

                     ENDIF
 909              CONTINUE

                  IF(IENTE.EQ.1) THEN
                     WRITE(IFI,'(A80)') (ENTETE(I),I=1,10)
                     IENTE=0
                  ENDIF
                  IF(LMASU) THEN
                     CALL LXLIIS(NOMEL(IEL)(2:8),IES,IER)
                  ELSE
                     IES=IEL
                  ENDIF
                  WRITE(IFI,'(2I10,5X,2A)')IES,NBCMPT,
     &                 '% MAILLE ',NOMEL(IEL)
                  WRITE (IFI,'(6(1PE13.5E3))') (ZR(IRVG-1+I),I=1,NBCMPT)


               ELSE
                  NPCALC = NSCAL/NCMPP
                  NBPG=NPCALC
                  DO 18 I=1,NBCMPT
                     ZR(IRVG-1+I)=0.D0
 18               CONTINUE
                  IC=0
                  DO 19 ICMP=1,NCMPMX
                  IF (EXISDG(ZI(IAEC),ICMP)) THEN
                  IC=IC+1
                  DO 37 ICMS=1,ZI(IBCMPS-1+ICHS)
                  ICMSUP = ZI(ICMPS-1+(ICHS-1)*NCMPMX+ICMS)
                  IF(ICMP.EQ.ICMSUP) THEN
                  IMPRE=1
                  DO 36 ISP=1,ZI(JSPT-1+IDA)
                  IF ( NCMPU .EQ. 0 ) THEN
                     IS0 = ISP
                  ELSE
                     IS0 = NUCMP((IDA-1)*6+ISP)
                  ENDIF
                  DO 17 IPG =1,NBPG

                     J=IACHML-1+NCMPP*ICOEF*(IPG-1)+NCMPP*(ICO-1)
                     ZR(IRVG-1+ICMS-1+ISP)= ZR(IRVG-1+ICMS-1+ISP)+
     &                    VALE(J+IC+NCMPP*(IS0-1))

 17               CONTINUE
                  ZR(IRVG-1+ICMS-1+ISP)=ZR(IRVG-1+ICMS-1+ISP)
     &                 / NBPG

 36            CONTINUE
               GOTO 19
            ENDIF
 37      CONTINUE
      END IF
 19   CONTINUE
      IF (IMPRE.EQ.1) THEN
         IF(IENTE.EQ.1) THEN
            WRITE(IFI,'(A80)') (ENTETE(I),I=1,10)
            IENTE=0
         ENDIF
         IF(IMPEL.EQ.1) THEN
            IF(LMASU) THEN
               CALL LXLIIS(NOMEL(IEL)(2:8),IES,IER)
            ELSE
               IES=IEL
            ENDIF
            WRITE(IFI,'(2I10,5X,2A)')IES,NBCMPT,
     &           '% MAILLE ',NOMEL(IEL)
            IMPEL=0
         ENDIF
         WRITE (IFI,'(6(1PE13.5E3))') (ZR(IRVG-1+I),I=1,NBCMPT)
         IMPRE=0
      ENDIF
      ENDIF
      ENDIF
      ENDIF
 13   CONTINUE
 12   CONTINUE
      IF (IENTE.EQ.0) WRITE (IFI,'(A)') '    -1'
 11   CONTINUE
 10   CONTINUE
C
      IF(LNOCEN)THEN
         CALL U2MESS('A','PREPOST_86')
      ENDIF
C
      CALL JEDETR('&&IRCERS.VALNOE')
      CALL JEDETR('&&IRCERS.VALGAU')
      CALL JEDETR('&&IRCERS.SOUS_PT')
      CALL JEDETR('&&IRCERS.ENT_COD')
      CALL JEDETR('&&IRCERS.NOMGDS')
      CALL JEDETR('&&IRCERS.NOMCHS')
      CALL JEDETR('&&IRCERS.NBCMPS')
      CALL JEDETR('&&IRCERS.IPCMPS')
      CALL JEDETR('&&IRCERS.LTABL')
      CALL JEDETR('&&IRCERS.PERM')
      CALL JEDETR('&&IRCERS.NBCMPS_GREL')
      CALL JEDETR('&&IRCERS.POS')
      CALL JEDETR('&&IRCERS.NBCMPT_GREL')
      CALL JEDETR('&&IRCERS.SNBCPS')
      CALL JEDETR('&&IRCERS.CMP_GREL')
      CALL JEDETR('&&IRCERS.SCMP_DATS')
      CALL JEDEMA()
      END
