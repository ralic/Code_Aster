      SUBROUTINE IRCECS(IFI,LIGREL,NBGREL,LONGR,NCMPMX,VALE,
     &   NOMCMP,TITR,NOMEL,LOC,CELD,NBNOMA,PERMUT,MAXNOD,TYPMA,
     &   NOMSD,NOMSYM,IR,NBMAT,NUMMAI,LMASU,NCMPU,NUCMP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER       IFI, LIGREL(*), NBGREL, LONGR(*), NCMPMX,
     &              CELD(*), NBNOMA(*), PERMUT(MAXNOD,*), TYPMA(*),
     &              NBMAT, NUMMAI(*), NCMPU, NUCMP(*)
      CHARACTER*(*) NOMCMP(*),NOMEL(*),LOC,TITR,NOMSYM,NOMSD
      COMPLEX*16                                 VALE(*)
      LOGICAL       LMASU
C--------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/09/2010   AUTEUR REZETTE C.REZETTE 
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
C TOLE CRP_21
C        ECRITURE D'UN CHAMELEM SUR FICHIER UNIVERSEL, DATASET TYPE 56
C                                                                OU 57
C        A VALEURS COMPLEXES
C  ENTREE:
C     IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
C     LIGREL: LIGREL COMPLET
C     NBGREL: NOMBRE DE GRELS
C     LONGR : POINTEUR DE LONGUEUR DE LIGREL
C     NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR
C     VALE  : VALEURS DU CHAM_NO
C     NOMCMP: NOMS DES CMP
C     TITR  : 1 LIGNE  DE TITRE
C     NOMEL : NOMS DES MAILLES SUPPORTS DES ELEMENTS
C     LOC   : LOCALISATION DES VALEURS (ELNO =>57, ELGA=>56)
C     CELD  : DESCRIPTEUR DU CHAM_ELEM (MODES LOCAUX,ADRESSES->.CELV)
C     NBNOMA: NOMBRE DE NOEUDS DE CHAQUE MAILLE
C     PERMUT: TABLEAU DES PERMUTATIONS DES NOEUDS DE CHAQUE TYPE-MA
C     MAXNOD: NOMBRE MAXI DE NOEUDS POUR LES DIFF. TYPE_MAILLES
C     TYPMA : TYPE_MAILLES
C     IR    : NUMERO D'ORDRE DU CHAMP
C     NBMAT : NOMBRE DE MAILLES A IMPRIMER
C     NUMMAI: NUMEROS DES MAILLES A IMPRIMER
C     LMASU : INDIQUE SI MAILLAGE ISSU DE SUPERTAB  .TRUE.
C
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI,DIGDEL
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL,EXISTE,EXISDG
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM,JEXNOM,JEXATR
      CHARACTER*80 ZK80
C     ------------------------------------------------------------------
      CHARACTER*3  TOTO
      CHARACTER*4  FITYPE,NOMGS
      CHARACTER*8  NOCMP,KTYPE
      CHARACTER*80 ENTETE(10),TITRE,TEXTE
      CHARACTER*24 NOMST
      INTEGER      NBCHS,NBCMPT,ENTIER,NBSPT,NNOE
      INTEGER      IMPRE,IENTE,IMPEL,ILONG,IMODEL
      LOGICAL      AFAIRE,LCMP,LNOCEN
C
C  --- INITIALISATIONS ----
C
      CALL JEMARQ()
C
      CALL WKVECT('&&IRCECS.NOMGDS','V V K8',NCMPMX,INOGDS)
      CALL WKVECT('&&IRCECS.NOMCHS','V V K8',NCMPMX,INOCHS)
      CALL WKVECT('&&IRCECS.NBCMPS','V V I' ,NCMPMX,IBCMPS)
      CALL WKVECT('&&IRCECS.IPCMPS','V V I' ,NCMPMX*NCMPMX,ICMPS)
      CALL WKVECT('&&IRCECS.LTABL' ,'V V L' ,NCMPMX,ITABL)
C
      NOMST= '&&IRECRI.SOUS_TITRE.TITR'
      CALL JEVEUO(NOMST,'L',JTITR)
      CALL JEVEUO ('&CATA.TE.MODELOC', 'L', IMODEL )
      CALL JEVEUO (JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',ILONG)
      TITRE = ZK80(JTITR)
      DO 1 I=1,NCMPMX
        ZL(ITABL-1+I)=.FALSE.
 1    CONTINUE
      LNOCEN=.FALSE.
C
C  --- RECHERCHE DES GRANDEURS SUPERTAB ----
C
      CALL IRGAGS(NCMPMX,NOMCMP,NOMSYM,NBCHS,ZK8(INOCHS),
     &                ZI(IBCMPS),ZK8(INOGDS),ZI(ICMPS))
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
        CALL JEDETR('&&IRCECS.VRNOE')
        CALL WKVECT('&&IRCECS.VRNOE','V V R',NCMPMX*ICOMM,IRVN)
        CALL JEDETR('&&IRCECS.VCNOE')
        CALL WKVECT('&&IRCECS.VCNOE','V V R',NCMPMX*ICOMM,ICVN)
      ELSEIF (LOC.EQ.'ELGA') THEN
        CALL JEDETR('&&IRCECS.VRGAU')
        CALL WKVECT('&&IRCECS.VRGAU','V V R',NCMPMX*ICOMM,IRVG)
        CALL JEDETR('&&IRCECS.VCGAU')
        CALL WKVECT('&&IRCECS.VCGAU','V V R',NCMPMX*ICOMM,ICVG)
      ENDIF
C
C ---- BOUCLE SUR LES DIVERSES GRANDEURS SUPERTAB ----
      DO 10 ICHS = 1,NBCHS
        IF( ICHS.GT.1) THEN
         AFAIRE = .FALSE.
         DO 2 ICP=1,ZI(IBCMPS-1+ICHS)
           AFAIRE= (AFAIRE.OR.ZL(ITABL-1+(ZI(ICMPS-1+
     &       (ICHS-1)*NCMPMX+ICP))))
 2       CONTINUE
         IF(.NOT. AFAIRE) GO TO 10
        ENDIF
C
        CALL JEDETR('&&IRCECS.SOUS_PT')
        CALL WKVECT('&&IRCECS.SOUS_PT','V V I',ICOMAX,JSPT)
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
        LCMP=.FALSE.
        CALL ECRTES(NOMSD,TITR,NOMGS,IR,LOC,NBCMPT,5,ENTETE,LCMP)
C
C --- IMPRESSION DES DATASETS SUPERTAB ---
C
        DO 11 IDA =1,NBDATS
          IENTE = 1
          IMPRE = 0
          IDEBU = 1
          IFIN  = 1
C
C --- ECRITURE DANS L'ENTETE SUPERTAB DES NOMS DE COMPOSANTES---
C
          NBSPT=ZI(JSPT-1+IDA)
          IDEBU = 1
          ENTETE(4) = ' '
          TEXTE = ' '
          IDEBU = 21
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
                IFIN = IDEBU+IUTIL+2
                TEXTE(IDEBU:IFIN)=' '//
     &                    NOCMP(1:IUTIL) //'_'//TOTO
                IDEBU = IFIN + 1
  6           CONTINUE
            ELSE
              NOCMP = NOMCMP(ZI(ICMPS-1+(ICHS-1)*NCMPMX+ICP))
              IUTIL=LXLGUT(NOCMP)
              IFIN = IDEBU+IUTIL
              TEXTE(IDEBU:IFIN)=' '//NOCMP(1:IUTIL)
              IDEBU = IFIN + 1
            ENDIF
  5       CONTINUE
          TEXTE(IFIN+2:IFIN+7)= '('//LOC
          IDERN = IFIN+7
          IF(ZI(IBCMPS-1+ICHS).GT.1.AND.ICOMAX.GT.1) THEN
            CALL CODENT(IDA,'G',TOTO)
            TEXTE(IFIN+8:IFIN+14)= 'SPT_'//TOTO
            IDERN = IFIN + 14
          ENDIF
          TEXTE(IDERN:IDERN+1)= ')'
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
            CALL JEDETR('&&IRCECS.ENT_COD')
            CALL WKVECT('&&IRCECS.ENT_COD','V V I',NEC,IAEC)
            CALL DGMODE(MODE,IMODEL,ILONG,NEC,ZI(IAEC))
            IAD=CELD(CELD(4+IGREL)+8)
            NSCAL = DIGDEL(MODE)
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
              IF (EXISDG(ZI(IAEC),ZI(ICMPS-1+
     &             (ICHS-1)*NCMPMX+I))) GOTO 62
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
C    --- CHAMELEM AUX NOEUDS ---
C
            IF (LOC.EQ.'ELNO') THEN
               NPCALC = NSCAL / NCMPP
               NNOE   = NBNOMA(IEL)
               ITYPE  = TYPMA(IEL)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPE),KTYPE)
               IF ( KTYPE .EQ. 'TRIA7' ) THEN
                  NNOE = NNOE - 1
                  LNOCEN=.TRUE.
               ELSEIF ( KTYPE .EQ. 'QUAD9' ) THEN
                  NNOE = NNOE - 1
                  LNOCEN=.TRUE.
               ELSEIF ( KTYPE .EQ. 'PENTA18' ) THEN
                  NNOE = NNOE - 3
                  LNOCEN=.TRUE.
               ELSEIF ( KTYPE .EQ. 'HEXA27' ) THEN
                  NNOE = NNOE - 7
                  LNOCEN=.TRUE.
               ELSEIF ( KTYPE .EQ. 'SEG4' ) THEN
                  NNOE = NNOE - 2
                 CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG2'),ITSEG2)
                 ITYPE=ITSEG2
               ENDIF
               NBCOU  = NPCALC / NNOE
               DO 16 INOS=1,NNOE
                  INOA=0
                  DO 28 IAST=1,NNOE
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
     &                          (ICOU-1)*NCMPP*ICOEF*NNOE+NCMPP*(ICO-1)
                     DO 21 I=1,NBCMPT
                        ZR(IRVN-1+I)=0.D0
                        ZR(ICVN-1+I)=0.D0
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
     &                                  DBLE(VALE(J+IC+NCMPP*(ISP-1)))
                              ZR(ICVN-1+ICMS-1+ISP)=
     &                                 DIMAG(VALE(J+IC+NCMPP*(ISP-1)))
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
     &                         IES,1,NNOE,NBCMPT,'% MAILLE ',NOMEL(IEL)
                       IMPEL=0
                       ENDIF
                       WRITE (IFI,'(6(1PE13.5))') (ZR(IRVN-1+I),
     &                    ZR(ICVN-1+I),I=1,NBCMPT)
                     ENDIF
  161             CONTINUE
   16          CONTINUE
C
C  --- CHAMELEM AUX POINTS DE GAUSS---
C
            ELSE IF (LOC.EQ.'ELGA') THEN
               NPCALC = NSCAL/NCMPP
               NBPG=NPCALC
               DO 18 I=1,NBCMPT
                 ZR(IRVG-1+I)=0.D0
                 ZR(ICVG-1+I)=0.D0
   18          CONTINUE
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
     &                              DBLE(VALE(J+IC+NCMPP*(IS0-1)))
                           ZR(ICVG-1+ICMS-1+ISP)= ZR(ICVG-1+ICMS-1+ISP)+
     &                             DIMAG(VALE(J+IC+NCMPP*(IS0-1)))
   17                    CONTINUE
                         ZR(IRVG-1+ICMS-1+ISP)=ZR(IRVG-1+ICMS-1+ISP)
     &                                               / NBPG
                         ZR(ICVG-1+ICMS-1+ISP)=ZR(ICVG-1+ICMS-1+ISP)
     &                                               / NBPG
   36                  CONTINUE
                       GOTO 19
                     ENDIF
   37              CONTINUE
                  END IF
   19          CONTINUE
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
                   WRITE(IFI,'(2I10,5X,2A)') IES,NBCMPT,
     &                              '% MAILLE ',NOMEL(IEL)
                   IMPEL=0
                 ENDIF
                 WRITE (IFI,'(6(1PE13.5))') (ZR(IRVG-1+I),
     &               ZR(ICVG-1+I),I=1,NBCMPT)
                 IMPRE=0
               ENDIF
            END IF
   13    CONTINUE
   12 CONTINUE
      IF (IENTE.EQ.0) WRITE (IFI,'(A)') '    -1'
   11 CONTINUE
   10 CONTINUE
      IF(LNOCEN)THEN
         CALL U2MESS('A','PREPOST_80')
      ENDIF
C
      CALL JEDETR('&&IRCECS.VRNOE')
      CALL JEDETR('&&IRCECS.VCNOE')
      CALL JEDETR('&&IRCECS.VRGAU')
      CALL JEDETR('&&IRCECS.VCGAU')
      CALL JEDETR('&&IRCECS.SOUS_PT')
      CALL JEDETR('&&IRCECS.ENT_COD')
      CALL JEDETR('&&IRCECS.NOMGDS')
      CALL JEDETR('&&IRCECS.NOMCHS')
      CALL JEDETR('&&IRCECS.NBCMPS')
      CALL JEDETR('&&IRCECS.IPCMPS')
      CALL JEDETR('&&IRCECS.LTABL')
      CALL JEDEMA()
      END
