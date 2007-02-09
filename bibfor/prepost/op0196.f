      SUBROUTINE OP0196(IER)
      IMPLICIT NONE
      INTEGER IER
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/02/2007   AUTEUR MARKOVIC D.MARKOVIC 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C     =================================================================
C                      OPERATEUR POST_CHAM_XFEM
C                      ------------------------
C     BUT : DETERMNATION DES CHAMPS DE DEPLACEMENTS ET DE CONTRAINTES
C           SUR LE MAILLAGE FISSURE X-FEM
C     =================================================================
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
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='OP0196')

      INTEGER IRET,IBID,JLICHA,NBSY,NBORDR,JNOCMP,IORD,JFISS,ICHA,JLON,
     &        IOR,JORD,JCNSV1,JCNSD1,JCNSK1,JNO,NUMNOE,JCNSV2,JCNSD2,
     &        JJCNS,IICNS,JNOLOC,IIHEA,JJHEA,JHEA,JLNNO,NBHEA,DIMTO,
     &        JCNSK2,NBCMP,JDIM,JCNSC2,I,JCNSL1,JCNSL2,K,KK,JCO,JCNSC1,
     &        JINST,JCESV,JCESD,JCESK,JCESL,JCESC,IAD,NBMX,J,NBCHIN,
     &        KX,KY,KZ,IDX,IDY,IDZ,JCNS,I1000,NB1000,JNIM,II,ILOC,
     &        NBGREL,NBMAGL,IMA,NBNIM,JINDMF,I1,IGREL,DEBXHT,
     &        ID,NBTNIM,JLNONX,NUM,NUNXP,NUNXM,NBNOAJ,NUNX,NUNOI,NBCNS,
     &        JTYPMA,NBNOSO,NBHEAV,NBCTIP,NBHECT, JWXFEM, NBINMX,FINXHT,
     &        NCESV,NBPT,NBSP,NBCP,ICP,DIM,JMMF,J1,J2,J3,KMMF,
     &        JNFIS,NFIS,IFIS,ADLON,ADCNS,JLOND,JCNSD
      REAL*8  INST,EPS,R8B,RBID
      PARAMETER(EPS=1.D-10, NBINMX=9)
      CHARACTER*1  KBID
      CHARACTER*8  MO,RESUCO,RESUC1,MA,LDEP(3),NOMNOE,NOMNOI,FISS,
     &             LPAIN(NBINMX),LPAOUT(1),K8B,NOMA,TYPMA,CHNUM
      CHARACTER*16 K16B,TYSD,OPTION,NOTYPE,NOMCHA
      CHARACTER*19 CHS,CHNO,CHNOS,CH,CHOUT,CHAMS
      CHARACTER*24 ORDR,LIGREL,LCHIN(NBINMX),LCHOUT(1),HEAV,CONNEX,LIEL,
     &             CHGEOM,CTIP,HECT
      LOGICAL      EXIGEO,QUMAFI
      DATA LDEP/ 'DX','DY','DZ'/
C
      CALL JEMARQ()
C
      CHS   ='&&'//NOMPRO//'.CHS_MA  '
      CHNO  ='&&'//NOMPRO//'.DEP_INIT'
      CHNOS ='&&'//NOMPRO//'.CHS_EXTR'
      CHOUT ='&&'//NOMPRO//'.DEP_RESU'
      CHAMS ='&&'//NOMPRO//'.CHAMS   '
C
C     ===========================================
C      1. RECUPERATION DES CONCEPTS UTILISATEURS
C     ===========================================
C
C --- MODELE FISSURE : MO
C     ON VERIFIE QUE LE MODELE EST COMPATIBLE AVEC LA METHODE XFEM
      CALL GETVID(' ','MODELE',0,1,1,MO,IBID)
      CALL JEEXIN(MO//'.FISS',IRET)
      IF(IRET.EQ.0)THEN
        CALL UTDEBM('F',NOMPRO,'LE MODELE')
        CALL UTIMPK('S',' ',1,MO)
        CALL UTIMPK('S',' EST INCOMPATIBLE AVEC LA METHODE XFEM',1,KBID)
        CALL UTFINM()
      ENDIF
      CALL JEVEUO(MO//'.FISS','L',JFISS)
C
C --- DIMENSION GEOMETRIQUE
      CALL DISMOI('F','DIM_GEOM',MO,'MODELE',DIM,K8B,IRET)

C --- RECUPERATION DU CHAMP GEOMETRIQUE
      CALL MEGEOM(MO,' ',EXIGEO,CHGEOM)

C --- NOM ET TYPE DE LA SD RESULTAT EN ENTREE : RESUCO , TYSD
      CALL GETVID(' ','RESULTAT',1,1,1,RESUCO,IBID)
      CALL GETTCO(RESUCO,TYSD)
C
C --- NOM DE LA SD RESULTAT A CREER : RESUC1
      CALL GETRES(RESUC1,K16B,K16B)
C
C --- NOMBRE DE NUMEROS D'ORDRE : NBORDR
      ORDR=RESUCO//'           .ORDR'
      CALL JEVEUO(ORDR,'L',JORD)
      CALL JELIRA(ORDR,'LONUTI',NBORDR,KBID)
C
C --- NOM DU MAILLAGE ET NOM DU CHAMPS D'ENTREE: MA , LICHAM
      CALL GETVID(' ','MAILLAGE',1,1,1,MA,IBID)
      CALL WKVECT('&&'//NOMPRO//'.LICHAM','V V K16',2,JLICHA)
      CALL GETVTX(' ','NOM_CHAM',1,1,0,ZK16(JLICHA),NBSY)
      NBSY=-NBSY
      CALL GETVTX(' ','NOM_CHAM',1,1,NBSY,ZK16(JLICHA),IRET)
      
C     RECUPERATION DU NOMBRE DE FISSURES DANS MODELE
      CALL JEVEUO(MO//'.NFIS','L',JNFIS)
      NFIS = ZI(JNFIS)       
      
C
C --- CREATION DE LA NOUVELLE SD RESULTAT
      CALL RSCRSD(RESUC1,TYSD,NBORDR)
C
C     ==========================
C      2. TRAITEMENT DES CHAMPS
C     ==========================
C
      DO 10 ICHA = 1 , NBSY
C
        NOMCHA = ZK16(JLICHA+ICHA-1)
C
C       2.1  TRAITEMENT DES DEPLACEMENTS
C       ================================
C
        IF (NOMCHA(1:4).EQ.'DEPL')THEN
C
           IF(DIM.EQ.3)THEN
             NBCMP=3
           ELSE
             NBCMP=2
           ENDIF

           DO 20 IOR = 1 , NBORDR
C
C             1 - COPIE DES DEPLACEMENTS DES NOEUDS CLASSIQUES
C             ------------------------------------------------
C
C             CREATION D'UN CHAMP SIMPLE : CHS
              CALL CNSCRE(MA,'DEPL_R',NBCMP,LDEP,'V',CHS)
C
C             EXTRACTION DES DEPLACEMENTS
              IORD=ZI(JORD+IOR-1)
              CALL RSADPA(RESUCO,'L',1,'INST',IORD,0,JINST,KBID)
              INST=ZR(JINST)
              CALL RSEXCH(RESUCO,NOMCHA,IORD,CHNO,IRET)
C
C             PASSAGE : CHAMP --> CHAMP_S
              CALL CNOCNS(CHNO,'V',CHNOS)
C
C             COPIE DES DEPLACEMENTS DANS CHS
              CALL JEVEUO(CHNOS//'.CNSV','L',JCNSV1)
              CALL JEVEUO(CHNOS//'.CNSD','L',JCNSD1)
              CALL JEVEUO(CHNOS//'.CNSK','L',JCNSK1)
              CALL JEVEUO(CHNOS//'.CNSL','L',JCNSL1)
              CALL JEVEUO(CHNOS//'.CNSC','L',JCNSC1)
              CALL JEVEUO(CHS  //'.CNSV','E',JCNSV2)
              CALL JEVEUO(CHS  //'.CNSD','E',JCNSD2)
              CALL JEVEUO(CHS  //'.CNSK','E',JCNSK2)
              CALL JEVEUO(CHS  //'.CNSC','E',JCNSC2)
              CALL JEVEUO(CHS  //'.CNSL','E',JCNSL2)
              CALL JEVEUO(MA//'.DIME','L',JDIM)
              ZK8(JCNSK2)=MA
              ZK8(JCNSK2+1)=ZK8(JCNSK1+1)
              ZI(JCNSD2)=ZI(JDIM)
              ZI(JCNSD2+1)=NBCMP
              DO 25 I=1,NBCMP
                 ZK8(JCNSC2+I-1)=LDEP(I)
 25           CONTINUE

              IDX=0
              IDY=0
              IDZ=0
              DO 30 JCO = 1 , ZI(JCNSD1+1)
                 IF(ZK8(JCNSC1+JCO-1)(1:2).EQ.'DX')THEN
                    IDX=JCO
                 ELSEIF(ZK8(JCNSC1+JCO-1)(1:2).EQ.'DY')THEN
                    IDY=JCO
                 ELSEIF(ZK8(JCNSC1+JCO-1)(1:2).EQ.'DZ')THEN
                    IDZ=JCO
                 ENDIF
 30           CONTINUE
C
C             SI LE MAILLAGE COMPORTE QUE DES MAILLES XFEM
C             TRAVERSEES PAR LA FISSURE
              ID=IDX+IDY+IDZ
              QUMAFI=.FALSE.
              IF(ID.EQ.0)THEN
                 QUMAFI=.TRUE.
                 GOTO 45
              ENDIF
C
              DO 40 JNO = 1 ,ZI(JCNSD1)
                 CALL JENUNO(JEXNUM(ZK8(JCNSK1)//'.NOMNOE',JNO),NOMNOE)
                 CALL JENONU(JEXNOM(MA//'.NOMNOE',NOMNOE),NUMNOE)
                 KK=(NUMNOE-1)*ZI(JCNSD2+1)
                 IF(ZL(JCNSL1+(JNO-1)*ZI(JCNSD1+1)+IDX-1))THEN
                    KX=KK
                    ZR(JCNSV2+KX)=ZR(JCNSV1+ZI(JCNSD1+1)*(JNO-1)+IDX-1)
                    ZL(JCNSL2+KX)=.TRUE.
                 ENDIF
                 IF(ZL(JCNSL1+(JNO-1)*ZI(JCNSD1+1)+IDY-1))THEN
                    KY=KK+1
                    ZR(JCNSV2+KY)=ZR(JCNSV1+ZI(JCNSD1+1)*(JNO-1)+IDY-1)
                    ZL(JCNSL2+KY)=.TRUE.
                 ENDIF
                 IF(DIM.EQ.3)THEN
                   IF(ZL(JCNSL1+(JNO-1)*ZI(JCNSD1+1)+IDZ-1))THEN
                     KZ=KK+2
                     ZR(JCNSV2+KZ)=ZR(JCNSV1+ZI(JCNSD1+1)*(JNO-1)+IDZ-1)
                     ZL(JCNSL2+KZ)=.TRUE.
                   ENDIF
                 ENDIF
 40           CONTINUE
 45           CONTINUE

C
C             2 - DETERMINATION DES DEPLACEMENTS AUX NOEUDS X-FEM
C             ---------------------------------------------------
C
C             PREPARATION AVANT L'APPEL A CALCUL
              OPTION='DEPL_POST_XFEM'
              CALL JENONU(JEXNOM('&CATA.OP.NOMOPT',OPTION),IRET)
              LIGREL=MO//'.MODELE'
              LPAIN(1)='PGEOMER'
              LCHIN(1)=CHGEOM
              LPAIN(2)='PDEPLPR'
              LCHIN(2)=CHNO
              LPAIN(3)='PPINTTO'
              LCHIN(3)=MO//'.TOPOSE.PINTTO'
              LPAIN(4)='PCNSETO'
              LCHIN(4)=MO//'.TOPOSE.CNSETO'
              LPAIN(5)='PHEAVTO'
              LCHIN(5)=MO//'.TOPOSE.HEAVTO'
              LPAIN(6)='PLONCHA'
              LCHIN(6)=MO//'.TOPOSE.LONCHAM'
              LPAIN(7)='PLSN'
              LCHIN(7)=MO//'.LNNO'
              LPAIN(8)='PLST'
              LCHIN(8)=MO//'.LTNO'
              LPAIN(9)='PBASLOR'
              LCHIN(9)=MO//'.BASLOC'
              NBCHIN=9

              LPAOUT(1)='PDEXFEM'
              LCHOUT(1)=CHOUT
C
C             CALCUL DES DEPLACEMENTS AUX NOEUDS X-FEM
              CALL ASSERT(NBCHIN.LE.NBINMX)
              CALL CALCUL('S',OPTION,LIGREL,NBCHIN,LCHIN,LPAIN,1,LCHOUT,
     &                    LPAOUT,'V')
C
C             PASSAGE D'UN CHAM_ELEM EN UN CHAM_ELEM_S
              CALL CELCES ( LCHOUT(1), 'V', CHAMS )

              CALL JEVEUO(CHAMS//'.CESV','L',JCESV)
              CALL JEVEUO(CHAMS//'.CESD','L',JCESD)
              CALL JEVEUO(CHAMS//'.CESK','L',JCESK)
              CALL JEVEUO(CHAMS//'.CESL','L',JCESL)
              CALL JEVEUO(CHAMS//'.CESC','L',JCESC)
C
              CALL JEVEUO(MO//'.TOPOSE.LON.CELV','L',JLON)
              CALL JEVEUO(MO//'.TOPOSE.LON.CELD','L',JLOND)
              CALL JEVEUO(MO//'.TOPOSE.CNS.CELV','L',JCNS)
              CALL JEVEUO(MO//'.TOPOSE.CNS.CELD','L',JCNSD)
      
              LIEL=MO//'.MODELE    .LIEL'
              CALL JELIRA(LIEL,'NMAXOC',NBGREL,KBID)
              CALL JEVEUO(MO//'.LNNO      .VALE','L',JLNNO)
              CALL JEVEUO(ZK8(JCNSK1)//'.DIME','L',JDIM)
              CALL WKVECT('&&OP0196.MA_MO_FISS','V V I',ZI(JDIM+2),JMMF)
              DO 9 I=1,ZI(JDIM+2)
               ZI(JMMF+I-1)=0
 9            CONTINUE

C             NOMBRE DE MAILLES XFEM : NBMX
C             INITIALISATION DE NBMF A ZERO

              NBMX=0
C             ON BOUCLE SUR LES FISSURES

              DO 2000 IFIS=1, NFIS
     
                CALL JEVEUO(MO//'.FISS','L',JFISS)
                FISS=ZK8(JFISS-1+IFIS)
                HEAV=FISS//'.MAILFISS  .HEAV'
                CALL JEEXIN(HEAV,IRET)
                IF(IRET.NE.0)THEN
                  CALL JELIRA(HEAV,'LONMAX',NBHEAV,KBID)
                  CALL JEVEUO(HEAV,'E',J1)
                  DO 8 I=1,NBHEAV
                    ZI(JMMF+ZI(J1+I-1)-1)=1
   8              CONTINUE
                ELSE
                  NBHEAV=0
                ENDIF
                CTIP=FISS//'.MAILFISS  .CTIP'
                CALL JEEXIN(CTIP,IRET)
                IF(IRET.NE.0)THEN
                  CALL JELIRA(CTIP,'LONMAX',NBCTIP,KBID)
                  CALL JEVEUO(CTIP,'E',J2)
                  DO 7 I=1,NBCTIP
                    ZI(JMMF+ZI(J2+I-1)-1)=1
   7              CONTINUE
                ELSE
                  NBCTIP=0
                ENDIF
                HECT=FISS//'.MAILFISS  .HECT'
                CALL JEEXIN(HECT,IRET)
                IF(IRET.NE.0)THEN
                  CALL JELIRA(HECT,'LONMAX',NBHECT,KBID)
                  CALL JEVEUO(HECT,'E',J3)
                  DO 6 I=1,NBHECT
                   ZI(JMMF+ZI(J3+I-1)-1)=1
   6              CONTINUE
                ELSE
                  NBHECT=0
                ENDIF
                NBMX=NBMX+NBHEAV+NBCTIP+NBHECT
 2000         CONTINUE

C             TABLEAU DES MAILLES XFEM
              CALL WKVECT('&&OP0196.MAIL_XFEM','V V I',NBMX,JWXFEM)
C             TABLEAU INDICATEUR DE MAILLES FISSUREES

              CALL WKVECT('&&'//NOMPRO//'.IND_MAIL','V V I',
     &                    ZI(JDIM+2),JINDMF)

C             TABLEAU DE POSITION DANS '.TOPOSE.CSETTO': ZI(JJCNS)
              CALL WKVECT('&&OP0196.POSI_CNS','V V I',NBMX,JJCNS)
C             TABLEAU DE POSITION DANS '.TOPOSE.HEAVTO': ZI(JJHEA)
              CALL WKVECT('&&OP0196.POSI_HEA','V V I',NBMX,JJHEA)
              CALL JEVEUO(MO//'.TOPOSE.HEA.CELV','L',JHEA)
              DO 139 I=1,ZI(JDIM+2)
                 ZI(JINDMF+I-1)=0
 139          CONTINUE
 
              NBTNIM=0
              IICNS=0
              IIHEA=0
              KK=0
C             BOUCLE SUR LES GRELS:
              DO 140 I=1,NBGREL
                 CALL JEVEUO(JEXNUM(LIEL,I),'L',IGREL)
                 CALL JELIRA(JEXNUM(LIEL,I),'LONMAX',NBMAGL,KBID)
                 CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',
     &                ZI(IGREL+NBMAGL-1)),NOTYPE)
                 CALL DISMOI('F','DIM_TOPO',NOTYPE,'TYPE_ELEM',DIMTO,
     &                K8B,IRET)
                 IF(DIM.EQ.3)THEN
                   IF(DIMTO.EQ.3)THEN
                     NBCNS=128
                     NBHEA=36
                   ELSE
                     NBCNS=18
                     NBHEA=6
                     IF(NOTYPE(1:9).EQ.'MECA_FACE')THEN
                     GOTO 140
                     ENDIF
                   ENDIF
                 ELSE
                   IF(NOTYPE(8:9).NE.'_X')GOTO 140
                   NBCNS=18
                   NBHEA=6
                 ENDIF
C                BOUCLE SUR LES MAILLES DU GREL:
                 DO 150 J=1,NBMAGL
                    IMA=ZI(IGREL+J-1)
                    IF (ZI(JMMF+IMA-1).EQ. 1) THEN
                      ZI(JMMF+IMA-1)=0
                      ADLON = ZI(JLOND-1+ZI(JLOND-1+I +4) + 4 + 
     &                                            4*(J-1) + 4)
                      ADCNS = ZI(JCNSD-1+ZI(JCNSD-1+I +4) + 4 + 
     &                                            4*(J-1) + 4)
                      IF (ADLON .EQ. 0) THEN
                        I1 = 0
                      ELSE             
                        I1 = ZI(JLON-1 + ADLON)
                      ENDIF  

C                      IF(I1.NE.0)THEN
                        KK=KK+1
                        NBNIM = ZI(JLON-1 +ADLON + I1 + 1)

                        IF(NBNIM.GT.0)THEN
                           CALL WKVECT('&&OP0196.1000','V V I',
     &                                 NBNIM,JNIM)
                           DO 144 II=1,NBNIM
                              ZI(JNIM+II-1)=0
 144                       CONTINUE
                           DO 145 I1000=1,NBCNS
                              ILOC=ZI(JCNS-1+ADCNS+I1000-1)
                              IF(ILOC.GT.1000)THEN
                                 ZI(JNIM+ILOC-1001)=1
                              ENDIF
 145                       CONTINUE
                           NB1000=0
                           DO 146 II=1,NBNIM
                              NB1000=NB1000+ZI(JNIM+II-1)
 146                       CONTINUE
                           NBNIM=NB1000
                           CALL JEDETR('&&OP0196.1000')
                        ENDIF
                       NBTNIM=NBTNIM+NBNIM
                       ZI(JWXFEM+KK-1)=IMA
                       ZI(JINDMF+IMA-1)=NBNIM
                       ZI(JJCNS+KK-1)=IICNS
                       ZI(JJHEA+KK-1)=IIHEA
C                     ENDIF
                     IICNS=IICNS+NBCNS
                     IIHEA=IIHEA+NBHEA
                   ENDIF                    
 150             CONTINUE
 140          CONTINUE
C
              KMMF=0
              DO 5 I=1,ZI(JDIM+2)
                IF(ZI(JMMF+I-1).EQ.1)KMMF=KMMF+1
 5            CONTINUE
              NBMX=NBMX-KMMF

C             LISTE DES NOEUDS NX : ZI(JLNONX)
              CALL JEVEUO(MA//'.DIME','L',JDIM)
              CALL JENONU(JEXNOM(MA//'.NOMNOE','NXP1'),NUNXP)
              CALL JENONU(JEXNOM(MA//'.NOMNOE','NXM1'),NUNXM)
              NUNX=MIN(NUNXP,NUNXM)
              IF(NUNX.EQ.0)THEN
                 DO 774 I=1,ZI(JDIM)
                     CALL JENUNO(JEXNUM(MA//'.NOMNOE',I),NOMA)
                     IF(NOMA(1:2).EQ.'NI')THEN
                        NUNX=I
                        GOTO 775
                     ENDIF
 774              CONTINUE
               ENDIF
 775           CONTINUE
C
              CONNEX=ZK8(JCNSK1)//'.CONNEX'
              CALL JEVEUO(MA//'.DIME','L',JDIM)
              NBNOAJ=ZI(JDIM)-NUNX+1
              CALL WKVECT('&&OP0196.LIST_NO_NX','V V I',NBNOAJ,JLNONX)
              K=0
              DO 776 I=1,NBNOAJ
                  NUM=NUNX+I-1
                  CALL JENUNO(JEXNUM(MA//'.NOMNOE',NUM),NOMA)
                  IF(NOMA(1:2).EQ.'NX')THEN
                     K=K+1
                     ZI(JLNONX+K-1)=NUM
                  ENDIF
 776           CONTINUE
C
C  ---        REMPLISSAGE DU CHAM_NO
C
              CALL JEVEUO(ZK8(JCNSK1)//'.TYPMAIL','L',JTYPMA)
 1000         CONTINUE
              KK=0
C
             DO 777 I=1,NBMX
                 CALL JEVEUO(JEXNUM(CONNEX,ZI(JWXFEM+I-1)),'L',JCO)
                 CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',
     &                ZI(JTYPMA+ZI(JWXFEM+I-1)-1)),TYPMA)
                 IF(TYPMA(1:4).EQ.'HEXA')THEN
                       NBNOSO=8
                       NBCNS=128
                 ELSEIF(TYPMA(1:4).EQ.'PENT')THEN
                       NBNOSO=6
                       NBCNS=128
                 ELSEIF(TYPMA(1:4).EQ.'TETR')THEN
                       NBNOSO=4
                       NBCNS=128
                 ELSEIF(TYPMA(1:4).EQ.'TRIA')THEN
                       NBNOSO=3
                       NBCNS=18
                 ELSEIF(TYPMA(1:4).EQ.'QUAD')THEN
                       NBNOSO=4
                       NBCNS=18
                 ENDIF
C
                  CALL WKVECT('&&OP0196.NO_LOC','V V I',NBNOSO,JNOLOC)
                  DO 780 II=1,NBNOSO
                     ZI(JNOLOC+II-1)=0
 780              CONTINUE
C                  JNOLOC: INDICE SIGNIFIANT QUE LE NOEUD EST SOMMET
                  DO 781 II=1,NBCNS
                     IF(ZI(JCNS+ZI(JJCNS+I-1)+II-1).LT.1000 .AND.
     &                  ZI(JCNS+ZI(JJCNS+I-1)+II-1).GT.0)THEN
                        ZI(JNOLOC+ZI(JCNS+ZI(JJCNS+I-1)+II-1)-1)=1
                     ENDIF
 781              CONTINUE

                 IF(ZI(JINDMF+ZI(JWXFEM+I-1)-1).NE.0)THEN
C
                    DO 779 J=1,2*ZI(JINDMF+ZI(JWXFEM+I-1)-1)
                       KK=KK+1
                       NUMNOE=ZI(JLNONX+KK-1)
                       CALL JENUNO(JEXNUM(MA//'.NOMNOE',NUMNOE),NOMNOE)
                       DO 800 ICP=1,NBCMP
                         CALL CESEXI('C',JCESD,JCESL,ZI(JWXFEM+I-1),1,1,
     &                               NBCMP*(NBNOSO+J-1)+ICP,IAD)
                       ZR(JCNSV2+NBCMP*(NUMNOE-1)+ICP-1)=ZR(JCESV+IAD-1)
                         ZL(JCNSL2+NBCMP*(NUMNOE-1)+ICP-1)=.TRUE.
 800                   CONTINUE
 779                CONTINUE
C
                   DO 799 J=1,NBNOSO
                      IF(ZI(JNOLOC+J-1).EQ.0)THEN
                             GOTO 799
                      ENDIF
                      IF(ABS(ZR(JLNNO+ZI(JCO+J-1)-1)).LT.EPS)GOTO 799
                      CALL JENUNO(JEXNUM(ZK8(JCNSK1)//'.NOMNOE',
     &                     ZI(JCO+J-1)),NOMNOE)
                       CALL JENONU(JEXNOM(MA//'.NOMNOE',NOMNOE),NUMNOE)
                       DO 801 ICP=1,NBCMP
                          CALL CESEXI('C',JCESD,JCESL,ZI(JWXFEM+I-1),
     &                                 1,1,NBCMP*(J-1)+ICP,IAD)
                       ZR(JCNSV2+NBCMP*(NUMNOE-1)+ICP-1)=ZR(JCESV+IAD-1)
                          ZL(JCNSL2+NBCMP*(NUMNOE-1)+ICP-1)=.TRUE.
 801                   CONTINUE
 799               CONTINUE
                   ENDIF
C
                  IF(ZI(JINDMF+ZI(JWXFEM+I-1)-1).EQ.0 .OR. QUMAFI)THEN
                    DO 778 J=1,NBNOSO
                       CALL JENUNO(JEXNUM(ZK8(JCNSK1)//'.NOMNOE',
     &                     ZI(JCO+J-1)),NOMNOE)
                       CALL CODENT(ZI(JCO+J-1),'G',CHNUM)
                       NOMNOI='NI'//CHNUM
                       CALL JEEXIN(JEXNOM(MA//'.NOMNOE',NOMNOI),IRET)
                       IF(IRET.NE.0)THEN
                         IF(ZI(JHEA+ZI(JJHEA+I-1)).EQ.-1)THEN
                            NOMNOE=NOMNOI
                         ENDIF
                        ENDIF
                        CALL JENONU(JEXNOM(MA//'.NOMNOE',NOMNOE),NUMNOE)
                        DO 802 ICP=1,NBCMP
                          CALL CESEXI('C',JCESD,JCESL,ZI(JWXFEM+I-1),
     &                                 1,1,NBCMP*(J-1)+ICP,IAD)
                       ZR(JCNSV2+NBCMP*(NUMNOE-1)+ICP-1)=ZR(JCESV+IAD-1)
                          ZL(JCNSL2+NBCMP*(NUMNOE-1)+ICP-1)=.TRUE.
 802                    CONTINUE
  778                CONTINUE
                  ENDIF
                 CALL JEDETR('&&OP0196.NO_LOC')
 777          CONTINUE
C

              CALL RSEXCH(RESUC1,'DEPL',IORD,CH,IRET)
              CALL CNSCNO(CHS,' ','NON','G',CH)
C
C             3 - AJOUT DU CHAMP DANS LA SD RESULTAT
C             --------------------------------------
              CALL RSNOCH(RESUC1,'DEPL',IORD,' ')
              CALL RSADPA(RESUC1,'E',1,'INST',IORD,0,JINST,KBID)
              ZR(JINST)=INST
C             DESTRUCTION DU CHAMP_S
              CALL DETRSD('CHAM_NO_S',CHS)
              CALL JEDETR('&&OP0196.MA_MO_FISS')
              CALL JEDETR('&&OP0196.MAIL_XFEM')
              CALL JEDETR('&&'//NOMPRO//'.IND_MAIL')
              CALL JEDETR('&&OP0196.POSI_CNS')
              CALL JEDETR('&&OP0196.POSI_HEA')
              CALL JEDETR('&&OP0196.LIST_NO_NX')
 20        CONTINUE

C       2.2  TRAITEMENT DES CONTRAINTES
C       ================================
C
        ELSEIF (NOMCHA(1:9).EQ.'SIEF_ELGA')THEN
          CALL U2MESS('A','PREPOST4_25')
        ENDIF
 10   CONTINUE

      CALL JEDEMA()
C
      END
