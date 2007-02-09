      SUBROUTINE OP0187(IER)
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
C TOLE  CRP_20 
C     =================================================================
C                      OPERATEUR POST_MAIL_XFEM
C                      ------------------------
C     BUT : GENERER UN MAILLAGE DESTINE UNIQUEMENT AU POST-TRAITEMENT
C           DES ELEMENTS XFEM, ET METTANT EN EVIDENCE LES SOUS-ELEMENTS
C           DES MAILLES FISSUREES
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
      INTEGER IRET,JNOMA,JDIM,NBMF,NBMNF,JIND,NBTMA,JHEAV,I,J,K,NIS,
     &        JFISS,NBNI,NBSE,JLON,IINF,ISUP,NBTGMA,NBMAGP,JGPMA,KK,
     &        JMAGP,JGM,JTYP,JTYPF,NBNOMX,NBCO,NBTCO,JCO,JCOF,JINGM,
     &        JREFE,JVAL,NBMAGT,JNBNIM,JNBSEM,NBSEM,JNGMF,IPOMA,INUMA,
     &        IREGM,NUMAF,NUMSE,NBUTI,JJ,JNBNFG,ITET,JCNS,NMVTNF,I1,I2,
     &        JPIN,NUMNI,INO,KTET,IMA,JC,NBNIM,ICNS,NLIG,NBNONO,
     &        NBHEA,NTET,TYPSE,IC,DIM,JFACE,IFACE,JMMF,J1,J2,J3,KMMF,
     &        NXIS,NBNIS,JNIS,JNISID,JCOFE,INIS,NUISP,NUISM,NBCNS,
     &        JCORF,ILOC,JNBSE,IIND,JSE,JIIND,JIINDP,NBGEL,NBMAGL,JCOFL,
     &        JJCNS,JJPIN,IGREL,NBGREL,JWXFEM,JL,JDIMI,JJSE,
     &        JHTET,JINDNP,JINDNM,IIHEA,JJHEA,JNTE,JNUNO,NUTYEL,JCONN,
     &        NBTNO,JH20N,JH20M,JCOR,NUNO,NBNH20,NCO,NTGEO,IAD,IMX,
     &        NBTGNO,NBNOGP,JNO,NBNOF,JNOGP,JGPNO,JNIM,II,I1000,NB1000,
     &        NBMANI,JMANIS,JNDNIS,JLNNO,JINDNS,JNNEW,NBNOIS,IRE,JMX,
     &        NBNOSO,NBNOMI,JNX,NBHECT,NBCTIP,NBHEAV,JHECT,JCTIP,DIMTO,
     &        JNFIS,NFIS,IFIS,JCNSMV,JLOND,ADLON,JCNSD,
     &        ADCNS,ILIREP,NUGR,NUGE,JHEAVD,ADHEA,JPIND,ADPIN
      REAL*8  XNIS,YNIS,ZNIS,XNXIS,YNXIS,ZNXIS,EPS,VALLN
      PARAMETER(EPS=1.D-10)
      CHARACTER*1 KBID
      CHARACTER*8 MO,MAF,MA,FISS,NOMA,NOMGMA,NOMNO,CH,NOMSE,NOMNI,TMP,
     &            NOMGNO,NXISM,NXISP,K8B,NOXIS
      CHARACTER*16 K16B,NOTYPE
      CHARACTER*19 COORDO,COORD1,CNSFM
      CHARACTER*24 HEAV,PINTTO,CNSETO,GRPMA,CONNEX,LIEL,GRPNO,CTIP,HECT
      CHARACTER*24 REPE
      LOGICAL LNIS,LMNIS
C
      CALL JEMARQ()

C
C     ===========================================     
C      1. RECUPERATION DES CONCEPTS UTILISATEURS
C         ET CREATION DES TABLEAUX DE TRAVAIL
C     ===========================================
C
C --- MODELE FISSURE : MO
C     ON VERIFIE QUE LE MODELE EST COMPATIBLE AVEC LA METHODE XFEM
      CALL GETVID(' ','MODELE',0,1,1,MO,IRET)
      CALL JEEXIN(MO//'.FISS',IRET)
      IF(IRET.EQ.0)THEN
        CALL UTDEBM('F','OP0187','LE MODELE')
        CALL UTIMPK('S',' ',1,MO)
        CALL UTIMPK('S',' EST INCOMPATIBLE AVEC LA METHODE XFEM',1,KBID)
        CALL UTFINM()
      ENDIF
C
      CALL DISMOI('F','DIM_GEOM',MO,'MODELE',DIM,K8B,IRET)

      LIEL=MO//'.MODELE    .LIEL'
      REPE=MO//'.MODELE    .REPE'
      CALL JELIRA(LIEL,'NMAXOC',NBGREL,KBID)
C
C --- NOM DU MAILLAGE D'ENTREE : MA
      CALL JEVEUO(MO//'.MODELE    .NOMA','L',JNOMA)
      MA=ZK8(JNOMA)
C
C --- NOM DU MAILLAGE DE SORTIE : MAF
      CALL GETRES(MAF,K16B,K16B)
C
C --- NOMBRE DE MAILLES DU MAILLAGE INITIAL :  NBTMA
      CALL JEVEUO(MA//'.DIME','L',JDIMI)
      NBTMA=ZI(JDIMI+2)
      CALL WKVECT('&&OP0187.MAIL_MODEL_FISS','V V I',NBTMA,JMMF)
      DO 9 I=1,NBTMA
        ZI(JMMF+I-1)=0
 9    CONTINUE

C     RECUPERATION DU NOMBRE DE FISSURES DANS MODELE
      CALL JEVEUO(MO//'.NFIS','L',JNFIS)
      NFIS = ZI(JNFIS)  

      NBTNO=ZI(JDIMI)

C     NOMBRE DE MAILLES X-FEM :  NBMF
C     ON DESIGNERA PAR MAILLES X-FEM LES MAILLES VOISINES DE LA FISSURES
C     & LES MAILLES TRAVERSEES PAR LA FISSURE 
C     (ON ECARTE DONC LES MAILLES DU LIGREL QUI SONT ELOIGNEES 
C      DE LA FISSURE)

C     ON BOUCLE SUR LES FISSURES
      
      NBMF = 0
      
      DO 1000 IFIS=1, NFIS

        CALL JEVEUO(MO//'.FISS','L',JFISS)
        FISS=ZK8(JFISS-1+IFIS)
        HEAV=FISS//'.MAILFISS  .HEAV'
        CALL JEEXIN(HEAV,IRET)
        IF(IRET.NE.0)THEN
           CALL JELIRA(HEAV,'LONMAX',NBHEAV,KBID)
           CALL JEVEUO(HEAV,'L',J1)

           DO 8 I=1,NBHEAV
             ZI(JMMF+ZI(J1+I-1)-1)=1
   8       CONTINUE
        ELSE
           NBHEAV=0
        ENDIF
        CTIP=FISS//'.MAILFISS  .CTIP'
        CALL JEEXIN(CTIP,IRET)
        IF(IRET.NE.0)THEN
           CALL JELIRA(CTIP,'LONMAX',NBCTIP,KBID)
           CALL JEVEUO(CTIP,'L',J2)
           DO 7 I=1,NBCTIP
             ZI(JMMF+ZI(J2+I-1)-1)=1
   7       CONTINUE
        ELSE
           NBCTIP=0
        ENDIF
        HECT=FISS//'.MAILFISS  .HECT'
        CALL JEEXIN(HECT,IRET)
        IF(IRET.NE.0)THEN
           CALL JELIRA(HECT,'LONMAX',NBHECT,KBID)
           CALL JEVEUO(HECT,'L',J3)
           DO 6 I=1,NBHECT
             ZI(JMMF+ZI(J3+I-1)-1)=1
   6       CONTINUE
        ELSE
           NBHECT=0
        ENDIF

C     NOMBRE DE MAILLES FISSUREES
        NBMF=NBMF+NBHEAV+NBCTIP+NBHECT

 1000 CONTINUE     
C
C     FIN BOUCLE SUR LES FISSURES
C --- CREATION DE TABLEAUX DE TRAVAIL ET RECUPERATION DE DIMENSIONS:
C     T1 - TABLEAU DES MAILLES X-FEM :ZI(JWXFEM)
      CALL WKVECT('&&OP0187.MAIL_XFEM','V V I',NBMF,JWXFEM)
C     T2 - TABLEAU INDICATEUR DES MAILLES X-FEM
      CALL WKVECT('&&OP0187.MAIL_INDI_FISS','V V I',NBTMA,JIND)
C     T3 - TABLEAU DU NOMBRE DE SOUS-ELEMENTS PAR MAILLE: ZI(JNBSEM)
      CALL WKVECT('&&OP0187.NBSEM','V V I',NBMF,JNBSEM)
C     T4 - TABLEAU DU NOMBRE DE POINTS D'INTERSECT PAR MAILLE:ZI(JNBNIM)
      CALL WKVECT('&&OP0187.NBNIM','V V I',NBMF,JNBNIM)
C     T5 - TABLEAU DE POSITION DANS '.TOPOSE.PINTTO': ZI(JJPIN)
      CALL WKVECT('&&OP0187.POSI_PIN','V V I',NBMF,JJPIN)
C     T6 - TABLEAU DE POSITION DANS '.TOPOSE.CSETTO': ZI(JJCNS)
      CALL WKVECT('&&OP0187.POSI_CNS','V V I',NBMF,JJCNS)
C     T7 - TABLEAU DE POSITION DANS '.TOPOSE.HEAV': ZI(JJHEA)
      CALL WKVECT('&&OP0187.POSI_HEA','V V I',NBMF,JJHEA)
C     T8 - TABLEAU DE POSITION DU NOMBRE DE SOUS-ELEMENTS: ZI(JJSE)
      CALL WKVECT('&&OP0187.POSI_JSE','V V I',NBMF,JJSE)
C     T9 - TABLEAU DE POSITION DES  MAILLES HEXA20 NON XFEM: ZI(JH20M)
      CALL WKVECT('&&OP0187.POSI_MA_H20','V V I',NBTMA,JH20M)
C     T9 - TABLEAU DE POSITION DES  MAILLES HEXA20 NON XFEM: ZI(JH20N)
      CALL WKVECT('&&OP0187.POSI_NO_H20','V V I',NBTNO,JH20N)
C     T10 - TABLEAU DES MAIILES XFEM AYANT POUR NOEUD D'INTERSECTION
C     QUE DES NOEUDS SOMMETS.
      CALL WKVECT('&&OP0187.MA_NSNI','V V I',NBMF,JMANIS)
C     T11 - TABLEAU INDICATEUR ASSOCIE A T10
      CALL WKVECT('&&OP0187.IND_NSNI','V V I',NBTMA,JNDNIS)

C     T12 - TABLEAU D'INDICES DES ELEMENTS DE BORD:ZI(JFACE)
      CALL WKVECT('&&OP0187.FACE','V V I',NBMF,JFACE)
C
      DO 10 I=1,NBTMA
         ZI(JIND+I-1)=0
         ZI(JH20M+I-1)=0
 10   CONTINUE
      DO 11 I=1,NBTNO
         ZI(JH20N+I-1)=0
 11   CONTINUE
      DO 12 I=1,NBTMA
         ZI(JNDNIS+I-1)=0
 12   CONTINUE
C
C     ON PARCOURT LE LIGREL:
      CALL JEVEUO(MO//'.TOPOSE.LON.CELV','L',JLON)
      CALL JEVEUO(MO//'.TOPOSE.LON.CELD','L',JLOND)
      CALL JEVEUO(MO//'.TOPOSE.CNS.CELV','L',JCNS)
      CALL JEVEUO(MO//'.TOPOSE.CNS.CELD','L',JCNSD)
      CALL JEVEUO(MO//'.TOPOSE.PIN.CELV','L',JPIN)
      CALL JEVEUO(MO//'.TOPOSE.PIN.CELD','L',JPIND)
      
      KK=0
      IIHEA=0
      NBSE=0
      NBNI=0
      NBNOIS=0
      NBMANI=0

      DO 40 I=1,NBGREL
         CALL JEVEUO(JEXNUM(LIEL,I),'L',IGREL)
         CALL JELIRA(JEXNUM(LIEL,I),'LONMAX',NBMAGL,KBID)
      
         NUTYEL=ZI(IGREL+NBMAGL-1)
         CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',NUTYEL),NOTYPE)
         CALL DISMOI('F','DIM_TOPO',NOTYPE,'TYPE_ELEM',DIMTO,K8B,IRET)
         IFACE=0
         IF(DIM.EQ.3)THEN
           IF(DIMTO.EQ.3)THEN
             NBCNS=128
             NBHEA=36
           ELSE
             NBCNS=18
             NBHEA=6
             IF(NOTYPE(1:9).EQ.'MECA_FACE')THEN
               GOTO 40
             ELSEIF(NOTYPE(8:11).EQ.'FACE' .OR. NOTYPE(9:12).EQ.'FACE'
     &         .OR. NOTYPE(10:13).EQ.'FACE')THEN
                IFACE=1
             ENDIF
           ENDIF
         ELSE
             IF(NOTYPE(8:9).NE.'_X')GOTO 40
             NBCNS=18
             NBHEA=6
         ENDIF

         DO 50 J=1,NBMAGL-1
           IMA=ZI(IGREL+J-1)
           IF (ZI(JMMF+IMA-1).EQ. 1) THEN

             ZI(JMMF+IMA-1)=0 

             ADLON = ZI(JLOND-1+ZI(JLOND-1+I +4) + 4 + 4*(J-1) + 4)
             ADCNS = ZI(JCNSD-1+ZI(JCNSD-1+I +4) + 4 + 4*(J-1) + 4)
             ADPIN = ZI(JPIND-1+ZI(JPIND-1+I +4) + 4 + 4*(J-1) + 4)
             IF (ADLON .EQ. 0) THEN
               I1 = 0
             ELSE             
               I1 = ZI(JLON-1 + ADLON)
             ENDIF  

             IF(I1.NE.0)THEN
C            LA MAILLE X-FEM N'EST PAS ELOIGNEE DE LA FISSURE:
               I2 = ZI(JLON-1 +ADLON + I1 + 1)

               KK=KK+1
               ZI(JNDNIS+KK-1)=0

               IF(I2.GT.0)THEN
C               ON RECUPERE LES MAILLES XFEM QUI POSSEDENT POUR
C               NOEUDS D'INTERSECTION QUE DES NOEUDS SOMMETS
                  CALL WKVECT('&&OP0187.1000','V V I',I2,JNIM)
                  DO 144 II=1,I2
                     ZI(JNIM+II-1)=0
 144              CONTINUE
                  DO 145 I1000=1,NBCNS
                     ILOC=ZI(JCNS-1+ADCNS+I1000-1)
                     IF(ILOC.GT.1000)THEN
                        ZI(JNIM+ILOC-1001)=1
                     ENDIF
 145              CONTINUE
                  NB1000=0
                  DO 146 II=1,I2
                     NB1000=NB1000+ZI(JNIM+II-1)
 146              CONTINUE
                  IF(NB1000.NE.I2)THEN
                     NBNOIS=NBNOIS+I2
                     NBMANI=NBMANI+1
                     ZI(JMANIS+NBMANI-1)=IMA
                     ZI(JNDNIS+KK-1)=IMA
                  ENDIF
                  CALL JEDETR('&&OP0187.1000')
                ENDIF
                ZI(JWXFEM+KK-1)=IMA
                ZI(JFACE+KK-1)=IFACE
                ZI(JIND+IMA-1)=KK
                ZI(JJCNS+KK-1)=ADCNS-1
                ZI(JJPIN+KK-1)=ADPIN-1
                ZI(JJHEA+KK-1)=IIHEA

C               NOMBRE DE POINTS D'INTERSECTION
                NBNI=NBNI+I2
                ZI(JNBNIM+KK-1)=I2
C               NOMBRE DE SOUS-ELEMENTS
                JJ=0
                DO 45 JL=1,I1
                   JJ=JJ+ZI(JLON-1 +ADLON+JL)
 45             CONTINUE
                ZI(JNBSEM+KK-1)=JJ
                ZI(JJSE+KK-1)=NBSE
                NBSE=NBSE+JJ
             ELSE

C            ON RECUPERE LES MAILLES QUADRATIQUES EN VUE DE LES
C            LINEARISER
                CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMA),'L',JCO)
                IF(NOTYPE(1:13).EQ.'MECA_X_HEXA20'  .OR.
     &             NOTYPE(1:14).EQ.'MECA_X_PENTA15' .OR.
     &             NOTYPE(1:14).EQ.'MECA_X_TETRA10' )THEN
                  CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMA),'L',JCO)
                  IF(NOTYPE(8:11).EQ.'HEXA')THEN
                        NBNOSO=8
                        NBNOMI=12
                        ZI(JH20M+IMA-1)=24
                  ELSEIF(NOTYPE(8:12).EQ.'PENTA')THEN
                        NBNOSO=6
                        NBNOMI=9
                        ZI(JH20M+IMA-1)=20
                  ELSEIF(NOTYPE(8:12).EQ.'TETRA')THEN
                        NBNOSO=4
                        NBNOMI=6
                        ZI(JH20M+IMA-1)=18
                  ENDIF
                ELSEIF(NOTYPE.EQ.'MECPTR6_X'.OR.NOTYPE.EQ.'MEDPTR6_X'
     &             .OR. NOTYPE.EQ.'MECA_X_FACE6')THEN
                  CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMA),'L',JCO)
                  NBNOSO=3
                  NBNOMI=3
                  ZI(JH20M+IMA-1)=7
                ELSEIF(NOTYPE.EQ.'MECPQU8_X'.OR.NOTYPE.EQ.'MEDPQU8_X'
     &             .OR. NOTYPE.EQ.'MECA_X_FACE8')THEN
                  NBNOSO=4
                  NBNOMI=4
                  ZI(JH20M+IMA-1)=12
                ENDIF
                DO 46 INO=1,NBNOMI
                   ZI(JH20N+ZI(JCO+NBNOSO+INO-1)-1)=1
 46             CONTINUE
             ENDIF
             IIHEA=IIHEA+NBHEA

C	SI LA MAILLE N'EST PAS XFEM, ON VA LA LINEARISEE SEULEMENT
           ELSE

             CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMA),'L',JCO)
             IF(NOTYPE(1:13).EQ.'MECA_X_HEXA20'  .OR.
     &          NOTYPE(1:14).EQ.'MECA_X_PENTA15' .OR.
     &          NOTYPE(1:14).EQ.'MECA_X_TETRA10' )THEN
                CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMA),'L',JCO)
                IF(NOTYPE(8:11).EQ.'HEXA')THEN
                      NBNOSO=8
                      NBNOMI=12
                      ZI(JH20M+IMA-1)=24
                ELSEIF(NOTYPE(8:12).EQ.'PENTA')THEN
                      NBNOSO=6
                      NBNOMI=9
                      ZI(JH20M+IMA-1)=20
                ELSEIF(NOTYPE(8:12).EQ.'TETRA')THEN
                      NBNOSO=4
                      NBNOMI=6
                      ZI(JH20M+IMA-1)=18
                ENDIF
             ELSEIF(NOTYPE.EQ.'MECPTR6_X'.OR.NOTYPE.EQ.'MEDPTR6_X'.OR.
     &              NOTYPE.EQ.'MECA_X_FACE6')THEN
               CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMA),'L',JCO)
               NBNOSO=3
               NBNOMI=3
               ZI(JH20M+IMA-1)=7
             ELSEIF(NOTYPE.EQ.'MECPQU8_X'.OR.NOTYPE.EQ.'MEDPQU8_X'.OR.
     &              NOTYPE.EQ.'MECA_X_FACE8')THEN
               NBNOSO=4
               NBNOMI=4
               ZI(JH20M+IMA-1)=12
             ENDIF
             DO 4600 INO=1,NBNOMI
                ZI(JH20N+ZI(JCO+NBNOSO+INO-1)-1)=1
 4600        CONTINUE
           ENDIF
 50      CONTINUE
 40   CONTINUE
C

      NBNOIS=NBNOIS/2

C     ON AJUSTE LE NOMBRE DE MAILLES FISSUREES (CELLES DU MODELE XFEM)
      KMMF=0
      DO 5 I=1,NBTMA
        IF(ZI(JMMF+I-1).EQ.1)KMMF=KMMF+1
 5    CONTINUE
      NBMF=NBMF-KMMF
      NBMNF=NBTMA-NBMF
C
C     ===============================================================
C      2. CONTRIBUTION DES NOEUDS ET DES MAILLES NON XFEM POUR
C         LA CREATION DE LA NOUVELLE SD MAILLAGE
C     ===============================================================
C
C
C --- CREATION DES OBJETS: '.DIME','.NOMNOE','.COORDO'
      CALL JEDUPO(MA//'.DIME','G',MAF//'.DIME',.FALSE.)
      CALL JEVEUO(MA //'.DIME','L',JDIMI)
      CALL JEVEUO(MAF//'.DIME','E',JDIM)
      NBNH20=0
      DO 51 I=1,NBTNO
         NBNH20=NBNH20+ZI(JH20N+I-1)
 51   CONTINUE
      ZI(JDIM)=ZI(JDIMI)+2*NBNI-NBNH20+NBNOIS
      ZI(JDIM+2)=NBMNF+NBSE
      NCO=3*ZI(JDIM)

      COORD1=MA//'.COORDO'
      CALL JEVEUO(COORD1//'.VALE','L',JCOR)

      COORDO=MAF//'.COORDO'
      CALL JECREO(COORDO//'.VALE','G V R')
      CALL JEECRA(COORDO//'.VALE','LONMAX',NCO,' ')
      CALL JEVEUO(COORDO//'.VALE','E',JCORF)
 
      TMP='NOMTMP'
      CALL JECREO(TMP//'.NOMNOE','V N K8')
      CALL JEECRA(TMP//'.NOMNOE','NOMMAX',ZI(JDIM),KBID)

      DO 52 I=1,NBTNO
        IF(ZI(JH20N+I-1).EQ.0)THEN
           CALL JENUNO(JEXNUM(MA//'.NOMNOE',I),NOMNO)
           CALL JECROC(JEXNOM(TMP//'.NOMNOE',NOMNO))
           CALL JENONU(JEXNOM(TMP//'.NOMNOE',NOMNO),NUNO)
           DO 53 IC=1,3
              ZR(JCORF+3*(NUNO-1)+IC-1)=ZR(JCOR+3*(I-1)+IC-1)
 53        CONTINUE
        ENDIF
 52   CONTINUE
C
      CALL WKVECT(COORDO//'.REFE','G V K24',2,JREFE)
      ZK24(JREFE) = MAF
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD','GEOM_R'),NTGEO)
      CALL JECREO(COORDO//'.DESC','G V I')
      CALL JEECRA(COORDO//'.DESC','LONMAX',3,' ')
      CALL JEECRA(COORDO//'.DESC','DOCU',0,'CHNO')
      CALL JEVEUO(COORDO//'.DESC','E',IAD)
      ZI(IAD)   =  NTGEO
      ZI(IAD+1) = -3
      ZI(IAD+2) = 14

C --- CREATION DES OBJETS : '.NOMMAI','.TYPMAIL','.CONNEX'
      CONNEX=MAF//'.CONNEX'
      CALL JECREO(MAF//'.NOMMAI','G N K8')
      CALL JEECRA(MAF//'.NOMMAI','NOMMAX',ZI(JDIM+2),KBID)
      CALL WKVECT(MAF//'.TYPMAIL','G V I',ZI(JDIM+2),JTYPF)
      CALL JEVEUO(MA//'.TYPMAIL','L',JTYP)
      CALL DISMOI('F','NB_NO_MAX','&CATA','CATALOGUE',NBNOMX,KBID,IRET)
      CALL JECREC(CONNEX,'G V I','NU','CONTIG','VARIABLE',ZI(JDIM+2))
      NBTCO=NBNOMX*ZI(JDIM+2)
      CALL JEECRA(CONNEX,'LONT',NBTCO,KBID)
      J=0
      DO 60 I=1,NBTMA
         CALL JENUNO(JEXNUM(MA//'.NOMMAI',I),NOMA)
         IF(ZI(JIND+I-1).EQ.0)THEN
           J=J+1
           CALL JECROC(JEXNOM(MAF//'.NOMMAI',NOMA))
           CALL JECROC(JEXNUM(CONNEX,J))
           CALL JEVEUO(JEXNUM(MA//'.CONNEX',I),'L',JCO)
           IF(ZI(JH20M+I-1).NE.0)THEN
               IF(ZI(JTYP+I-1).EQ.25)THEN
                 NBNOSO=8
               ELSEIF(ZI(JTYP+I-1).EQ.21)THEN
                 NBNOSO=6
               ELSEIF(ZI(JTYP+I-1).EQ.19)THEN
                 NBNOSO=4
               ELSEIF(ZI(JTYP+I-1).EQ.9)THEN
                 NBNOSO=3
               ELSEIF(ZI(JTYP+I-1).EQ.14)THEN
                 NBNOSO=4
               ENDIF
              NBCO=NBNOSO
              ZI(JTYPF+J-1)=ZI(JH20M+I-1)
           ELSE
              CALL JELIRA(JEXNUM(MA//'.CONNEX',I),'LONMAX',NBCO,KBID)
              ZI(JTYPF+J-1)=ZI(JTYP+I-1)
           ENDIF
           CALL JEECRA(JEXNUM(CONNEX,J),'LONMAX',NBCO,KBID)
           CALL JEVEUO(JEXNUM(CONNEX,J),'E',JCOF)
           DO 65 K=1,NBCO
              KK=ZI(JCO+K-1)
              CALL JENUNO(JEXNUM(MA //'.NOMNOE',KK),NOMNO)
              CALL JENONU(JEXNOM(TMP//'.NOMNOE',NOMNO),ZI(JCOF+K-1))
 65        CONTINUE
         ENDIF
 60   CONTINUE
C
C --- DUPLICATION A L'IDENTIQUE DES OBJETS DE LA SD MAILLAGE INITIALE
C     QUI NE NECESSITENT AUCUNE MODIFICATION
      CALL JEEXIN(MA//'.GROUPENO',IRET)
      IF(IRET.NE.0)THEN
         CALL JELIRA(MA//'.GROUPENO','NMAXOC',NBTGNO,KBID)
         GRPNO=MAF//'.GROUPENO'
         CALL JECREC(GRPNO,'G V I','NO','DISPERSE','VARIABLE',NBTGNO)

         DO 66 I=1,NBTGNO
            CALL JENUNO(JEXNUM(MA//'.GROUPENO',I),NOMGNO)
            CALL JELIRA(JEXNOM(MA//'.GROUPENO',NOMGNO),'LONMAX',
     &           NBNOGP,KBID)
            CALL JEVEUO(JEXNOM(MA//'.GROUPENO',NOMGNO),'L',JGPNO)
            CALL WKVECT('&&OP0187.NO_GROUP','V V I',NBNOGP,JNOGP)
            K=0
            DO 67 J=1,NBNOGP
               IF(ZI(JH20N+ZI(JGPNO+J-1)-1).EQ.0)THEN
                  K=K+1
                  ZI(JNOGP+K-1)=ZI(JGPNO+J-1)
               ENDIF
 67         CONTINUE
            NBNOF=K
            CALL JECROC(JEXNOM(GRPNO,NOMGNO))
            CALL JEECRA(JEXNOM(GRPNO,NOMGNO),'LONMAX',NBNOF,KBID)
            CALL JEVEUO(JEXNOM(GRPNO,NOMGNO),'E',JNO)
            DO 68 J=1,NBNOF
               CALL JENUNO(JEXNUM(MA //'.NOMNOE',ZI(JNOGP+J-1)),NOMNO)
               CALL JENONU(JEXNOM(TMP //'.NOMNOE',NOMNO),ZI(JNO+J-1))
 68         CONTINUE
            CALL JEDETR('&&OP0187.NO_GROUP')
 66      CONTINUE
      ENDIF
C
C     ================================================================
C      3. CONTRIBUTION DES NOEUDS D'INTERSECTION ET DES SOUS-ELEMENTS
C         POUR LA CREATION DE LA NOUVELLE SD MAILLAGE
C     ================================================================
C
      CALL JEVEUO(MO//'.LNNO      .VALE','L',JLNNO)
      CNSFM='&&OP0187.CNSM'
      CALL CNOCNS(MO//'.LNNO','V',CNSFM)
      CALL JEVEUO(CNSFM//'.CNSV','L',JCNSMV)
      
      CALL JEVEUO(COORDO//'.VALE','E',JCORF)
      CALL WKVECT('&&OP0187.ALL_SS_ELEM','V V I',NBSE,JNBSE)
      INO=0
      NBNONO=0
      IMA=0
      
C     I- ON PARCOURT LES MAILLES X-FEM
C     --------------------------------
C

      DO 100 I=1,NBMF
C        NUMERO DE LA MAILLE X-FEM
         NUMAF=ZI(JWXFEM+I-1)
         CALL JEVEUO(REPE,'L',ILIREP)
         NUGR = ZI(ILIREP-1 + 2*(NUMAF-1) + 1)
         NUGE = ZI(ILIREP-1 + 2*(NUMAF-1) + 2)
         IF(ZI(JFACE+I-1).EQ.1.OR.DIM.EQ.2)THEN
             NBHEA=6
             NTET=3
             TYPSE=7
         ELSE
             NBHEA=36
             NTET=4
             TYPSE=18
         ENDIF
C        NOMBRE DE NOEUDS SOMMETS:
         IF(ZI(JTYP+NUMAF-1).EQ.25)THEN
                 NBNOSO=8
         ELSEIF(ZI(JTYP+NUMAF-1).EQ.21)THEN
                 NBNOSO=6
         ELSEIF(ZI(JTYP+NUMAF-1).EQ.19)THEN
                 NBNOSO=4
         ELSEIF(ZI(JTYP+NUMAF-1).EQ.9)THEN
                 NBNOSO=3
         ELSEIF(ZI(JTYP+NUMAF-1).EQ.14)THEN
                 NBNOSO=4
         ENDIF
         CALL JEVEUO(JEXNUM(MA//'.CONNEX',NUMAF),'L',JCO)
C        NBRE DE SOUS-ELEMENTS DE LA MAILLE:NBSEM
         NBSEM=ZI(JNBSEM+I-1)
C        NBRE DE NOEUDS D'INTERSECTION DE LA MAILLE
         NBNIM=ZI(JNBNIM+I-1)

C        CREATION DE TABLEAUX DE TRAVAIL:
C        - CARACTERISANT LES TETRAS PAR RAPPORT A LA FISS:ZI(JHTET)
C        - PERMETTANT DE SAVOIR SI LES NOEUDS
C          D'INTERSECTION SITUES AU-DESSUS ET EN-DESSOUS DE LA
C          FISSURE ONT ETE RENSEIGNES:ZI(JINDNP),ZI(JINDNM)
C        - UTILE POUR LA NUMEROTATION DES NOEUDS D'INTER :ZI(JNUNO)
         LNIS=.FALSE.
         LMNIS=.FALSE.
         IMX=0
         II=0
         IF(NBNIM.GT.0)THEN
            CALL WKVECT('&&OP0187.NOEUD_INTER_P','V V I',NBNIM,JINDNP)
            CALL WKVECT('&&OP0187.NOEUD_INTER_M','V V I',NBNIM,JINDNM)
            DO 101 J=1,NBNIM
               ZI(JINDNP+J-1)=0
               ZI(JINDNM+J-1)=0
 101        CONTINUE
            CALL WKVECT('&&OP0187.CARA_TET','V V I',NBSEM,JHTET)
            CALL JEVEUO(MO//'.TOPOSE.HEA.CELV','L',JHEAV)
            CALL JEVEUO(MO//'.TOPOSE.HEA.CELD','L',JHEAVD)
            K=0
            DO 102 J=1,NBHEA
               ADHEA = ZI(JHEAVD-1+ZI(JHEAVD-1+NUGR +4)+4+4*(NUGE-1)+4)
               IF(ZI(JHEAV-1+ADHEA+J-1).NE.0) THEN
                K=K+1
                ZI(JHTET+K-1)=ZI(JHEAV-1+ADHEA+J-1)
               ENDIF
 102        CONTINUE
            CALL WKVECT('&&OP0187.NUM_NOEU','V V I',NBNIM,JNUNO)
            DO 103 J=1,NBNIM
               ZI(JNUNO+J-1)=0
 103        CONTINUE
            CALL WKVECT('&&OP0187.NEW_NO','V V I',NBNOSO,JNNEW)
            DO 104 J=1,NBNOSO
               ZI(JNNEW+J-1)=0
 104        CONTINUE
            DO 105 J=1,NBNOSO
              VALLN=ZR(JCNSMV-1+ZI(JCO+J-1))
              IF(ABS(VALLN).LT.EPS)THEN
                ZI(JNNEW+J-1)=1
                LNIS=.TRUE.
              ENDIF
 105        CONTINUE
            IF(ZI(JNDNIS+I-1).EQ.0 .AND. LNIS)THEN
              LMNIS=.TRUE.
              CALL WKVECT('&&OP0187.MX','V V I',2*NBSEM,JMX)
              CALL WKVECT('&&OP0187.NX','V V I',2*NBNIM,JNX)
            ENDIF
            IF(ZI(JNDNIS+I-1).NE.0)THEN
               CALL WKVECT('&&OP0187.NOIS','V V I',NBNOSO,JINDNS)
               DO 106 J=1,NBNOSO
                  ZI(JINDNS+J-1)=0
 106           CONTINUE
            ENDIF
         ENDIF
C
C        II- ON PARCOURT LES SOUS-ELEMENTS
C        ---------------------------------  
         DO 110 J=1,NBSEM
            IMA=IMA+1
            CALL CODENT(IMA,'G',CH)
            NOMSE='MX'//CH(1:LEN(CH))
            CALL JECROC(JEXNOM(MAF//'.NOMMAI',NOMSE))
            CALL JENONU(JEXNOM(MAF//'.NOMMAI',NOMSE),NUMSE)
            IF(LMNIS)THEN
              IMX=IMX+1
              ZI(JMX+2*IMX-2)=NUMSE
              ZI(JMX+2*IMX-1)=ZI(JHTET+J-1)
            ENDIF
            ZI(JNBSE+IMA-1)=NUMSE
            ZI(JTYPF+NBMNF+IMA-1)=TYPSE
            CALL JECROC(JEXNUM(CONNEX,NUMSE))
            CALL JEECRA(JEXNUM(CONNEX,NUMSE),'LONMAX',NTET,KBID)
            CALL JEVEUO(JEXNUM(CONNEX,NUMSE),'E',JCOF)
            KTET=0
C
C           III- ON PARCOURT LES NOEUDS DES SOUS-ELEMENTS
C           ---------------------------------------------
            DO 150 ITET=1,NTET
C              NUMERO LOCAL DU NOEUD DU SOUS-ELEMENT: ICNS
               ICNS=ZI(JCNS+ZI(JJCNS+I-1)+NTET*(J-1)+ITET-1)
               KTET=KTET+1
C
C              III-1 SI LE NOEUD EST UN NOEUD D'INTERSECTION
C              ---------------------------------------------
               IF(ICNS.GT.1000)THEN
                  ILOC=ICNS-1000

C                III-1.1 SI LE NOEUD EST AU DESSUS DE LA FISSURE
C                 ----------------------------------------------
                  IF(ZI(JHTET+J-1).EQ.1)THEN
C                 SI LE NOEUD N'A PAS ETE RENSEIGNE, ON LE CREE
                     IF(ZI(JINDNP+ILOC-1).EQ.0)THEN
                        NBNONO=NBNONO+1
                        IF(ZI(JNUNO+ILOC-1).EQ.0)THEN
                           INO=INO+1
                           ZI(JNUNO+ILOC-1)=INO
                        ENDIF
                        CALL CODENT(ZI(JNUNO+ILOC-1),'G',CH)
                        NOMNI='NXP'//CH(1:LEN(CH))
                        CALL JECROC(JEXNOM(TMP//'.NOMNOE',NOMNI))
                        CALL JENONU(JEXNOM(TMP//'.NOMNOE',NOMNI),NUMNI)
                        ZI(JINDNP+ILOC-1)= NUMNI
                        IF(LMNIS)THEN
                           II=II+1
                           ZI(JNX+II-1)=NUMNI
                        ENDIF
C                       COORDONNEES
                        ZR(JCORF+3*(NUMNI-1))=
     &                      ZR(JPIN+ZI(JJPIN+I-1)+DIM*(ILOC-1))
                        ZR(JCORF+3*(NUMNI-1)+1)=
     &                      ZR(JPIN+ZI(JJPIN+I-1)+DIM*(ILOC-1)+1)
                        IF(DIM.EQ.3)THEN
                           ZR(JCORF+3*(NUMNI-1)+2)=
     &                      ZR(JPIN+ZI(JJPIN+I-1)+3*(ILOC-1)+2)
                        ELSE
                           ZR(JCORF+3*(NUMNI-1)+2)=0.D0
                        ENDIF
C                       CONNECTIVITES
                        ZI(JCOF+KTET-1)=NUMNI
                     ELSE
C                       CONNECTIVITES
                        ZI(JCOF+KTET-1)=ZI(JINDNP+ILOC-1)
                     ENDIF
C
C                 III-1.2 SI LE NOEUD EST EN-DESSUS DE LA FISSURE
C                 -----------------------------------------------
                  ELSEIF(ZI(JHTET+J-1).EQ.-1)THEN
C                 SI LE NOEUD N'A PAS ETE RENSEIGNE, ON LE CREE
                     IF(ZI(JINDNM+ILOC-1).EQ.0)THEN
                        NBNONO=NBNONO+1
                        IF(ZI(JNUNO+ILOC-1).EQ.0)THEN
                           INO=INO+1
                           ZI(JNUNO+ILOC-1)=INO
                        ENDIF
                        CALL CODENT(ZI(JNUNO+ILOC-1),'G',CH)
                        NOMNI='NXM'//CH(1:LEN(CH))
                        CALL JECROC(JEXNOM(TMP//'.NOMNOE',NOMNI))
                        CALL JENONU(JEXNOM(TMP//'.NOMNOE',NOMNI),NUMNI)
                        ZI(JINDNM+ILOC-1)= NUMNI
                        IF(LMNIS)THEN
                           II=II+1
                           ZI(JNX+II-1)=NUMNI
                        ENDIF
C                       COORDONNEES
                        ZR(JCORF+3*(NUMNI-1))=
     &                      ZR(JPIN+ZI(JJPIN+I-1)+DIM*(ILOC-1))
                        ZR(JCORF+3*(NUMNI-1)+1)=
     &                      ZR(JPIN+ZI(JJPIN+I-1)+DIM*(ILOC-1)+1)
                        IF(DIM.EQ.3)THEN
                           ZR(JCORF+3*(NUMNI-1)+2)=
     &                      ZR(JPIN+ZI(JJPIN+I-1)+3*(ILOC-1)+2)
                        ELSE
                           ZR(JCORF+3*(NUMNI-1)+2)=0.D0
                        ENDIF
C                       CONNECTIVITES
                        ZI(JCOF+KTET-1)=NUMNI
                     ELSE
C                       CONNECTIVITES
                        ZI(JCOF+KTET-1)=ZI(JINDNM+ILOC-1)
                     ENDIF
                  ENDIF
C
C              III-2 SI LE NOEUD EST UN NOEUD SOMMET
C              -------------------------------------
               ELSE
C                 III-2.1 SI LE NOEUD SOMMET FAIT PARTI D'UNE MAILLE
C                 POSSEDANT DES NOEUDS SOMMETS QUI SONT NOEUDS
C                 D'INTERSECTION
C                 ---------------------------------------------------
                  IF(ZI(JNDNIS+I-1).NE.0)THEN
C
C                    III-2.1.1 SI LE TETRA EST EN-DESSOUS DE LA FISSURE
C                    --------------------------------------------------
                     IF(ZI(JHTET+J-1).EQ.-1)THEN
C
C                       SI LE NOEUD EST UN NOEUD D'INTERSECTION
C                       ---------------------------------------
                        IF(ZI(JNNEW+ICNS-1).NE.0)THEN
                        
C                        SI LE NOEUD N'A PAS ETE RENSEIGNE, ON LE CREE
                         IF(ZI(JINDNS+ICNS-1).EQ.0)THEN
                           CALL CODENT(ZI(JCO+ICNS-1),'G',CH)
                           NOMNI='NI'//CH(1:LEN(CH))
                           CALL JEEXIN(JEXNOM(TMP//'.NOMNOE',NOMNI),IRE)
                           IF(IRE.EQ.0)THEN
                              NBNONO=NBNONO+1
                              CALL JECROC(JEXNOM(TMP//'.NOMNOE',NOMNI))
                              CALL JENONU(JEXNOM(TMP//'.NOMNOE',NOMNI),
     &                             NUMNI)
                              DO 153 JC=1,3
                                 ZR(JCORF+3*(NUMNI-1)+JC-1)=
     &                             ZR(JCOR+3*(ZI(JCO+ICNS-1)-1)+JC-1)
 153                          CONTINUE
                           ELSE
                              CALL JENONU(JEXNOM(TMP//'.NOMNOE',NOMNI),
     &                             NUMNI)
                           ENDIF
                           ZI(JINDNS+ICNS-1)= NUMNI
                           ZI(JCOF+KTET-1)=NUMNI
C                        SI LE NOEUD A DEJA ETE RENSEIGNE
                         ELSE
                            ZI(JCOF+KTET-1)=ZI(JINDNS+ICNS-1)
                         ENDIF
C
C                       SI C'EST UN NOEUD CLASSIQUE
C                       ---------------------------
                        ELSE
                           ZI(JCOF+KTET-1)=ZI(JCO+ICNS-1)
                        ENDIF
C
C                    III-2.1.2 SI LE TETRA EST AU-DESSUS DE LA FISSURE

                     ELSE
                        ZI(JCOF+KTET-1)=ZI(JCO+ICNS-1)
                     ENDIF
C
C                 III-2.2 SI LE NOEUD SOMMET EST NOEUD CLASSIQUE D'UNE
C                 MAILLE XFEM SANS NOEUD D'INTERSECTION
C                 -------------------------------------
                  ELSE
                     ZI(JCOF+KTET-1)=ZI(JCO+ICNS-1)
                  ENDIF
               ENDIF
 150        CONTINUE
 110     CONTINUE
C
C        ACTUALISATION DES CONNECTIVITES POUR LES MAILLES POSSEDANT
C        DES NOEUDS D'INTERSECTION QUI SONT NOEUDS SOMMETS
         IF(LMNIS)THEN

             NBNIS=0
             CALL WKVECT('&&OP0187.NIS','V V I',NBNOSO,JNIS)
             DO 399 J=1,NBNOSO
                IF(ZI(JNNEW+J-1).EQ.1)THEN
                   NBNIS=NBNIS+1
                   ZI(JNIS+NBNIS-1)=ZI(JCO+J-1)
                ENDIF
 399         CONTINUE

             DO 400 INIS=1,NBNIS
                NIS=ZI(JNIS+INIS-1)
                XNIS=ZR(JCOR+3*(NIS-1))
                YNIS=ZR(JCOR+3*(NIS-1)+1)
                ZNIS=ZR(JCOR+3*(NIS-1)+2)
                DO 401 J=1,NBSEM
                   CALL JEVEUO(JEXNUM(CONNEX,ZI(JMX+2*J-2)),'L',JCOFL)
                   DO 402 JJ=1,NTET
                      NXIS=ZI(JCOFL+JJ-1)
                      XNXIS=ZR(JCORF+3*(NXIS-1))
                      YNXIS=ZR(JCORF+3*(NXIS-1)+1)
                      ZNXIS=ZR(JCORF+3*(NXIS-1)+2)
                      IF(ABS(XNIS-XNXIS).LT.EPS)THEN
                         IF(ABS(YNIS-YNXIS).LT.EPS)THEN
                            IF(ABS(ZNIS-ZNXIS).LT.EPS)THEN
                               CALL JENUNO(JEXNUM(TMP//'.NOMNOE',NXIS),
     &                                     CH)
                               IF(CH(2:2).NE.'X')GOTO 401
                               NXISM='NXM'//CH(4:LEN(CH))
                               NXISP='NXP'//CH(4:LEN(CH))
                               CALL JENONU(JEXNOM(TMP//'.NOMNOE',NXISM),
     &                              NUISM)
                               CALL JENONU(JEXNOM(TMP//'.NOMNOE',NXISP),
     &                              NUISP)
                               GOTO 403
                            ENDIF
                         ENDIF
                      ENDIF
 402               CONTINUE
 401            CONTINUE
                GOTO 400
 403            CONTINUE
                CALL JENUNO(JEXNUM(MA//'.NOMNOE',NIS),NOXIS)
                CALL JENONU(JEXNOM(TMP//'.NOMNOE',NOXIS),NIS)
                DO 404 J=1,NBSEM
                   CALL JEVEUO(JEXNUM(CONNEX,ZI(JMX+2*J-2)),'E',JCOFE)
                   DO 405 JJ=1,NTET
                      IF(NIS.EQ.ZI(JCOFE+JJ-1))THEN
                          IF(ZI(JMX+2*J-1).EQ.-1)THEN
                             CALL JENUNO(JEXNUM(MAF//'.NOMMAI',
     &                          ZI(JMX+2*J-2)),K8B)
                             ZI(JCOFE+JJ-1)=NUISM
                          ELSE
                            CALL JENUNO(JEXNUM(MAF//'.NOMMAI',
     &                          ZI(JMX+2*J-2)),K8B)
                             ZI(JCOFE+JJ-1)=NUISP
                          ENDIF
                       ENDIF
 405                CONTINUE
 404             CONTINUE
 400          CONTINUE
            CALL JEDETR('&&OP0187.NIS')
           ENDIF

         IF(NBNIM.GT.0)THEN
            CALL JEDETR('&&OP0187.NOEUD_INTER_P')
            CALL JEDETR('&&OP0187.NOEUD_INTER_M')
            CALL JEDETR('&&OP0187.CARA_TET')
            CALL JEDETR('&&OP0187.NUM_NOEU')
            CALL JEDETR('&&OP0187.NEW_NO')
         ENDIF
         IF(ZI(JNDNIS+I-1).NE.0)THEN
            CALL JEDETR('&&OP0187.NOIS')
         ENDIF
         IF(LMNIS)THEN
            CALL JEDETR('&&OP0187.NX')
            CALL JEDETR('&&OP0187.MX')
         ENDIF
 100  CONTINUE
      CALL JEDETR('&&OP0187.CNSM') 
C
C    --- CREATION DE L' OBJETS: '.NOMNOE'  
      CALL JECREO(MAF//'.NOMNOE','G N K8')
      CALL JEVEUO(MA//'.DIME','L',JDIMI)
      NBNONO=NBNONO+ZI(JDIMI)-NBNH20
      CALL JEECRA(MAF//'.NOMNOE','NOMMAX',NBNONO,KBID)
      DO 777 I=1,NBNONO
        CALL JENUNO(JEXNUM(TMP//'.NOMNOE',I),NOMNO)
        CALL JECROC(JEXNOM(MAF//'.NOMNOE',NOMNO))
 777   CONTINUE
       CALL JEVEUO(MAF//'.DIME','E',JDIM)
       ZI(JDIM)=NBNONO
       CALL JUVECA(COORDO//'.VALE',3*NBNONO)
C
C
C     ================================================================
C      4. MISE A JOUR DES GROUPES DE MAILLES LORSQU'ILS SONT PRESENTS
C     ================================================================
C
      CALL JEEXIN(MA//'.GROUPEMA',IRET)
      IF(IRET.NE.0)THEN
         CALL JELIRA(MA//'.GROUPEMA','NMAXOC',NBTGMA,KBID)
         GRPMA=MAF//'.GROUPEMA'
         CALL JECREC(GRPMA,'G V I','NO','DISPERSE','VARIABLE',NBTGMA)
         DO 160 I=1,NBTGMA
            CALL JENUNO(JEXNUM(MA//'.GROUPEMA',I),NOMGMA)
C          NOMBRE DE MAILLES DU GROUPE : NBMAGP
            CALL JELIRA(JEXNOM(MA//'.GROUPEMA',NOMGMA),'LONMAX',
     &           NBMAGP,KBID)
            CALL JEVEUO(JEXNOM(MA//'.GROUPEMA',NOMGMA),'L',JGPMA)
C           CREATION D'UN TABLEAU TEMPORAIRE POUR RECUPERER LES NUMEROS
C           DES MAILLES DU GROUPES (MAILLES NON-FISS + SOUS-ELEMENTS)
            CALL WKVECT('&&OP0187.MAIL_GROUP','V V I',NBMAGP*NBSE,JMAGP)
C           ON PARCOURT LES MAILLES DU GROUPES
            K=0
            DO 170 J=1,NBMAGP
C              SI LA MAILLE EST NON FISSUREE
               IF(ZI(JIND+ZI(JGPMA+J-1)-1).EQ.0)THEN
                  K=K+1
                  CALL JENUNO(JEXNUM(MA//'.NOMMAI',ZI(JGPMA+J-1)),NOMA)
                  CALL JENONU(JEXNOM(MAF//'.NOMMAI',NOMA),INUMA)
                  ZI(JMAGP+K-1)=INUMA
C              SINON
               ELSE
                  IIND=ZI(JIND+ZI(JGPMA+J-1)-1)
                  DO 180 JSE=1,ZI(JNBSEM+IIND-1)
                    K=K+1
                    ZI(JMAGP+K-1)=ZI(JNBSE+ZI(JJSE+IIND-1)+JSE-1)
 180             CONTINUE
              ENDIF
 170       CONTINUE
C          NOMBRE DE MAILLES DANS LE GROUPE (NON FISS + SOUS-ELEM)
           NBMAGT=K
C          INSERTION DU NOM DE GROUPE DANS LA COLLECTION
           CALL JECROC(JEXNOM(GRPMA,NOMGMA))
C          ON DIMENSIONNE LE GROUPE DE MAILLES
           CALL JEECRA(JEXNOM(GRPMA,NOMGMA),'LONMAX',NBMAGT,KBID)
C          ON REMPLIT L'OBJET DE COLLECTION
           CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'E',JGM)
           DO 190 J=1,NBMAGT
              ZI(JGM+J-1)=ZI(JMAGP+J-1)
 190       CONTINUE
           CALL JEDETR('&&OP0187.MAIL_GROUP')
 160    CONTINUE
      ENDIF

      CALL TITRE()
      CALL JEDEMA()

      END
