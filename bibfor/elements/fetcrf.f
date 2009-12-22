      SUBROUTINE FETCRF(SDFET1)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_4
C RESPONSABLE ASSIRE A.ASSIRE
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CREATION DE LA STRUCTURE DE DONNEES FETI.
C      ELLE EST CONTENUE DANS UN CONCEPT PRODUIT DE TYPE SD_FETI
C
C IN SDFETI   : NOM DU CONCEPT PRODUIT
C OUT SDFETI  : LE CONCEPT EST CREE ET INSTANCIE
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       MESSAGE:INFNIV.
C       JEVEUX:JEMARQ,JEDEMA,JECROC,JEECRA,JEVEUO,WKVECT.
C
C     FONCTIONS INTRINSEQUES:
C       NONE.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       02/11/03 (OB): CREATION.
C       NIV>2  JXVERI
C       NIV>3  UTIMSD + AFFICHAGES GLOBAUX
C       NIV>4  AFFICHAGES DETAILLES
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*8 SDFET1

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION VARIABLES LOCALES
      INTEGER      NBNO,NGMA,NBRD,NBMABD,LSTGMA,LSTBRD,NOMCHA,NOMSD,
     &             JADR,I,J,NBSD,INTBUF,IFM,NIV,
     &             NBCHAR,MULTC,NBID,ITMP,
     &             I1,NEC30,NBNOT2,J2,N31,
     &             IRET,IER,NBNOTO,IERD,IALINO,JTRAV,IANBNO,
     &             NBMAIL,IALIMA,JJ,NBNOSD,NB,IANBMA,IALIBD,
     &             IS9,INCRS,L,XT,YT,ZT,IALSNO,IALSMU,K,NN,NBMA,
     &             LINOMA,IALSMA,JNOMA,JPRNM,NEC,N,INO,
     &             IALSK,NUMSD,IALSPO,IPOS,IAJADR,JTMP,IALSTR,
     &             JADRI,JADRJ,IALSFG,NBMATO,NBVM, NBFETE,IALSML,IALSMD,
     &             NBMAMA,ILIDDL,IAUX1,IAUX2,IAUX3,IAUX0,
     &             ND,IADR,N1,N2,N3,NBOBJ1,LONLI1,IDLIG1,NVAL,
     &             NBMATA,NDTAR,IAUX,
     &             ISDLCH,ISDMAT,INBMCH,IFLIM,IFLII,DEC,
     &             ICH,ISD,ILSCHA,IDFLII,IDFLIM,IDFLM,IDFLN,IFNT,
     &             NB1,NB2,BIFLII,BIFLIM,BIFLN,JADRH,NBNOTA,NBMATR,
     &             NOINCH,ILIAIS,INBNO,ICOMPT,LL,NBDDLI,JMETH,
     &             VALI(5),IAFETA,NBMABO,IAL,IALS,JAL,ITMA,NBER,
     &             LIL,LILS,IAO,IAOS,ADDR,K1,KTMP,
     &             NZOCO,IZONE,IFCFL,JSUMA,JSUNO
      INTEGER      CFDISI,CFMMVD,IFORM,ZDIME
      INTEGER      ISURF,JDECMA,JDECNO,JMACO,IDD,CFSD,IFCFM,NUMMA,
     &             IFCFN,IFCFB,NZOCOM,KADR,LADR,IMA,JZONE,ISUCO,JNOCO,
     &             IFETB,LFETB,DDLM,MADR,IIAUX1,JDIM,IFCNM,NBSURF,INDDZ
      CHARACTER*4  K4TMP
      CHARACTER*8  K8BID,K8BUFF,NOM,MA,K8B,NOMGMA,NOMO,NOMN1,
     &             NOMN,NOMA
      CHARACTER*19 SDFETI,LIGRMO,LIGRCH
      CHARACTER*24 NOMSDA,NOMSDB,NOMSDI,NOMSDG,NOMSDM,NOMSDH,NOMSDJ,
     &             NOMSLN,NOMSLI,NOMSLM,NOMNOE,
     &             NOMREF,GRPMA,VALK(3),METHCO,PZONE,
     &             PSURMA,PSURNO,CONTMA,NOMFCL,NOMFCM,NOMFCN,CONTNO,
     &             K24BID,NOMFCI,NDIMCO
      CHARACTER*32 JEXNOM,JEXNUM
      LOGICAL      EXISDG,LBORD,LCFC1,LPAIRE


C CORPS DU PROGRAMME
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
      IF (NIV.GE.3) CALL JXVERI('MESSAGE','DEBUT FETCRF')

C**********************************************************************
C INITIALISATIONS
C**********************************************************************
      SDFETI=SDFET1
      NOMREF=SDFETI//'.FREF'
      NOMSDM=SDFETI//'.FDIM'
      NOMSDA=SDFETI//'.FETA'
      NOMSDB=SDFETI//'.FETB'
      NOMSDG=SDFETI//'.FETG'
      NOMSDH=SDFETI//'.FETH'
      NOMSDI=SDFETI//'.FETI'
      NOMSDJ=SDFETI//'.FETJ'
      NOMSLN=SDFETI//'.FLIN'
      NOMSLI=SDFETI//'.FLII'
      NOMSLM=SDFETI//'.FLIM'
      NOMFCL=SDFETI//'.FCFL'
      NOMFCI=SDFETI//'.FCFI'
      NOMFCM=SDFETI//'.FCFM'
      NOMFCN=SDFETI//'.FCFN'
C     RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
      CALL GETVID(' ','MODELE',1,1,1,NOMO,NBVM)

C     LIGREL DU MODELE
      LIGRMO = NOMO(1:8)//'.MODELE'
C     NBCHAR: NBRE DE CHARGE
      CALL GETFAC('EXCIT',NBCHAR)
C     VECTEURS TEMPORAIRES DES CHARGES
      IF (NBCHAR.GT.0) THEN
        CALL WKVECT('&&FETCRF.NOMCHA   ','V V K8',NBCHAR,NOMCHA)
        DO 800 I = 1,NBCHAR
          CALL GETVID('EXCIT','CHARGE',I,1,1,K8B,ND)
          ZK8(NOMCHA-1+I)=K8B
  800   CONTINUE
      ENDIF
C     NBSD: NBRE D'OCCURENCE DU MOT-CLE DEFI
      CALL GETFAC('DEFI',NBSD)
      NBSD = ABS(NBSD)
C     VECTEUR DES NBRE DE NOEUDS
      CALL WKVECT('&&FETCRF.NBNO     ','V V I',NBSD,NBNO)
C     FLAGS DE L'EXISTENCE DES GROUP_MA VOLUMIQUES
      CALL WKVECT('&&FETCRF.NGMA     ','V V I',NBSD,NGMA)
C     IDEM GROUP_MA DE BORDS
      CALL WKVECT('&&FETCRF.NBRD     ','V V I',NBSD,NBRD)
C     VECTEUR DES NBRE DE MAILLES DE BORD
      CALL WKVECT('&&FETCRF.NBMABD   ','V V I',NBSD,NBMABD)
C     LISTES DES GROUP_MA VOLUMIQUES
      CALL WKVECT('&&FETCRF.LSTGMA   ','V V K8',NBSD,LSTGMA)
C     IDEM DE BORDS
      CALL WKVECT('&&FETCRF.LSTBRD   ','V V K8',NBSD,LSTBRD)
      CALL WKVECT('&&FETCRF.NOMSD    ','V V K8',NBSD,NOMSD)

C     CREATION .FREF
      INTBUF=NBCHAR+1
      CALL WKVECT(NOMREF,'G V K8',INTBUF,JADR)
      ZK8(JADR)=NOMO
      DO 50 I=1,NBCHAR
        ZK8(JADR+I)=ZK8(NOMCHA-1+I)
   50 CONTINUE
      IF (NIV.GE.4)
     &  CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMREF,1,'G')
C     MA: MAILLAGE ASSOCIE AU MODELE
      CALL GETVID(' ','MAILLAGE',1,1,1,NOMA,NBVM)
      IF (NBVM.EQ.0) THEN
        CALL JEVEUO(LIGRMO//'.LGRF','L',JNOMA)
        MA = ZK8(JNOMA)
      ELSE
        MA=NOMA
      ENDIF
C     DETERMINATION DU NB TOTAL DE NOEUDS ET DE MAILLES DU MAILLAGE
      CALL DISMOI('F','NB_NO_MAILLA',MA,'MAILLAGE',NBNOTO,K8B,IERD)
      CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMATO,K8B,IERD)
      NBMATR=NBMATO
      GRPMA = MA//'.GROUPEMA       '
      CALL JEVEUO(LIGRMO//'.PRNM','L',JPRNM)
      CALL JELIRA(LIGRMO//'.PRNM','LONMAX',N,K8B)
      NEC = N/NBNOTO
      NEC30=30*NEC

C     LECTURE DU CONTENU DES MOT-CLES DEFI
      DO 1 I = 1, NBSD
        CALL GETVTX('DEFI','GROUP_MA',I,1,1,ZK8(LSTGMA-1+I),
     &                                         ZI(NGMA-1+I))
        IF (ZI(NGMA-1+I).NE.1) THEN
          VALI(1)=I
          CALL U2MESI('F','ELEMENTS5_25',1,VALI)
        ENDIF
        CALL GETVTX('DEFI','GROUP_MA_BORD',I,1,1,ZK8(LSTBRD-1+I),
     &                                              ZI(NBRD-1+I))
  1   CONTINUE

C     NOMS DES SOUS-DOMAINES : NOMSD
C     ON PREND LE MOT-CLE NOM TRONQUE A 4  ET ON AJOUTE LE NUM
      CALL GETVTX(' ','NOM',1,1,1,NOM,NBID)
      K4TMP=NOM(1:4)
      ITMP=0
      IAUX=LEN(K4TMP)
      DO 3 I = 1,IAUX
        IF ( K4TMP(I:I) .NE. ' ' ) ITMP=ITMP+1
  3   CONTINUE
      DO 5 I = 1, NBSD
        WRITE(K8BID,'(I4)') I
        CALL LXCADR(K8BID)
        ZK8(NOMSD-1+I) = K4TMP(1:ITMP)//K8BID
  5   CONTINUE

C TEST POUR VERFIFIER LA COHERENCE DES PARAMETRES DE LA MACRO ET DES
C GROUP_MA CREES EFFECTIVEMENT DANS LE MAILLAGE
      IER = 0
      DO 21 I = 1,NBSD
        NOMGMA=ZK8(LSTGMA-1+I)
        CALL JEEXIN(JEXNOM(GRPMA,NOMGMA),IRET)
        IF (IRET.EQ.0) THEN
          IER = IER + 1
          CALL U2MESK('E','ELEMENTS_62',1,NOMGMA)
        ENDIF
   21 CONTINUE
      CALL ASSERT (IER.EQ.0)

C     CREATION SDFETI / .FETB
      CALL JECREC(NOMSDB,'G V I','NO','DISPERSE','VARIABLE',NBSD)
C     VECTEUR DES NBRE DE NOEUDS
      CALL WKVECT('&&FETCRF.NB_NO    ','V V I',NBSD,IANBNO)
C     VECTEUR DES NBRE DE MAILLES
      CALL WKVECT('&&FETCRF.NB_MA    ','V V I',NBSD,IANBMA)
C     STOCKAGE DES ADRESSES DES .FETB DE CHAQUE SD
      CALL WKVECT('&&FETCRF.L_NO_JADR','V V I',NBSD,IAJADR)
C     STOCKAGE DES ADRESSES DES .FETA DE CHAQUE SD
      CALL WKVECT('&&FETCRF.ADR_FETA','V V I',NBSD,IAFETA)
C     VECTEUR DES DDL CUMULES DE CHAQUE SD
      CALL WKVECT('&&FETCRF.L_K      ','V V I',NBSD,IALSK)
      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES INITIALISATIONS')

C***********************************************************************
C BOUCLE 1 SUR LES SOUS-DOMAINES POUR DETERMINER:
C PAR SD: NBRE DE MAILLES (NBMAIL), ADDRESSE GROUP_MA (IALIMA),
C         NB DE NOEUDS (IANBNO)
C NBRE TOTAL DE MAILLES VOLUMIQUES (NBMATO)/NOEUDS(NB) AVEC MULTIPLICITE
C***********************************************************************

      NBMATO=0
      NB=0
      NBNOT2=2*NBNOTO
      CALL WKVECT('&&FETCRF.TRAV ','V V I',NBNOT2,JTRAV)
      CALL WKVECT('&&FETCRF.TMP  ','V V I',NBNOT2,JTMP)
      DO 22 I = 1,NBSD
        NOMGMA=ZK8(LSTGMA-1+I)
        CALL JELIRA(JEXNOM(GRPMA,NOMGMA),'LONMAX',NBMAIL,K8B)
        CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'L',IALIMA)
        ZI(IAFETA+I-1)=IALIMA
        ZI(IANBMA-1+I)=NBMAIL
        NBMATO=NBMATO+NBMAIL

C       GMGNRE : donne la liste des noeuds ZI(JTMP) d'une liste de
C                mailles ZI(IALIMA)
C                sortie : liste des noeuds = ZI(JTMP)
C                         nombre de noeuds = ZI(IANBNO-1+I)
        CALL GMGNRE(MA,NBNOTO,ZI(JTRAV),ZI(IALIMA),NBMAIL,ZI(JTMP),
     &              NBNOSD,'TOUS')
        ZI(IANBNO-1+I)=NBNOSD
        ZI(NBNO-1+I)=NBNOSD
        NB=NB+NBNOSD
  22  CONTINUE

C ****** ON PEUT MAINTENANT DIMENSIONNER EXACTEMENT:
C     LISTE DES NOEUDS AVEC MULTIPLICITES
      CALL WKVECT('&&FETCRF.LISTE_NO ','V V I',NB,IALINO)
C     NUMERO DE LA SD POUR UN NOEUD DONNE
      CALL WKVECT('&&FETCRF.L_NO_GMA ','V V I',NB,LINOMA)
C     INDICE DANS CETTE SD POUR UN NOEUD DONNE
      CALL WKVECT('&&FETCRF.L_NO_POS ','V V I',NB,IALSPO)
      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES BOUCLE 1')

C**********************************************************************
C BOUCLE 2 SUR LES SOUS-DOMAINES POUR DETERMINER:
C PAR SD: NBRE DE MAILLES (NBMAIL), DE NOEUDS (NBNOSD), DE DDLS (K)
C REMPLIR .FETB
C**********************************************************************
      NB=0
      DO 222 I=1,NBSD
C       ON REFAIT UN COUP POUR CETTE FOIS REMPLIR IALINO
        IALIMA= ZI(IAFETA+I-1)
        NBMAIL=ZI(IANBMA-1+I)
        CALL JERAZO('&&FETCRF.TMP  ',NBNOT2,1)
        CALL GMGNRE(MA,NBNOTO,ZI(JTRAV),ZI(IALIMA),NBMAIL,ZI(JTMP),
     &              NBNOSD,'TOUS')
C       CREATION DE SDFETI.FETB
        K8BUFF=ZK8(NOMSD-1+I)
        CALL JECROC(JEXNOM(NOMSDB,K8BUFF))
        INTBUF=2*NBNOSD
        CALL JEECRA(JEXNOM(NOMSDB,K8BUFF),'LONMAX',INTBUF,K8BID)
        CALL JEVEUO(JEXNOM(NOMSDB,K8BUFF),'E',JADR)
        ZI(IAJADR-1+I)=JADR
C **** SOUS-BOUCLE 2.1 SUR LES NOEUDS POUR DETERMINER LE NBRE DE DDLS
C      ET REMPLIR .FETB (SANS LES SIGNES DES INTERFACES)
        K=0
        DO 210 J=1,NBNOSD
          INO = ZI(JTMP-1+J)
          ZI(JADR+2*(J-1))=INO
          DO 7 L=1,NEC30
            IF (EXISDG(ZI(JPRNM-1+NEC*(INO-1)+1),L)) K=K+1
  7       CONTINUE
          ZI(JADR+2*(J-1)+1)=K
  210   CONTINUE
C       NB DDL TOTAL DU SOUS-DOMAINE I
        ZI(IALSK-1+I)=K
C **** SOUS-BOUCLE 2.2 SUR LES NOEUDS POUR REMPLIR
C       VECTEUR DES NOEUDS :  ZI(IALINO-1+...NB)
C       VECTEUR DE CORRESPONDANCE NOEUD -> SD: ZI(LINOMA-1+...NB)
C       VECTEUR DE CORRESPONDANCE NOEUD -> POSITION DANS SD: ZI(IALSPO)
        IPOS=1
        DO 25 J = 1,NBNOSD
          ZI(IALINO+NB-1+J)= ZI(JTMP-1+J)
          ZI(LINOMA+NB-1+J)= I
          ZI(IALSPO+NB-1+J)= IPOS
          IPOS=IPOS+1
   25   CONTINUE
        NB=NB+NBNOSD
  222 CONTINUE
      CALL JEDETR('&&FETCRF.TMP')
      CALL JEDETR('&&FETCRF.TRAV')
      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES BOUCLE 2')

C***********************************************************************
C BOUCLE 3 SUR LES SOUS-DOMAINES POUR REMPLIR .FETA
C          RAJOUT DES MAILLES SURFACIQUES A NBMATO
C***********************************************************************
C     PREPARATION DU TEST SUR LES MAILLES
      CALL WKVECT('&&FETCRF.TESTMA ','V V I',NBMATR,ITMA)
      CALL JECREC(NOMSDA,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      DO 100 I=1,NBSD
        K8BUFF=ZK8(NOMSD-1+I)
        CALL JECROC(JEXNOM(NOMSDA,K8BUFF))

C       NB MAILLES/ADDRESSE DE LEURS NUMEROS
        NBMAIL=ZI(IANBMA-1+I)
        IALIMA=ZI(IAFETA+I-1)

C       NB MAILLES DE BORDS SI BESOIN...
        ZI(NBMABD-1+I)=0
        IF (ZI(NBRD-1+I).EQ.1) THEN
          NOMGMA=ZK8(LSTBRD-1+I)
          CALL JELIRA(JEXNOM(GRPMA,NOMGMA),'LONMAX',NBMABO,K8B)
          CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'L',IALIBD)
          ZI(NBMABD-1+I)=NBMABO
          LBORD=.TRUE.
        ELSE
          NBMABO=0
          LBORD=.FALSE.
        ENDIF
C       ON RAJOUTE LES MAILLES DE BORDS AU TOTAL DES MAILLES DES SD
        NBMATO=NBMATO+NBMABO
C       NB MAILLES TOTALES (BORDS+VOLUME) DU SD I
        NBID=NBMAIL+NBMABO
C       REMPLISSAGE EFFECTIF DE FETA
        CALL JEECRA(JEXNOM(NOMSDA,K8BUFF),'LONMAX',NBID,K8BID)
        CALL JEVEUO(JEXNOM(NOMSDA,K8BUFF),'E',JADR)
C       MAILLES VOLUMIQUES
        DO 90 J=1,NBMAIL
          IAL=ZI(IALIMA+J-1)
          ZI(JADR+J-1)=IAL
C         POUR TEST SUR LES MAILLES
          ZI(ITMA+IAL-1)=1
   90   CONTINUE
C       MAILLES DE BORD
        IF (LBORD) THEN
          DO 91 J=1,NBMABO
            IAL=ZI(IALIBD-1+J)
            ZI(JADR+J-1+NBMAIL)=IAL
            ZI(ITMA+IAL-1)=1
   91     CONTINUE
        ENDIF
  100 CONTINUE

C **** TEST NBRE DE MAILLES MODELE = SOMME DES MAILLES DES GROUP_MA
      CALL JELIRA(NOMO(1:8)//'.MAILLE','LONMAX',NBMA,K8B)
      CALL JEVEUO(NOMO(1:8)//'.MAILLE','L',IAL)
      NBMAMA=0
      NBER=0
      DO 93 I=1,NBMA
C       MAILLE DU MODELE
        IF (ZI(IAL+I-1).NE.0) THEN
          NBMAMA=NBMAMA+1
C         MAILLE DANS UN SD OU PAS ?
          IF (ZI(ITMA-1+I).EQ.0) NBER=NBER+1
        ENDIF
   93 CONTINUE
      IF (NBMAMA.NE.NBMATO) THEN
        VALI(1)=NBMAMA
        VALI(2)=NBMATO
        CALL U2MESI('F','ELEMENTS5_24',2,VALI)
      ENDIF
      IF (NBER.GT.0) CALL U2MESI('F','ELEMENTS5_28',1,NBER)
      NBER=0
      DO 94 I=1,NBMATO
C       MAILLE DANS UN SD
        IF (ZI(ITMA-1+I).NE.0) THEN
C         MAILLE DU MODELE ?
          IF (ZI(IAL+I-1).EQ.0) NBER=NBER+1
        ENDIF
   94 CONTINUE
      IF (NBER.GT.0) CALL U2MESI('F','ELEMENTS5_29',1,NBER)
      CALL JEDETR('&&FETCRF.TESTMA ')
      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES BOUCLE 3')
      IF (NIV.GE.4)
     &  CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSDA,1,'G')

C**********************************************************************
C FIN .FETA ET .FETB (SAUF SIGNE) ET AFFICHAGE
C**********************************************************************
      IF (NIV.GE.4) THEN
        WRITE(IFM,*) ' '
        WRITE(IFM,*) '------------------------------------'
        WRITE(IFM,*) '|           FETA ET FETB            |'
        WRITE(IFM,*) '|  PAR SD :   - NB DDL              |'
        WRITE(IFM,*) '|             - NB MAILLES          |'
        WRITE(IFM,*) '|             - NB NOEUDS           |'
        WRITE(IFM,*) ' '
        DO 941 I = 1,NBSD
          WRITE(IFM,*) 'SD=',I
          WRITE(IFM,*) 'DDL TOTAL   =',ZI(IALSK-1+I)
          WRITE(IFM,*) 'MA VOL/BORD =',ZI(IANBMA-1+I),ZI(NBMABD-1+I)
          WRITE(IFM,*) 'NO TOTAL    =',ZI(IANBNO-1+I)
  941   CONTINUE
      ENDIF

      IF (NIV.GE.5) THEN
        WRITE(IFM,*) ' '
        WRITE(IFM,*) '------------------------------------'
        WRITE(IFM,*) '|    TOUS LES NOEUDS ET LEUR SD     |'
        WRITE(IFM,*) ' '
        DO 24 J = 1,NB
          WRITE(IFM,*) 'NOEUD=',ZI(IALINO-1+J), 'SD=',ZI(LINOMA-1+J)
   24   CONTINUE
        WRITE(IFM,*)  'TOTAL NOEUDS (AVEC MULTIPLICITE)=',NB
        WRITE(IFM,*) ' '
        WRITE(IFM,*) '------------------------------------'
      ENDIF

C**********************************************************************
C ON REORDONNE LA LISTE DE TOUS LES NOEUDS DES SD (ET LEUR SD)
C   (INSPIRE DU TRI A BULLE UTTRIF.F)
C   PARTIE A OPTIMISER (VOIR TRI PAR INSERTION)
C**********************************************************************

C        --- TRI BULLE ---
      IF ( NB .GT. 1 ) THEN
C         --- CHOIX DE L'INCREMENT ---
        INCRS = 1
        IS9   = NB / 9
 11     CONTINUE
        IF (INCRS .LT. IS9) THEN
          INCRS = 3*INCRS+1
          GOTO 11
        ENDIF
C         --- REMONTEE DES BULLES ---
12      CONTINUE
        DO 15 J=INCRS+1,NB
          L = J-INCRS
13        CONTINUE
          IF ( L.GT.0) THEN
            IAL=IALINO-1+L
            IALS=IAL+INCRS
            LIL=LINOMA-1+L
            LILS=LIL+INCRS
            IAO=IALSPO-1+L
            IAOS=IAO+INCRS
            IF ( ZI(IAL) .GT. ZI(IALS) ) THEN
C            --- PERMUTATION DES VALEURS ---
              XT        = ZI(IAL)
              ZI(IAL)   = ZI(IALS)
              ZI(IALS)  = XT
              YT        = ZI(LIL)
              ZI(LIL)   = ZI(LILS)
              ZI(LILS)  = YT
              ZT        = ZI(IAO)
              ZI(IAO)   = ZI(IAOS)
              ZI(IAOS)  = ZT
              L = L - INCRS
              GOTO 13
            ENDIF
          ENDIF
15      CONTINUE
        INCRS = INCRS/3
        IF (INCRS.GE.1) GOTO 12
      ENDIF

C***********************************************************************
C UN NOEUD D'INTERFACE EST DEFINI COMME ETANT COMMUN A PLUSIEURS SD
C ----------------------------------------------------------------------
C CONSTRUCTION DES LISTES DES NOEUDS D'INTERFACE, DE LEUR MULTIPLICITE
C   (IE LES NOEUDS PRESENTS AU MOINS DEUX FOIS DANS LA LISTE PRECEDENTE)
C   ET DE LA LISTE DES SD AUXQUELS ILS APPARTIENNENT :
C    ZI(IALSNO-1+K)  , K=1,NB  : LISTE DES NOEUDS D'INTERFACE
C    ZI(IALSMU-1+K)  , K=1,NB  : LISTE DE LEUR MULTIPLICITE
C    ZI(IALSMA-1+KK) , KK=1,(SOMME DES PRECEDENTS) : LISTE DES SD
C***********************************************************************

C     VECTEUR DES NOEUDS D'INTERFACE
      CALL WKVECT('&&FETCRF.LST_ORDO ','V V I',NB,IALSNO)
C     LEUR MULTIPLICITE
      CALL WKVECT('&&FETCRF.LST_MULT ','V V I',NB,IALSMU)
C     LES COUPLES (SD1,SD2)
      CALL WKVECT('&&FETCRF.LST_GMA  ','V V I',2*NB,IALSMA)
C     VECTEURS DES TRIPLETS (NOEUDS INTERFACE,SD1,SD2)
      CALL WKVECT('&&FETCRF.LST_TRI  ','V V I',3*NB+3,IALSTR)

      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES TRI A BULLE')

C***********************************************************************
C DOUBLE BOUCLE 4 SUR LES NOEUDS AVEC LEUR MULTIPLICITE POUR FINIR DE
C REMPLIR FETB (SIGNE) + IALSTR
C***********************************************************************
C     K1:  NBRE DE NOEUDS D'INTERFACE AVEC MULTIPLICITE
C     NB: NBRE DE NOEUDS TOTAL EN COMPTANT LES MULTIPLICITES
      K=1
      DO 28 I = 1,NB
        IAL=ZI(IALINO-1+I)
        I1=I+1
        DO 29 J = I1,NB
          JAL=ZI(IALINO-1+J)
C         SI ON A TROUVE UN NOEUD D'INTERFACE (CAD COMMUN A 2 SD)
          IF (IAL.EQ.JAL) THEN
C           ON COMPLETE LE TABLEAU DES NOEUDS D'INTERFACE ENTRE 2 SD
C           ON PREND ITMP < JTMP
            ITMP=ZI(LINOMA-1+I)
            JTMP=ZI(LINOMA-1+J)
            IF (JTMP.LT.ITMP) THEN
              ITMP=ZI(LINOMA-1+J)
              JTMP=ZI(LINOMA-1+I)
            ENDIF

C           ON MODIFIE LE SIGNE DU NOEUD DANS .FETB POUR LES 2 SD
            JADR=ZI(IAJADR-1+ZI(LINOMA-1+I))
            ADDR=JADR+2*(ZI(IALSPO-1+I)-1)
            ZI(ADDR)=-1*ABS(ZI(ADDR))
            JADR=ZI(IAJADR-1+ZI(LINOMA-1+J))
            ADDR=JADR+2*(ZI(IALSPO-1+J)-1)
            ZI(ADDR)=-1*ABS(ZI(ADDR))

C           ON REMPLI LA LISTE DES TRIPLETS (NOEUD, SD1, SD2)
            ZI(IALSTR+3*(K-1))  =IAL
            ZI(IALSTR+3*(K-1)+1)=ITMP
            ZI(IALSTR+3*(K-1)+2)=JTMP
C           ON INCREMENTE LE NOMBRE DE NOEUDS TROUVES
            K=K+1
          ENDIF

   29   CONTINUE
   28 CONTINUE
      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES BOUCLE 4')
      IF (NIV.GE.4)
     &  CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSDB,1,'G')

C***********************************************************************
C DOUBLE BOUCLE 5 SUR LES NOEUDS D'INTERFACE AVEC LEUR MULTIPLICITE
C GEOMETRIQUE
C***********************************************************************
      K1=K-1
C     MULTIPLICITE (GEOMETRIQUE) DES NOEUDS D'INTERFACE
      CALL WKVECT('&&FETCRF.LST_MLT  ','V V I',K1,IALSML)
C     VECTEUR AUXILIAIRE POUR LA BOUCLE CI DESSOUS
      CALL WKVECT('&&FETCRF.LST_MSD  ','V V I',NBSD,IALSMD)
      DO 34 L = 1,K1
        IAL=ZI(IALSTR+3*(L-1))
        CALL JERAZO('&&FETCRF.LST_MSD  ',NBSD,1)
        DO 30 I = 1,K1
          JAL=IALSTR+3*(I-1)
          IF ( ZI(JAL).EQ.IAL ) THEN
            JTMP=ZI(JAL+1)
            ZI(IALSMD+JTMP-1)=1
            JTMP=ZI(JAL+2)
            ZI(IALSMD+JTMP-1)=1
          ENDIF
   30   CONTINUE
        NN=0
        DO 35 J = 1,NBSD
          IF ( ZI(IALSMD+J-1).EQ.1 ) NN=NN+1
   35   CONTINUE
        ZI(IALSML+L-1)=NN
   34 CONTINUE

      IF (NIV.GE.5) THEN
        WRITE(IFM,*) ' '
        WRITE(IFM,*) '------------------------------------'
        WRITE(IFM,*) '  NOEUDS ET LAGRANGES D''INTERFACE  |'
        WRITE(IFM,*) ' '
      ENDIF

C***********************************************************************
C DOUBLE BOUCLE 6 SUR LES NOEUDS D'INTERFACE AVEC LEUR MULTIPLICITE
C TENANT COMPTE DES INTERFACES DE MESURE NULLE
C NBFETE : NOEUD D'INTERFACE AVEC MULTIPLICITE=K1
C***********************************************************************
      DO 37 I = 1,K1
        ADDR=IALSTR+3*(I-1)
        ITMP=ZI(ADDR+1)
        JTMP=ZI(ADDR+2)
        KTMP=ZI(ADDR)
        IF (NIV.GE.5)
     &    WRITE(IFM,*)'NOEUD/SD1/SD2/NB: ',KTMP,ITMP,JTMP,ZI(IALSML+I-1)
        ZI(IALSNO-1+I)=KTMP
        ZI(IALSMU-1+I)=ZI(IALSML+I-1)
   37 CONTINUE
C     NB NOEUDS INTERFACE (EN COMPTANT LEUR MULTIPLICITE)
      NBFETE=K1

      IF (NIV.GE.5) THEN
        WRITE(IFM,*) 'IL Y A :',NBFETE,' NOEUDS INTERFACE'
        WRITE(IFM,*) '-------------------------------------'
        WRITE(IFM,*) ' '
      ENDIF
      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES BOUCLE 6')

C***********************************************************************
C REMPLISSAGE EFFECTIF DE .FETI ET .FETJ
C***********************************************************************
      CALL WKVECT(NOMSDJ,'G V I',2*NBFETE,JADRJ)
      CALL WKVECT(NOMSDI,'G V I',4*NBFETE,JADRI)
      MULTC = 1
      K=0
      DO 500 I=1,NBFETE

C       NUMERO DU NOEUD D'INTERFACE ET MULTIPLICITE
        ADDR=JADRI+4*(I-1)
        INO=ZI(IALSNO-1+I)
        ZI(ADDR)=INO
        ZI(ADDR+1)=ZI(IALSMU-1+I)
C       NB DDL CUMULES
        DO 2 J=1,NEC30
          IF (EXISDG(ZI(JPRNM-1+NEC*(INO-1)+1),J)) K=K+1
  2     CONTINUE
        ZI(ADDR+2)=K
C       LISTE DES SD D'APPARTENANCE
        ZI(ADDR+3)=MULTC
C       VALEUR DES SDS
        ZI(JADRJ-1+MULTC)  =ZI(IALSTR+3*(I-1)+1)
        ZI(JADRJ-1+MULTC+1)=ZI(IALSTR+3*(I-1)+2)
        MULTC=MULTC+2
  500 CONTINUE

C     ON STOCKE LE NB DE DDL TOTAL DES NOEUDS D'INTERFACE
      NBDDLI=K

      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES .FETI/.FETJ')
      IF (NIV.GE.4) THEN
        CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSDI,1,'G')
        CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSDJ,1,'G')
      ENDIF

C***********************************************************************
C DOUBLE BOUCLE 7 POUR CONSTRUIRE .FETG
C***********************************************************************
      CALL JECREC(NOMSDG,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      CALL WKVECT('&&FETCRF.LST_FETG ','V V I',2*NBFETE,IALSFG)
      DO 51 I = 1,NBSD
C**** ON DETERMINE LES DONNEES A METTRE DANS .FETG ET SA TAILLE
C       NB D'OCCURENCE DANS FETG
        NN=0
C       ADRESSE DE DEBUT DE FETB POUR LE SD I
        JADR=ZI(IAJADR-1+I)
C       NBRE DE NOEUDS DU SD I
        NBNOSD=ZI(IANBNO-1+I)
        DO 52 J = 1,NBNOSD
C         SI LE NOEUD DANS FETB EST NEGATIF (IE NOEUD D'INTERFACE)
          IAUX=ZI(JADR+2*(J-1))
          IF (IAUX.LT.0) THEN
            IAUX=ABS(IAUX)
C           ON RECHERCHE DANS FETI LE NOEUD CORRESPONDANT
            DO 53 K = 1,NBFETE

C             SI LES NOEUDS CORRESPONDENT (JADRI/J, ADDR DE .FETI/J)
              IF (ZI(JADRI+4*(K-1)).EQ.IAUX) THEN
C               INDICE DANS .FETJ DU PREMIER SD
                IAUX0=ZI(JADRI+4*(K-1)+3)
C               SD1 ET SD2 DE FETJ
                IAUX1=ZI(JADRJ-1+IAUX0)
                IAUX2=ZI(JADRJ+IAUX0)
C               SI LE SD I EST LE 1ER SD DU NOEUD COURANT
                NBID=0
                IF (I.EQ.IAUX1) THEN
C                   SI LE SD COURANT EST PLUS FAIBLE QUE LE 1ER SD
                    IF (I.LT.IAUX2) THEN
                      NBID=-K
                    ELSE
                      NBID=K
                    ENDIF
C               OU SI LE SD I EST LE DEUXIEME SD DU NOEUD COURANT
                ELSEIF (I.EQ.IAUX2) THEN
                  IF (I.LT.IAUX1) THEN
                    NBID=-K
                  ELSE
                    NBID=K
                  ENDIF
                ENDIF

                IF (NBID.NE.0) THEN
                  NN=NN+1
                  IAUX3=IALSFG+2*(NN-1)
                  ZI(IAUX3)  = NBID
                  ZI(IAUX3+1)= J
                ENDIF
              ENDIF
  53        CONTINUE
          ENDIF
  52    CONTINUE

C***** ON CONSTRUIT EFFECTIVEMENT .FETG
        K8BUFF=ZK8(NOMSD-1+I)
        CALL JECROC(JEXNOM(NOMSDG,K8BUFF))
        INTBUF=2*NN
        CALL JEECRA(JEXNOM(NOMSDG,K8BUFF),'LONMAX',INTBUF,K8BID)
        CALL JEVEUO(JEXNOM(NOMSDG,K8BUFF),'E',JADR)
        DO 610 J=1,NN
          J2=2*(J-1)
          IAUX1=JADR+J2
          IAUX2=IALSFG+J2
          ZI(IAUX1)  =ZI(IAUX2)
          ZI(IAUX1+1)=ZI(IAUX2+1)
  610   CONTINUE

C FIN BOUCLE SUR LES SD
  51  CONTINUE
      CALL JEDETR('&&FETCRF.LST_FETG ')
      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES .FETG')
      IF (NIV.GE.4)
     &  CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSDG,1,'G')

C***********************************************************************
C***********************************************************************
C
C       T R A I T E M E N T   D E S   C H A R G E S  A MAILLES ET/OU
C       POINTS TARDIFS
C
C***********************************************************************
C***********************************************************************

C     NB NOMBRE DE MAILLES TARDIVES ET DE NOEUDS TARDIFS
      NBMATA=0
      NBNOTA=0
      NOMNOE = MA//'.NOMNOE'

C***********************************************************************
C BOUCLE 8 CALCUL MAILLES TARDIVES DE TOUTES LES CHARGES
C***********************************************************************

      IF (NBCHAR.GE.1) THEN
C       TABLE TEMPORAIRE : 2xNB CHARGE PAR SD
        CALL WKVECT('&&FETCRF.L_2CHA_SD','V V I',NBSD*2*NBCHAR,ISDLCH)
C       LISTE TEMPORAIRE : NB MAILLES TARDIVES PAR CHARGE
        CALL WKVECT('&&FETCRF.L_NBMT_CH','V V I',NBSD*2*NBCHAR,INBMCH)
C       LISTE TEMPORAIRE : NOMS DES LIGREL DE CHARGE
        CALL WKVECT('&&FETCRF.LST_CHA  ','V V K24',NBCHAR,ILSCHA)
C       PREMIERE PASSE : CALCUL DU NB TOTAL DE MAILLES TARDIVES
        DO 811 I = 1,NBCHAR
          LIGRCH = ZK8(NOMCHA-1+I)//'.CHME.LIGRE'
          ZK24(ILSCHA-1+I)= LIGRCH
          CALL JEEXIN(LIGRCH//'.NEMA',N2)
          IF (N2.NE.0) THEN
            CALL JELIRA(LIGRCH//'.NEMA','NUTIOC',NBOBJ1,K8BID)
            NBMATA=NBMATA+NBOBJ1
            ZI(INBMCH-1+I)=NBOBJ1
          ENDIF
  811   CONTINUE
        IF (NIV.GE.4) WRITE(IFM,*) 'NBRE MAILLES TARDIVES:', NBMATA
      ENDIF

      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES BOUCLE 8')

C **** IF SUR LES CHARGES A MAILLES TARDIVES
      IF ( (NBCHAR.GE.1).AND.(NBMATA.GT.0) ) THEN

C       CREATION DES OBJETS TEMPORAIRES
C MAILLES TARDIVES TROUVEES PAR SD
C IFLIM  :  MT  1          2          3          4          5 (4+1) ...
C           SD  1 2 3 4    1 2 3 4    1 2 3 4    1 2 3 4    1 2 3 4 ...
C      oui/non  1 0 0 0    1 0 0 0    0 0 1 0    0 0 1 0    1 0 0 0 ...
C NOMBRE DE MAILLES TARDIVES PAR SD
C IDFLIM :  SD  1 2 3 4
C        nb MT  2 0 2 0
        CALL WKVECT('&&FETCRF.FLIM     ','V V I',NBSD*NBMATA,IFLIM)
        CALL WKVECT('&&FETCRF.IDFLIM   ','V V I',NBSD,IDFLIM)
C NOMBRE DE DDLS SUPPLEMENTAIRES ASSOCIES A UNE MAILLE TARDIVE
C       IFNT   :  MT  1 2 3 4
C             nb NoT  2 2 2 2
C
        CALL WKVECT('&&FETCRF.IFNT     ','V V I',NBMATA,IFNT)
C NBRE DE MAILLES TARDIVES PAR SD POUR UN CHARGEMENT DONNE
C       IFLII  :  CH  1          2          3       ...
C                 SD  1 2 3 4    1 2 3 4    1 2 3 4 ...
C              nb MT  0 0 0 0    2 0 2 0            ...
        CALL WKVECT('&&FETCRF.FLII     ','V V I',NBSD*NBCHAR,IFLII)

C       DECALAGE POUR PRENDRE EN COMPTE PLUSIEURS CHARGES
        NB2=0
C       NB DE NOEUDS DE L'INTERFACE QUI SONT SUR DES CHARGEMENTS
        NOINCH=0

C***********************************************************************
C MAXI BOUCLE 9 SUR LES CHARGES
C***********************************************************************
        DO 801 ICH = 1,NBCHAR
          LIGRCH = ZK8(NOMCHA-1+ICH)//'.CHME.LIGRE'
          IF (NIV.GE.4) THEN
            WRITE(IFM,*) '--------------------------------------------'
            WRITE(IFM,*) '        CHARGE : ', ICH
            WRITE(IFM,*) '--------------------------------------------'
            WRITE(IFM,*) 'LIGRCH:', LIGRCH
          ENDIF

C***** 9.0 LIGREL DE CONTACT OU PAS (DEDUIT DE SURFCL.F)
          LCFC1=.FALSE.
          METHCO = ZK8(NOMCHA-1+ICH)//'.CONTACT.METHCO'
          CALL JEEXIN(METHCO,IRET)
          IF (IRET.NE.0) THEN
            CALL JEVEUO(METHCO,'L',JMETH)
            NZOCO  = CFDISI(ZK8(NOMCHA-1+ICH)//'.CONTACT','NZOCO')
            IFORM  = CFDISI(ZK8(NOMCHA-1+ICH)//'.CONTACT','FORMULATION')
            
            DO 805 IZONE=1,NZOCO
              IF (IFORM.EQ.2) THEN
                LCFC1=.TRUE.
              ELSE
                CALL U2MESS('F','ELEMENTS5_32')
              ENDIF
  805       CONTINUE
          ENDIF
C **** 9.1 SI ON TROUVE DES NOEUDS TARDIFS DANS LA CHARGE
          INBNO=0
          CALL JEEXIN(LIGRCH//'.NBNO',N1)
          IF (N1.NE.0) THEN
            CALL JEVEUO(LIGRCH//'.NBNO','L',IADR)
C         OBJET TEMP POUR STOCKER LES NOEUDS TARDIFS DEJA TRAITES ET
C         AINSI EVITER LES NOEUDS COMPTES 2 FOIS AVEC LIAISON_DDL....
            INBNO=ZI(IADR)
            IF (INBNO.NE.0) THEN
              IF (LCFC1) CALL U2MESS('F','ELEMENTS5_33')
C             VECTEUR DES NOEUDS TARDIFS DEJA COMPTES
              CALL WKVECT('&&FETCRF.LIAISON','V V I',INBNO,ILIAIS)
              IAUX1=3*INBNO
C             ILIDDL(1,J)=NOEUD PHYSIQUE ASSOCIE AU NOEUD TARDIF
C             ILIDDL(2,J)= SON NUMERO DE SD;
C             ILIDDL(3,J)= COMPTEUR (SI > 1 LIAISON_***)
              CALL WKVECT('&&FETCRF.LIAISONDDL','V V I',IAUX1,ILIDDL)
            ENDIF
            ICOMPT=0
          ENDIF

C **** 9.2 SI ON TROUVE DES MAILLES TARDIVES DANS LA CHARGE
          CALL JEEXIN(LIGRCH//'.NEMA',N2)
          IF (N2.NE.0) THEN
            IF (LCFC1) CALL U2MESS('F','ELEMENTS5_33')
C           NB MAILLES TARDIVES
            CALL JELIRA(LIGRCH//'.NEMA','NUTIOC',NBOBJ1,K8BID)
C           LONGUEUR TOTALE DE LA COLLECTION
            CALL JELIRA(LIGRCH//'.NEMA','LONT',LONLI1,K8BID)
C           ADRESSE DE DEBUT DE LA COLLECTION
            CALL JEVEUO(LIGRCH//'.NEMA','L',IDLIG1)
C           DIMENSIONNEMENT DES OBJETS TEMPORAIRES
C           LISTE DES MAILLES TARDIVES DE CHAQUE SD
C           ISDMAT(I,J)=1 SI LA MAILLE TARDIVE J APPARTIENT AU SD I
            CALL WKVECT('&&FETCRF.L_MAT_SD ','V V I',NBSD*NBOBJ1,ISDMAT)

C **** 9.3 BOUCLE SUR LES OBJETS DE LA COLLECTION .NEMA
C           POINTEUR VERS LA POSITION DANS LA COLLECTION
            IADR=IDLIG1
            DO 802 J = 1, NBOBJ1
C             NB NOEUDS DE CHAQUE MAILLE
              CALL JELIRA(JEXNUM(LIGRCH//'.NEMA',J),'LONMAX',N3,K8B)
              N31=N3-1
C             ON PARCOURS LES NOEUDS DE LA MAILLE TARDIVE
              DO 803 K = 1, N31
                NDTAR=ZI(IADR-1+K)

C               SI C'EST UN NOEUD PHYSIQUE, ON CHERCHE SON(SES) SD
                IF ( NDTAR.GT.0 ) THEN
                  NB1=0
                  DO 804 L = 1, NB
C **** 9.3.1 ON A TROUVE LE NOEUDS PHYSIQUE CORRESPONDANT
                    IF (NDTAR.EQ.ZI(IALINO-1+L)) THEN
                      NUMSD=ZI(LINOMA-1+L)
                      DEC=(J-1)*NBSD+NUMSD
                      NB1=NB1+1
C POUR REMPLISSAGE OBJETS FLIM, FLIN, FLII
C ON RAJOUTE CE TEST SUR NB1 POUR NE PAS DUPLIQUER LES NOEUDS OU MAILLES
C TARDIVES SITUEES SUR L'INTERFACE.
                      IF (NB1.EQ.1) THEN
                        IF (ZI(ISDMAT-1+DEC).EQ.0) ZI(ISDMAT-1+DEC)=1
                      ENDIF

C REMPLISSAGE OBJET TEMPORAIRE POUR DETECTER LA PRESENCE DE LIAISONS
C TRAVERSANT LES INTERFACES. ON NE S'INTERESSE QU'AUX TRIA3 DANS LA
C CONFIGURATION: NOEUD PHYS NOEUD TARDIF1 NOEUD TARDIF2
C ON NE STOCKE QUE LES INFOS RELATIVES AU NOEUD TARDIF1
                      IAUX1=ZI(IADR+1)
                      IAUX3=ZI(IADR+2)
C **** 9.3.2 CONFIGURATION TRIA3 QUI NOUS INTERESSE
                      IF ((N3.EQ.4).AND.(K.EQ.1).AND.(IAUX1.LT.0).AND.
     &                     (IAUX3.LT.0)) THEN
                        IAUX2=ILIDDL+3*(-IAUX1-1)
C                       CAS DU LIAISON INTER-SOUS-DOMAINES
                        IF ((ZI(IAUX2+1).NE.0).AND.(ZI(IAUX2+1).NE.
     &                       NUMSD).AND.(NB1.EQ.1)) THEN
                          CALL JENUNO(JEXNUM(NOMNOE,NDTAR),NOMN)
                          CALL JENUNO(JEXNUM(NOMNOE,ZI(IAUX2)),NOMN1)
                          VALK(1)=ZK8(NOMCHA-1+ICH)
                          VALK(2)=NOMN
                          VALK(3)=NOMN1
                          CALL U2MESK('F','ELEMENTS_64',3,VALK)
                        ENDIF
                        IF (ZI(IAUX2+2).LT.2) THEN
                          ZI(IAUX2)=NDTAR
                          ZI(IAUX2+1)=NUMSD
                          ZI(IAUX2+2)=NB1
                        ENDIF
                      ENDIF
                    ENDIF
  804             CONTINUE

C                 .AJOUT DU NOEUD CHARGE SUR L'INTERFACE DANS LA LISTE
                  IF (NB1.GT.1) THEN
                    NOINCH=NOINCH+1
                    IF (NIV.GE.2)THEN
                      CALL JENUNO(JEXNUM(NOMNOE,NDTAR),NOMN)
                      WRITE(IFM,*)'FETCRF, LE NOEUD '//NOMN//' DU '//
     &                'CHARGEMENT '//ZK8(NOMCHA-1+ICH)//' EST SUR '//
     &                'L''INTERFACE.'
                    ENDIF
                  ENDIF

C               SINON C'EST UN NOEUD TARDIF (NDTAR)
                ELSE
C **** 9.3.3 BOUCLE SUR LES NOEUDS TARDIFS DEJA COMPTES
                  DO 205 LL=1,ICOMPT
                    IF (NDTAR.EQ.ZI(ILIAIS-1+LL)) THEN
C TEST SUPPLEMENTAIRE POUR SAVOIR SI IL EST CONCERNE PAR UN LIGREL DE
C CHARGE TOUCHANT L'INTERFACE
                      IAUX0=ILIDDL+3*(-NDTAR-1)
                      IF (ZI(IAUX0+2).GT.1) THEN
                        CALL JENUNO(JEXNUM(NOMNOE,ZI(IAUX0)),NOMN)
                        VALK(1)=ZK8(NOMCHA-1+ICH)
                        VALK(2)=NOMN
                        CALL U2MESK('F','ELEMENTS_65',2,VALK)
                      ENDIF
                      GOTO 206
                    ENDIF
  205             CONTINUE
                  ZI(IFNT-1+J+NB2)=ZI(IFNT-1+J+NB2)+1
                  NBNOTA=NBNOTA+1
                  ICOMPT=ICOMPT+1
                  ZI(ILIAIS-1+ICOMPT)=NDTAR
  206             CONTINUE
                ENDIF
C             FIN BOUCLE SUR LES MAILLES TARDIVES
  803         CONTINUE
C             POSITION DE LA MAILLE TARDIVE SUIVANTE DANS LA COLLECTION
              IADR=IADR+N3
  802       CONTINUE

C **** 9.4 MONITORING
            IF (NIV.GE.5) THEN
              WRITE(IFM,*) '----------'
              DO 810 J = 1, NBOBJ1
                DO 812 NUMSD = 1, NBSD
                  DEC=(J-1)*NBSD+NUMSD
              WRITE(IFM,*) 'SD:',NUMSD,' MT:',J,' VAL:',ZI(ISDMAT-1+DEC)
  812           CONTINUE
  810         CONTINUE
              WRITE(IFM,*) '----------'
              DO 815 NUMSD = 1, NBSD
                DO 814 J = 1, NBOBJ1
                  DEC=(J-1)*NBSD+NUMSD
              WRITE(IFM,*) 'MT:',J,' SD:',NUMSD,' VAL:',ZI(ISDMAT-1+DEC)
  814           CONTINUE
  815         CONTINUE
              WRITE(IFM,*) '----------', NB2
            ENDIF

C **** 9.5 MISE A JOUR DE L'OBJET TEMPORAIRE POUR FLIM
            DO 900 J = 1, NBOBJ1
              DO 901 NUMSD = 1, NBSD
                DEC=(J-1)*NBSD+NUMSD
                NVAL=ZI(ISDMAT-1+DEC)
C               LOGICAL=1 SI LA MAILLE TARDIVE J A UN POINT TARDIF DANS
C               LE SD NUMSD. EN CAS DE MULTIPLICITE, C'EST LE SD DE
C               PLUS PETIT NUMERO QUI L'EMPORTE
                IF (NVAL.GT.0) THEN
C                 MAJ de IDFLIM (NB DE MAILLES TARDIVES TROUVEES PAR SD)
                  ZI(IDFLIM-1+NUMSD)=ZI(IDFLIM-1+NUMSD)+1
C                 MAJ DE IFLIM (NUM DE MAILLES TARDIVES TROUVEES PAR SD)
                  DEC=(J-1 + NB2)*NBSD+ NUMSD
                  ZI(IFLIM-1+DEC) = J
C                 MAJ DE IFLII (NB DE MAILLES TARDIVES TROUVEES PAR SD)
                  DEC=(ICH-1)*NBSD+NUMSD
                  ZI(IFLII-1+ DEC) = ZI(IFLII-1+ DEC) + 1
                ENDIF
  901         CONTINUE
  900       CONTINUE
C           DECALAGE POUR IFLIM (LES NOUVELLES MaT COMMENCENT A LA FIN)
            NB2=NB2+NBOBJ1
            CALL JEDETR('&&FETCRF.L_MAT_SD')
C         FIN SI ON A TROUVE DES MAILLES TARDIVES DANS CETTE CHARGE...
          ENDIF

C         FIN BOUCLE DE 1 A NBCHAR
          IF (INBNO.NE.0) THEN
            CALL JEDETR('&&FETCRF.LIAISON')
            CALL JEDETR('&&FETCRF.LIAISONDDL')
          ENDIF
  801   CONTINUE

C***********************************************************************
C FIN MAXI BOUCLE 9 SUR LES CHARGES
C***********************************************************************

C***********************************************************************
C ETAPE 10 MONITORING
C***********************************************************************
        IF (NIV.GE.4) THEN
          WRITE(IFM,*) ' '
          WRITE(IFM,*) '----------------------------------------------'
          WRITE(IFM,*) '         FIN DES CHARGES'
          WRITE(IFM,*) '----------------------------------------------'
          WRITE(IFM,*) ' '
          WRITE(IFM,*) 'NB1=',NB1
          WRITE(IFM,*) 'NB2=',NB2
          WRITE(IFM,*) 'NBMATA=',NBMATA
          WRITE(IFM,*) 'NBSD=',NBSD
          WRITE(IFM,*) 'NBCHAR=',NBCHAR

          WRITE(IFM,*) '--------------------------'
          DO 902 J = 1, NBMATA
            WRITE(IFM,*) ' '
            DO 903 NUMSD = 1, NBSD
              DEC=(J-1)*NBSD+NUMSD
             WRITE(IFM,*) 'SD:',NUMSD,' MT:',J,' IFLIM:',ZI(IFLIM-1+DEC)
  903       CONTINUE
  902     CONTINUE
          WRITE(IFM,*) '--------------------------'
          DO 905 K = 1, NBSD*NBMATA
            WRITE(IFM,*) 'IFLIM:',ZI(IFLIM-1+K)
  905     CONTINUE
          WRITE(IFM,*) '--------------------------'
          DO 904 ICH = 1, NBCHAR
            DO 908 ISD = 1, NBSD
              DEC=(ICH-1)*NBSD+ISD
             WRITE(IFM,*) 'CH:',ICH,' SD:',ISD,' IFLII:',ZI(IFLII-1+DEC)
  908       CONTINUE
  904     CONTINUE
          WRITE(IFM,*) '--------------------------'
          DO 907 K = 1, NBSD
            WRITE(IFM,*) 'SD:',K,' IDFLIM:',ZI(IDFLIM-1+ K)
  907     CONTINUE
          WRITE(IFM,*) '--------------------------'
          DO 906 K = 1, NBCHAR
            WRITE(IFM,*) 'CHA:',ZK24(ILSCHA-1+ K)
  906     CONTINUE
          WRITE(IFM,*) '--------------------------'
          DO 824 K = 1, NBMATA
            WRITE(IFM,*) 'MAT',K,' NBNOT:',ZI(IFNT-1+K)
  824     CONTINUE
          WRITE(IFM,*) '--------------------------'
        ENDIF
C **** FIN IF SUR LES CHARGES A MAILLES TARDIVES
      ENDIF
      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES BOUCLE 9')
C***********************************************************************
C ETAPE 11 CREATION .FDIM
C***********************************************************************

      CALL WKVECT(NOMSDM,'G V I',5,JADR)
      ZI(JADR)=NBSD
      ZI(JADR+1)=NBFETE
      ZI(JADR+2)=NBMATR
      ZI(JADR+3)=NBDDLI
      ZI(JADR+4)=NBNOTO

      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES CREATION .FDIM')
      IF (NIV.GE.4)
     &  CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSDM,1,'G')
C***********************************************************************
C ETAPE 12 CREATION .FETH
C***********************************************************************
C     PRISE EN COMPTE DES NOEUDS PHYSIQUES
      CALL WKVECT(NOMSDH,'G V I',NBSD,JADRH)
      DO 700 ISD=1,NBSD
        ZI(JADRH+ISD-1)=ZI(IALSK-1+ISD)
  700 CONTINUE

C     DES NOEUDS TARDIFS
      IF (NBNOTA.GT.0) THEN
        DO 701 J = 1, NBMATA
          DO 702 NUMSD = 1, NBSD
            DEC=(J-1)*NBSD+NUMSD
            IF ( ZI(IFLIM-1+DEC).GT.0 ) THEN
              ZI(JADRH-1+NUMSD)=ZI(JADRH-1+NUMSD) + ZI(IFNT-1+J)
            ENDIF
  702     CONTINUE
  701   CONTINUE
      ENDIF

      IF (NIV.GE.3) CALL JXVERI('MESSAGE','APRES CREATION .FETH')
      IF (NIV.GE.4)
     &  CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSDH,1,'G')
C***********************************************************************
C ETAPE 13 CREATION .FLII/.FLIM/.FLIN
C***********************************************************************

C     CREATION DES TROIS COLLECTIONS
      CALL JECREC(NOMSLN,'G V K24','NO','DISPERSE','VARIABLE',NBSD)
      CALL JECREC(NOMSLI,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      CALL JECREC(NOMSLM,'G V I','NO','DISPERSE','VARIABLE',NBSD)

C     ON NE REMPLIT CES COLLECTIONS QUE SI ON A DES MAILLES TARDIVES
      IF (NBMATA.GT.0) THEN

C       VECTEURS TEMPORAIRES CONTENANT LES TAILLES DES COLLECTIONS FLIx
        CALL WKVECT('&&FETCRF.DIM_FLII ','V V I',NBSD,IDFLII)
        CALL WKVECT('&&FETCRF.DIM_FLIM ','V V I',NBSD,IDFLM)
        CALL WKVECT('&&FETCRF.DIM_FLIN ','V V I',NBSD,IDFLN)
C       NBRE DE MAILLES TARDIVES POUR UN CHARGEMENT DONNE
        CALL WKVECT('&&FETCRF.TEMP     ','V V I',NBCHAR,ITMP)

C ****  13.1 PREMIER PASSAGE : RECUPERATION DE LA TAILLE DES COLLECTIONS
        DO 910 ICH = 1, NBCHAR
C         NBRE DE MAILLES TARDIVES DU CHARGEMENT ICH
          NN = 0
          DO 911 ISD = 1, NBSD
            DEC=(ICH-1)*NBSD+ISD
            NN=NN+ZI(IFLII-1+DEC)
  911     CONTINUE
          ZI(ITMP+ICH-1)=NN
C         SI LA CHARGE COURANTE A DES MAILLES TARDIVES
          IF (NN.GT.0) THEN
            DO 913 ISD = 1, NBSD
              DEC=(ICH-1)*NBSD+ISD
C             SI LA CHARGE COURANTE A DES MAILLES TARDIVES DANS LE SD
C             ON INCREMENTE LES TAILLES DES 3 COLLECTIONS POUR LE SD
              IF (ZI(IFLII-1+ DEC).GT.0) THEN
                ZI(IDFLN-1+ISD)=ZI(IDFLN-1+ISD)+1
                ZI(IDFLII-1+ISD)=ZI(IDFLII-1+ISD)+2
                ZI(IDFLM-1+ISD)=ZI(IDFLM-1+ISD)+ZI(IFLII-1+DEC)
              ENDIF
  913       CONTINUE
          ENDIF
  910   CONTINUE

C       VECTEURS DE POINTEUR VERS L'ADDRESSE JEVEUX DES OBJETS DE COLL.:
C       CA VAUT 0 SI L'OBJET N'A PAS ENCORE ETE CREE
C       SINON CA VAUT L'ADRESSE MEMOIRE COURANTE POUR CHAQUE SD
        CALL WKVECT('&&FETCRF.BIM_FLII ','V V I',NBSD,BIFLII)
        CALL WKVECT('&&FETCRF.BIM_FLIM ','V V I',NBSD,BIFLIM)
        CALL WKVECT('&&FETCRF.BIM_FLIN ','V V I',NBSD,BIFLN)

C **** 13.2 DEUXIEME PASSAGE : REMPLISSAGE DES COLLECTIONS FLII ET FLIN
        DO 920 ICH = 1, NBCHAR
C         NN = NB DE MAILLES TARDIVE DE LA CHARGE
          NN = ZI(ITMP+ICH-1)
C         SI LA CHARGE COURANTE A DES MAILLES TARDIVES
          IF (NN.GT.0) THEN
            DO 923 ISD = 1, NBSD
              DEC=(ICH-1)*NBSD+ISD
C             SI LA CHARGE COURANTE A DES MAILLES TARDIVES DANS LE SD
              IF (ZI(IFLII-1+DEC).GT.0) THEN

C **** 13.2.1   CREATION DE .FLIN
                N1 = ZI(IDFLN-1+ISD)
                IF ( N1.GT.0 ) THEN
                  IF ( ZI(BIFLN-1+ISD).EQ.0 ) THEN
                    K8BUFF=ZK8(NOMSD-1+ISD)
                    CALL JECROC(JEXNOM(NOMSLN,K8BUFF))
                    CALL JEECRA(JEXNOM(NOMSLN,K8BUFF),'LONMAX',N1,K8B)
                    CALL JEVEUO(JEXNOM(NOMSLN,K8BUFF),'E',JADR)
C                   ADDR DE DEPART POUR FLIN/ISD
                    ZI(BIFLN-1+ISD)=JADR
                  ENDIF
C                 ADDR COURANTE POUR FLIN/ISD
                  JADR=ZI(BIFLN-1+ISD)
C                 REMPLISSAGE DE FLIN/ISD
                  ZK24(JADR)=ZK24(ILSCHA-1+ICH)
C                 ADDR COURANTE POUR FLIN/ISD INCREMENTEE DE 1
                  ZI(BIFLN-1+ISD)=JADR+1
                ENDIF

C **** 13.2.2   CREATION DE .FLII
                N1 = ZI(IDFLII-1+ISD)
                IF ( N1.GT.0 ) THEN
                  IF ( ZI(BIFLII-1+ISD).EQ.0 ) THEN
                    K8BUFF=ZK8(NOMSD-1+ISD)
                    CALL JECROC(JEXNOM(NOMSLI,K8BUFF))
                    CALL JEECRA(JEXNOM(NOMSLI,K8BUFF),'LONMAX',N1,K8B)
                    CALL JEVEUO(JEXNOM(NOMSLI,K8BUFF),'E',JADR)
C                   ADDR DE DEPART POUR FLII/ISD
                    ZI(BIFLII-1+ISD)=JADR
                  ENDIF
C                 ADDR COURANTE POUR FLII/ISD
                  JADR=ZI(BIFLII-1+ISD)
C                 REMPLISSAGE DE FLII/ISD
                  ZI(JADR)=NN
                  ZI(JADR+1)=ZI(IFLII-1+DEC)
C                 ADDR COURANTE POUR FLII/ISD INCREMENTEE DE 2
                  ZI(BIFLII-1+ISD)=JADR+2
                ENDIF

C **** 13.2.3   CREATION DE .FLIM
                N1 = ZI(IDFLM-1+ISD)
                IF ( N1.GT.0 ) THEN
                  IF ( ZI(BIFLIM-1+ISD).EQ.0 ) THEN
                    K8BUFF=ZK8(NOMSD-1+ISD)
                    CALL JECROC(JEXNOM(NOMSLM,K8BUFF))
                    CALL JEECRA(JEXNOM(NOMSLM,K8BUFF),'LONMAX',N1,K8B)
                    CALL JEVEUO(JEXNOM(NOMSLM,K8BUFF),'E',JADR)
C                   ADDR DE DEPART POUR FLIM/ISD
                    ZI(BIFLIM-1+ISD)=JADR
                  ENDIF
                ENDIF
C             FIN SI LA CHARGE EST CONCERNEE PAR LE SD
              ENDIF
C           FIN BOUCLE SUR LES SD
  923       CONTINUE
C         FIN SI LA CHARGE COURANTE A DES MAILLES TARDIVES
          ENDIF
C       FIN BOUCLE SUR LES CHARGES
  920   CONTINUE

C **** 13.3 TROISIEME PASSAGE : REMPLISSAGE DE LA COLLECTION FLIM
        DO 930 J = 1, NBMATA
          DO 932 NUMSD = 1, NBSD
            DEC=(J-1)*NBSD+NUMSD
            IF (ZI(IFLIM-1+DEC).GT.0) THEN
C             ADDR COURANTE POUR FLIM/ISD
              JADR=ZI(BIFLIM-1+NUMSD)
C             REMPLISSAGE DE FLIM/ISD
              ZI(JADR)=ZI(IFLIM-1+DEC)
C             ADDR COURANTE POUR FLIM/ISD INCREMENTEE DE 1
              JADR=JADR+1
              ZI(BIFLIM-1+NUMSD)=JADR
            ENDIF
  932     CONTINUE
  930   CONTINUE

C     FIN DE CREATION DE FLII, FLIM ET FLIN, IF (NBNOTA.GT.0)
      ENDIF
      IF (NIV.GE.3)
     &  CALL JXVERI('MESSAGE','APRES CREATION .FLII/FLIM/FLIN')
      IF (NIV.GE.4) THEN
        CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSLN,1,'G')
        CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSLI,1,'G')
        CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,NOMSLM,1,'G')
      ENDIF

C***********************************************************************
C MAXI BOUCLE 14 SUR LES CHARGES POUR LE CONTACT CONTINUE (CF. SURFCL.F)
C***********************************************************************
C     ON NE RETESTE PAS L'HOMOGENIEITE DES ZONES DE CONTACT EN CONTACT
C     CONTINUE ET LEUR NON JUXTAPOSITION AVEC UNE ZONE DE DIRICHLET OU
C     DE FORCE NODALE DANS UN MEME AFFE_CHAR_MECA. CELA A ETE DEJA FAIT
C     PRECEDEMMENT.

C VECTEURS DE NBRE DE MAILLES/NOEUDS ESCLAVES PAR SD
      CALL WKVECT('&&FETCRF.FCFM     ','V V I',NBSD,IFCFM)
      CALL JERAZO('&&FETCRF.FCFM     ',NBSD,1)
      CALL WKVECT('&&FETCRF.FCFN     ','V V I',NBSD,IFCFN)
      CALL JERAZO('&&FETCRF.FCFN     ',NBSD,1)
C VECTEURS AUXILIAIRES DE POINTEURS DE CONTACT
      IF (NBCHAR.NE.0)
     &  CALL WKVECT('&&FETCRF.FCFB     ','V V I',9*NBCHAR,IFCFB)

C **** 14.1 BOUCLE POUR DETERMINER LE NOMBRE DE ZONES MAXI DE CONTACT
C  ET LES POINTEURS DE CONTACT ADHOC
      NZOCOM=0
      ZDIME  = CFMMVD('ZDIME')
      DO 940 ICH = 1,NBCHAR
        K8BID=ZK8(NOMCHA-1+ICH)
        METHCO = K8BID//'.CONTACT.METHCO'
        PZONE  = K8BID//'.CONTACT.PZONECO'
        PSURMA = K8BID//'.CONTACT.PSUMACO'
        PSURNO = K8BID//'.CONTACT.PSUNOCO'
        CONTMA = K8BID//'.CONTACT.MAILCO'
        CONTNO = K8BID//'.CONTACT.NOEUCO'
        NDIMCO = K8BID//'.CONTACT.NDIMCO'
        
        CALL JEEXIN(METHCO,IRET)
        ZI(IFCFB+ZDIME*(ICH-1))=0
        IF (IRET.NE.0) THEN
          ZI(IFCFB+ZDIME*(ICH-1))=1
          CALL JEVEUO(METHCO,'L',JMETH)
          ZI(IFCFB+ZDIME*(ICH-1)+1)=JMETH
          CALL JEVEUO(PZONE ,'L',JZONE)
          ZI(IFCFB+ZDIME*(ICH-1)+2)=JZONE
          CALL JEVEUO(PSURMA,'L',JSUMA)
          ZI(IFCFB+ZDIME*(ICH-1)+3)=JSUMA
          CALL JEVEUO(PSURNO,'L',JSUNO)
          ZI(IFCFB+ZDIME*(ICH-1)+4)=JSUNO
          CALL JEVEUO(CONTMA,'L',JMACO)
          ZI(IFCFB+ZDIME*(ICH-1)+5)=JMACO
          NZOCO  = CFDISI(K8BID//'.CONTACT','NZOCO')
          
          ZI(IFCFB+ZDIME*(ICH-1)+6)=NZOCO
          IF (NZOCO.GT.NZOCOM) NZOCOM=NZOCO
          CALL JEVEUO(CONTNO,'L',JNOCO)
          ZI(IFCFB+ZDIME*(ICH-1)+7)=JNOCO
          CALL JEVEUO(NDIMCO,'L',JDIM)
          ZI(IFCFB+ZDIME*(ICH-1)+8)=JDIM
        ENDIF
  940 CONTINUE
C VECTEUR INDIQUANT LE NUMERO DE SD CONCERNE PAR LA JIEME ZONE
C DU IEME CHARGEMENT
C     CHAR     1        2        3...
C     ZONE     1 2 0 0  0 1 0 0
      IAUX0=NZOCOM*NBCHAR
      IF (IAUX0.NE.0)
     &  CALL WKVECT('&&FETCRF.FCFL     ','V V I',IAUX0,IFCFL)
C NOMBRE TOTAL DE MAILLES DE CONTACT ESCLAVES PAR CHARGEMENT
      CALL WKVECT('&&FETCRF.FCNM     ','V V I',NBCHAR,IFCNM)

C **** 14.2 BOUCLE SUR LES CHARGEMENTS POUR DETERMINER LES COUPLES
C (SD,CHAR), LES NOMBRES DE MAILLES ET NOEUDS MAITRES PAR SD
      DO 950 ICH = 1,NBCHAR
        IF (ZI(IFCFB+9*(ICH-1)).EQ.1) THEN
          JMETH=ZI(IFCFB+9*(ICH-1)+1)
          JZONE=ZI(IFCFB+9*(ICH-1)+2)
          JSUMA=ZI(IFCFB+9*(ICH-1)+3)
          JSUNO=ZI(IFCFB+9*(ICH-1)+4)
          JMACO=ZI(IFCFB+9*(ICH-1)+5)
          NZOCO=ZI(IFCFB+9*(ICH-1)+6)
          ISUCO  = 0
C BOUCLE SUR LES ZONES DE CONTACT
          DO 951 IZONE=1,NZOCO
            NBSURF = ZI(JZONE+IZONE) - ZI(JZONE+IZONE-1)
            DO 949 ISURF = 1,NBSURF
C CETTE INITIALISATION PERMET DE TESTER SI TOUTES LES MAILLES D'UNE ZONE
C APPARTIENNENT A UN SEUL SOUS-DOMAINE
              CFSD=-1
C ISUCO PILOTE LE CHANGEMENT DE SURFACE: 1=ESCLAVE, 2=MAITRE
              ISUCO  = ISUCO + 1
              NBMA   = ZI(JSUMA+ISUCO) - ZI(JSUMA+ISUCO-1)
              NBNO   = ZI(JSUNO+ISUCO) - ZI(JSUNO+ISUCO-1)
              JDECMA = ZI(JSUMA+ISUCO-1)
              JDECNO = ZI(JSUNO+ISUCO-1)

              ZI(IFCNM+ICH-1)=ZI(IFCNM+ICH-1)+NBMA
C BOUCLE SUR SES MAILLES
              DO 953 IMA=1,NBMA
                NUMMA = ZI(JMACO+JDECMA+IMA-1)
C BOUCLE SUR LES SOUS-DOMAINES
                DO 954 IDD=1,NBSD
C ILS ONT DES MAILLES DE BORD OU PAS ?
                  IF (ZI(NBRD-1+IDD).EQ.1) THEN
                    NOMGMA=ZK8(LSTBRD-1+IDD)
                    CALL JELIRA(JEXNOM(GRPMA,NOMGMA),'LONMAX',NBMABO,
     &                          K8B)
                    CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'L',IALIBD)
                    NBMABO=NBMABO-1
C BOUCLE SUR LES MAILLES DE BORD
                    DO 955 J=0,NBMABO
                      IF (ZI(IALIBD+J).EQ.NUMMA) THEN
                        IF ((CFSD.GT.0).AND.(CFSD.NE.IDD)) THEN
                          VALI(1)=NUMMA
                          VALI(2)=IZONE
                          VALI(3)=ICH
                          VALI(4)=IDD
                          VALI(5)=CFSD
                          CALL U2MESI('F','ELEMENTS5_34',5,VALI)
                        ENDIF
                        CFSD=IDD
                        GOTO 952
                      ENDIF
C FIN BOUCLE SUR LES MAILLES DE BORD DU SD
  955               CONTINUE
                  ENDIF
C SORTIE DU IF; ON A TROUVE QUE LA MAILLE DE CONTACT NUMMA EST CONCERNEE
C PAR LE SD IDD. ON VA VERIFIER QU'ELLE NE L'EST PAS AUCUN AUTRE SD
  952             CONTINUE
C FIN BOUCLE SUR LES SD
  954           CONTINUE
C FIN BOUCLE SUR LES MAILLES DE CONTACT
  953         CONTINUE
C SI UNE SURFACE DE CONTACT N'A TROUVE AUCUN SD, UTMESS_F
              IF (CFSD.GT.0) THEN
                ZI(IFCFL+(ICH-1)*NZOCOM+IZONE)=CFSD
                ZI(IFCFM+CFSD-1)=ZI(IFCFM+CFSD-1)+NBMA
                ZI(IFCFN+CFSD-1)=ZI(IFCFN+CFSD-1)+NBNO
              ELSE
                VALI(1)=ISURF
                VALI(2)=IZONE
                VALI(3)=ICH
                CALL U2MESI('F','ELEMENTS5_35',3,VALI)
              ENDIF
C FIN BOUCLE SUR LES SURFACES
  949       CONTINUE
C FIN BOUCLE SUR LES ZONES DE CONTACT
  951     CONTINUE
        ENDIF
C FIN BOUCLE SUR LES CHARGES
  950 CONTINUE
C CREATION OBJETS .FCFL, .FCFI, .FCFM ET .FCFN POUR FETI+CONTACT
      CALL JECREC(NOMFCL,'G V K24','NO','DISPERSE','VARIABLE',NBSD)
      CALL JECREC(NOMFCI,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      CALL JECREC(NOMFCM,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      CALL JECREC(NOMFCN,'G V I','NO','DISPERSE','VARIABLE',NBSD)

C **** 14.3 ON REPARCOURT LES CHARGES PAR SD CETTE FOIS POUR REMPLIR
C LES OBJETS PRECEDENTS
      DO 960 IDD=1,NBSD
C ON CHERCHE A DIMENSIONNER L'OBJET IDD DE NOMFCL
C IAUX: NOMBRE DE CHARGEMENT DE CONTACT CONCERNANT IDD
        IAUX=0
        DO 965 ICH=1,NBCHAR
          DO 967 IZONE=1,NZOCOM
            IF (ZI(IFCFL+(ICH-1)*NZOCOM+IZONE).EQ.IDD) THEN
              IAUX=IAUX+1
              GOTO 966
            ENDIF
  967     CONTINUE
  966     CONTINUE
  965   CONTINUE
        K8BUFF=ZK8(NOMSD-1+IDD)
        IF (IAUX.NE.0) THEN
C TRAVAIL PREPARATOIRE 1 POUR CALCULER DDLS DE CONTACT SUPPLEMENTAIRES
          CALL JELIRA(JEXNOM(NOMSDB,K8BUFF),'LONMAX',LFETB,K8B)
          LFETB=LFETB/2
          CALL JEVEUO(JEXNOM(NOMSDB,K8BUFF),'L',IFETB)
C CREATION NOMFCL
          CALL JECROC(JEXNOM(NOMFCL,K8BUFF))
          CALL JEECRA(JEXNOM(NOMFCL,K8BUFF),'LONMAX',IAUX,K8B)
          CALL JEVEUO(JEXNOM(NOMFCL,K8BUFF),'E',JADR)
          IIAUX1=0
          IAUX1=0
C CREATION NOMFCI
          CALL JECROC(JEXNOM(NOMFCI,K8BUFF))
          CALL JEECRA(JEXNOM(NOMFCI,K8BUFF),'LONMAX',2*IAUX,K8B)
          CALL JEVEUO(JEXNOM(NOMFCI,K8BUFF),'E',MADR)
C CREATION NOMFCM
          CALL JECROC(JEXNOM(NOMFCM,K8BUFF))
          CALL JEECRA(JEXNOM(NOMFCM,K8BUFF),'LONMAX',ZI(IFCFM+IDD-1),
     &                K8B)
          CALL JEVEUO(JEXNOM(NOMFCM,K8BUFF),'E',KADR)
          IAUX2=0
C CREATION NOMFCN
          CALL JECROC(JEXNOM(NOMFCN,K8BUFF))
          CALL JEECRA(JEXNOM(NOMFCN,K8BUFF),'LONMAX',ZI(IFCFN+IDD-1),
     &                K8B)
          CALL JEVEUO(JEXNOM(NOMFCN,K8BUFF),'E',LADR)
          IAUX3=0
          DO 961 ICH = 1,NBCHAR
            IF (ZI(IFCFB+9*(ICH-1)).EQ.1) THEN

              NZOCO=ZI(IFCFB+9*(ICH-1)+6)
              JZONE=ZI(IFCFB+9*(ICH-1)+2)
C BOUCLE SUR LES ZONES DE CONTACT
C INDDZ: PERMET DE DECIDER SI UN NOEUD COMPTE PLUSIEURS FOIS EST AU SEIN
C D'UNE MEME ZONE OU NON
              INDDZ=0
              ISUCO= 0
              LPAIRE=.TRUE.
              DO 962 IZONE=1,NZOCO
                NBSURF = ZI(JZONE+IZONE) - ZI(JZONE+IZONE-1)
                DO 959 ISURF = 1,NBSURF
C ISUCO PILOTE LE CHANGEMENT DE SURFACE: IMPAIRE=ESCLAVE, PAIRE=MAITRE
                  ISUCO  = ISUCO + 1
                  LPAIRE=.NOT.LPAIRE
                  IF (ZI(IFCFL+(ICH-1)*NZOCOM+IZONE).EQ.IDD) THEN
C ON STOCKE LE NOM DE LA CHARGE
                    K8BID=ZK8(NOMCHA-1+ICH)
                    DO 963 I=1,IAUX1
                      IF (ZK24(JADR+I-1)(1:8).EQ.K8BID) THEN
                        IIAUX1=I
                        GOTO 964
                      ENDIF
  963               CONTINUE
                    ZK24(JADR+IAUX1)=K8BID//'.CHME.LIGRE'
                    IIAUX1=IAUX1+1
                    IAUX1=IAUX1+1
C LABEL POUR NE PAS ENREGISTRER PLUSIEURS FOIS LE NOM DE LA CHARGE
  964               CONTINUE
C TRAVAIL PREPARATOIRE 2 POUR CALCULER DDLS DE CONTACT SUPPLEMENTAIRES
                    K24BID=K8BID//'.CHME.LIGRE.PRNM'
                    CALL JEVEUO(K24BID,'L',JPRNM)

                    JMETH=ZI(IFCFB+9*(ICH-1)+1)
                    JSUMA=ZI(IFCFB+9*(ICH-1)+3)
                    JSUNO=ZI(IFCFB+9*(ICH-1)+4)
                    JMACO=ZI(IFCFB+9*(ICH-1)+5)
                    JNOCO=ZI(IFCFB+9*(ICH-1)+7)
                    JDIM =ZI(IFCFB+9*(ICH-1)+8)
                    NBMA   = ZI(JSUMA+ISUCO) - ZI(JSUMA+ISUCO-1)
                    NBNO   = ZI(JSUNO+ISUCO) - ZI(JSUNO+ISUCO-1)
                    JDECMA = ZI(JSUMA+ISUCO-1)
                    JDECNO = ZI(JSUNO+ISUCO-1)
C ON REMPLI .FCFM POUR IDD
                    DO 968 IMA=1,NBMA
                      ZI(KADR+IAUX2+IMA-1)=ZI(JMACO+JDECMA+IMA-1)
  968               CONTINUE
                    IAUX2=IAUX2+NBMA
C ON REMPLI .FCFI POUR IDD
                    ZI(MADR+2*(IIAUX1-1))=ZI(IFCNM+ICH-1)
                    ZI(MADR+2*(IIAUX1-1)+1)=ZI(MADR+2*(IIAUX1-1)+1)+NBMA
C ON REMPLI .FCFN POUR IDD (EN EVITANT LES DOUBLONS POUR LES DDLS)
C ON NE COMPTE QU'UNE FOIS UN NOEUD COMMUN AUX DEUX PARTIES D'UNE
C MEME ZONE OU A PLUSIEURS ZONES
                    DO 969 INO=1,NBNO
                      J2=IAUX3+INO-2
                      JJ=ZI(JNOCO+JDECNO+INO-1)
                      ZI(LADR+J2+1)=JJ
                      DO 369 J=1,NBFETE
                        IF (ZI(JADRI+4*(J-1)).EQ.JJ) THEN
                          VALI(1)=JJ
                          VALI(2)=IZONE
                          CALL U2MESI('F','ELEMENTS5_38',2,VALI)
                        ENDIF
  369                 CONTINUE
                      DO 469 J=0,J2
                        IF (ZI(LADR+J).EQ.JJ) THEN
C NOEUD COMPTE DEUX FOIS
                          VALI(1)=I
                          VALI(2)=IZONE
                          IF (J.GE.INDDZ) THEN
C DANS LA MEME ZONE, ON NE VA PAS LE COMPTER
                            CALL U2MESI('A','ELEMENTS5_36',2,VALI)
                            GOTO 869
                          ELSE
C ENTRE ZONES, ON LE COMPTE MAIS CELA VA SANS DOUTE POSER PB A L'ALGO
C DE CONTACT
                            CALL U2MESI('A','ELEMENTS5_37',1,VALI)
                          ENDIF
                        ENDIF
  469                 CONTINUE
C POUR LES NOEUDS ESCLAVES (ISUCO IMPAIR)
C ON DETERMINE LE NOMBRE DE DDL DUS AU CONTACT SANS LES DDLS
C PHYSIQUES DEJA COMPTES
                      DDLM=0
                      K=0
                      IF (LPAIRE) THEN
                        DO 569 L=1,LFETB
                          IF (ABS(ZI(IFETB+2*(L-1))).EQ.JJ) THEN
                            IF (L.EQ.1) THEN
                              DDLM=ZI(IFETB+1)
                            ELSE
                              DDLM=ZI(IFETB+2*(L-1)+1)-
     &                             ZI(IFETB+2*(L-2)+1)
                            ENDIF
                            GOTO 669
                          ENDIF
  569                   CONTINUE
  669                   CONTINUE
                        K=-DDLM
                        DO 769 L=1,NEC30
                          IF (EXISDG(ZI(JPRNM-1+NEC*(JJ-1)+1),L)) K=K+1
  769                   CONTINUE
C MAJ DE .FETH EN NE TENANT COMPTE QUE DES DDLS DE CONTACT
                        ZI(JADRH-1+IDD)=ZI(JADRH-1+IDD)+K
                      ENDIF

C MONITORING
C                    WRITE(IFM,*)'NOEUD/DDL CONTACT',JJ,DDLM,K
  869                 CONTINUE
C FIN BOUCLE SUR LES NOEUDS
  969               CONTINUE
                    IAUX3=IAUX3+NBNO
C FIN SI ZONE CONCERNANT LE SD
                  ENDIF
C FIN BOUCLE SUR LES SURFACES
  959           CONTINUE
C FIN BOUCLE SUR LES ZONES
                INDDZ=INDDZ+IAUX3
  962         CONTINUE
C FIN SI CHARGEMENT DE CONTACT
            ENDIF
C FIN BOUCLE SUR LES CHARGES
  961     CONTINUE
C SI SD CONCERNE PAR CONTACT
        ENDIF
C FIN BOUCLE SUR LES SD
  960 CONTINUE

C***********************************************************************
C ETAPE 15 DESTRUCTION OBJETS TEMPORAIRES
C***********************************************************************
      CALL JEDETC('V','&&FETCRF',1)
      CALL JEDEMA()
      END
