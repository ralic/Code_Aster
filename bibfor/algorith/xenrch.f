      SUBROUTINE XENRCH(IFM,NIV,NOMA,CNSLT,CNSLN,PFI,VOR,ORI,FISS)
      IMPLICIT NONE
      REAL*8        PFI(3),VOR(3),ORI(3)
      INTEGER       IFM,NIV
      CHARACTER*8   NOMA,FISS
      CHARACTER*19  CNSLT,CNSLN
         
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
C     TOLE CRP_20
C
C         CALCUL DE L'ENRICHISSEMENT ET DES POINTS DU FOND DE FISSURE
C
C
C    ENTREE :
C              IFM    :   FICHIER D'IMPRESSION
C              NOMA   :   OBJET MAILLAGE
C              CNSLT  :   LEVEL-SET TANGENTE (TRACE DE LA FISSURE)
C              CNSLN  :   LEVEL-SET NORMALE  (PLAN DE LA FISSURE)
C              PFI    :   POINT INITIAL SOUHAITÉ DU FOND DE FISSURE
C              VOR    :   VECTEUR D'ORIENTATION DU FOND DE FISSURE
C              ORI    :   POINT D'ANCRAGE DE CE VECTEUR
C
C    SORTIE : 
C              FISS   :    SD_FISS
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IRET,NBNO,INO,IMAE,IMA,EN,EM,NMAFON,JFON,J,NFON,JMA3
      INTEGER       JCOOR,JCONX1,JCONX2,JLTSV,JLNSV,JSTANO,JABSC,JINDIC
      INTEGER       NMAABS,NBNOMA,I,IA,AR(12,2),NMAFIS,EM1,EM2,JMAEN1
      INTEGER       NUNO,JMAFIS,NXMAFI,JMAFON,JFO,K,JMA1,JMA2,NXPTFF
      INTEGER       IM1,IM2,IM3,IN,NMAEN1,NMAEN2,NMAEN3,JMAEN2,JMAEN3
      CHARACTER*8   K8BID
      CHARACTER*12  K12
      CHARACTER*24  MAFIS,LISNO,STANO
      CHARACTER*32  JEXATR,JEXNUM
      REAL*8        M(3),P(3),D,DIMIN,Q(4),ARMIN,PADIST
      LOGICAL       DEBUG
      PARAMETER    (NXMAFI=5000)
      PARAMETER    (NXPTFF=500)
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
C
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8BID,IRET)
C
      CALL JEVEUO(CNSLT//'.CNSV','L',JLTSV)
      CALL JEVEUO(CNSLN//'.CNSV','L',JLNSV)
C
C     VOIR ALGORITHME DÉTAILLÉ DANS BOOK II (16/12/03)
C
C-------------------------------------------------------------------
C    1) ON RESTREINT LA ZONE D'ENRICHISSEMENT AUTOUR DE LA FISSURE
C-------------------------------------------------------------------

      WRITE(IFM,*)'XENRCH-1) RESTRICTION DE LA ZONE D ENRICHISSEMENT'

      MAFIS='&&XENRCH.MAFIS'
      CALL WKVECT(MAFIS,'V V I',NXMAFI,JMAFIS)
C     ATTENTION, MAFIS EST LIMITÉ À NXMAFI MAILLES

      CALL XMAFIS(NOMA,CNSLN,NXMAFI,MAFIS,NMAFIS)
      WRITE(IFM,*)'NOMBRE DE MAILLES DE LA ZONE FISSURE :',NMAFIS
      IF (NIV.GT.2) THEN
        WRITE(IFM,*)'NUMERO DES MAILLES DE LA ZONE FISSURE'
        DO 110 IMAE=1,NMAFIS
          WRITE(6,*)' ',ZI(JMAFIS-1+IMAE)
 110    CONTINUE
      ENDIF

C--------------------------------------------------------------------
C    2°) ON ATTRIBUE LE STATUT DES NOEUDS DE GROUP_ENRI
C--------------------------------------------------------------------

      WRITE(IFM,*)'XENRCH-2) ATTRIBUTION DU STATUT DES NOEUDS '//
     &                          'DE GROUPENRI'

C     CREATION DU VECTEUR ENRICH DES NOEUDS
      STANO='&&XENRCH.STANO'
      CALL WKVECT(STANO,'V V I',NBNO,JSTANO)

C     ON INITIALISE POUR TOUS LES NOEUDS DU MAILLAGE ENR À 0
      DO 200 INO=1,NBNO
        ZI(JSTANO-1+(INO-1)+1)=0
 200  CONTINUE

      CALL XSTANO(NOMA,LISNO,NMAFIS,JMAFIS,CNSLT,CNSLN,STANO)

      IF (NIV.GT.2) THEN
        DO 299 INO=1,NBNO
          WRITE(IFM,*)'INO ET ENR',INO,ZI(JSTANO-1+INO)
 299    CONTINUE
      ENDIF
      
C--------------------------------------------------------------------
C    3°) ON ATTRIBUE LE STATUT DES MAILLES DE MAFIS
C        ET ON CONSTRUIT LES MAILLES DE MAFOND (NB MAX = NMAFIS)
C--------------------------------------------------------------------

      WRITE(IFM,*)'XENRCH-3) ATTRIBUTION DU STATUT DES MAILLES DE MAFIS'

      CALL WKVECT('&&XENRCH.MAFOND','V V I',NMAFIS,JMAFON)
      CALL WKVECT('&&XENRCH.MAENR1','V V I',NMAFIS,JMAEN1)      
      CALL WKVECT('&&XENRCH.MAENR2','V V I',NMAFIS,JMAEN2)      
      CALL WKVECT('&&XENRCH.MAENR3','V V I',NMAFIS,JMAEN3)

      I=0
      IM1=0
      IM2=0
      IM3=0
C     BOUCLE SUR LES MAILLES DE MAFIS

      DO 310 IMA=1,NMAFIS
        EM=0
        EM1=0
        EM2=0
        NMAABS=ZI(JMAFIS-1+(IMA-1)+1)
        NBNOMA=ZI(JCONX2+NMAABS) - ZI(JCONX2+NMAABS-1)
        DO 311 IN=1,NBNOMA
          NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+IN-1)
          EN=ZI(JSTANO-1+(NUNO-1)+1)
          IF (EN.EQ.1.OR.EN.EQ.3) EM1=EM1+1
          IF (EN.EQ.2.OR.EN.EQ.3) EM2=EM2+1
 311    CONTINUE
        IF (EM1.GE.1) EM=1
        IF (EM2.GE.1) EM=2
        IF (EM1.GE.1.AND.EM2.GE.1) EM=3
        IF (EM2.EQ.NBNOMA) THEN
C         MAILLE RETENUE POUR MAFOND (TS LS NOEUDS SONT 'CARRÉS')
          I=I+1
          ZI(JMAFON-1+I)=NMAABS
        ENDIF
C       ON RÉCUPÈRE LES NUMEROS DES MAILLES ENRICHIES
        IF (EM.EQ.1)   THEN
          IM1=IM1+1     
          ZI(JMAEN1-1+IM1)=NMAABS
        ELSEIF (EM.EQ.2)   THEN
          IM2=IM2+1
          ZI(JMAEN2-1+IM2)=NMAABS
        ELSEIF (EM.EQ.3)   THEN
          IM3=IM3+1
          ZI(JMAEN3-1+IM3)=NMAABS
        ENDIF
 310  CONTINUE

      NMAFON=I
      NMAEN1=IM1
      NMAEN2=IM2
      NMAEN3=IM3

      IF (NIV.GT.2) THEN
        WRITE(IFM,*)'NOMBRE DE MAILLES DE MAFON :',NMAFON
        DO 320 IMA=1,NMAFON
          WRITE(IFM,*)'MA',ZI(JMAFON-1+IMA)
 320    CONTINUE
        WRITE(IFM,*)'NOMBRE DE MAILLES DE MAENR1 :',NMAEN1     
        DO 321 IMA=1,IM1
          WRITE(IFM,*)' ',ZI(JMAEN1-1+IMA)
 321    CONTINUE
        WRITE(IFM,*)'NOMBRE DE MAILLES DE MAENR2 :',NMAEN2    
        DO 322 IMA=1,IM2
          WRITE(IFM,*)' ',ZI(JMAEN2-1+IMA)
 322    CONTINUE
        WRITE(IFM,*)'NOMBRE DE MAILLES DE MAENR3 :',NMAEN3      
        DO 323 IMA=1,IM3
          WRITE(IFM,*)' ',ZI(JMAEN3-1+IMA)
 323    CONTINUE
      ENDIF

C--------------------------------------------------------------------
C    4°) RECHERCHES DES POINTS DE FONFIS (ALGO BOOK I 18/12/03)
C--------------------------------------------------------------------

      WRITE(IFM,*)'XENRCH-4) RECHERCHE DES POINTS DE FONFIS'

      CALL WKVECT('&&XENRCH.FONFIS','V V R',4*NXPTFF,JFON)

      CALL XPTFON(NOMA,NMAFON,CNSLT,CNSLN,JMAFON,NXPTFF,JFON,NFON,ARMIN)
      WRITE(K12,'(E12.5)') ARMIN 
      WRITE(IFM,*)'LA LONGUEUR DE LA PLUS PETITE ARETE '//
     &      'DU MAILLAGE EST '//K12//'.'
      WRITE(IFM,*)'NOMBRE DE POINTS DE FONFIS',NFON

C--------------------------------------------------------------------
C    5°) ORIENTATION DES POINTS DE FONFIS (ALGO BOOK I 19/12/03)
C--------------------------------------------------------------------

      WRITE(IFM,*)'XENRCH-5) ORIENTATION DE FONFIS'

      CALL XORIFF(NFON,JFON,PFI,ORI,VOR,DIMIN)

      WRITE(IFM,*)'DISTANCE ENTRE PFON_INI DEMANDE ET TROUVE :',DIMIN

C--------------------------------------------------------------------
C    6°) CALCUL DE L'ABSCISSE CURVILIGNE : S
C--------------------------------------------------------------------

      WRITE(IFM,*)'XENRCH-6) CALCUL DES ABSCISSES CURVILIGNES'

      CALL WKVECT('&&XENRCH.ABSC','V V R',NFON,JABSC)

C     INITIALISATIONS
      I=1
      DO 600 K=1,3
        P(K)=ZR(JFON-1+4*(I-1)+K)
 600  CONTINUE
      ZR(JABSC-1+(I-1)+1)=0

C     CALCUL DE LA DISTANCE ENTRE CHAQUE POINT ET
C     CALCUL DE L'ABSCISSE CURVILIGNE EN SOMMANT CES DISTANCES
      DO 610 I=2,NFON
         DO 611 K=1,3
          M(K)=ZR(JFON-1+4*(I-1)+K)
 611    CONTINUE
        ZR(JABSC-1+(I-1)+1)=ZR(JABSC-1+(I-2)+1)+PADIST(3,M,P)
        P(1)=M(1)
        P(2)=M(2)
        P(3)=M(3)
 610  CONTINUE

C     ON REMPLACE LES VALEURS DE THETA PAR CELLES DE S
      DO 620 I=1,NFON
        ZR(JFON-1+4*(I-1)+4)=ZR(JABSC-1+(I-1)+1)
 620  CONTINUE

      WRITE(IFM,*)'COORDONNEES DES POINTS DE FONFIS'
      WRITE(6,697)
      DO 699 I=1,NFON
        Q(1)=ZR(JFON-1+4*(I-1)+1)
        Q(2)=ZR(JFON-1+4*(I-1)+2)
        Q(3)=ZR(JFON-1+4*(I-1)+3)
        Q(4)=ZR(JFON-1+4*(I-1)+4)
        WRITE(6,698)(Q(K),K=1,4)
 699  CONTINUE
 697  FORMAT(7X,'X',9X,'Y',9X,'Z',9X,'S')
 698  FORMAT(2X,4(F8.3,2X))


C------------------------------------------------------------------
C     FIN
C------------------------------------------------------------------

C     CREATION D'UN INDICATEUR : PRESENCE GR_ENR (0 OU 1) +  NMAEN
      CALL WKVECT(FISS//'.MAILFISS .INDIC','G V I',6,JINDIC)
      DO 750 I=1,6
        ZI(JINDIC-1+I)=0
 750  CONTINUE
C     ENREGISTREMENT DES GROUP_MA DE SORTIE SI NON VIDE
      IF (NMAEN1.NE.0) THEN
        CALL WKVECT(FISS//'.MAILFISS  .HEAV','G V I',NMAEN1,JMA1)
        ZI(JINDIC)=1
        ZI(JINDIC+1)=NMAEN1
        DO 700 I=1,NMAEN1
           ZI(JMA1-1+I)=ZI(JMAEN1-1+I)
 700    CONTINUE
      ENDIF
      IF (NMAEN2.NE.0) THEN
        CALL WKVECT(FISS//'.MAILFISS  .CTIP','G V I',NMAEN2,JMA2)
        ZI(JINDIC+2)=1
        ZI(JINDIC+3)=NMAEN2
        DO 710 I=1,NMAEN2
           ZI(JMA2-1+I)=ZI(JMAEN2-1+I)
 710    CONTINUE
      ENDIF
      IF (NMAEN3.NE.0) THEN
        CALL WKVECT(FISS//'.MAILFISS  .HECT','G V I',NMAEN3,JMA3)
        ZI(JINDIC+4)=1
        ZI(JINDIC+5)=NMAEN3
        DO 720 I=1,NMAEN3
           ZI(JMA3-1+I)=ZI(JMAEN3-1+I)
 720    CONTINUE
      ENDIF

C     ENREGISTREMENT DES COORD ET DES ABS CURV
      CALL WKVECT(FISS//'.FONDFISS','G V R',4*NFON,JFO)
      DO 800 I=1,NFON
        DO 810 K=1,4
          ZR(JFO-1+4*(I-1)+K)=ZR(JFON-1+4*(I-1)+K)
 810    CONTINUE
 800  CONTINUE

C      A VOIR
      CALL JEDETR ('&&XENRCH.FONFIS')
      CALL JEDETR ('&&XENRCH.ABSC')
      CALL JEDETR ('&&XENRCH.FONFIS')
      CALL JEDETR ('&&XENRCH.MAFOND')

      WRITE(IFM,*)'XENRCH-7) FIN DE XENRCH'

      CALL JEDEMA()
      END
