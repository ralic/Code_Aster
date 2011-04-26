      SUBROUTINE IRGMMA(NOMAIN, NOMAOU, NBMAT, NUMMAI, BASZ,
     &                  NOBJ, NBEL, VERSIO)
      IMPLICIT NONE
      CHARACTER*8   NOMAIN, NOMAOU
      INTEGER       NBMAT, NUMMAI(*), VERSIO
      CHARACTER*(*) BASZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     TRANSFORME LE MAILLAGE "NOMAIN" EN UN MAILLAGE "NOMAOU"
C     LE MAILLAGE "NOMAOU" NE POSSEDE QUE DES MAILLES DE TYPE
C     POI1, SEG2, TRIA3, TETRA4 EN VERSION 1.0
C     + QUAD4, PENTA6, PYRAM5, HEXA8 EN VERSIO 1.2 (VOIR IRGMTB)
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      I, IMA, NBMA, NBMAIL, IFM, NIV,
     &             INO, IMA2, IMAV, IATYMA, JREFE, JTITR
      INTEGER      JTYPM, JDIME, JOPT, JNPT,
     &             NBMAC, JMAIL, IM, JNUMOL
      LOGICAL      LOGIC
      CHARACTER*1  BASE
      CHARACTER*8  K8B, NOMG, TYPM, TYPM2
      CHARACTER*24 NOMMAI, TYPMAI, CONNEX, NODIME, NOMNOE,
     &             COOVAL, COODSC, COOREF, TITRE, NUMOLD
      CHARACTER*24 TYPMAV, CONNEV, NODIMV, NOMNOV,
     &             COOVAV, COODSV, COOREV
      CHARACTER*24 VALK(2)
      INTEGER      IND, NUMEL, NBCR, NBP
      INTEGER      NBMMAX
      PARAMETER   (NBMMAX = 9999999)
C     --- TABLEAU DE DECOUPAGE
      INTEGER    NTYELE,MAXEL,MAXNO
      PARAMETER (NTYELE = 28)
      PARAMETER (MAXEL  = 48)
      PARAMETER (MAXNO  =  8)
      INTEGER             TDEC(NTYELE,MAXEL,MAXNO)
      INTEGER                   TYPD(NTYELE,3)
      INTEGER    VALI(3)
C     NBRE, POINTEURS, NOM D'OBJET POUR CHAQUE TYPE D'ELEMENT
      INTEGER      NBEL(NTYELE),JEL(NTYELE),IMPR
      CHARACTER*24 NOBJ(NTYELE)
C     ------------------------------------------------------------------
C
      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()
C
C --- INIT
      DO 101 I=1,NTYELE
         NBEL(I) = 0
         JEL(I)  = 0
 101  CONTINUE
C
C --- TABLEAU DES INFOS DE DECOUPAGE
      CALL IRGMTB(TDEC,TYPD,VERSIO)
C
      BASE = BASZ
C
      NOMNOV = NOMAIN//'.NOMNOE         '
      TYPMAV = NOMAIN//'.TYPMAIL        '
      CONNEV = NOMAIN//'.CONNEX         '
      NODIMV = NOMAIN//'.DIME           '
      COOVAV = NOMAIN//'.COORDO    .VALE'
      COODSV = NOMAIN//'.COORDO    .DESC'
      COOREV = NOMAIN//'.COORDO    .REFE'
C
      NOMMAI = NOMAOU//'.NOMMAI         '
      NOMNOE = NOMAOU//'.NOMNOE         '
      TYPMAI = NOMAOU//'.TYPMAIL        '
      CONNEX = NOMAOU//'.CONNEX         '
      NODIME = NOMAOU//'.DIME           '
      COOVAL = NOMAOU//'.COORDO    .VALE'
      COODSC = NOMAOU//'.COORDO    .DESC'
      COOREF = NOMAOU//'.COORDO    .REFE'
      TITRE  = NOMAOU//'           .TITR'
      NUMOLD = NOMAOU//'.NUMOLD         '
C
      CALL WKVECT ( TITRE, BASE//' V K80', 1, JTITR )
      ZK80(JTITR) = 'MAILLAGE CREE PAR IRGMMA POUR GMSH'
C
      CALL JEVEUO ( TYPMAV, 'L', JTYPM )
      CALL JEVEUO ( NODIMV, 'L', JDIME )
      NBMA = ZI(JDIME+3-1)
C
      LOGIC = .FALSE.
C
      IF ( NBMAT .NE. 0 ) THEN
         NBMAC = NBMAT
         CALL WKVECT ( '&&IRGMMA.NUME_MAILLE', 'V V I', NBMAC, JMAIL )
         DO 20 IMA = 1, NBMAC
            ZI(JMAIL+IMA-1) = NUMMAI(IMA)
 20      CONTINUE
      ELSE
         NBMAC = NBMA
         CALL WKVECT ( '&&IRGMMA.NUME_MAILLE', 'V V I', NBMAC, JMAIL )
         DO 22 IMA = 1, NBMAC
            ZI(JMAIL+IMA-1) = IMA
 22      CONTINUE
      ENDIF
C
C --- COMBIEN D'ELEMENTS DE CHAQUE TYPE VA-T-ON CREER ?
      DO 10 IM = 1 , NBMAC
         IMA = ZI(JMAIL+IM-1)
C
         IND=ZI(JTYPM+IMA-1)
         CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',IND),TYPM)
C
C ---    NUMEL = EN QUOI ON DECOUPE, NBCR = COMBIEN ON EN CREER
         NUMEL = TYPD(IND,1)
         NBCR = TYPD(IND,2)
         IF(NUMEL.NE.0) THEN
            NBEL(NUMEL)=NBEL(NUMEL)+NBCR
         ELSE
            CALL U2MESK('A','ALGELINE_64',1,TYPM)
         ENDIF
 10   CONTINUE
C
      NBMAIL = 0
      IMPR   = 0
      DO 102 I=1,NTYELE
         NBMAIL = NBMAIL + NBEL(I)
C
         IF(NOBJ(I).NE.' ') THEN
            CALL WKVECT ( NOBJ(I), 'V V I', MAX(1,NBEL(I)), JEL(I) )
            IF(NIV.GE.1)THEN
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',I),        TYPM)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',TYPD(I,1)),TYPM2)
               NBCR=TYPD(I,2)
               NBP =TYPD(I,3)
               IF(NBEL(I).GT.0) THEN
                 IF(IMPR.EQ.0) THEN
                    CALL U2MESS('I','ALGELINE5_54')
                    IMPR=1
                 ENDIF
                 VALK(1) = TYPM
                 VALK(2) = TYPM2
                 VALI(1) = NBEL(I)
                 VALI(2) = NBCR
                 VALI(3) = NBP
                 CALL U2MESG('I','ALGELINE5_55',2,VALK,3,VALI,0,0.D0)
               ENDIF
            ENDIF
         ENDIF
 102  CONTINUE
C
      CALL WKVECT ( NUMOLD, 'V V I', MAX(1,NBMAIL), JNUMOL )
C
      CALL JEDUPO ( NODIMV, BASE, NODIME, LOGIC )
      CALL JEDUPO ( NOMNOV, BASE, NOMNOE, LOGIC )
      CALL JEDUPO ( COOVAV, BASE, COOVAL, LOGIC )
      CALL JEDUPO ( COODSV, BASE, COODSC, LOGIC )
      CALL JEDUPO ( COOREV, BASE, COOREF, LOGIC )
C
      CALL JEVEUO ( COOREF, 'E', JREFE )
      ZK24(JREFE) = NOMAOU
C
      CALL JEVEUO ( NODIME, 'E', JDIME )
      ZI(JDIME+3-1) = NBMAIL

C ----------------------------------------------------------------------
C     LE '.NOMMAI' ET LE '.CONNEX'
C ----------------------------------------------------------------------
      CALL JECREO ( NOMMAI, BASE//' N K8' )
      CALL JEECRA ( NOMMAI, 'NOMMAX', NBMAIL, ' ' )

      CALL WKVECT ( TYPMAI, BASE//' V I', NBMAIL, IATYMA )

      CALL JECREC ( CONNEX, BASE//' V I', 'NU', 'CONTIG', 'VARIABLE',
     &                                                    NBMAIL )
C#MC  1*NBMAIL NE SUFFIT PAS ?
      CALL JEECRA ( CONNEX, 'LONT', NTYELE*NBMAIL, ' ' )

      DO 103 I=1,NTYELE
         NBEL(I) = 0
 103  CONTINUE
      IMAV  = 0
C
      DO 100 IM = 1 , NBMAC
         IMA = ZI(JMAIL+IM-1)
C
         IND=ZI(JTYPM+IMA-1)
         CALL JENUNO ( JEXNUM('&CATA.TM.NOMTM',IND), TYPM )
         CALL JEVEUO ( JEXNUM(CONNEV,IMA), 'L', JOPT )
C
C ---    NUMEL = EN QUOI ON DECOUPE, NBCR = COMBIEN ON EN CREER
C        NBP = NBRE DE POINTS PAR ELEMENTS CREES
         NUMEL = TYPD(IND,1)
         NBCR  = TYPD(IND,2)
         NBP   = TYPD(IND,3)
         DO 110 I = 1 , NBCR
            IMAV = IMAV + 1
            IF(IMAV.GT.NBMMAX)THEN
               CALL CODENT(NBMMAX,'G',K8B)
               CALL U2MESK('F','ALGELINE_65',1,K8B)
            ENDIF
            NOMG = 'M       '
            CALL CODENT ( IMAV, 'G', NOMG(2:8) )
            CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
            CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
            ZI(IATYMA-1+IMA2) = NUMEL
C    STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
            ZI(JNUMOL-1+IMA2)=IMA
C
            CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBP, K8B )
            CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
            DO 115 INO = 1 , NBP
               ZI(JNPT-1+INO) = ZI(JOPT-1+TDEC(IND,I,INO))
 115        CONTINUE
            NBEL(NUMEL) = NBEL(NUMEL) + 1
            ZI(JEL(NUMEL)-1+NBEL(NUMEL)) = IMAV
 110     CONTINUE
C
 100  CONTINUE
C
      CALL JEDETR ( '&&IRGMMA.NUME_MAILLE' )
C
      CALL JEDEMA()
C
      END
