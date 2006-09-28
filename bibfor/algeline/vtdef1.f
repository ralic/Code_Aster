      SUBROUTINE VTDEF1(CHPOUT,CHPIN,BASE,TYPC,LFETI)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C     DEFINITION DE LA STRUCTURE D'UN CHAM_NO OU CHAM_ELEM "CHPOUT"
C                    QUI S'APPUIE SUR LA MEME NUMEROTATION QUE "CHPIN",
C     LE CHAM_... "CHPOUT" EST CREEE SUR LA BASE "BASE".
C     LE CHAM_... "CHPOUT" EST A COEFFICIENTS "TYPE".
C     ------------------------------------------------------------------
C IN : CHPOUT : NOM DU CHAM_NO OU CHAM_ELEM A CREER
C IN : CHPIN  : NOM DU CHAM_NO OU CHAM_ELEM MODELE
C IN : BASE   : NOM DE LA BASE SUR LAQUELLE LE CHAM_... DOIT ETRE CREER
C IN : TYPC   : TYPE DES VALEURS DU CHAM_... A CREER
C                    'R'  ==> COEFFICIENTS REELS
C                    'C'  ==> COEFFICIENTS COMPLEXES
C                    ' '  ==> COEFFICIENTS DU TYPE DU CHAM_... CHPIN
C IN : LFETI  : FLAG INDIQUANT SI LA METHODE EST DE FETI OU NON
C     ------------------------------------------------------------------
C     PRECAUTIONS D'EMPLOI :
C       1) LE CHAM_... "CHPOUT" NE DOIT PAS EXISTER
C       2) LES COEFFICIENTS DU CHAM_... "CHPOUT" NE SONT PAS AFFECTES
C     -----------------------------------------------------------------
C     ASTER INFORMATIONS:
C       16/01/04 (OB): CREATION.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*(*)     CHPOUT,CHPIN,BASE,TYPC
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

C DECLARATION VARIABLES LOCALES
      INTEGER      NBVAL,IVAL,LCHPOU,LCHPIN,IBID,IER,LCHP,NBVAL1
      CHARACTER*1  CLASSE,TYPE
      CHARACTER*4  TYCH ,DOCU
      CHARACTER*8  CBID
      CHARACTER*19 CH19
      CHARACTER*24 VALE,REFE,DESC,CELK,TAMP
      LOGICAL      LFETI

C     ------------------------------------------------------------------
      DATA REFE / '                   .REFE' /
      DATA CELK / '                   .CELK' /
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CLASSE = BASE(1:1)
      CH19 = CHPIN

      CALL DISMOI('F','TYPE_CHAMP',CH19,'CHAMP',IBID,TYCH,IER)

      IF (TYCH.EQ.'NOEU') THEN
         DOCU='CHNO'
         TAMP = REFE
         DESC(20:24)='.DESC'
         VALE(20:24)='.VALE'
      ELSEIF (TYCH(1:2).EQ.'EL') THEN
         DOCU='CHML'
         DESC(20:24)='.CELD'
         VALE(20:24)='.CELV'
         TAMP = CELK
      ELSE
        CALL U2MESS('F','UTILITAI_21')
      ENDIF

C     --------------------------- REFE --------------------------------
C     --------------------------- CELK --------------------------------
C     --- RECUPERATION DES INFORMATIONS DE CHPIN ---
      TAMP(1:19) = CHPIN
      CALL JELIRA(TAMP,'LONMAX',NBVAL,CBID)
      CALL JEVEUO(TAMP,'L',LCHPIN)

C     --- AFFECTATION DES INFORMATIONS A CHPOUT ---
      TAMP(1:19) = CHPOUT
      CALL JECREO(TAMP,CLASSE//' V K24')
      CALL JEECRA(TAMP,'LONMAX',NBVAL,'  ')
      CALL JEVEUO(TAMP,'E',LCHPOU)
      NBVAL1=NBVAL-1
      DO 10 IVAL = 0,NBVAL1
         ZK24(LCHPOU+IVAL) = ZK24(LCHPIN+IVAL)
 10   CONTINUE

C     --- LIBERATION ---
      TAMP(1:19) = CHPIN
      TAMP(1:19) = CHPOUT

C     --------------------------- DESC --------------------------------
C     --- RECUPERATION DES INFORMATIONS DU DESCRIPTEUR CHPIN ---
      DESC(1:19) = CHPIN
      CALL JELIRA(DESC,'LONMAX',NBVAL,CBID)
      CALL JEVEUO(DESC,'L',LCHPIN)

C     --- AFFECTATION DES INFORMATIONS DE DESCRIPTEUR CHPOUT ---
      DESC(1:19) = CHPOUT
      CALL JECREO(DESC,CLASSE//' V I')
      CALL JEECRA(DESC,'LONMAX',NBVAL,'  ')
      NBVAL1=NBVAL-1

      CALL JEECRA(DESC,'DOCU',NBVAL,DOCU)

      CALL JEVEUO(DESC,'E',LCHPOU)
      DO 20 IVAL = 0,NBVAL1
         ZI(LCHPOU+IVAL) = ZI(LCHPIN+IVAL)
   20 CONTINUE

C     --- LIBERATION ---
      DESC(1:19) = CHPIN
      DESC(1:19) = CHPOUT

C     --------------------------- VALE --------------------------------
C     ------------- CREATION DE L'OBJET SIMPLE DES VALEURS -------------
C     --- TYPE DES VALEURS, LONGUEUR D'UN VECTEUR ---
      VALE(1:19) = CHPIN
      TYPE = TYPC(1:1)
      IF (TYPE.EQ.' ') CALL JELIRA(VALE,'TYPE',IVAL,TYPE)
      CALL JELIRA(VALE,'LONMAX',NBVAL,CBID)
      IF (LFETI) THEN
        CALL JEVEUO(VALE,'L',LCHPIN)
        NBVAL1=NBVAL-1
      ENDIF
      VALE(1:19) = CHPOUT
      CALL JECREO(VALE,CLASSE//' V '//TYPE)
      CALL JEECRA(VALE,'LONMAX',NBVAL,CBID)
      CALL JEVEUO(VALE,'E',LCHP)

C SI FETI INITIALISATION DU .VALE DIRECTEMENT SANS PASSER PAR
C COPISD+COPICH+JEDUPO LORS D'UN APPEL DANS RESOUD
      IF (LFETI) THEN
        IF (TYPE.EQ.'R') THEN
          DO 30 IVAL = 0,NBVAL1
             ZR(LCHP+IVAL) = 0.D0
   30     CONTINUE
        ELSE IF (TYPE.EQ.'C') THEN
          DO 40 IVAL = 0,NBVAL1
             ZC(LCHP+IVAL) = DCMPLX(0.D0,0.D0)
   40     CONTINUE
        ENDIF
      ENDIF

C     --- CHANGER LA GRANDEUR ---
      CALL SDCHGD(CHPOUT,TYPE)
      CALL JEDEMA()
      END
