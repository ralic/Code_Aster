      SUBROUTINE OP0073( IER )
      IMPLICIT  NONE
      INTEGER   IER
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/10/2006   AUTEUR MCOURTOI M.COURTOIS 
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
C
C     DEFINITION D UN OBSTACLE DE CHOC DISCRETISE PAR FACETTES
C
C-----------------------------------------------------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*8   NOMRES
      INTEGER       NBPARA, NBINFO
      PARAMETER   ( NBPARA = 3 )
      CHARACTER*8   TYPARA(NBPARA)
      CHARACTER*16  TYPRES, NOMCOM, NOPARA(NBPARA)
      CHARACTER*24  TYPE, TABK(NBPARA)
      CHARACTER*19  NOMFON
      INTEGER       LVAL, LPRO, LFON, NBVAL, NBPAIR
      INTEGER       IBID, IDTEMP, I
      INTEGER       IFM, NIV
      REAL*8        R8BID, DENC, R8DGRD, RAD, RCARTE
      COMPLEX*16    CBID
      LOGICAL       CRPROL
C     ------------------------------------------------------------------
      DATA NOPARA / 'LIEU'    , 'TYPE'    , 'FONCTION' /
      DATA TYPARA / 'K8'      , 'K24'     , 'K24'      /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV ( IFM, NIV )
C
      CALL GETRES(NOMRES,TYPRES,NOMCOM)
C
C     --- VERIFICATIONS DE PREMIER NIVEAU ---
      CALL GETVR8(' ','VALE',0,1,0,R8BID,NBVAL)
      NBVAL = -NBVAL
      IF ( (NBVAL/2)*2 .NE. NBVAL ) THEN
         CALL U2MESS('F','ALGORITH9_43')
      ENDIF
C
C --- CREATION DE LA TABLE
      CALL TBCRSD(NOMRES, 'G')
      CALL TBAJPA(NOMRES, NBPARA, NOPARA, TYPARA)
C
C --- TYPE DE L'OBSTACLE
      CALL GETVTX(' ','TYPE',0,1,1,TYPE,IBID)
C
C --- FONCTION R=F(THETA EN RADIAN) DECRIVANT LA GEOMETRIE
      NOMFON = NOMRES//'   _INITIAL'
      CRPROL = .TRUE.
C
C --- LIGNE DESCRIPTIVE
      NBINFO = NBPARA
C     ON LIMITERA AU 2 PREMIERS PARAMETRES S'IL N'Y A PAS DE FONCTION...
      TABK(1) = 'DEFIOBST'
      TABK(2) = TYPE
      TABK(3) = NOMFON
C
C ===================================================================
C
C --- DIMENSIONNEMENT DES OBJETS DE STOCKAGE ---
      RAD = R8DGRD()
      NBPAIR = NBVAL / 2
C
      IF (TYPE(1:7).EQ.'DISCRET') THEN
         IF (NBVAL.GT.0) THEN
            CALL WKVECT('&&OP0073.TEMP','V V R',NBVAL,IDTEMP)
            CALL GETVR8(' ','VALE',0,1,NBVAL,ZR(IDTEMP),IBID)
C
            CALL WKVECT(NOMFON//'.VALE','G V R',NBVAL,LVAL)
            LFON = LVAL + NBPAIR
            DO 10 I = 1,NBPAIR
               ZR(LVAL-1+I) = ZR(IDTEMP+2*(I-1)) * RAD
               ZR(LFON-1+I) = ZR(IDTEMP+2*(I-1)+1)
 10         CONTINUE
         ENDIF
C
      ELSEIF (TYPE(1:5).EQ.'GUID_') THEN
         NBPAIR = 801
         NBVAL = NBPAIR * 2
         CALL WKVECT(NOMFON//'.VALE','G V R',NBVAL,LVAL)
         LFON = LVAL + NBPAIR
C
         IF (TYPE(8:12).EQ.'CARTE') THEN
            IF (TYPE(14:17).EQ.'1300') THEN
               RCARTE = 5.34D-3
               DENC = 3.05D-3
            ELSE IF (TYPE(14:16).EQ.'900') THEN
               RCARTE = 5.325D-3
               DENC = 3.05D-3
            ENDIF
            IF (TYPE(6:6).EQ.'A'.OR.TYPE(6:6).EQ.'B'.OR.
     &          TYPE(6:6).EQ.'C'.OR.TYPE(6:6).EQ.'D') THEN
               CALL GUIDE1(RCARTE,DENC,ZR(LVAL),ZR(LFON))
            ELSEIF (TYPE(6:6).EQ.'E'.OR.TYPE(6:6).EQ.'F') THEN
               CALL GUIDE2(RCARTE,DENC,ZR(LVAL),ZR(LFON))
            ENDIF
         ELSEIF (TYPE(8:12).EQ.'CARSP') THEN
            IF (TYPE(14:17).EQ.'1300') THEN
               RCARTE = 5.59D-3
               DENC = 3.05D-3
            ELSE IF (TYPE(14:16).EQ.'900') THEN
               RCARTE = 5.59D-3
               DENC = 3.05D-3
            ENDIF
            IF (TYPE(6:6).EQ.'A'.OR.TYPE(6:6).EQ.'B'.OR.
     &          TYPE(6:6).EQ.'C'.OR.TYPE(6:6).EQ.'D') THEN
               CALL GUIDE1(RCARTE,DENC,ZR(LVAL),ZR(LFON))
            ELSEIF (TYPE(6:6).EQ.'E'.OR.TYPE(6:6).EQ.'F') THEN
               CALL GUIDE2(RCARTE,DENC,ZR(LVAL),ZR(LFON))
            ENDIF
         ELSEIF (TYPE(8:12).EQ.'GCONT') THEN
            IF(TYPE(14:17).EQ.'1300') THEN
               RCARTE=5.44D-3
            ELSE IF (TYPE(14:16).EQ.'900') THEN
               RCARTE=5.425D-3
            ENDIF
            IF(TYPE(6:6).EQ.'A'.OR.TYPE(6:6).EQ.'C'.OR.
     &         TYPE(6:6).EQ.'E'.OR.TYPE(6:6).EQ.'F') THEN
               DENC=3.05D-3
               IF(TYPE(6:6).EQ.'A'.OR.TYPE(6:6).EQ.'C') THEN
               CALL GUIDE1(RCARTE,DENC,ZR(LVAL),ZR(LFON))
               ELSE IF(TYPE(6:6).EQ.'E'.OR.TYPE(6:6).EQ.'F') THEN
               CALL GUIDE2(RCARTE,DENC,ZR(LVAL),ZR(LFON))
               ENDIF
            ELSE IF(TYPE(6:6).EQ.'B'.OR.TYPE(6:6).EQ.'D') THEN
               DENC=3.175D-3
               CALL GUIDE1(RCARTE,DENC,ZR(LVAL),ZR(LFON))
            ENDIF
         ELSEIF (TYPE(8:12).EQ.'GCOMB') THEN
            IF (TYPE(14:17).EQ.'1300') THEN
               RCARTE = 5.49D-3
            ELSE IF (TYPE(14:16).EQ.'900') THEN
               RCARTE = 5.665D-3
            ENDIF
            DO 20 I = 1,NBPAIR
               ZR(LVAL+I-1) = (I-1) * RAD * 4.5D-1
               ZR(LFON+I-1) = RCARTE
 20         CONTINUE   
         ENDIF
C
C --- CAS CRAYON_xxx
      ELSEIF (TYPE(1:7).EQ.'CRAYON_') THEN
         NBPAIR = 801
         NBVAL = NBPAIR * 2
         CALL WKVECT(NOMFON//'.VALE','G V R',NBVAL,LVAL)
         LFON = LVAL + NBPAIR
C
         IF (TYPE(8:11).EQ.'1300') THEN
            RCARTE = 4.84D-3
         ELSEIF (TYPE(8:10).EQ.'900') THEN
            RCARTE = 4.825D-3
         ENDIF
         DO 21 I = 1,NBPAIR
            ZR(LVAL+I-1) = (I-1) * RAD * 4.5D-1
            ZR(LFON+I-1) = RCARTE
 21      CONTINUE     
C
C --- CAS CERCLE, PLAN... SEUL LE .REFO ETAIT PRODUIT DANS L'ANCIENNE SD
      ELSE
         CRPROL = .FALSE.
         NBINFO = 2
      ENDIF      
C
      IF (CRPROL) THEN
         CALL WKVECT(NOMFON//'.PROL','G V K16',5,LPRO)
         ZK16(LPRO) = 'FONCTION'
         ZK16(LPRO+1) = 'LINLIN'
         ZK16(LPRO+2) = 'THETA'
         ZK16(LPRO+3) = 'R'
         ZK16(LPRO+4) = 'EE'
      ENDIF

C --- INSERTION EFFECTIVE DE LA LIGNE DANS LA TABLE
      CALL TBAJLI(NOMRES,NBINFO,NOPARA,IBID,R8BID,CBID,TABK,0)
C
      CALL JEDEMA()
      END
