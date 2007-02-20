      SUBROUTINE JELIRA ( NOMLU , CATR , IVAL , CVAL )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 19/02/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_720 CFT_726 CRP_18 CRP_6 CRS_508  CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER *(*)      NOMLU , CATR        , CVAL
      INTEGER                            IVAL
C     ------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     ------------------------------------------------------------------
      INTEGER          ISSTAT
      COMMON /ICONJE/  ISSTAT
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ   
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      CHARACTER*4      KSTAT
      COMMON /KSTAJE/  KSTAT
C     ------------------------------------------------------------------
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,             IDLONG     ,
     &               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,             IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C     ------------------------------------------------------------------
      CHARACTER*75     CMESS
      CHARACTER*32     NOM32 , NOML32
      CHARACTER*33     CVA
      CHARACTER*1      GENRI
      CHARACTER*8      CATRLU
      INTEGER          LCV , ICRE , IRET , SHIFTR
      LOGICAL          LCONST , LCONTI , LLONG , LLUTI , LCOL
C DEB ------------------------------------------------------------------

      IPGCEX = IPGC
      NOML32 = NOMLU
      CATRLU = CATR
C
      IVA  = -1
      CVA  = ' '
      LCV  = 0
      ICRE = 0
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IC = ICLAS
      IF ( IRET .EQ. 0 ) THEN
        CALL U2MESK('F','JEVEUX_26',1,NOML32(1:24))
      ELSE IF ( IRET .EQ. 1 ) THEN
        LCOL = .FALSE.
        IC = ICLAOS
        ID = IDATOS
        LCONST = .TRUE.
        LCONTI = .TRUE.
        IF ( NOML32(25:32) .NE. '        ' ) THEN
          CMESS = 'APPEL INVALIDE POUR L''OBJET SIMPLE >'//NOML32(1:24)
     &            //'<'
          CALL U2MESK('F','JEVEUX_01',1,CMESS)
        ENDIF
      ELSE
        LCOL = .TRUE.
        IPGC = -2
        IC = ICLACO
        CALL JJALLC ( IC , IDATCO , 'L' , IBACOL )
        IF ( NOML32(25:32) .NE. '        ' ) THEN
           IRET = 3
           CALL JJCROC ( NOML32(25:32) , ICRE )
           IF ( IDATOC .EQ. 0 ) THEN
             CALL U2MESK('F','JEVEUX_30',1,NOML32(1:24))
           ENDIF
        ENDIF
        IXDESO  = ISZON ( JISZON + IBACOL + IDDESO )
        ID      = IXDESO
        IXIADD  = ISZON ( JISZON + IBACOL + IDIADD )
        LCONTI  = ( IXIADD .EQ. 0 )
        IXLONG  = ISZON ( JISZON + IBACOL + IDLONG )
        IXLONO  = ISZON ( JISZON + IBACOL + IDLONO )
        IXLUTI  = ISZON ( JISZON + IBACOL + IDLUTI )
        IXNOM   = ISZON ( JISZON + IBACOL + IDNOM  )
        IXNUM   = ISZON ( JISZON + IBACOL + IDNUM  )
        LCONST  = (IXLONG .EQ. 0 )
        IF ( IRET .EQ. 2 ) THEN
          IF ( CATRLU .EQ. 'ACCES   ' ) THEN
            IF ( IXNOM .NE. 0 ) THEN
              NOM32 = RNOM ( JRNOM(IC) + IXNOM )
              CVA = 'NO'
              LCV = 2
              IF ( INDEX (NOM32,'$$') .EQ. 0  ) THEN
                 CVA = CVA(1:2)//' '//NOM32(1:24)
                 LCV = LCV + 1 + 24
              ENDIF
            ELSE
              CVA = 'NU'
              LCV = 2
            ENDIF
          ELSE IF ( CATRLU .EQ. 'STOCKAGE' ) THEN
            IF ( IXIADD .NE. 0 ) THEN
              NOM32 = RNOM ( JRNOM(IC) + IXIADD )
              CVA = 'DISPERSE'
              LCV = 8
              IF ( INDEX (NOM32,'$$') .EQ. 0  ) THEN
                 CVA = CVA(1:8)//' '//NOM32(1:24)
                 LCV = LCV + 1 + 24
              ENDIF
            ELSE
              CVA = 'CONTIG'
              LCV = 6
            ENDIF
          ELSE IF ( CATRLU .EQ. 'MODELONG' ) THEN
            IF ( .NOT. LCONST ) THEN
              NOM32 = RNOM ( JRNOM(IC) + IXLONG )
              CVA = 'VARIABLE'
              LCV = 8
              IF ( INDEX (NOM32,'$$') .EQ. 0  ) THEN
                 CVA = CVA(1:8)//' '//NOM32(1:24)
                 LCV = LCV + 1 + 24
              ENDIF
            ELSE
              CVA = 'CONSTANT'
              LCV = 8
            ENDIF
          ELSE IF ( CATRLU .EQ. 'LONT    ' .AND. LCONTI ) THEN
            IVA  = LONO ( JLONO(IC) + IXDESO )
          ELSE IF ( CATRLU .EQ. 'NMAXOC  ' ) THEN
            IVA  = ISZON ( JISZON + IBACOL + IVNMAX )
          ELSE IF ( CATRLU .EQ. 'NUTIOC  ' ) THEN
            IF ( IXNOM .GT. 0 ) THEN
              IVA  = LUTI ( JLUTI(IC) + IXNOM )
            ELSE IF ( IXNUM .GT. 0 ) THEN
              IBNUM = IADM ( JIADM(IC) + 2*IXNUM-1 )
              IVA   = ISZON ( JISZON + IBNUM - 1 + 2 )
            ENDIF
          ELSE IF ( CATRLU .EQ. 'NOMUTI  ' ) THEN
            IF ( IXNOM .GT. 0 ) THEN
              IVA  = LUTI ( JLUTI(IC) + IXNOM )
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF ( IVA  .GE. 0 .OR. LCV .GT. 0 ) GOTO 100
      GENRI = GENR ( JGENR(IC) + ID )
      LTYPI = LTYP ( JLTYP(IC) + ID )
      LLONG = ( CATRLU(4:6) .EQ. 'MAX' )
      LLUTI = ( CATRLU(4:6) .EQ. 'UTI' )
      IF( (GENRI .NE. 'N'          .AND. CATRLU(1:3).EQ. 'NOM')  .OR.
     &    (INDEX('CRS',GENRI).EQ.0 .AND. CATRLU(1:3).EQ. 'NOL')  .OR.
     &    (GENRI .NE. 'R'          .AND. CATRLU(1:3).EQ. 'NOC')  .OR.
     &    (INDEX('EV' ,GENRI).EQ.0 .AND. CATRLU(1:4).EQ. 'LONM') .OR.
     &    (INDEX('EV' ,GENRI).EQ.0 .AND. CATRLU(1:4).EQ. 'LONU') ) THEN
        CMESS= 'NOM D''ATTRIBUT >'//CATRLU//
     &         '< INCOMPATIBLE AVEC LE GENRE '//GENRI
        CALL U2MESK('F','JEVEUX_01',1,CMESS)
      ENDIF
C
      IF      ( CATRLU .EQ. 'CLAS    ' ) THEN
         CVA = CLASSE ( IC : IC )
         LCV = 1
      ELSE IF ( CATRLU .EQ. 'GENR    ' ) THEN
         CVA = GENRI
         LCV = LEN(GENR(1))
      ELSE IF ( CATRLU(1:4) .EQ. 'TYPE' ) THEN
         CVA = TYPE ( JTYPE(IC) + ID )
         LCV = LEN(TYPE(1))
         IF ( CVA .EQ. 'K' ) THEN
           CALL CODENT(LTYPI,'G',CVA(2:4))
           IF ( CATRLU .EQ. 'TYPELONG' ) THEN
             LCV = LEN(TYPE(1))+2
           ENDIF
         ENDIF
      ELSE IF ( CATRLU .EQ. 'LTYP    ' ) THEN
         IVA  = LTYPI
      ELSE IF ( CATRLU .EQ. 'DOCU    ' ) THEN
         CVA = DOCU ( JDOCU(IC) + ID )
         LCV = LEN(DOCU(1))
      ELSE IF ( CATRLU .EQ. 'DATE    ' ) THEN
         IVA  = DATE ( JDATE(IC) + ID )
      ELSE IF ( CATRLU .EQ. 'ORIG    ' ) THEN
         CVA = ORIG ( JORIG(IC) + ID )
         LCV = LEN(ORIG(1))
      ELSE IF ( CATRLU .EQ. 'XOUS    ' ) THEN
         IF ( IRET .EQ. 1 ) THEN
            CVA = 'S'
         ELSE
            CVA = 'X'
         ENDIF
         LCV = 1
      ELSE
        IF ( LLONG .AND. LCONST ) THEN
          IF ( CATRLU.EQ.'LONMAX  ' .OR. CATRLU.EQ.'NOMMAX  ' ) THEN
            IVA  = LONG ( JLONG(IC) + ID )
          ENDIF
        ELSE IF ( LLONG .AND. IRET .EQ. 3) THEN
          IB = JISZON + IADM ( JIADM(IC) + 2*IXLONG-1 ) - 1 + IDATOC
          IF ( CATRLU.EQ.'LONMAX  ' .OR. CATRLU.EQ.'NOMMAX  ' ) THEN
            IVA  = ISZON (IB)
          ENDIF
        ELSE IF ( LLUTI .AND. LCONST ) THEN
          IF ( CATRLU.EQ.'LONUTI  ' .OR. CATRLU.EQ.'NOMUTI  ' ) THEN
            IVA  = LUTI ( JLUTI(IC) + ID )
          ENDIF
        ELSE IF ( LLUTI .AND. IRET .EQ. 3) THEN
          IB = JISZON + IADM ( JIADM(IC) + 2*IXLUTI-1 ) - 1 + IDATOC
          IF ( CATRLU .EQ. 'LONUTI  ' ) THEN
            IVA  = ISZON (IB)
          ENDIF
        ELSE IF ( CATRLU.EQ.'LONO    ' ) THEN
          IF ( LCONTI .OR. LCONST  ) THEN
            IVA  = LONO( JLONO(IC) + ID )
          ELSE
            IB = JISZON + IADM ( JIADM(IC) + 2*IXLONO-1 ) - 1 + IDATOC
            IVA  = ISZON (IB)
          ENDIF
        ELSE IF ( CATRLU .EQ. 'IADD    ' ) THEN
          IF ( LCONTI ) THEN
            IVA  = IADD ( JIADD(IC) + 2*ID-1 )
          ELSE
            IB = JISZON + IADM (JIADM(IC) + 2*IXIADD-1) - 1 + 2*IDATOC-1
            IVA  = ISZON(IB)
          ENDIF
        ELSE IF ( CATRLU .EQ. 'LADD    ' ) THEN
          IF ( LCONTI ) THEN
            IVA  = IADD ( JIADD(IC) + 2*ID  )
          ELSE
            IB = JISZON + IADM (JIADM(IC) + 2*IXIADD-1) - 1 + 2*IDATOC
            IVA  = ISZON(IB)
          ENDIF
        ELSE IF ( CATRLU .EQ. 'IADM    ' ) THEN
          IF ( LCONTI ) THEN
            IVA  = IADM ( JIADM(IC) + 2*ID-1 )
          ELSE
            IXIADM  = ISZON ( JISZON + IBACOL + IDIADM )
            IB = JISZON + IADM (JIADM(IC) + 2*IXIADM-1) - 1 + 2*IDATOC-1
            IVA  = ISZON(IB)
          ENDIF
        ELSE IF ( CATRLU .EQ. 'USAGE   ' ) THEN
          LCV = 3
          IF ( LCONTI ) THEN
            IADMI = IADM ( JIADM(IC) + 2*ID-1 )
            IF (IADMI .EQ. 0 ) THEN
              CVA = 'X X'
            ELSE
              CALL JJLIRS ( IADMI , IC , ID , 0 , IUU , ISS )
              IU = IUU / ISSTAT
              IS = ISS / ISSTAT
              CVA = KSTAT(IU:IU)//' '//KSTAT(IS:IS)
            ENDIF
          ELSE
            IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
            IB     = JISZON + IADM(JIADM(IC)+2*IXIADM-1) -1 + 2*IDATOC-1
            IADMI  = ISZON(IB)
            IF ( IADMI .NE. 0 ) THEN
              CALL JJLIRS ( IADMI, IC, IDATOC, IDATCO, IUU, ISS)
              IU = IUU / ISSTAT
              IS = ISS / ISSTAT
              CVA = KSTAT(IU:IU)//' '//KSTAT(IS:IS)
            ELSE
              CVA = 'X X'
            ENDIF
          ENDIF
        ELSE
          CMESS=' ATTRIBUT >'//CATRLU//'< ERRONE OU NON ACCESSIBLE'
          CALL U2MESK('F','JEVEUX_01',1,CMESS)
        ENDIF
      ENDIF
 100  CONTINUE
      IF ( LCV .NE. 0 ) THEN
        CVAL = CVA(1:MIN(LEN(CVAL),LCV))
      ELSE IF ( IVA .GE. 0 ) THEN
        IVAL = IVA
      ENDIF
C
      IF ( LCOL ) THEN
        CALL JJLIDE ( 'JELIBE' , NOML32(1:24) , 2 )
      ENDIF
      IPGC = IPGCEX
C
      END
