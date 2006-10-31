      SUBROUTINE JEPRAT ( UNIT , NOMLU , CIDATR , PARM , MESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 30/10/2006   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C TOLE CFT_726 CFT_720 CRP_18 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             UNIT
      CHARACTER *(*)     NOMLU , CIDATR , PARM , MESS
C ----------------------------------------------------------------------
C ROUTINE D'IMPRESSION DES OBJETS SYSTEME OU DES OBJETS ATTRIBUT DE
C COLLECTION
C
C IN  UNIT  : UNITE LOGIQUE D'IMPRESSION
C IN  NOMLU : NOM DE L'OBJET A IMPRIMER OU NOM DE CLASSE
C IN  CIDATR: NOM DE L'ATTRIBUT
C IN  PARM  : PARAMETRE D'IMPRESSION (NON IMPLEMENTE)
C IN  MESS  : MESSAGE UTILISATEUR
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     -----------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
C     ------------------------------------------------------------------
      CHARACTER *75   CMESS
      CHARACTER *32   NOML32
      CHARACTER *1    GENRI , TYPEI
      CHARACTER *8    NOM
      INTEGER         ICRE , IRET , JCTAB , LTYPI , LONOI
      INTEGER         IBACOL
      LOGICAL         LCOL
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,IDREEL     , IDLONG     ,
     &               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,IDREEL = 6 , IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
      CHARACTER*8    CIDNOM(IDNUM)
      INTEGER        IDPAR
      PARAMETER    ( IDPAR  = 3 )
      CHARACTER*8    CIDPAR(IDPAR)
      INTEGER         LIDBAS
      PARAMETER     ( LIDBAS = 20 )
      CHARACTER*8     CIDBAS(LIDBAS)
      DATA CIDNOM  / '$$DESO  ' , '$$IADD  ' , '$$IADM  ' , '$$MARQ  ' ,
     &               '$$NOM   ' , '$$REEL  ' , '$$LONG  ' , '$$LONO  ' ,
     &               '$$LUTI  ' , '$$NUM   '  /
      DATA CIDPAR  / '&&LONO  ' , '&&LUTI  ' , '&&PART  ' /
      DATA CIDBAS  / '$$CARA  ' , '$$IADD  ' , '$$GENR  ' , '$$TYPE  ' ,
     &               '$$DOCU  ' , '$$ORIG  ' , '$$RNOM  ' , '$$LTYP  ' ,
     &               '$$LONG  ' , '$$LONO  ' , '$$DATE  ' , '$$LUTI  ' ,
     &               '$$HCOD  ' , '$$USADI ' , '$$ACCE  ' , '$$MARQ  ' ,
     &               '$$INDX  ' , '$$TLEC  ' , '$$TECR  ' , '$$IADM  ' /
C DEB ------------------------------------------------------------------
      IPGCEX = IPGC
      IPGC   = -2
      NOML32 = NOMLU(1:MIN(24,LEN(NOMLU)))
      NOM    = CIDATR
C
      IF ( NOML32(1:1) .EQ. '$' ) THEN
        ICLAS = INDEX ( CLASSE , NOML32(2:2) )
        IF ( ICLAS .EQ. 0 ) THEN
          CMESS = ' CLASSE INVALIDE >'//NOML32(2:2)//'<'
          CALL U2MESK('F','JEVEUX_01',1,CMESS)
        ENDIF
        DO 1 K = 1, LIDBAS
           IF ( NOM .EQ. CIDBAS(K) ) THEN
              IDATR = K
              IDECI = 0
              IADMI = IADM ( JIADM(ICLAS) + IDATR )
              GENRI = GENR ( JGENR(ICLAS) + IDATR )
              TYPEI = TYPE ( JTYPE(ICLAS) + IDATR )
              LTYPI = LTYP ( JLTYP(ICLAS) + IDATR )
              LONOI = LONO ( JLONO(ICLAS) + IDATR ) * LTYPI
              CALL JJIMPO ( UNIT,IADMI, IDECI, 0, GENRI, TYPEI, LTYPI,
     &                      LONOI , MESS , PARM)
              GO TO 10
           ENDIF
 1      CONTINUE
        CMESS = ' NOM D''OBJET ATTRIBUT INVALIDE '
        CALL U2MESK('F','JEVEUX_01',1,CMESS)
 10     CONTINUE
C
      ELSE
        LCOL = .FALSE.
        ICRE = 0
        CALL JJVERN ( NOML32 , ICRE , IRET )
C
        IF ( IRET .EQ. 1 .AND. NOM(1:2) .EQ. '&&' ) THEN
          DO 2 K = 1, IDPAR
            IF ( NOM .EQ. CIDPAR(K) ) THEN
              IDATR = K
              GO TO 20
            ENDIF
 2        CONTINUE
          CMESS = ' NOM D''ATTRIBUT INVALIDE '
          CALL U2MESK('F','JEVEUX_01',1,CMESS)
 20       CONTINUE
          CALL JJCREN (NOML32(1:24)//NOM , 0 , IRET2)
          IF ( IRET2 .EQ. 0 ) THEN
            CALL U2MESK('F','JEVEUX_26',1,NOML32(1:24))
          ENDIF
          IADMI  = IADM ( JIADM(ICLAOS) + IDATOS )
          IADMEX = IADMI
          GENRI  = GENR ( JGENR(ICLAOS) + IDATOS )
          TYPEI  = TYPE ( JTYPE(ICLAOS) + IDATOS )
          LTYPI  = LTYP ( JLTYP(ICLAOS) + IDATOS )
          LONOI  = LONO ( JLONO(ICLAOS) + IDATOS ) * LTYPI
          IF ( IADMEX .EQ. 0 ) THEN
            CALL JJALTY ( TYPEI , LTYPI , 'L' , 1 , JCTAB )
            IADMI  = IADM ( JIADM(ICLAOS) + IDATOS )
          ENDIF
          IDECI = 0
          CALL JJIMPO(UNIT,IADMI, IDECI, 0, GENRI, TYPEI, LTYPI, LONOI,
     &                MESS , PARM)
          IF ( IADMEX .EQ. 0 ) THEN
            CALL JJLIDE ( 'JEIMPO' , NOML32(1:24)//NOM , 1 )
            IPGC = IPGCEX
          ENDIF
        ELSE IF ( IRET .NE. 2 ) THEN
          CMESS = 'IMPRESSION D''OBJET ATTRIBUT IMPOSSIBLE'
          CALL U2MESK('F','JEVEUX_01',1,CMESS)
        ELSE
          LCOL = .TRUE.
          CALL JJALLC ( ICLACO , IDATCO , 'L' , IBACOL )
          DO 3 K = 1, IDNUM
            IF ( NOM .EQ. CIDNOM(K) ) THEN
              IDATR = K
              GO TO 30
            ENDIF
 3        CONTINUE
          CMESS = ' NOM D''ATTRIBUT INVALIDE '
          CALL U2MESK('F','JEVEUX_01',1,CMESS)
 30       CONTINUE
          IXATR = ISZON ( JISZON + IBACOL + IDATR )
          IF ( IXATR .GT. 0 ) THEN
             IBATR = IADM( JIADM(ICLACO) + IXATR )
             IF ( IBATR .EQ. 0 ) THEN
               CMESS = ' SEGMENT DE VALEUR ASSOCIE A L''ATTRIBUT '//
     &                 NOM//' NON ACCESSIBLE '
               CALL U2MESK('F','JEVEUX_01',1,CMESS)
             ENDIF
             IDECI = 0
             GENRI = GENR( JGENR(ICLACO) + IXATR )
             TYPEI = TYPE( JTYPE(ICLACO) + IXATR )
             LTYPI = LTYP( JLTYP(ICLACO) + IXATR )
             LONOI = LONO( JLONO(ICLACO) + IXATR ) * LTYPI
             CALL JJIMPO(UNIT,IBATR,IDECI,0,GENRI,TYPEI,LTYPI,LONOI,
     &                   MESS , PARM)
          ENDIF
        ENDIF
        IF ( LCOL ) THEN
           CALL JJLIDE ( 'JEIMPO' , NOML32 , 2 )
        ENDIF
      ENDIF
      IPGC = IPGCEX
C FIN -----------------------------------------------------------------
      END
