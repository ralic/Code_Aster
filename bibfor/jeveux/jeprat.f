      SUBROUTINE JEPRAT ( UNIT , NOMLU , CIDATR , MESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      INTEGER             UNIT
      CHARACTER *(*)     NOMLU , CIDATR , MESS
C ----------------------------------------------------------------------
C ROUTINE D'IMPRESSION DES OBJETS SYSTEME OU DES OBJETS ATTRIBUT DE
C COLLECTION
C
C IN  UNIT  : UNITE LOGIQUE D'IMPRESSION
C IN  NOMLU : NOM DE L'OBJET A IMPRIMER OU NOM DE CLASSE
C IN  CIDATR: NOM DE L'ATTRIBUT
C IN  MESS  : MESSAGE UTILISATEUR
C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C     -----------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IADMEX ,IADMI ,IBATR ,IDATR ,IDECI ,IPGCEX ,IRET2
      INTEGER IXATR ,JCARA ,JDATE ,JDOCU ,JGENR ,JHCOD ,JIADD
      INTEGER JIADM ,JLONG ,JLONO ,JLTYP ,JLUTI ,JMARQ ,JORIG
      INTEGER JRNOM ,JTYPE ,K ,N
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
C     ------------------------------------------------------------------
      CHARACTER *32   NOML32,VALK(2)
      CHARACTER *1    GENRI , TYPEI
      CHARACTER *8    NOM
      INTEGER         ICRE , IRET , JCTAB , LTYPI , LONOI
      INTEGER         IBACOL
      LOGICAL         LCOL
C     ------------------------------------------------------------------
      INTEGER          IDNUM
      PARAMETER    (   IDNUM  = 10 )
      CHARACTER*8    CIDNOM(IDNUM)
      INTEGER        IDPAR
      PARAMETER    ( IDPAR  = 3 )
      CHARACTER*8    CIDPAR(IDPAR)
      INTEGER         LIDBAS
      PARAMETER     ( LIDBAS = 20 )
      CHARACTER*8     CIDBAS(LIDBAS)
      DATA CIDNOM  / '$$DESO  ' , '$$IADD  ' , '$$IADM  ' , '$$MARQ  ' ,
     &               '$$NOM   ' , '$$XXXX  ' , '$$LONG  ' , '$$LONO  ' ,
     &               '$$LUTI  ' , '$$NUM   '  /
      DATA CIDPAR  / '&&LONO  ' , '&&LUTI  ' , '&&PART  ' /
      DATA CIDBAS  / '$$CARA  ' , '$$IADD  ' , '$$GENR  ' , '$$TYPE  ' ,
     &               '$$DOCU  ' , '$$ORIG  ' , '$$RNOM  ' , '$$LTYP  ' ,
     &               '$$LONG  ' , '$$LONO  ' , '$$DATE  ' , '$$LUTI  ' ,
     &               '$$HCOD  ' , '$$USADI ' , '$$ACCE  ' , '$$MARQ  ' ,
     &               '$$XXXX  ' , '$$TLEC  ' , '$$TECR  ' , '$$IADM  ' /
C DEB ------------------------------------------------------------------
      IPGCEX = IPGC
      IPGC   = -2
      NOML32 = NOMLU(1:MIN(24,LEN(NOMLU)))
      NOM    = CIDATR
C
      IF ( NOML32(1:1) .EQ. '$' ) THEN
        ICLAS = INDEX ( CLASSE , NOML32(2:2) )
        IF ( ICLAS .EQ. 0 ) THEN
          CALL U2MESK('F','JEVEUX1_15',1,NOML32(2:2))
        ENDIF
        DO 1 K = 1, LIDBAS
           IF ( NOM .EQ. CIDBAS(K) ) THEN
              IDATR = K
              IDECI = 0
              IADMI = IADM ( JIADM(ICLAS) + 2*IDATR-1 )
              GENRI = GENR ( JGENR(ICLAS) + IDATR )
              TYPEI = TYPE ( JTYPE(ICLAS) + IDATR )
              LTYPI = LTYP ( JLTYP(ICLAS) + IDATR )
              LONOI = LONO ( JLONO(ICLAS) + IDATR ) * LTYPI
              CALL JJIMPO ( UNIT,IADMI, IDECI, 0, GENRI, TYPEI, LTYPI,
     &                      LONOI , MESS)
              GO TO 10
           ENDIF
 1      CONTINUE
        CALL U2MESK('F','JEVEUX1_16',1,NOM)
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
          CALL U2MESK('F','JEVEUX1_17',1,NOM)
 20       CONTINUE
          CALL JJCREN (NOML32(1:24)//NOM , 0 , IRET2)
          IF ( IRET2 .EQ. 0 ) THEN
            CALL U2MESK('F','JEVEUX_26',1,NOML32(1:24))
          ENDIF
          IADMI  = IADM ( JIADM(ICLAOS) + 2*IDATOS-1 )
          IADMEX = IADMI
          GENRI  = GENR ( JGENR(ICLAOS) + IDATOS )
          TYPEI  = TYPE ( JTYPE(ICLAOS) + IDATOS )
          LTYPI  = LTYP ( JLTYP(ICLAOS) + IDATOS )
          LONOI  = LONO ( JLONO(ICLAOS) + IDATOS ) * LTYPI
          IF ( IADMEX .EQ. 0 ) THEN
            CALL JJALTY ( TYPEI , LTYPI , 'L' , 1 , JCTAB )
            IADMI  = IADM ( JIADM(ICLAOS) + 2*IDATOS-1 )
          ENDIF
          IDECI = 0
          CALL JJIMPO(UNIT,IADMI, IDECI, 0, GENRI, TYPEI, LTYPI, LONOI,
     &                MESS)
          IF ( IADMEX .EQ. 0 ) THEN
            CALL JJLIDE ( 'JEIMPO' , NOML32(1:24)//NOM , 1 )
            IPGC = IPGCEX
          ENDIF
        ELSE IF ( IRET .NE. 2 ) THEN
          VALK(1) = NOM
          VALK(2) = NOML32
          CALL U2MESK('F','JEVEUX1_18',2,VALK)
        ELSE
          LCOL = .TRUE.
          CALL JJALLC ( ICLACO , IDATCO , 'L' , IBACOL )
          DO 3 K = 1, IDNUM
            IF ( NOM .EQ. CIDNOM(K) ) THEN
              IDATR = K
              GO TO 30
            ENDIF
 3        CONTINUE
          CALL U2MESK('F','JEVEUX1_17',1,NOM)
 30       CONTINUE
          IXATR = ISZON ( JISZON + IBACOL + IDATR )
          IF ( IXATR .GT. 0 ) THEN
             IBATR = IADM( JIADM(ICLACO) + 2*IXATR-1 )
             IF ( IBATR .EQ. 0 ) THEN
               CALL U2MESK('F','JEVEUX1_19',1,NOM)
             ENDIF
             IDECI = 0
             GENRI = GENR( JGENR(ICLACO) + IXATR )
             TYPEI = TYPE( JTYPE(ICLACO) + IXATR )
             LTYPI = LTYP( JLTYP(ICLACO) + IXATR )
             LONOI = LONO( JLONO(ICLACO) + IXATR ) * LTYPI
             CALL JJIMPO(UNIT,IBATR,IDECI,0,GENRI,TYPEI,LTYPI,LONOI,
     &                   MESS)
          ENDIF
        ENDIF
        IF ( LCOL ) THEN
           CALL JJLIDE ( 'JEIMPO' , NOML32 , 2 )
        ENDIF
      ENDIF
      IPGC = IPGCEX
C FIN -----------------------------------------------------------------
      END
