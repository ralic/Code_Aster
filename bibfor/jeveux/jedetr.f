      SUBROUTINE JEDETR ( NOMLU )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 01/10/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_726 CFT_720 CRP_18 CRS_508  CRS_512 CRS_505
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       NOMLU
C ----------------------------------------------------------------------
C DESTRUCTION D'UN OBJET JEVEUX
C
C IN  NOMLU  : NOM D'OBJET JEVEUX
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
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
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          IFNIVO, NIVO
      COMMON /JVNIVO/  IFNIVO, NIVO
      INTEGER          LDYN , LGDYN , MXDYN , MCDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , MXDYN , MCDYN , NBDYN , NBFREE
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,              IDLONG     ,
     &               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 , IDNUM  = 10 )
C     ------------------------------------------------------------------
      CHARACTER*32    NOML32 , NOM32
      INTEGER         ICRE , IRET , ID(IDNUM) , IADDI(2) , IBID
C DEB ------------------------------------------------------------------
      NOML32 = NOMLU
      ICRE = 0
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IF ( IRET .EQ. 0 ) THEN
        GOTO 9999
      ELSE IF ( IRET .EQ. 1 ) THEN
        IC = ICLAOS
        IADMI = IADM (JIADM(IC) + 2*IDATOS-1 )
        IADYN = IADM (JIADM(IC) + 2*IDATOS   )
        IF ( IADYN .NE. 0 ) THEN
          MCDYN = MCDYN - LONO(JLONO(IC)+IDATOS)*LTYP(JLTYP(IC)+IDATOS)
          CALL  HPDEALLC ( IADYN , NBFREE , IBID )
        ELSE IF ( IADMI .NE. 0 ) THEN
          CALL JJLIBP ( IADMI )
        ENDIF
        IADDI(1) = IADD (JIADD(IC) + 2*IDATOS-1 )
        IADDI(2) = IADD (JIADD(IC) + 2*IDATOS   )
        IF ( IADDI(1) .GT. 0 ) THEN
          LONOI = LONO(JLONO(IC)+IDATOS)*LTYP(JLTYP(IC)+IDATOS)
          CALL JXLIBD ( 0, IDATOS , IC , IADDI , LONOI )
        ENDIF
        IF (NIVO .GE. 2) THEN
          CALL U2MESK('I','JEVEUX_07',1,NOML32)
        ENDIF
        CALL JJCREN ( NOML32 , -1 , IRET )
        NOMOS = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
        CALL JJMZAT ( ICLAOS , IDATOS )
      ELSE
        IC = ICLACO
        CALL JJALLC (IC , IDATCO , 'E' , IBACOL )
        IF ( NOML32(25:32) .NE. '        ' ) THEN
          CALL JJCROC ( NOML32(25:32) , ICRE )
          IRET = 3
        ENDIF
        IF ( IRET .EQ. 2 ) THEN
          IXIADM = ISZON( JISZON + IBACOL + IDIADM )
          IXIADD = ISZON( JISZON + IBACOL + IDIADD )
          IXLONO = ISZON( JISZON + IBACOL + IDLONO )
          IXDESO = ISZON( JISZON + IBACOL + IDDESO )
          IXMARQ = ISZON( JISZON + IBACOL + IDMARQ )
          IF ( IXIADM .GT. 0 ) THEN
            IBIADM = IADM ( JIADM(IC) + 2*IXIADM-1 )
            IBMARQ = IADM ( JIADM(IC) + 2*IXMARQ-1 )
            NMAX   = ISZON(JISZON+IBACOL+IVNMAX)
            DO 10 K = 1,NMAX
              IADMAR = ISZON( JISZON + IBMARQ - 1 + 2*K )
              IF ( IADMAR .NE. 0 ) THEN
                ISZON(JISZON+KDESMA(1)+IADMAR-1) = 0
              ENDIF
              IADMOC = ISZON( JISZON + IBIADM - 1 + 2*K-1 )
              IADYOC = ISZON( JISZON + IBIADM - 1 + 2*K   )
              IF ( IADYOC .NE. 0 ) THEN
                IF ( IXLONO .GT. 0 ) THEN
                  IBLONO=IADM(JIADM(IC)+2*IXLONO-1)
                  LONOI =ISZON(JISZON+IBLONO+K-1)*LTYP(JLTYP(IC)+IXDESO)
                ELSE
                  LONOI = LONO(JLONO(IC)+IXDESO)*LTYP(JLTYP(IC)+IXDESO)
                ENDIF
                MCDYN = MCDYN - LONOI
                CALL  HPDEALLC ( IADYOC , NBFREE , IBID )
              ELSE IF ( IADMOC .NE. 0 ) THEN
                CALL JJLIBP ( IADMOC )
              ENDIF
              IBIADD = IADM ( JIADM(IC) + 2*IXIADD-1 )
              IADDI(1) = ISZON( JISZON + IBIADD -1 + 2*K-1 )
              IADDI(2) = ISZON( JISZON + IBIADD -1 + 2*K   )
              IF ( IADDI(1) .GT. 0 ) THEN
                IF ( IXLONO .GT. 0 ) THEN
                  IBLONO=IADM(JIADM(IC)+2*IXLONO-1)
                  LONOI =ISZON(JISZON+IBLONO+K-1)*LTYP(JLTYP(IC)+IXDESO)
                ELSE
                  LONOI = LONO(JLONO(IC)+IXDESO)*LTYP(JLTYP(IC)+IXDESO)
                ENDIF
                CALL JXLIBD ( IDATCO, K, IC , IADDI , LONOI )
              ENDIF
 10        CONTINUE
          ENDIF
          DO 1 K = 1 , IDNUM
            ID (K) = ISZON ( JISZON + IBACOL + K )
            IF ( ID(K) .GT. 0 ) THEN
              NOM32 = RNOM ( JRNOM(IC) + ID(K) )
              IF ( NOM32(1:24) .EQ. NOML32(1:24) .OR.
     &             NOM32(25:26) .EQ. '&&'             ) THEN
                IADMI = IADM (JIADM(IC) + 2*ID(K)-1 )
                IADYN = IADM (JIADM(IC) + 2*ID(K)   )
                IF ( IADYN .NE. 0 ) THEN
                  MCDYN = MCDYN - LONO(JLONO(IC)+ID(K))*
     &                            LTYP(JLTYP(IC)+ID(K))
                  CALL  HPDEALLC ( IADYN , NBFREE , IBID )
                ELSE IF ( IADMI .NE. 0 ) THEN
                  CALL JJLIBP ( IADMI )
                ENDIF
                IADDI(1) = IADD (JIADD(IC) + 2*ID(K)-1 )
                IADDI(2) = IADD (JIADD(IC) + 2*ID(K)   )
                IF ( IADDI(1) .GT. 0 ) THEN
                  LONOI = LONO(JLONO(IC)+ID(K))*LTYP(JLTYP(IC)+ID(K))
                  CALL JXLIBD ( 0, ID(K), IC, IADDI, LONOI )
                ENDIF
              ELSE
                ID(K) = 0
              ENDIF
            ENDIF
 1        CONTINUE
          DO 2 K = 1 , IDNUM
            IF ( ID(K) .GT. 0 ) THEN
              NOM32 = RNOM ( JRNOM(IC) + ID(K) )
              IF (NIVO .GE. 2) THEN
                CALL U2MESK('I','JEVEUX_07',1,NOML32(1:24))
              ENDIF
              CALL JJCREN ( NOM32 , -2 , IRET )
              CALL JJMZAT ( IC , ID(K) )
            ENDIF
 2        CONTINUE
          IADYN = IADM(JIADM(IC)+2*IDATCO)
          IF ( IADYN .NE. 0 ) THEN
            MCDYN = MCDYN -LONO(JLONO(IC)+IDATCO)*LTYP(JLTYP(IC)+IDATCO)
            CALL  HPDEALLC ( IADYN , NBFREE , IBID )
          ELSE IF ( IADMI .NE. 0 ) THEN
            CALL JJLIBP ( IBACOL )
          ENDIF
          IADDI(1) = IADD (JIADD(IC) + 2*IDATCO-1)
          IADDI(2) = IADD (JIADD(IC) + 2*IDATCO  )
          IF ( IADDI(1) .GT. 0 ) THEN
            LONOI = LONO(JLONO(IC)+IDATCO)*LTYP(JLTYP(IC)+IDATCO)
            CALL JXLIBD ( 0 ,IDATCO, IC , IADDI , LONOI )
          ENDIF
          IF (NIVO .GE. 2) THEN
            CALL U2MESK('I','JEVEUX_07',1,NOML32(1:24))
          ENDIF
          CALL JJCREN ( NOML32(1:24) , -2 , IRET )
          CALL JJMZAT ( IC , IDATCO )
          NOMCO = '$$$$$$$$$$$$$$$$$$$$$$$$'
        ELSE IF ( IRET .EQ. 3 ) THEN
          IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
          IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
          IXLONG = ISZON ( JISZON + IBACOL + IDLONG )
          IXNOM  = ISZON ( JISZON + IBACOL + IDNOM  )
          IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
          IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
          IXMARQ = ISZON ( JISZON + IBACOL + IDMARQ )
C         
C         DESTRUCTION D''UN OBJET DE COLLECTION CONTIGUE REFUSEE
          CALL ASSERT (IXIADD .GT. 0)
C         
C         DESTRUCTION DANS UNE COLLECTION NON NOMMEE REFUSEE
          CALL ASSERT (IXNOM .GT. 0 )
C         
          IBIADD = IADM ( JIADM(IC) + 2*IXIADD-1 )
          IADDI(1) = ISZON ( JISZON + IBIADD - 1 + 2*IDATOC-1 )
          IADDI(2) = ISZON ( JISZON + IBIADD - 1 + 2*IDATOC   )
          IF ( IADDI(1) .GT. 0 ) THEN
            IF ( IXLONO .GT. 0 ) THEN
              IBLONO = IADM(JIADM(IC)+2*IXLONO-1)
              LONOI = ISZON(JISZON+IBLONO+IDATOC-1)*
     &                      LTYP(JLTYP(IC)+IXDESO)
            ELSE
              LONOI = LONO(JLONO(IC)+IXDESO)*LTYP(JLTYP(IC)+IXDESO)
            ENDIF
            CALL JXLIBD ( IDATCO, IDATOC, IC , IADDI , LONOI )
          ENDIF
          ISZON ( JISZON + IBIADD + IDATOC - 1 ) = 0
          IBMARQ = IADM ( JIADM(IC) + 2*IXMARQ-1 )
          IADMAR = ISZON( JISZON + IBMARQ - 1 + 2*IDATOC )
          IF ( IADMAR .NE. 0 ) THEN
            ISZON(JISZON+KDESMA(1)+IADMAR-1) = 0
          ENDIF
          IBIADM = IADM ( JIADM(IC) + 2*IXIADM-1 )
          IADMI = ISZON ( JISZON + IBIADM - 1 + 2*IDATOC-1 )
          IADYN = ISZON ( JISZON + IBIADM - 1 + 2*IDATOC   )
          IF ( IADYN .NE. 0 ) THEN
            MCDYN = MCDYN - LONOI
            CALL  HPDEALLC ( IADYN , NBFREE , IBID )
          ELSE IF ( IADMI .NE. 0 ) THEN
            CALL JJLIBP ( IADMI )
          ENDIF
          ISZON ( JISZON + IBIADM - 1 + 2*IDATOC-1 ) = 0
          ISZON ( JISZON + IBIADM - 1 + 2*IDATOC   ) = 0
          IF ( IXLONG .GT. 0 ) THEN
            IBLONG = IADM ( JIADM(IC) + 2*IXLONG-1 )
            ISZON ( JISZON + IBLONG + IDATOC - 1 ) = 0
          ENDIF
          IF (NIVO .GE. 2) THEN
            CALL U2MESK('I','JEVEUX_07',1,NOML32)
          ENDIF
          CALL JJCROC ( NOMLU(25:32) , -3 )
          NOMOC = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
        ENDIF
      ENDIF
 9999 CONTINUE
C FIN ------------------------------------------------------------------
      END
