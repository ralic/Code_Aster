      SUBROUTINE JEDETV()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C TOLE CRP_18 CRS_508 CRS_512 CRS_505
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C DETRUIT TOUS LES OBJETS JEVEUX PRESENTS SUR LA BASE VOLATILE A
C L'EXCEPTION DES OBJETS SYSTEME
C
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
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
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          IFNIVO, NIVO
      COMMON /JVNIVO/  IFNIVO, NIVO
      COMMON /JIACCE/  JIACCE(N),NBACCE(2*N)
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD    , IDIADM     ,
     &               IDMARQ     , IDNOM      ,             IDLONG     ,
     &               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 , IDNUM  = 10 )
C     ------------------------------------------------------------------
      INTEGER          LIDBAS      , LIDEFF
      PARAMETER      ( LIDBAS = 20 , LIDEFF = 15 )
      INTEGER          IC,ID(IDNUM),IDO,IADDI(2)
      REAL*8           VALR(3)
      CHARACTER*1      CGENR
      CHARACTER*32     CRNOM,NOM32
C DEB ------------------------------------------------------------------
C
      IC = INDEX ( CLASSE , 'V')
C
      VALR(1)= 100.D0
      VALR(2)= NBACCE(2*IC-1)*LONGBL(IC)*LOIS/1024.D0
      VALR(3)= NBACCE(2*IC  )*LONGBL(IC)*LOIS/1024.D0
      IF ( VALR(3) .GT. VALR(1)*VALR(2) .AND. VALR(2) .NE. 0) THEN
        CALL U2MESR ('A','JEVEUX1_64',3,VALR)
      ENDIF
C
      DO 150 IDO = LIDBAS+1 , NREMAX(IC)
        CRNOM = RNOM(JRNOM(IC)+IDO)
        IF ( CRNOM(1:1) .EQ. '?' .OR.
     &       CRNOM(25:32) .NE. '     ' ) GOTO 150
        CGENR = GENR(JGENR(IC)+IDO)
        IF ( CGENR .EQ. 'X' ) THEN
C
C    ON TRAITE D'ABORD LES COLLECTIONS
C
        CALL JJALLC (IC ,IDO ,'E' ,IBACOL )
        IXIADM = ISZON( JISZON + IBACOL + IDIADM )
        IXIADD = ISZON( JISZON + IBACOL + IDIADD )
        IXLONO = ISZON( JISZON + IBACOL + IDLONO )
        IXDESO = ISZON( JISZON + IBACOL + IDDESO )
        IXMARQ = ISZON( JISZON + IBACOL + IDMARQ )
        IF ( IXIADM .NE. 0 ) THEN
          IBIADM = IADM ( JIADM(IC) + 2*IXIADM-1 )
          IBIADD = IADM ( JIADM(IC) + 2*IXIADD-1 )
          IBMARQ = IADM ( JIADM(IC) + 2*IXMARQ-1 )
          NMAX   = ISZON(JISZON+IBACOL+IVNMAX )
          DO 10 K = 1,NMAX
            IADMAR = ISZON( JISZON + IBMARQ -1 + 2*K )
            IF ( IADMAR .NE. 0 ) THEN
              ISZON(JISZON+KDESMA(1)+IADMAR-1) = 0
            ENDIF
            IADMOC = ISZON( JISZON + IBIADM - 1 + 2*K-1 )
            IADYOC = ISZON( JISZON + IBIADM - 1 + 2*K   )
            IF ( IADYOC .NE. 0 ) THEN
              CALL JJLIDY( IADYOC , IADMOC )
            ELSE IF ( IADMOC .NE. 0 ) THEN
              CALL JJLIBP ( IADMOC )
            ENDIF
            IADDI(1) = ISZON( JISZON + IBIADD - 1 + 2*K-1 )
            IADDI(2) = ISZON( JISZON + IBIADD - 1 + 2*K   )
            IF ( IADDI(1) .GT. 0 ) THEN
              IF ( IXLONO .GT. 0 ) THEN
                IBLONO=IADM(JIADM(IC)+2*IXLONO-1)
                LONOI =ISZON(JISZON+IBLONO+K-1)*LTYP(JLTYP(IC)+IXDESO)
              ELSE
                LONOI = LONO(JLONO(IC)+IXDESO)*LTYP(JLTYP(IC)+IXDESO)
              ENDIF
              CALL JXLIBD (IDO, K, IC, IADDI, LONOI)
            ENDIF
 10       CONTINUE
        ENDIF
        DO 1 K = 1 , IDNUM
          ID(K) = ISZON ( JISZON + IBACOL + K )
          IF ( ID(K) .GT. 0 ) THEN
            NOM32 = RNOM ( JRNOM(IC) + ID(K) )
            IF ( NOM32(1:24) .EQ. CRNOM(1:24) .OR.
     &           NOM32(25:26) .EQ. '&&'       ) THEN
              IADMI = IADM (JIADM(IC) + 2*ID(K)-1 )
              IADYN = IADM (JIADM(IC) + 2*ID(K)   )
              IF ( IADYN .NE. 0 ) THEN
                CALL JJLIDY( IADYN , IADMI )
              ELSE IF ( IADMI .NE. 0 ) THEN
                CALL JJLIBP ( IADMI )
              ENDIF
              IADDI(1) = IADD (JIADD(IC) + 2*ID(K)-1 )
              IADDI(2) = IADD (JIADD(IC) + 2*ID(K)   )
              IF ( IADDI(1) .GT. 0 ) THEN
                LONOI=LONO(JLONO(IC)+ID(K))*LTYP(JLTYP(IC)+ID(K))
                CALL JXLIBD ( 0 , ID(K) , IC , IADDI , LONOI )
              ENDIF
            ELSE
              ID(K) = 0
            ENDIF
          ENDIF
 1      CONTINUE
        DO 2 K = 1 , IDNUM
          IF ( ID(K) .GT. 0 ) THEN
            NOM32 = RNOM ( JRNOM(IC) + ID(K) )
            IF (NIVO .GE. 2) THEN
              CALL U2MESK('I','JEVEUX_07',1,NOM32)
            ENDIF
            CALL JJCREN ( NOM32 , -2 , IRET )
            CALL JJMZAT ( IC , ID(K) )
          ENDIF
2       CONTINUE
        CRNOM = RNOM ( JRNOM(IC) + IDO )
        IADYN = IADM (JIADM(IC) + 2*IDO)
        IF ( IADYN .NE. 0 ) THEN
          CALL JJLIDY( IADYN , IBACOL )
        ELSE IF ( IBACOL .NE. 0 ) THEN
          CALL JJLIBP (IBACOL)
        ENDIF
        IADDI(1) = IADD (JIADD(IC) + 2*IDO-1)
        IADDI(2) = IADD (JIADD(IC) + 2*IDO  )
        IF ( IADDI(1) .GT. 0 ) THEN
          LONOI = LONO(JLONO(IC)+IDO)*LTYP(JLTYP(IC)+IDO)
          CALL JXLIBD ( 0, IDO, IC, IADDI, LONOI )
        ENDIF
        IF (NIVO .GE. 2) THEN
          CALL U2MESK('I','JEVEUX_07',1,CRNOM(1:24))
        ENDIF
        CALL JJCREN ( CRNOM(1:24) , -2 , IRET )
        CALL JJMZAT ( IC , IDO )
        NOMCO = '$$$$$$$$$$$$$$$$$$$$$$$$'
      ENDIF
 150  CONTINUE
C
      DO 250 IDO = LIDBAS+1 , NREMAX(IC)
        CRNOM = RNOM(JRNOM(IC)+IDO)
        IF ( CRNOM(1:1) .EQ. '?' .OR.
     &       CRNOM(25:32) .NE. '      ' ) GOTO 250
        CGENR = GENR(JGENR(IC)+IDO)
C
C    ON TRAITE LES OBJETS SIMPLES
C
        IF ( CGENR .NE. 'X' ) THEN
          IADMI = IADM (JIADM(IC) + 2*IDO-1)
          IADYN = IADM (JIADM(IC) + 2*IDO  )
          IF ( IADYN .NE. 0 ) THEN
            CALL JJLIDY( IADYN , IADMI )
          ELSE IF ( IADMI .NE. 0 ) THEN
            CALL JJLIBP ( IADMI )
          ENDIF
          IADDI(1) = IADD (JIADD(IC) + 2*IDO-1)
          IADDI(2) = IADD (JIADD(IC) + 2*IDO  )
          IF ( IADDI(1) .GT. 0 ) THEN
            LONOI = LONO(JLONO(IC)+IDO)*LTYP(JLTYP(IC)+IDO)
            CALL JXLIBD ( 0, IDO , IC , IADDI , LONOI )
          ENDIF
          IF (NIVO .GE. 2) THEN
            CALL U2MESK('I','JEVEUX_07',1,CRNOM(1:24))
          ENDIF
          CALL JJCREN ( CRNOM , -1 , IRET )
          NOMOS = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
          CALL JJMZAT ( IC , IDO )
        ENDIF
 250  CONTINUE
C FIN ------------------------------------------------------------------
      END
