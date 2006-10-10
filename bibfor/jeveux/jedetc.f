      SUBROUTINE JEDETC ( CLAS , SOUCH , IPOS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 10/10/2006   AUTEUR VABHHTS J.PELLET 
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
C TOLE CFT_726 CFT_720 CRP_18 CRS_508  CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       CLAS , SOUCH
      INTEGER                            IPOS
C ----------------------------------------------------------------------
C DESTRUCTION D'UN ENSEMBLE D'OBJETS JEVEUX
C
C IN  CLAS   : CLASSE DES OBJETS ( ' ' TOUTES LES BASES OUVERTES)
C IN  SOUCH  : SOUS-CHAINE RECHERCHEE
C IN  IPOS   : POSITION DE LA SOUS-CHAINE RECHERCHEE
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
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
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
      INTEGER          IFNIVO, NIVO
      COMMON /JVNIVO/  IFNIVO, NIVO
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,IDREEL     , IDLONG     ,
     &               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,IDREEL = 6 , IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C     ------------------------------------------------------------------
      INTEGER          NCLA1,NCLA2,IC,J,IRET,ID(IDNUM),NMAX,IADDI(2)
      CHARACTER*75     CMESS
      CHARACTER*32     CRNOM,NOM32
      CHARACTER*1      KCLAS
C DEB ------------------------------------------------------------------
      L = LEN ( SOUCH )
      IF ( IPOS + L .GT. 25 .OR. IPOS .LT. 0 .OR. L .EQ. 0 ) THEN
        CMESS = ' LONGUEUR OU POSITION DE LA SOUS-CHAINE '//SOUCH//
     &       ' INVALIDE'
        CALL U2MESK('F','JEVEUX_1',1,CMESS)
      ENDIF
      KCLAS  = CLAS (1:MIN(1,LEN(CLAS)))
      IF ( KCLAS .EQ. ' ' ) THEN
        NCLA1 = 1
        NCLA2 = INDEX ( CLASSE , '$' ) - 1
        IF ( NCLA2 .LT. 0 ) NCLA2 = N
      ELSE
        NCLA1 = INDEX ( CLASSE , KCLAS)
        NCLA2 = NCLA1
      ENDIF
      DO 100 IC = NCLA1 , NCLA2
        DO 160 KK = 1, 2
          DO 150 J = 1 , NREMAX(IC)
            CRNOM = RNOM(JRNOM(IC)+J)
            IF ( CRNOM(1:1) .EQ. '?' .OR.
     &          CRNOM(25:32) .NE. '        ' ) GOTO 150
            IF ( SOUCH .EQ. CRNOM(IPOS:IPOS+L-1) ) THEN
              CALL JJCREN ( CRNOM(1:24) , 0 , IRET )
              IF ( IRET .EQ. 1 .AND. KK .EQ. 2) THEN
                IADMI = IADM (JIADM(IC) + IDATOS )
                IF ( IADMI .GT. 0 ) THEN
                  CALL JJLIBP ( IADMI )
                ENDIF
                IADDI(1) = IADD (JIADD(IC) + 2*IDATOS-1 )
                IADDI(2) = IADD (JIADD(IC) + 2*IDATOS   )
                IF ( IADDI(1) .GT. 0 ) THEN
                  LONOI = LONO(JLONO(IC)+IDATOS)*LTYP(JLTYP(IC)+IDATOS)
                  CALL JXLIBD ( 0, IDATOS , IC , IADDI , LONOI )
                ENDIF
                IF (NIVO .GE. 2) THEN
                  CALL U2MESK('I','JEVEUX_7',1,CRNOM(1:24))
                ENDIF
                CALL JJCREN ( CRNOM(1:24) , -1 , IRET )
                NOMOS = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
                CALL JJMZAT ( ICLAOS , IDATOS )
              ELSE IF( IRET .GT. 1 ) THEN
                CALL JJALLC (IC ,IDATCO ,'E' ,IBACOL )
                IXIADM = ISZON( JISZON + IBACOL + IDIADM )
                IXIADD = ISZON( JISZON + IBACOL + IDIADD )
                IXLONO = ISZON( JISZON + IBACOL + IDLONO )
                IXDESO = ISZON( JISZON + IBACOL + IDDESO )
                IXMARQ = ISZON( JISZON + IBACOL + IDMARQ )
                IF ( IXIADM .GT. 0 ) THEN
                  IBIADM = IADM ( JIADM(IC) + IXIADM )
                  IBIADD = IADM ( JIADM(IC) + IXIADD )
                  IBMARQ = IADM ( JIADM(IC) + IXMARQ )
                  NMAX   = ISZON(JISZON+IBACOL+IVNMAX )
                  DO 10 K = 1,NMAX
                    IADMAR = ISZON( JISZON + IBMARQ -1 + 2*K )
                    IF ( IADMAR .GT. 0 ) THEN
                      ISZON(JISZON+KDESMA+IADMAR-1) = 0
                    ENDIF
                    IADMOC = ISZON( JISZON + IBIADM + K - 1 )
                    IF ( IADMOC .GT. 0 ) THEN
                       CALL JJLIBP ( IADMOC )
                    ENDIF
                    IADDI(1) = ISZON( JISZON + IBIADD - 1 + 2*K-1 )
                    IADDI(2) = ISZON( JISZON + IBIADD - 1 + 2*K   )
                    IF ( IADDI(1) .GT. 0 ) THEN
                      IF ( IXLONO .GT. 0 ) THEN
                         IBLONO=IADM(JIADM(IC)+IXLONO)
                         LONOI =ISZON(JISZON+IBLONO+K-1)
     &                        *LTYP(JLTYP(IC)+IXDESO)
                      ELSE
                         LONOI = LONO(JLONO(IC)+IXDESO)
     &                        *LTYP(JLTYP(IC)+IXDESO)
                      ENDIF
                      CALL JXLIBD (IDATCO, K, IC, IADDI, LONOI)
                    ENDIF
 10               CONTINUE
                ENDIF
                DO 1 K = 1 , IDNUM
                  ID(K) = ISZON ( JISZON + IBACOL + K )
                  IF ( ID(K) .GT. 0 ) THEN
                    NOM32 = RNOM ( JRNOM(IC) + ID(K) )
                    IF ( NOM32(1:24) .EQ. CRNOM(1:24) .OR.
     &                  NOM32(25:26) .EQ. '&&'          ) THEN
                      IADMI = IADM (JIADM(IC) + ID(K) )
                      IF ( IADMI .GT. 0 ) THEN
                        CALL JJLIBP ( IADMI )
                      ENDIF
                      IADDI(1) = IADD (JIADD(IC) + 2*ID(K)-1 )
                      IADDI(2) = IADD (JIADD(IC) + 2*ID(K)   )
                      IF ( IADDI(1) .GT. 0 ) THEN
                        LONOI=LONO(JLONO(IC)+ID(K))
     &                        *LTYP(JLTYP(IC)+ID(K))
                        CALL JXLIBD ( 0 , ID(K) , IC , IADDI , LONOI )
                      ENDIF
                    ELSE
                      ID(K) = 0
                    ENDIF
                  ENDIF
 1              CONTINUE
                DO 2 K = 1 , IDNUM
                  IF ( ID(K) .GT. 0 ) THEN
                    NOM32 = RNOM ( JRNOM(IC) + ID(K) )
                    IF (NIVO .GE. 2) THEN
                      CALL U2MESK('I','JEVEUX_7',1,NOM32)
                    ENDIF
                    CALL JJCREN ( NOM32 , -2 , IRET )
                    CALL JJMZAT ( IC , ID(K) )
                  ENDIF
2               CONTINUE
                CRNOM = RNOM ( JRNOM(IC) + IDATCO )
                CALL JJLIBP ( IBACOL)
                IADDI(1) = IADD (JIADD(IC) + 2*IDATCO-1)
                IADDI(2) = IADD (JIADD(IC) + 2*IDATCO  )
                IF ( IADDI(1) .GT. 0 ) THEN
                  LONOI = LONO(JLONO(IC)+IDATCO)*LTYP(JLTYP(IC)+IDATCO)
                  CALL JXLIBD ( 0 ,IDATCO, IC , IADDI , LONOI )
                ENDIF
                IF (NIVO .GE. 2) THEN
                  CALL U2MESK('I','JEVEUX_7',1,CRNOM(1:24))
                ENDIF
                CALL JJCREN ( CRNOM(1:24) , -2 , IRET )
                CALL JJMZAT ( IC , IDATCO )
                NOMCO = '$$$$$$$$$$$$$$$$$$$$$$$$'
              ENDIF
            ENDIF
 150      CONTINUE
 160    CONTINUE
 100  CONTINUE
C FIN ------------------------------------------------------------------
      END
