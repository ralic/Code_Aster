      SUBROUTINE JELIBZ ( CLAS )
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
C TOLE CFT_726 CFT_720 CRP_18 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       CLAS
C ----------------------------------------------------------------------
C LIBERATION DE L'ENSEMBLE DES OBJETS MARQUES PAR -1
C
C IN  CLAS   : CLASSE DES OBJETS A LIBERER
C
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      PARAMETER      ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     +                 LONO    , HCOD    , CARA    , LUTI    , IMARQ   
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     +                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
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
     +                 DN2(N)
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     +               IDMARQ     , IDNOM      ,              IDLONG     ,
     +               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9 , IDNUM  = 10 )
C ----------------------------------------------------------------------
      INTEGER          NCLA1,NCLA2,IBACOL,IBMARQ,IC,ID,IRET,IX
      INTEGER          J,K,MARQI
      CHARACTER*32     CRNOM
      CHARACTER*1      KCLAS
C DEB ------------------------------------------------------------------
      KCLAS  = CLAS
      IF ( KCLAS .EQ. ' ' ) THEN
         NCLA1 = 1
         NCLA2 = INDEX ( CLASSE , '$' ) - 1
         IF ( NCLA2 .LT. 0 ) NCLA2 = N
      ELSE
         NCLA1 = INDEX ( CLASSE , KCLAS)
         NCLA2 = NCLA1
      ENDIF
      DO 100 IC = NCLA1 , NCLA2
        DO 150 J = 1 , NREMAX(IC)
          CRNOM = RNOM(JRNOM(IC)+J)
          IF ( CRNOM(1:1) .EQ. '?' .OR.
     &         CRNOM(25:26) .EQ. '$$' ) GOTO 150
          CALL JJCREN ( CRNOM , 0 , IRET )
          IF ( GENR(JGENR(IC)+J) .EQ. 'X' ) THEN
             IBACOL = IADM(JIADM(IC)+2*J-1)
             IF ( IBACOL .EQ. 0 ) GOTO 150
             ID = ISZON(JISZON + IBACOL + IDIADM)
             IF ( ID .GT. 0 ) THEN
C
C ------------- COLLECTION DISPERSEE ( OBJETS DE COLLECTION )
C
                IX     = ISZON(JISZON + IBACOL + IDMARQ)
                IBMARQ = IADM(JIADM(IC)+2*IX-1)
                NMAX   = ISZON(JISZON+IBACOL+IVNMAX )
                DO 170 K=1,NMAX
                  MARQI = ISZON(JISZON+IBMARQ-1+2*K-1)
                  IF ( MARQI .EQ. -1 ) THEN
                    CALL JJLIDE ( 'JELIBZ' , CRNOM , 2 )
                    GOTO 171
                  ENDIF
 170            CONTINUE
             ENDIF
C
C ---------- COLLECTION CONTIGUE OU DISPERSEE ( OBJETS ATTRIBUTS )
C
             DO 162 K = IDNUM,1,-1
               ID = ISZON(JISZON + IBACOL + K)
               IF ( ID .GT. 0 ) THEN
                 MARQI = IMARQ(JMARQ(IC)+2*ID-1)
                 IF ( MARQI .EQ. -1 ) THEN
                   CALL JJLIDE ( 'JELIBZ' , CRNOM , 2 )
                   GO TO 171
                 ENDIF
               ENDIF
 162         CONTINUE
 171         CONTINUE
          ELSE
C
C --------- OBJET SIMPLE
C
             MARQI = IMARQ(JMARQ(IC)+2*J-1)
             IF ( MARQI .EQ. -1 ) THEN
               CALL JJLIDE ( 'JELIBZ' , CRNOM , 1 )
             ENDIF
          ENDIF
 150    CONTINUE
 100  CONTINUE
C FIN ------------------------------------------------------------------
      END
