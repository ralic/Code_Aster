      SUBROUTINE JELIBZ ( CLAS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 18/03/2013   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      CHARACTER*(*)       CLAS
C ----------------------------------------------------------------------
C LIBERATION DE L'ENSEMBLE DES OBJETS MARQUES PAR -1
C
C IN  CLAS   : CLASSE DES OBJETS A LIBERER
C
C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER JCARA ,JDATE ,JDOCU ,JGENR ,JHCOD ,JIADD ,JIADM 
      INTEGER JLONG ,JLONO ,JLTYP ,JLUTI ,JMARQ ,JORIG ,JRNOM 
      INTEGER JTYPE ,N ,NMAX 
C-----------------------------------------------------------------------
      PARAMETER      ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
      INTEGER        IVNMAX               , IDIADM     ,
     +               IDMARQ                ,
     +                 IDNUM
      PARAMETER    ( IVNMAX = 0   , IDIADM = 3 ,
     +               IDMARQ = 4   ,
     +                 IDNUM  = 10 )
C ----------------------------------------------------------------------
      INTEGER          NCLA1,NCLA2,IBACOL,IBMARQ,IC,ID,IRET,IX
      INTEGER          J,K,MARQI,ICLASI
      CHARACTER*32     CRNOM, D32
      CHARACTER*1      KCLAS
      DATA             D32 /'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/
C DEB ------------------------------------------------------------------
      KCLAS  = CLAS
      ICLASI = ICLAS
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
C          CALL JJCREN ( CRNOM , 0 , IRET )
          IF ( GENR(JGENR(IC)+J) .EQ. 'X' ) THEN
             ICLAS = IC
             ICLACO = IC
             IDATCO = J
             NOMCO = CRNOM
             NOMOC = D32
             IF ( ICLASI .NE. ICLACO ) THEN
                NOMOS = D32
             ENDIF
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
             ICLAS = IC
             ICLAOS = IC
             IDATOS = J
             NOMOS = CRNOM
             IF ( ICLASI .NE. ICLAOS ) THEN
               NOMCO = D32
               NOMOC = D32
             ENDIF
             MARQI = IMARQ(JMARQ(IC)+2*J-1)
             IF ( MARQI .EQ. -1 ) THEN
               CALL JJLIDE ( 'JELIBZ' , CRNOM , 1 )
             ENDIF
          ENDIF
 150    CONTINUE
 100  CONTINUE
C FIN ------------------------------------------------------------------
      END
