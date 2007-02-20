      SUBROUTINE JERECU ( CLAS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 19/02/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE  CRP_18 CRS_508 CRS_512 CRS_505
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*1         CLAS
C ----------------------------------------------------------------------
C MARQUE LIBRES LES ENREGISTREMENTS ASSOCIÉS AUX PETITS OBJETS QUAND
C L'ENSEMBLE DES OBJETS ASSOCIÉS A ETE DETRUIT
C
C IN  CLAS   : NOM DE CLASSE ASSOCIEE
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     ------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
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
C
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    ,             KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      LOGICAL          LITLEC
      COMMON /LFICJE/  LITLEC(N)
      COMMON /KUSADI/  IUSADI(1)
      COMMON /JUSADI/  JUSADI(N)
      COMMON /INBDET/  NBLIM(N),NBGROS(N),NBPETI(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     +               IDMARQ     , IDNOM      ,             IDLONG     ,
     +               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,             IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C     ------------------------------------------------------------------
      LOGICAL          ACTU
      CHARACTER*1      KCLAS
      INTEGER          ITP(1),JITP,IADITP,IADDI(2),IADDIB(2),LGBL,IADYN
C DEB ------------------------------------------------------------------
      IADDI(2)  = 0
      IADDIB(2) = 0
      KCLAS = CLAS
      IF ( KCLAS .EQ. ' ' ) THEN
        NCLA1 = 1
        NCLA2 = INDEX ( CLASSE , '$' ) - 1
        IF ( NCLA2 .LT. 0 ) NCLA2 = N
      ELSE
        NCLA1 = INDEX ( CLASSE , KCLAS)
        NCLA2 = NCLA1
      ENDIF
      DO 100 IC=NCLA1,NCLA2 
        IF ( NBPETI(IC) .LT. NBLIM(IC) ) GOTO 100
        LGBL = 1024*LONGBL(IC)*LOIS
        CALL JJALLS(LGBL,'V','I',LOIS,'INIT',ITP,JITP,IADITP,IADYN)
        ISZON(JISZON+IADITP-1) = ISTAT(2)
        ISZON(JISZON+ISZON(JISZON+IADITP-4)-4) = ISTAT(4)
C
C ----- DECHARGEMENT DES TAMPONS DE LECTURE ET D'ECRITURE
C ----- AFIN D'ACTUALISER LES ADRESSES DISQUES DES COLLECTIONS
C ----- STOCKEES DANS DES PETITS OBJETS
C
        IF ( IITECR(IC) .GT. 0 ) THEN
           CALL JXECRB (IC, IITECR(IC), KITECR(IC)+1, LGBL, 0, 0)
           IITECR(IC) = 0
        ENDIF
        IF ( LITLEC(IC) ) THEN
           CALL JXECRB (IC, IITLEC(IC), KITLEC(IC)+1, LGBL, 0, 0)
           LITLEC(IC) = .FALSE.
           IITLEC(IC) = 0
        ENDIF
C
C ----- BOUCLE "TANT QUE" SUR LES ENREGISTREMENTS UTILISES
C
        K = 1
        IDOSP = 0
        IDCOP = 0
 200    CONTINUE
C --------L'ENREGISTREMENT 1 N'EST JAMAIS RECUPERABLE
        K = K + 1
        IF ( K .LE. NBLUTI(IC) ) THEN
          IDCO  = IUSADI(JUSADI(IC)+3*K-2)
          IDOS  = IUSADI(JUSADI(IC)+3*K-1)
          NBDET = IUSADI(JUSADI(IC)+3*K  ) 
C
C --------L'ENREGISTREMENT CONTIENT DES PETITS OBJETS 
C --------ON PARCOURT LE CHAINAGE POUR DETERMINER SI UNE
C --------PARTIE DE L'ENREGISTREMENT EST OCCUPEE
C --------ON EXPLORE L'ENREGISTREMENT
C
          IF ( IDCO .EQ. 0 .AND. IDOS .EQ. 0 .AND. NBDET .GT. 0 ) THEN
            IADDI(1) = K
            CALL JXLIRO (IC, IADITP, IADDI, LGBL)
            ACTU = .TRUE.
            IDEC = 0
 300        CONTINUE
            IDCOL  = ISZON(JISZON+IADITP+IDEC  )
            IDOSL  = ISZON(JISZON+IADITP+IDEC+1)
            LGL  = ISZON(JISZON+IADITP+IDEC+2)
            IF ( IDCOL .EQ. 0 .AND. IDOSL .EQ. 0 ) THEN
              GOTO 350
            ELSE IF ( IDCOL .LT. 0 .OR. IDOSL .LT. 0 ) THEN
              GOTO 320
            ENDIF
            ACTU = .FALSE.
            GOTO 350
 320        CONTINUE
            IDEC = IDEC+LGL+3
            GOTO 300
 350        CONTINUE
            IF ( ACTU ) THEN
              IUSADI(JUSADI(IC)+3*K-2) = -1
              IUSADI(JUSADI(IC)+3*K-1) = -1
              IUSADI(JUSADI(IC)+3*K  ) =  0
            ENDIF
          ENDIF
          GOTO 200
        ENDIF
        IF (IADYN .NE. 0 ) THEN
          CALL HPDEALLC (IADYN, IBID, IBID)
        ELSE IF (IADITP .NE. 0) THEN
          CALL JJLIBP (IADITP)
        ENDIF
        NBPETI(IC) = 0
 100  CONTINUE
C FIN ------------------------------------------------------------------
      END
