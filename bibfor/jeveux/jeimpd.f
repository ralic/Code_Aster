      SUBROUTINE JEIMPD ( UNIT , CLAS , CMESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C MODIF JEVEUX  DATE 10/04/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CRP_18 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           UNIT  
      CHARACTER*(*)            CLAS , CMESS
C ---------------------------------------------------------------------
C ROUTINE UTILISATEUR D'IMPRESSION DE LA LISTE DES OBJETS PRESENTS SUR
C LE FICHIER D'ACCES DIRECT ASSOCIE A UNE BASE
C
C IN  UNIT  : NUMERO D'UNITE LOGIQUE ASSOCIE AU FICHIER D'IMPRESSION
C IN  CLAS   : CLASSE ASSOCIEE A LA BASE ( ' ' : TOUTES LES CLASSES )
C IN  CMESS  : MESSAGE D'INFORMATION
C ---------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ---------------------------------------------------------------------
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
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    ,             KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
C ---------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IACCED/  IACCE(1)
      COMMON /JIACCE/  JIACCE(N),NBACCE(2*N)
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
C ---------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     +               IDMARQ     , IDNOM      ,             IDLONG     ,
     +               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,             IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C ---------------------------------------------------------------------
      CHARACTER*1      KCLAS , CGENR , CTYPE , CLASI , CGEN2
      CHARACTER*32     CRNOM
      LOGICAL          LCOL,LENTE
      INTEGER          IPGCEX,LGBL
C DEB -----------------------------------------------------------------
      IPGCEX = IPGC
      IPGC = -2
      LENTE = .TRUE.
      KCLAS = CLAS ( 1: MIN(1,LEN(CLAS)))
      IF ( UNIT .LE. 0 ) GOTO 9999
      IF ( KCLAS .EQ. ' ' ) THEN
         NCLA1 = 1
         NCLA2 = INDEX ( CLASSE , '$' ) - 1
         IF ( NCLA2 .LT. 0 ) NCLA2 = N
      ELSE
         NCLA1 = INDEX ( CLASSE , KCLAS )
         NCLA2 = NCLA1
      ENDIF
      DO 10 I = NCLA1 , NCLA2
        CLASI = CLASSE(I:I)
        IF ( CLASI .NE. ' ' ) THEN
          WRITE (UNIT,'(''1'',4A)' ) ('--------------------',K=1,4)
          WRITE(UNIT,*)'                                  '
          WRITE (UNIT,'(1X,2A)' )
     +          '       CONTENU DE LA BASE ',CLASI     ,
     +          '        ', CMESS(1:MIN(72,LEN(CMESS)))
          WRITE(UNIT,*)' NOM DE LA BASE               : ',NOMBAS(I)
          WRITE(UNIT,*)' NB D''ENREGISTREMENTS MAXIMUM : ',NBLMAX(I)
          LGBL=1024*LONGBL(I)*LOIS
          WRITE(UNIT,*)
     +               ' LONGUEUR D''ENREGISTREMENT (OCTETS): ',LGBL
          WRITE(UNIT,*)'                                  '
          WRITE (UNIT,'(    1X,4A)' ) ('--------------------',K=1,4)
          KJ = 1
          DO 5 J = 1 , NREMAX(I)
            CRNOM = RNOM(JRNOM(I)+J)
            IF ( CRNOM(1:1) .EQ. '?' ) GOTO 5
            IF ( MOD(KJ,25) .EQ. 1 .AND. LENTE ) THEN
               WRITE ( UNIT , '(/,A,A/)' )
     +     '---- NUM ------------- NOM ---------------- G T -L-'
     +     ,' -LOTY- -IADD- --LIADD- NB AC'
              LENTE = .FALSE.
            ENDIF
            CGENR = GENR(JGENR(I)+J)
            CTYPE = TYPE(JTYPE(I)+J)
            ILTYP = LTYP(JLTYP(I)+J)
            ILONO = LONO(JLONO(I)+J)
            IIADD = IADD(JIADD(I)+2*J-1)
            IF ( IIADD .EQ. 0 ) GOTO 6
            KJ = KJ + 1
            LENTE = .TRUE.
            LCOL = .FALSE.
            LIADD =        IADD(JIADD(I)+2*J)
            IACC   =       IACCE(JIACCE(I)+IIADD)
            WRITE(UNIT , 1001) J,CRNOM,CGENR,CTYPE,ILTYP,
     +                           ILONO,IIADD,LIADD,IACC
 6          CONTINUE
            IF ( CGENR .EQ. 'X' ) THEN
              IDATCO = J
              ICLACO = I
              LCOL   = .TRUE.
              CALL JJALLC ( I , J , 'L' , IBACOL )
              IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
              IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
              IF ( IXIADD .EQ. 0 ) GOTO 51
              IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
              NMAX   = ISZON ( JISZON + IBACOL + IVNMAX )
              CGEN2  = GENR(JGENR(I)+IXDESO)
              CTYPE  = TYPE(JTYPE(I)+IXDESO)
              ILTYP  = LTYP(JLTYP(I)+IXDESO)
              DO 50 KOC = 1,NMAX
                IBIADD = IADM ( JIADM(I) + 2*IXIADD-1 )
                KIADD    = ISZON ( JISZON + IBIADD - 1 + 2*KOC-1 )
                IF ( KIADD .EQ. 0 ) GOTO 50
                IF ( MOD(KJ,25) .EQ. 1 .AND. LENTE ) THEN
                  WRITE ( UNIT , '(/,A,A/)' )
     +            '---- NUM ------------- NOM -------------- E G T -L-'
     +           ,' -LOTY- -IADD- --LIADD- NB AC'
                  LENTE = .FALSE.
                ENDIF
                IIADD = ISZON ( JISZON + IBIADD - 1 + 2*KOC-1 )
                LIADD = ISZON ( JISZON + IBIADD - 1 + 2*KOC   )
                IACC  =       IACCE(JIACCE(I)+IIADD)
                IF ( IXLONO .EQ. 0 ) THEN
                 ILONO  = LONO(JLONO(I)+IXDESO)
                ELSE
                 IBLONO = IADM ( JIADM(I) + 2*IXLONO-1 )
                 ILONO  = ISZON ( JISZON + IBLONO - 1 + KOC )
                ENDIF
                KJ = KJ + 1
                LENTE = .TRUE.
                WRITE( CRNOM(25:32) , '(I8)' ) KOC
                WRITE(UNIT,1001) J,CRNOM,CGEN2,CTYPE,ILTYP,
     +                             ILONO,IIADD,LIADD,IACC
 50           CONTINUE
 51           CONTINUE
              IF ( LCOL ) THEN
                CALL JJLIDE ( 'JEIMPO' , CRNOM(1:24) , 2 )
              ENDIF
            ENDIF
    5     CONTINUE
          WRITE ( UNIT , '(/)' )
        ENDIF
   10 CONTINUE
 9999 CONTINUE
      IPGC = IPGCEX
 1001 FORMAT(I8,1X,A,'  -',2(A,'-'),I3,I7,I7,I9,I6)
C FIN -----------------------------------------------------------------
      END
