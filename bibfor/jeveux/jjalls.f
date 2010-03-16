      SUBROUTINE JJALLS(LONOI,IC,GENRI,TYPEI,LTY,CI,ITAB,JITAB,IADMI,
     &                  IADYN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 15/03/2010   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
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
C TOLE CFT_720 CFT_726 CRP_18 CRP_6 CRS_508 CRS_512 CRS_505 CRP_20
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            LONOI,LTY,ITAB(*),JITAB,IADMI,IADYN
      CHARACTER*(*)            GENRI,TYPEI     ,CI
C ----------------------------------------------------------------------
C ALLOUE UN SEGMENT DE VALEUR EN MEMOIRE
C
C IN  LONOI  : LONGUEUR EN OCTETS DU SEGMENT DE VALEUR
C IN  IC     : CLASSE DE L'OBJET
C IN  GENRI  : GENRE DE L'OBJET JEVEUX
C IN  TYPEI  : TYPE DE L'OBJET JEVEUX
C IN  LTY    : LONGUEUR DU TYPE DE L'OBJET JEVEUX
C IN  CI     : = 'INIT' POUR INITIALISER LE SEGMENT DE VALEUR
C IN  ITAB   : TABLEAU PAR RAPPORT AUQUEL ON DETERMINE JITAB
C OUT JITAB  : ADRESSE DANS ITAB DU SEGMENT DE VALEUR
C OUT IADMI  : ADRESSE DU PREMIER MOT DU SEGMENT DE VALEUR
C OUT IADYN  : ADRESSE DU TABLEAU ALLOUE DYNAMIQUEMENT
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
      PARAMETER      ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
C ----------------------------------------------------------------------
      INTEGER          IDINIT   ,IDXAXD   ,ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      COMMON /IXADJE/  IDINIT(2),IDXAXD(2),ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      REAL *8          MXDYN , MCDYN , MLDYN , VMXDYN , LGIO
      COMMON /RDYNJE/  MXDYN , MCDYN , MLDYN , VMXDYN , LGIO
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,              IDLONG     ,
     &               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 , IDNUM  = 10 )
C ----------------------------------------------------------------------
      INTEGER          INIT,IADDI(2),IBLANC,ID(2),IDEC(2),VALLOC,LSIC
      INTEGER          IC,IVAL(4)
      LOGICAL          LEXACT,LEPS,LINIT,LDEPS,LAMOV,LXA,LXD,RETRO
      CHARACTER *8     CBLANC
      EQUIVALENCE    ( CBLANC,IBLANC )
      PARAMETER      ( NDE = 6)
C ----------------------------------------------------------------------
C REMARQUE : LE PARAMETER NDE EST AUSSI DEFINI DANS JXLIRO JXECRO
C ----------------------------------------------------------------------
      DATA CBLANC     /'        '/
C DEB ------------------------------------------------------------------
      LXA = .NOT. ( IDXAXD(1) .EQ. IDINIT(1) )
      LXD = LXA
      LTOT  = 0
      JITAB = 0
      IADMI = 0
      IADYN = 0
      IDEC(1) = 3
      IDEC(2) = 4
      LINIT = ( CI(1:4) .EQ. 'INIT' )
      LSO = LONOI
C
C     LA TAILLE DU SEGMENT DE VALEURS EST AJUSTEE POUR S'ALLIGNER
C     SUIVANT LA LONGUEUR DU TYPE (SI SUPERIEUR A L'ENTIER)
C

      IF ( LTY .NE. LOIS ) THEN
        LSO = LSO + LTY
        IF ( MOD(LSO,LOIS) .NE. 0 ) LSO = (1 + LSO/LOIS) * LOIS
      ENDIF
C
C     LA TAILLE DU SEGMENT DE VALEURS EST AJUSTEE A LA LONGUEUR DE BLOC
C     SI ON EST COMPRIS ENTRE LGBL-(NDE*LOIS) ET LGBL POUR DISPOSER DE
C     LA PLACE MINIMUM NECESSAIRE POUR LES GROS OBJETS
C
      IF ( IC .NE. 0 ) THEN
        IF ( LONGBL(IC) .GT. 1 ) THEN
          LGBL = 1024*LONGBL(IC)*LOIS
          IF (LSO .GE. LGBL-NDE*LOIS .AND. LSO .LT. LGBL) THEN
            LSO = LGBL
          ENDIF
        ENDIF
      ENDIF
      CALL ASSERT(LOIS.NE.0)
      LSI = LSO / LOIS
C
C     LE SEGMENT DE VALEURS EST ALLOUE DYNAMIQUEMENT SI LDYN=1 ET SI
C     SA LONGUEUR EST SUPERIEURE A LGDYN
C
      IESSAI = 0
      ILDYNA = 0
      IF ( (LDYN .EQ. 1 .OR. LDYN.EQ. 2).AND. LSI.GE.LGDYN ) THEN
        LSIC = LSI + 9
 50     CONTINUE
        ILDYNA = ILDYNA+1
        IF ( MCDYN+LSIC*LOIS .GT. VMXDYN ) THEN
          IF ( ILDYNA .GT. 1 ) THEN
            IF (LDYN .EQ. 1) THEN
               CALL JEIMPM ( 'MESSAGE',' LIMITE MEMOIRE DYNAMIQUE,'
     &                     //' IMPOSEE ATTEINTE')
            ENDIF
            IVAL(1)=LSIC*LOIS
            IVAL(2)=NINT(VMXDYN)
            IVAL(3)=NINT(MCDYN)
            IVAL(4)=LTOT*LOIS
            CALL U2MESI('F','JEVEUX_62',4,IVAL)
          ELSE
            CALL JJLDYN(2,-1,LTOT)
            IF ( LTOT .LT. LSIC ) CALL JJLDYN(0,-1,LTOT)
            GOTO 50
          ENDIF
        ENDIF
        IESSAI = IESSAI+1
        CALL  HPALLOC ( IADA , LSIC , IERR , 0 )
        IF ( IERR .EQ. 0 ) THEN
          VALLOC = LOC(ISZON)
          JISZO2 = (IADA - VALLOC)/LOISEM()
          IADMI  = JISZO2 + 5 - JISZON
          IDM    = JISZO2 + 1
          IADYN  = IADA
          MCDYN  = MCDYN + LSIC*LOIS
          MXDYN  = MAX(MXDYN,MCDYN)
          NBDYN  = NBDYN + 1
        ELSE
          IF ( IESSAI .GT. 1 ) THEN
            IF (LDYN .EQ. 1) THEN
              CALL JEIMPM ( 'MESSAGE',' MEMOIRE INSUFFISANTE,'
     &                    //' ALLOCATION IMPOSSIBLE')
            ENDIF
            IVAL(1)=LSIC*LOIS
            IVAL(2)=LTOT*LOIS
            CALL U2MESI('F','JEVEUX_60',2,IVAL)
          ELSE
            CALL JJLDYN(2,-1,LTOT)
            IF ( LTOT .LT. LSIC ) CALL JJLDYN(0,-1,LTOT)
            GOTO 50
          ENDIF
        ENDIF
        ISZON( IDM        ) = IDM + LSI + 8 - JISZON
        ISZON( IDM     +1 ) = 0
        ISZON( IDM     +2 ) = 0
        ISZON( IDM     +3 ) = ISTAT(1)
        ISZON( IDM+LSI+ 4 ) = ISTAT(1)
        ISZON( IDM+LSI+ 5 ) = 0
        ISZON( IDM+LSI+ 6 ) = 0
        ISZON( IDM+LSI+ 7 ) = 0
        ISZON( IDM+LSI+ 8 ) = 0
        GOTO 10
      ENDIF
C
      MAPLAC = 0
C
      IF ( LONOI .LE. 0 ) THEN
        CALL U2MESI('F','JEVEUX_57',1,LONOI)
      ENDIF
      IF (ITCOL .EQ. 3 .AND. LSI .LT. LMOTS) THEN
        ITRC = 4
      ELSEIF (ITCOL .EQ. 4) THEN
        ITRC = 2
      ELSE
        ITRC = ITRECH
      ENDIF
C
 800  CONTINUE
      IZ = 1
      IF ( ITCOL .EQ. 4 .AND. LSI .GT. LMOTS) THEN
        IZ = 2
      ENDIF
      RETRO = ITRC .EQ. 4
      GOTO (100,200,300,200) ITRC
 100  CONTINUE
      ID(2) = LISZON - 4
      ID(1) = ISZON ( JISZON+ID(2) )
      IF( ISZON( JISZON+ID(2)-IDEC(1) ) .EQ. ISTAT(1) ) THEN
        IF( ISZON( JISZON+ID(1)+IDEC(2) ) .EQ. ISTAT(1) ) THEN
C
C ------- ZONE LIBRE ( XX ) DU FOND
C
          IF ( ID(1)-ID(2)-8 .GE. LSI ) THEN
            MAPLAC = ID(1)-ID(2)-8
            LEXACT = MAPLAC .EQ. LSI
            IDM    = ID(2)+1
            IS     = ISZON( JISZON+IDM )
            GOTO 500
          ENDIF
        ENDIF
      ENDIF
 200  CONTINUE
C
C --- ZONE AMOVIBLE ( XA + XX )
C
      IF ( RETRO ) THEN
        ID(1) = LISZON - 4
        LXA   = .FALSE.
        I1    = 2
        I2    = 1
      ELSE
        ID(1) = IDXAXD(IZ)
        I1    = 1
        I2    = 2
      ENDIF
      IDA    = ID(1)
      MAPLAC = 0
      LEXACT = .FALSE.
      LAMOV  = .FALSE.
 210  CONTINUE
      ID(2) = ISZON ( JISZON+ID(1) )
      IF ( ID(2) .EQ. 0 ) THEN
        IF ( LXA ) THEN
          LXA   = .FALSE.
          ID(1) = IDINIT(IZ)
          IDA   = IDINIT(IZ)
          GOTO 210
        ELSE
          GOTO 300
        ENDIF
      ENDIF
      ISTB = ISZON( JISZON+ID(I1)+IDEC(I1) )
      IF( ISTB .EQ. ISTAT(1) ) THEN
        ISTA = ISZON( JISZON+ID(I2)-IDEC(I2))
        IF( ISTA .EQ. ISTAT(3) .OR. ISTA .EQ. ISTAT(1) ) THEN
          IF ( .NOT. LAMOV ) THEN
            LAMOV = .TRUE.
            IDA   = ID(1)
          ENDIF
          LAPLAC =  ABS(ID(2)-IDA)-8
          IF  ( LAPLAC .GE. LSI ) THEN
            MAPLAC = LAPLAC
            LEXACT = LAPLAC .EQ. LSI
            IF (RETRO) THEN
              IDM    = ID(2) + 1
              IS     = IDA + 1
              IDA    = IDM
            ELSE
              IDM    = IDA
              IS     = ID(2)
            ENDIF
            GOTO 220
          ENDIF
          ID(1)  = ID(2)
          GOTO 210
        ENDIF
      ENDIF
      LAMOV = .FALSE.
      ID(1) = ID(2)
      GOTO 210
C --- DESTRUCTION DES SEGMENTS DE VALEURS LIBERES
 220  CONTINUE
      IDI = IDA
 225  CONTINUE
      ISI = ISZON ( JISZON+IDI )
      IF (ISI .EQ. 0 ) GOTO 500
      IF ( ISI .LE. IS ) THEN
        ISTB = ISZON(JISZON + ISI - 4 )
        IF ( ISTB .EQ. ISTAT(3) ) THEN
           IDATOI = ISZON( JISZON +IDI+2 )
           ICLAI  = ISZON( JISZON +ISI-2 )
           IDATCI = ISZON( JISZON +ISI-3 )
           IF ( IDATCI .GT. 0 ) THEN
             IF ( IDATOI .EQ. 0 ) THEN
                IADM ( JIADM(ICLAI)+2*IDATCI-1 ) = 0
                IADM ( JIADM(ICLAI)+2*IDATCI   ) = 0
             ELSE
                IBACOL = IADM ( JIADM(ICLAI) + 2*IDATCI-1 )
                CALL ASSERT (IBACOL .NE. 0)
                IXIADM = ISZON( JISZON+IBACOL+IDIADM )
                IBIADM = IADM( JIADM(ICLAI)+2*IXIADM-1 )
                IF ( IBIADM .GT. 0 ) THEN
                  ISZON( JISZON+IBIADM-1+2*IDATOI-1) = 0
                  ISZON( JISZON+IBIADM-1+2*IDATOI  ) = 0
                ENDIF
             ENDIF
           ELSE
              IADM ( JIADM(ICLAI)+2*IDATOI-1 ) = 0
              IADM ( JIADM(ICLAI)+2*IDATOI   ) = 0
           ENDIF
        ENDIF
        IDI = ISI
        GOTO 225
      ENDIF
 300  CONTINUE
      IF ( MAPLAC .LT. LSI  ) THEN
C
C ----- ZONE DECHARGEABLE ( XD + XA + XX )
C
        IF ( RETRO ) THEN
          ID(1) = LISZON - 4
          LXD   = .FALSE.
          I1    = 2
          I2    = 1
        ELSE
          ID(1) = IDXAXD(IZ)
          I1    = 1
          I2    = 2
        ENDIF
        IDA   = ID(1)
        MAPLAC = 0
        LEXACT = .FALSE.
        LAMOV  = .FALSE.
 310    CONTINUE
        ID(2) = ISZON ( JISZON+ID(1) )
        IF ( ID(2) .EQ. 0 ) THEN
          IF ( LXD ) THEN
            LXD   = .FALSE.
            ID(1) = IDINIT(IZ)
            IDA   = IDINIT(IZ)
            GOTO 310
          ELSE
            GOTO 500
          ENDIF
        ENDIF
        ISTB = ISZON( JISZON+ID(I1)+IDEC(I1) )
        IF( ISTB .EQ. ISTAT(1) ) THEN
          ISTA = ISZON(JISZON+ID(I2)-IDEC(I2) )
          IF (ISTA .EQ. ISTAT(3) .OR. ISTA .EQ. ISTAT(4) .OR.
     &        ISTA .EQ. ISTAT(1) ) THEN
            IF ( .NOT. LAMOV ) THEN
              LAMOV = .TRUE.
              IDA   = ID(1)
            ENDIF
            LAPLAC =  ABS(ID(2)-IDA)-8
            IF ( LAPLAC .GE. LSI ) THEN
              MAPLAC = LAPLAC
              LEXACT = LAPLAC .EQ. LSI
              IF (RETRO) THEN
                IDM    = ID(2) + 1
                IS     = IDA + 1
                IDA    = IDM
              ELSE
                IDM    = IDA
                IS     = ID(2)
              ENDIF
              GOTO 320
            ENDIF
            ID(1) = ID(2)
            GOTO 310
          ENDIF
        ENDIF
        LAMOV = .FALSE.
        ID(1) = ID(2)
        GOTO 310
      ENDIF
C --- DECHARGEMENT OU DESTRUCTION DES SEGMENTS DE VALEURS LIBERES
 320  CONTINUE
      IDI = IDA
 325  CONTINUE
      ISI = ISZON ( JISZON+IDI )
      IF (ISI .EQ. 0 ) GOTO 500
      IF ( ISI .LE. IS ) THEN
        ISTA = ISZON( JISZON+ISI-4 )
        IF ( ISTA .EQ. ISTAT(3) .OR. ISTA .EQ. ISTAT(4) ) THEN
           IDATOI = ISZON( JISZON+IDI+2 )
           ICLAI  = ISZON( JISZON+ISI-2 )
           IDATCI = ISZON( JISZON+ISI-3 )
           IF ( IDATCI .GT. 0 ) THEN
             IBACOL = IADM ( JIADM(ICLAI)+2*IDATCI-1 )
             CALL ASSERT (IBACOL .NE. 0)
             IXDESO = ISZON( JISZON+IBACOL+IDDESO )
             LTYPI  = LTYP ( JLTYP(ICLAI)+IXDESO )
             IXIADM = ISZON( JISZON+IBACOL+IDIADM )
             IBIADM = IADM ( JIADM(ICLAI)+2*IXIADM-1 )
             IXIADD = ISZON( JISZON +IBACOL+IDIADD )
             IBIADD = IADM ( JIADM(ICLAI)+2*IXIADD-1 )
             IF (ISTA .EQ. ISTAT(4) ) THEN
               IADMI = ISZON ( JISZON+IBIADM-1+2*IDATOI-1 )
               IADDI(1) = ISZON ( JISZON+IBIADD-1+2*IDATOI-1 )
               IADDI(2) = ISZON ( JISZON+IBIADD-1+2*IDATOI   )
               IXLONO = ISZON( JISZON+IBACOL+IDLONO )
               IF ( IXLONO .GT. 0 ) THEN
                 IBLONO = IADM ( JIADM(ICLAI)+2*IXLONO-1 )
                 LSV = ISZON( JISZON+IBLONO+IDATOI-1 ) * LTYPI
               ELSE
                 LSV = LONO( JLONO(ICLAI)+IXDESO ) * LTYPI
               ENDIF
               CALL JXECRO (ICLAI,IADMI,IADDI,LSV,IDATCI,IDATOI)
               ISZON( JISZON+IBIADD-1+2*IDATOI-1 ) = IADDI(1)
               ISZON( JISZON+IBIADD-1+2*IDATOI   ) = IADDI(2)
             ENDIF
             ISZON( JISZON+IBIADM-1+2*IDATOI-1 ) = 0
             ISZON( JISZON+IBIADM-1+2*IDATOI   ) = 0
           ELSE
             IF (ISTA .EQ. ISTAT(4) ) THEN
               IADMI = IADM ( JIADM(ICLAI)+2*IDATOI-1 )
               IADDI(1) = IADD ( JIADD(ICLAI)+2*IDATOI-1 )
               IADDI(2) = IADD ( JIADD(ICLAI)+2*IDATOI   )
               LTYPI = LTYP( JLTYP(ICLAI)+IDATOI )
               LSV   = LONO( JLONO(ICLAI)+IDATOI ) * LTYPI
               CALL JXECRO ( ICLAI, IADMI, IADDI, LSV, 0, IDATOI)
               IADD( JIADD(ICLAI)+2*IDATOI-1 ) = IADDI(1)
               IADD( JIADD(ICLAI)+2*IDATOI   ) = IADDI(2)
             ENDIF
             IADM( JIADM(ICLAI)+2*IDATOI-1 ) = 0
             IADM( JIADM(ICLAI)+2*IDATOI   ) = 0
           ENDIF
        ENDIF
        IDI = ISI
        GOTO 325
      ENDIF
 500  CONTINUE
      LEPS = ( MAPLAC .GT. LSI .AND. MAPLAC .LT. LSI+9 )
      IF ( LEXACT .OR. LEPS ) THEN
        LSI = MAPLAC
        IF (RETRO) THEN
          ISZON( JISZON+IS-LSI-8 ) = IS
          ISZON( JISZON+IS-LSI-7 ) = 0
          ISZON( JISZON+IS-LSI-6 ) = 0
          ISZON( JISZON+IS-LSI-5 ) = ISTAT(1)
          ISZON( JISZON+IS    -1 ) = IS-LSI-9
          ISZON( JISZON+IS    -2 ) = 0
          ISZON( JISZON+IS    -3 ) = 0
          ISZON( JISZON+IS    -4 ) = ISTAT(1)
          IDM = IS - LSI - 8
        ELSE
          ISZON( JISZON+IDM       ) = IDM+LSI+8
          ISZON( JISZON+IDM    +1 ) = 0
          ISZON( JISZON+IDM    +2 ) = 0
          ISZON( JISZON+IDM    +3 ) = ISTAT(1)
          ISZON( JISZON+IDM+LSI+4 ) = ISTAT(1)
          ISZON( JISZON+IDM+LSI+5 ) = 0
          ISZON( JISZON+IDM+LSI+6 ) = 0
          ISZON( JISZON+IDM+LSI+7 ) = IDM-1
        ENDIF
      ELSE IF ( MAPLAC .GE. LSI + 9 ) THEN
        ISZON( JISZON+IDM   + 1 ) = 0
        ISZON( JISZON+IDM   + 2 ) = 0
        ISZON( JISZON+IDM   + 3 ) = ISTAT(1)
        ISZON( JISZON+IS    - 2 ) = 0
        ISZON( JISZON+IS    - 3 ) = 0
        ISZON( JISZON+IS    - 4 ) = ISTAT(1)
        IF (RETRO) THEN
          ISZON( JISZON+IDM       ) = IS - LSI - 8
          ISZON( JISZON+IS-LSI-12 ) = ISTAT(1)
          ISZON( JISZON+IS-LSI-11 ) = 0
          ISZON( JISZON+IS-LSI-10 ) = 0
          ISZON( JISZON+IS-LSI- 9 ) = IDM - 1
          ISZON( JISZON+IS-LSI- 8 ) = IS
          ISZON( JISZON+IS-LSI- 7 ) = 0
          ISZON( JISZON+IS-LSI- 6 ) = 0
          ISZON( JISZON+IS-LSI- 5 ) = ISTAT(1)
          ISZON( JISZON+IS    - 1 ) = IS - LSI - 9
          IDM = IS - LSI - 8
        ELSE
          ISZON( JISZON+IDM        ) = IDM + LSI + 8
          ISZON( JISZON+IDM+LSI+ 4 ) = ISTAT(1)
          ISZON( JISZON+IDM+LSI+ 5 ) = 0
          ISZON( JISZON+IDM+LSI+ 6 ) = 0
          ISZON( JISZON+IDM+LSI+ 7 ) = IDM - 1
          ISZON( JISZON+IDM+LSI+ 8 ) = IS
          ISZON( JISZON+IDM+LSI+ 9 ) = 0
          ISZON( JISZON+IDM+LSI+10 ) = 0
          ISZON( JISZON+IDM+LSI+11 ) = ISTAT(1)
          ISZON( JISZON+IS     - 1 ) = IDM + LSI + 7
        ENDIF
      ELSE
         IF (IDFR .GT. 0) THEN
           CALL JJCPSG (0.0D0 , 0)
           GOTO 800
         ENDIF
         CALL JEPRSG ( 'MESSAGE', 0.1D0 , 1 )
         CALL JEIMPM ( 'MESSAGE',' MEMOIRE INSUFFISANTE ')
         CALL U2MESI ( 'F','JEVEUX_32' , 1 , LSI)
      ENDIF
C
      IF (.NOT.RETRO) THEN
        IF (ITIAD .EQ. 1 .OR. ITIAD .EQ. 3) THEN
          IDXAXD(IZ) = ISZON( JISZON+IDM )
        ELSE
          IDXAXD(IZ) = IDINIT(IZ)
        ENDIF
      ENDIF
C
      IADMI = IDM + 4
 10   CONTINUE
      LDEPS = .TRUE.
      CALL JXLOCS (ITAB, GENRI, LTY, LONOI, IADMI, LDEPS, JITAB)
C
      IF ( LINIT ) THEN
         INIT = 0
         IF ( TYPEI(1:1) .EQ. 'K' ) INIT = IBLANC
         DO 20 I = 1 , LSI
            ISZON ( JISZON+IADMI+I-1 ) = INIT
 20      CONTINUE
      END IF
C FIN ------------------------------------------------------------------
      END
