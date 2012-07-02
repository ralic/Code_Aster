      SUBROUTINE JJALLS(LONOI,IC,GENRI,TYPEI,LTY,CI,ITAB,JITAB,IADMI,
     &                  IADYN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
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
C TOLE CRP_18 CRP_6 CRS_508 CRS_512 CRS_505 CRP_20
      IMPLICIT NONE
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
C-----------------------------------------------------------------------
      INTEGER I ,IADA ,IDM ,IERR ,IESSAI ,ILDYNA ,JCARA 
      INTEGER JDATE ,JHCOD ,JIADD ,JIADM ,JISZO2 ,JLONG ,JLONO 
      INTEGER JLTYP ,JLUTI ,JMARQ ,LGBL ,LOISEM ,LSI ,LSO 
      INTEGER LTOT ,N ,NDE 
C-----------------------------------------------------------------------
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
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      REAL *8          MXDYN , MCDYN , MLDYN , VMXDYN , LGIO
      COMMON /RDYNJE/  MXDYN , MCDYN , MLDYN , VMXDYN , LGIO(2)
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,              IDLONG     ,
     &               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 , IDNUM  = 10 )
C ----------------------------------------------------------------------
      INTEGER          INIT,IBLANC,IDEC(2),VALLOC,LSIC
      INTEGER          IC,IVAL(4),UNMEGA
      LOGICAL          LINIT,LDEPS
      CHARACTER *8     CBLANC
      EQUIVALENCE    ( CBLANC,IBLANC )
      PARAMETER      ( NDE = 6)
C ----------------------------------------------------------------------
C REMARQUE : LE PARAMETER NDE EST AUSSI DEFINI DANS JXLIRO JXECRO
C ----------------------------------------------------------------------
      DATA CBLANC     /'        '/
C DEB ------------------------------------------------------------------
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
      CALL ASSERT(LSO.NE.0)
      LSI = LSO / LOIS
C
C     LE SEGMENT DE VALEURS EST ALLOUE DYNAMIQUEMENT SI LDYN=1 ET SI
C     SA LONGUEUR EST SUPERIEURE A LGDYN
C
      IESSAI = 0
      ILDYNA = 0
C
      LSIC = LSI + 8
 50   CONTINUE
      ILDYNA = ILDYNA+1
      IF ( MCDYN+LSIC*LOIS .GT. VMXDYN ) THEN
        IF ( ILDYNA .GT. 1 ) THEN
          UNMEGA=1048576
          IVAL(1)=(LSIC*LOIS)/UNMEGA
          IVAL(2)=NINT(VMXDYN)/UNMEGA
          IVAL(3)=NINT(MCDYN)/UNMEGA
          IVAL(4)=(LTOT*LOIS)/UNMEGA
          IF (LDYN .EQ. 1) THEN
             CALL JEIMPM ( 6 )
          ENDIF
          CALL U2MESI('F','JEVEUX_62',4,IVAL)
        ELSE
          CALL JJLDYN(2,-1,LTOT)
          IF ( LTOT/LOIS .LT. LSIC ) CALL JJLDYN(0,-1,LTOT)
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
            CALL JEIMPM ( 6 )
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
C
      ISZON( IDM        ) = IDM + LSI + 8 - JISZON
      ISZON( IDM     +1 ) = 0
      ISZON( IDM     +2 ) = 0
      ISZON( IDM     +3 ) = ISTAT(1)
      ISZON( IDM+LSI+ 4 ) = ISTAT(1)
      ISZON( IDM+LSI+ 5 ) = 0
      ISZON( IDM+LSI+ 6 ) = 0
      ISZON( IDM+LSI+ 7 ) = 0
C
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
