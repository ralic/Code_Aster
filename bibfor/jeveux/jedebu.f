      SUBROUTINE JEDEBU(NBFI, LZON, MXZON, IADZON, LMO, CMES, CVIG,IDB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 28/09/2009   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_720 CFT_726 CRP_18 CRP_6 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             NBFI, LZON, MXZON, IADZON, LMO, IDB
      CHARACTER*(*)       CMES, CVIG
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR D'INITIALISATION GENERALE
C ROUTINE AVEC ADHERENCE SYSTEME CRAY : LOC
C
C IN  NBFI   : NOMBRE MAXIMUM DE BASES SIMULTANEES ( =< 5)
C IN  LZON   : TAILLE DE LA MEMOIRE (EN ENTIER)
C IN  MXZON  : LIMITE MEMOIRE DYNAMIQUE (EN ENTIER words)
C IN  IADZON : SI DIFFERENTE DE 0, ADRESSE DE LA ZONE A GERER
C IN  LMO    : LONGUEUR DE ZONE PRE ALLOUEE EN OCTETS
C IN  CMES   : NOM LOCAL DU FICHIER D'ERREUR
C IN  CVIG   : NOM LOCAL DU FICHIER LIVRE DE BORD
C IN  IDB    : PARAMETRE DE DEBUG
C              ( 0: RIEN, 1: MISE A UNDEF DES SEGMENTS DE VALEURS )
C
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      INTEGER          NBFIC
      COMMON /IPARJE/  NBFIC
      INTEGER          ILOC
      COMMON /ILOCJE/  ILOC
C ----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
C
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ   
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C ----------------------------------------------------------------------
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      LOGICAL          LITLEC
      COMMON /LFICJE/  LITLEC(N)
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C ----------------------------------------------------------------------
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
C ----------------------------------------------------------------------
      INTEGER          ISSTAT
      COMMON /ICONJE/  ISSTAT
      INTEGER          MSSTAT, LSSTAT
      COMMON /JCONJE/  MSSTAT, LSSTAT
C ----------------------------------------------------------------------
      INTEGER          DATEI
      COMMON /IHEUJE/  DATEI
C ----------------------------------------------------------------------
      INTEGER          ILLICI , JCLASS(0:255)
      COMMON /JCHAJE/  ILLICI , JCLASS
C ----------------------------------------------------------------------
      INTEGER          IDINIT   ,IDXAXD   ,ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      COMMON /IXADJE/  IDINIT(2),IDXAXD(2),ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      CHARACTER*4      KSTAT
      COMMON /KSTAJE/  KSTAT
      INTEGER          MSLOIS
      COMMON /JENVJE/  MSLOIS
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          IDN    , IEXT    , NBENRG
      COMMON /IEXTJE/  IDN(N) , IEXT(N) , NBENRG(N)
      INTEGER          LFIC,MFIC  
      COMMON /FENVJE/  LFIC,MFIC
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      INTEGER          ICDYN , MXLTOT
      COMMON /XDYNJE/  ICDYN , MXLTOT
      REAL *8          MXDYN , MCDYN , MLDYN , VMXDYN  
      COMMON /RDYNJE/  MXDYN , MCDYN , MLDYN , VMXDYN 
      REAL *8          SVUSE,SMXUSE   
      COMMON /STATJE/  SVUSE,SMXUSE  
C --------------------------------- ------------------------------------
      INTEGER          MXLICI , IPREM  , INIT
      INTEGER          ISPBEM , LBISEM , LOISEM , LOLSEM, LOUAEM
      INTEGER          LOR8EM , LOC8EM , ISNNEM , IRT
      REAL*8           MAXBAS
      PARAMETER      ( MXLICI = 67 )
      CHARACTER *(MXLICI) CLICIT
      DATA CLICIT/' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.$&_abcdefghijkl
     &mnopqrstuvwxyz'/
C DEB ------------------------------------------------------------------
C
C -----------------  ENVIRONNEMENT MACHINE -----------------------------
      LFIC = LOFIEM()
      VAL = MAXBAS(-1.0D0)
      IF (VAL .LE. 0 ) THEN
         MFIC = MOFIEM()
      ELSE
         MFIC = NINT(VAL)*1024
      ENDIF
      WRITE(6,'(/,1X,A,F12.2,A)') 'LIMITE TAILLE DES BASES       : '    
     &      ,MFIC/(1024*1024.0D0),'   Go '
      LBIS = LBISEM()
      LOR8 = LOR8EM()
      LOC8 = LOC8EM()
      LOIS = LOISEM()
      LOLS = LOLSEM()
      LOUA = LOUAEM()
      LUNDEF = ISNNEM()
      MSLOIS = LOIS - 1
      LDYN   = 0
      LGDYN  = LUNDEF
      MXDYN  = 0
      MCDYN  = 0
      MLDYN  = 0
      NBDYN  = 0
      NBFREE = 0
      ICDYN  = 0
      MXLTOT = 0
      SVUSE  = 16
      SMXUSE = SVUSE
C -----------------  NOMBRE DE BASES -----------------------------------
      NBFIC = MIN ( NBFI , N , LEN(CLASSE) )
      CALL ASSERT ( NBFIC .GT. 0 .AND. NBFIC .EQ. NBFI)
C -----------------  CONSTANTES DE STATUT DES SEGMENTS DE VALEURS ------
      KSTAT  = 'XUAD'
      ISSTAT = ISPBEM( LBIS - 3 )
      DO 2 K = 1 , 4
        ISTAT(K) = K * ISSTAT
 2    CONTINUE
      IDEBUG = IDB
C -----------------  ZONE MEMOIRE  -------------------------------------
      CALL ASSERT ( LZON .GT. 0 )
      LOSZON = LZON*LOIS
      LORC   = MAX ( LOR8 , LOC8 )
      IF ( MOD ( LOSZON , LORC ) .NE. 0 ) THEN
         LOSZON = ( 1 + LOSZON / LORC ) * LORC
      ENDIF
      LISZON = LOSZON/LOIS
      IF (IADZON .GT. 0) THEN
         LISZON = MIN(LISZON,LMO/LOIS)
      ENDIF
      VMXDYN = MXZON*LOIS
      IF ( MXZON .EQ. 0 ) THEN
        VMXDYN = 1024
      ENDIF     
      CALL JXALLM ( IADZON, ISZON , LISZON , JISZON )
      LK1ZON = LISZON * LOIS
      JK1ZON = JISZON * LOIS
      ISZON(JISZON + 1 )         = 0
      ISZON(JISZON + 2 )         = 0
      ISZON(JISZON + 3 )         = 0
      ISZON(JISZON + 4 )         = 0
      ISZON(JISZON + 5 )         = LISZON - 3
      ISZON(JISZON + 6 )         = 0
      ISZON(JISZON + 7 )         = 0
      ISZON(JISZON + 8 )         = ISTAT(1)
      ISZON(JISZON + LISZON - 0) = ISTAT(1)
      ISZON(JISZON + LISZON - 1) = 0
      ISZON(JISZON + LISZON - 2) = 0
      ISZON(JISZON + LISZON - 3) = 0
      ISZON(JISZON + LISZON - 4) = 4
      ISZON(JISZON + LISZON - 5) = 0
      ISZON(JISZON + LISZON - 6) = 0
      ISZON(JISZON + LISZON - 7) = ISTAT(1)
      ILOC = LOC ( ISZON(JISZON) )
      IDINIT(1) = 5
      IDXAXD(1) = 5
      IDINIT(2) = 0
      ITRECH = 1
      ITIAD  = 1
      ITCOL  = 1
C -------------------  POINTEURS D'ATTRIBUTS  --------------------------
      DO 5 I = 1 , LEN(CLASSE)
         CLASSE(I:I) = '$'
   5  CONTINUE
      DO 10 I = 1 , NBFIC
        JGENR(I)  = 0
        JTYPE(I)  = 0
        JLTYP(I)  = 0
        JDOCU(I)  = 0
        JORIG(I)  = 0
        JRNOM(I)  = 0
        JLONO(I)  = 0
        JLONG(I)  = 0
        JDATE(I)  = 0
        JIADD(I)  = 0
        JIADM(I)  = 0
        JMARQ(I)  = 0
        JLUTI(I)  = 0
        JCARA(I)  = 0
        JHCOD(I)  = 0
        NREMAX(I) = 0
        NRHCOD(I) = 0
        NREUTI(I) = 0
        NBLMAX(I) = 0
        NBENRG(I) = 1
        NBLUTI(I) = 0
        LONGBL(I) = 0
        KITLEC(I) = 0
        KITECR(I) = 0
        KIADM (I) = 0
        IITLEC(I) = 0
        IITECR(I) = 0
        NITECR(I) = 0
        LITLEC(I) = .FALSE.
        NOMFIC(I)   = '        '
        KSTOUT(I)   = '        '
        KSTINI(I)   = '        '
        CLASSE(I:I) = ' '
        DN2(I)      = ' '
   10 CONTINUE
C -------------------  CONSTANTES DE GESTION  --------------------------
      LSSTAT = LBIS-4
      MSSTAT = 0
      DO 20 K = 1,LBIS-4
         MSSTAT = MSSTAT + ISPBEM(K)
 20   CONTINUE
      BL32  = ' '
C
      CALL JXDATE ( DATEI )
C
      ILLICI = -1
      DO 30 K = 0 , 255
         JCLASS(K) = ILLICI
 30   CONTINUE
      DO 31 K = 1 , MXLICI
         JCLASS(ICHAR( CLICIT(K:K) ) ) = K
 31   CONTINUE
C
      KDESMA(1) = 0
      KDESMA(2) = 0
      LGDUTI = 0
      KPOSMA(1) = 0
      KPOSMA(2) = 0
      LGPUTI = 0
      IPGC   = 0
      INIT   = 100
      IRT    = 0
      IPREM  = JJPREM(INIT,IRT)
C FIN ------------------------------------------------------------------
      END
