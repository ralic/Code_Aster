      SUBROUTINE JJLIDE ( NOMAP , NOMLU , ITYPE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 04/11/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C TOLE CFT_720 CFT_726 CRP_18 CRP_20 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       NOMAP , NOMLU
      INTEGER                             ITYPE
C ----------------------------------------------------------------------
C LIBERATION D'UN OBJET JEVEUX
C
C IN  NOMAP : NOM DE LA ROUTINE APPELANTE (JELIBE,JETASS,JELIBF)
C IN  NOMLU : NOM DE L'OBJET A LIBERER
C IN  ITYPE  : TYPE D'OBJET: 1, 2 OU 3
C
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
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
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
C ----------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          DATEI
      COMMON /IHEUJE/  DATEI
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
      INTEGER          IDINIT   ,IDXAXD   ,ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      COMMON /IXADJE/  IDINIT(2),IDXAXD(2),ITRECH,ITIAD,ITCOL,LMOTS,IDFR
C ----------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     +               IDMARQ     , IDNOM      ,IDREEL     , IDLONG     ,
     +               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,IDREEL = 6 , IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C ----------------------------------------------------------------------
      INTEGER          IIADM    ,IIADD    ,IIDOS    ,IIDCO   , IDATE   ,
     +                 IORIG    ,ILONO    ,ILTYP    ,IIMAR   , ISAUV   ,
     +                 NPARM
      PARAMETER      ( IIADM = 0,IIADD = 1,IIDOS = 2,IIDCO =3,IDATE = 4,
     +                 IORIG = 5,ILONO = 6,ILTYP = 7,IIMAR =8,ISAUV = 9,
     +                 NPARM =10)
C ----------------------------------------------------------------------
      INTEGER          ISTA1,ISTA2,IPGCL
      CHARACTER*4      FONC
      CHARACTER*75     CMESS
      CHARACTER*32     NOML32
      INTEGER          IADMI , IADDI(2) , NLD , IT(NPARM), KT(NPARM)
      LOGICAL          LSAUV,LDATE,LMARQ,LLIBP,LTOUT,LATTR,LXU,LAD
C DEB ------------------------------------------------------------------
      DO 1 K=1,NPARM
        KT(K) = 0
 1    CONTINUE
      NOML32 = NOMLU
      IADIT = 0
      IASIG = 0
      IPGCL = IPGC
      IF ( NOMAP .EQ. 'JELIBE' .OR. NOMAP .EQ. 'JELIBC' ) THEN
        FONC = 'LIBE'
        IF (IDEBUG .EQ. 1) THEN
          FONC = 'DEBG'
        ENDIF
      ELSE IF ( NOMAP .EQ. 'JELIBT' .OR. NOMAP .EQ. 'JELIBZ') THEN
        FONC = 'LIBE'
        IPGCL = -1
      ELSE IF ( NOMAP .EQ. 'JELIBS' ) THEN
        FONC = 'LIBE'
        IPGCL = -3
      ELSE IF ( NOMAP .EQ. 'JEDCHG' ) THEN
        FONC = 'DCHG'
        IPGCL = 0
      ELSE IF ( NOMAP .EQ. 'JETASS' ) THEN
        FONC = 'TASS'
        IF ( ITYPE.NE.1 ) THEN
          CMESS = 'APPEL A JJLIDE PAR '//NOMAP//' INVALIDE'
          CALL JVMESS ( 'S' , 'JJLIDE01' , CMESS )
        ENDIF
      ELSE IF ( NOMAP .EQ. 'JELIBF' ) THEN
        FONC   = 'LIBF'
      ELSE IF ( NOMAP .EQ. 'JEIMPO' .OR. NOMAP .EQ. 'JEIMPA' .OR.
     +          NOMAP .EQ. 'JENUNO' .OR. NOMAP .EQ. 'JENONU' ) THEN
        FONC = 'LIBE'
        IPGCL = -2
      ELSE
        GOTO 101
      ENDIF
C
      NLD   = 0
      NLDO  = 1
      JIT   = 1
      KIT   = 1
C
C --- CAS D'UN OBJET SIMPLE
C
      IF ( ITYPE .EQ. 1 ) THEN
        IC  = ICLAOS
        NLD = 1
        IT(JIT+IIADM) = JIADM(IC)+IDATOS
        IADMI = IADM(JIADM(IC)+IDATOS)
        IF ( IADMI .EQ. 0 ) THEN
          GOTO 101
        ELSE
          ISTA1 = ISZON (JISZON + IADMI - 1)
          IS    = JISZON+ISZON(JISZON + IADMI-4)
          ISTA2 = ISZON (IS - 4)
          IF( ISTA1.EQ.ISTAT(1) .AND. ISTA2.EQ.ISTAT(3) ) GOTO 101
        ENDIF
        IT(JIT+IIADD) = JIADD(IC)+2*IDATOS-1
        IT(JIT+IIDOS) = IDATOS
        IT(JIT+IIDCO) = 0
        IT(JIT+IDATE) = JDATE(IC)+IDATOS
        IT(JIT+IORIG) = JORIG(IC)+IDATOS
        IT(JIT+IIMAR) = JMARQ(IC)+2*IDATOS-1
        IT(JIT+ILONO) = JLONO(IC)+IDATOS
        IT(JIT+ILTYP) = JLTYP(IC)+IDATOS
        IT(JIT+ISAUV) = 1
C
C --- CAS D'UNE COLLECTION
C
      ELSE IF ( ITYPE .EQ. 2 ) THEN
        IC  = ICLACO
C
C ----- CAS D'UNE COLLECTION ENTIERE
C
        IF ( NOML32(25:32) .EQ. '        ' ) THEN
          IBACOL = IADM ( JIADM(IC) + IDATCO )
          IF ( IBACOL .EQ. 0 ) THEN
            GOTO 101
          ELSE
            ISTA1 = ISZON (JISZON + IBACOL - 1)
            IS    = JISZON+ISZON(JISZON + IBACOL-4)
            ISTA2 = ISZON (IS - 4)
            IF( ISTA1.EQ.ISTAT(1) .AND. ISTA2.EQ.ISTAT(3) ) GOTO 101
          ENDIF
          IXIADD = ISZON( JISZON + IBACOL + IDIADD )
          IXDESO = ISZON( JISZON + IBACOL + IDDESO )
          IXIADM = ISZON( JISZON + IBACOL + IDIADM )
          IXMARQ = ISZON( JISZON + IBACOL + IDMARQ )
          NALLOC = IDNUM
          NMAX   = 0
C
C ------- CAS D'UNE COLLECTION DISPERSEE
C
          IF ( IXIADD .NE. 0 ) THEN
            NMAX = ISZON(JISZON + IBACOL+IVNMAX)
            NALLOC = NALLOC + NMAX
          ENDIF
C
C --------TOUTE FORME DE COLLECTION
C
          ITROLD = ITRECH
          ITRECH = 2
          NNN =  NALLOC * NPARM * LOIS
          CALL JJALLS( NNN    ,'V','I',LOIS,'INIT' , IT , JIT , IADIT)
          ISZON(JISZON+IADIT-1) = ISTAT(2)
          ISZON(JISZON+ISZON(JISZON+IADIT-4)-4) = ISTAT(4)
          CALL JJALLS( NNN    ,'V','I',LOIS,'INIT' , KT , KIT , IASIG)
          ISZON(JISZON+IASIG-1) = ISTAT(2)
          ISZON(JISZON+ISZON(JISZON+IASIG-4)-4) = ISTAT(4)
          ITRECH = ITROLD
C --------OBJETS DE COLLECTION
C
          IF ( NMAX .NE. 0 ) THEN
            IXLONO = ISZON( JISZON + IBACOL + IDLONO )
            IBIADM = IADM ( JIADM(IC) + IXIADM )
            IBMARQ = IADM ( JIADM(IC) + IXMARQ )
            IBIADD = IADM ( JIADM(IC) + IXIADD )
            DO 10 K = 1,NMAX
              NADM  = JISZON + IBIADM - 1 + K
              IF ( ISZON( NADM ) .NE. 0 ) THEN
                IJIT = JIT + NLD * NPARM
                IKIT = KIT + NLD * NPARM
                NLD  = NLD + 1
                IT(IJIT + IIADM) =   NADM
                KT(IKIT + IIADM) = -1
                IT(IJIT + IIMAR) =   JISZON + IBMARQ - 1 + 2*K-1
                KT(IKIT + IIMAR) = -1
                IT(IJIT + IIADD) =   JISZON + IBIADD - 1 + 2*K-1
                KT(IKIT + IIADD) = -1
                IF ( IXLONO .NE. 0 ) THEN
                   IBLONO = IADM ( JIADM(IC) + IXLONO )
                   IT(IJIT + ILONO) = JISZON + IBLONO - 1 + K
                   KT(IKIT + ILONO) = -1
                ELSE
                   IT(IJIT + ILONO) = JLONO(IC) + IXDESO
                ENDIF
                IT(IJIT + IIDOS) = K
                IT(IJIT + IIDCO) = IDATCO
                IT(IJIT + IDATE) = JDATE(IC) + IXDESO
                IT(IJIT + IORIG) = JORIG(IC) + IXDESO
                IT(IJIT + ILTYP) = JLTYP(IC) + IXDESO
                IT(IJIT + ISAUV) = 1
              ENDIF
 10         CONTINUE
          ENDIF
          NLDO= NLD
C
C --------OBJETS ATTRIBUTS DE COLLECTION
C
          DO 20 K = 1,IDNUM
            IX  = ISZON( JISZON + IBACOL + K )
            IF ( IX .GT. 0 ) THEN
              IF ( RNOM(JRNOM(IC)+IX)(25:26) .EQ. '$$' .OR.
     &             IPGCL .EQ. -2) THEN
C
C ----------- UNIQUEMENT LES OBJETS $$ PRESENTS EN MEMOIRE
C ----------- LES POINTEURS PARTAGES DOIVENT ETRE LIBERES EXPLICITEMENT
C
                IF ( IADM ( JIADM(IC) + IX ) .NE. 0 ) THEN
                  IJIT = JIT + NLD * NPARM
                  IKIT = KIT + NLD * NPARM
                  NLD  = NLD + 1
                  IT(IJIT + IIADM) = JIADM(IC)+ IX
                  IT(IJIT + IIADD) = JIADD(IC)+ 2*IX-1
                  IT(IJIT + IIDOS) = IX
                  IT(IJIT + IIDCO) = 0
                  IT(IJIT + IDATE) = JDATE(IC) + IX
                  IT(IJIT + IORIG) = JORIG(IC) + IX
                  IT(IJIT + IIMAR) = JMARQ(IC) + 2*IX-1
                  IT(IJIT + ILONO) = JLONO(IC) + IX
                  IT(IJIT + ILTYP) = JLTYP(IC) + IX
                  IF ( IX .NE. IXIADM .AND. IX .NE. IXMARQ ) THEN
                    IT(IJIT + ISAUV) = 1
                  ELSE
                    IT(IJIT + ISAUV) = 0
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 20       CONTINUE
          IJIT = JIT + NLD * NPARM
          IKIT = KIT + NLD * NPARM
          NLD  = NLD + 1
          IT(IJIT + IIADM) = JIADM(IC)+ IDATCO
          IT(IJIT + IIADD) = JIADD(IC)+ 2*IDATCO-1
          IT(IJIT + IIDOS) = IDATCO
          IT(IJIT + IIDCO) = 0
          IT(IJIT + IDATE) = JDATE(IC)+ IDATCO
          IT(IJIT + IORIG) = JORIG(IC)+ IDATCO
          IT(IJIT + IIMAR) = JMARQ(IC)+ 2*IDATCO-1
          IT(IJIT + ILONO) = JLONO(IC)+ IDATCO
          IT(IJIT + ILTYP) = JLTYP(IC)+ IDATCO
          IT(IJIT + ISAUV) = 1
C
C ----- CAS D'UN OBJET DE COLLECTION ------
C
        ELSE
          ICRE = 0
          CALL JJCROC ( NOML32(25:32) , ICRE )
          IBACOL = IADM ( JIADM(IC) + IDATCO )
          IF ( IBACOL .EQ. 0 ) GOTO 101
          IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
          IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
          IXMARQ = ISZON ( JISZON + IBACOL + IDMARQ )
          IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
          IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
          IF ( IXIADD .EQ. 0 ) THEN
            CMESS = 'LIBERATION D''UN OBJET DE COLLECTION CONTIG '//
     +              'REFUSEE '
            CALL JVMESS ( 'S' , 'JJLIDE02' , CMESS )
          ELSE
            IBIADM = IADM ( JIADM(IC) + IXIADM )
            IBIADD = IADM ( JIADM(IC) + IXIADD )
            IBMARQ = IADM ( JIADM(IC) + IXMARQ )
            NLD = 1
            IT(JIT+IIADM) = JISZON + IBIADM -1 + IDATOC
            KT(KIT+IIADM) = -1
            IADMI = ISZON( JISZON + IBIADM -1 + IDATOC )
            IF ( IADMI .NE. 0 ) THEN
              ISTA1 = ISZON (JISZON + IADMI - 1)
              IS    = JISZON+ISZON(JISZON+IADMI-4)
              ISTA2 = ISZON (IS - 4)
              IF( ISTA1.EQ.ISTAT(1) .AND. ISTA2.EQ.ISTAT(3) ) GOTO 101
            ELSE
              GOTO 101
            ENDIF
            IT(JIT+IIADD) = JISZON + IBIADD -1 + 2*IDATOC-1
            KT(KIT+IIADD) = -1
            IT(JIT+IIMAR) = JISZON + IBMARQ -1 + 2*IDATOC-1
            KT(KIT+IIMAR) = -1
            IT(JIT+IIDOS) = IDATOC
            IT(JIT+IIDCO) = IDATCO
            IT(JIT+IDATE) = JDATE(IC)+IXDESO
            IT(JIT+IORIG) = JORIG(IC)+IXDESO
            IF ( IXLONO .NE. 0 ) THEN
               IBLONO = IADM ( JIADM(IC) + IXLONO )
               IT(JIT+ILONO) = JISZON + IBLONO - 1 + IDATOC
               KT(KIT+ILONO) = -1
            ELSE
               IT(JIT+ILONO) = JLONO(IC) + IXDESO
            ENDIF
            IT(JIT+ILTYP) = JLTYP(IC)+IXDESO
            IT(JIT+ISAUV) = 1
          ENDIF
        ENDIF
      ENDIF
C
      IF ( FONC .EQ. 'LIBE' ) THEN
        IF (ITIAD .EQ. 3 .AND. ITYPE .NE. 2) THEN
          IF ( IADMI .LT. IDFR ) THEN
            IDXAXD(1)=IADMI-4
          ELSE
            IDXAXD(2)=IADMI-4
          ENDIF
        ENDIF
      ENDIF
C
      LTOUT = .TRUE.
      DO 100 K = 1 , NLD
        LATTR = ( K .GT. NLDO .AND. NLDO .GT. 0 )
        LLIBP = .FALSE.
        LSAUV = .FALSE.
        LMARQ = .FALSE.
        LDATE = .FALSE.
        LXU   = .FALSE.
        LAD   = .FALSE.
        KK = ( K - 1 ) * NPARM
C
C ----- PREPARATION AUX DIVERSES OPERATIONS
C
        IF ( KT(KIT+KK+ IIADM) .EQ. 0 ) THEN
          IADMI = IADM ( IT(JIT+KK+ IIADM) )
        ELSE
          IADMI = ISZON ( IT(JIT+KK+ IIADM) )
        ENDIF
        IF ( KT(KIT+KK+ IIMAR) .EQ. 0 ) THEN
          MARQI = IMARQ ( IT(JIT+KK+ IIMAR) )
        ELSE
          MARQI = ISZON ( IT(JIT+KK+ IIMAR) )
        ENDIF
C
        IDOS  = IT( JIT+KK+ IIDOS )
        IDCO  = IT( JIT+KK+ IIDCO )
        ISTA1 = ISZON (JISZON + IADMI - 1)
        IS    = JISZON+ISZON(JISZON + IADMI - 4)
        ISTA2 = ISZON (IS - 4)
        IF ( ISTA1 .EQ. ISTAT(1) .AND. ISTA2 .EQ. ISTAT(1) ) THEN
          GOTO 100
        ENDIF
C
C ----- OPERATION LIBE
C       ==============
C ----- LES SEGMENTS XA OU XD RESTENT EN L'ETAT
C ----- SI LA MARQUE COURANTE CORRESPOND A CELLE DU SEGMENT DE
C ----- VALEUR :
C -----    POUR LES OBJETS SIMPLES :
C -----    - LES SEGMENTS UA PASSENT EN XA
C -----    - LES SEGMENTS UD PASSENT EN XD
C -----    POUR LES OBJETS ATTRIBUTS DE COLLECTION :
C -----    LES OBJETS ATTRIBUTS DE COLLECTION RESTENT U (UA OU UD)
C -----    SAUF SI IL N'Y A AUCUN OBJET DE COLLECTION EN  MEMOIRE
C
        IF ( FONC .EQ. 'LIBE' ) THEN
          IF ( ISTA1 .EQ. ISTAT(2) ) THEN
            IF ( IPGCL .EQ. MARQI ) THEN
              IF ( LATTR ) THEN
                IF (LTOUT .AND. ISTA2 .EQ. ISTAT(3)) LXU = .TRUE.
              ELSE
                LTOUT = .FALSE.
                LXU = .TRUE.
              ENDIF
              IF ( ISTA2 .EQ. ISTAT(4) .AND. LXU) THEN
                LDATE = .TRUE.
              ENDIF
              LMARQ = .TRUE.
            ELSE
              LTOUT = .FALSE.
              GOTO 100
            ENDIF
          ELSE
            IF (.NOT. LATTR ) LTOUT = .FALSE.
            GOTO 100
          ENDIF
C
C ----- OPERATION TASS
C       ==============
C -----    UNIQUEMENT UTILISEE POUR ECRITURE DANS JETASS (POUR DES
C -----    OBJETS SIMPLES ADRESSE DISQUE DES OBJETS DE COLLECTION)
C
        ELSE IF ( FONC .EQ. 'TASS' ) THEN
          LMARQ = .FALSE.
          LLIBP = .FALSE.
          LSAUV = .TRUE.
          LDATE = .TRUE.
C
C ----- OPERATION DEBG
C       ==============
C -----    UTILISEE POUR ECRITURE IMMEDIATE AVEC L'OPTION DE DEBUG
C
        ELSE IF ( FONC .EQ. 'DEBG' ) THEN
          IF ( ISTA1 .EQ. ISTAT(2) ) THEN
            IF ( IPGCL .EQ. MARQI ) THEN
              IF ( LATTR ) THEN
                IF (LTOUT .AND. ISTA2 .EQ. ISTAT(3)) LXU = .TRUE.
              ELSE
                LXU = .TRUE.
              ENDIF
              IF ( ISTA2 .EQ. ISTAT(4) .AND. LXU) THEN
                LAD   = .TRUE.
                LSAUV = .TRUE.
                LDATE = .TRUE.
                LLIBP = .TRUE.
              ELSE IF ( LXU ) THEN
                LLIBP = .TRUE.
              ENDIF
              LMARQ = .TRUE.
            ELSE
              LTOUT = .FALSE.
              GOTO 100
            ENDIF
          ELSE IF ( ISTA2 .EQ. ISTAT(4) ) THEN
            IF (.NOT. LATTR .OR. (LATTR .AND. LTOUT) ) THEN
              LAD   = .TRUE.
              LSAUV = .TRUE.
              LDATE = .TRUE.
              LLIBP = .TRUE.
            ENDIF
          ELSE
            IF (.NOT. LATTR ) LTOUT = .FALSE.
            GOTO 100
          ENDIF
C
C ----- OPERATION DCHG
C       ==============
C ----- LA MARQUE ASSOCIEE EST NECESSAIREMENT = 0
C ----- ON PROVOQUE LA LIBERATION
C
        ELSE IF ( FONC .EQ. 'DCHG' ) THEN
          IF ( NLDO .GT. 0 ) GOTO 101
          IF ( MARQI .EQ. 0 ) THEN
            LLIBP = .TRUE.
            IF ( ISTA1 .EQ. ISTAT(2) ) THEN
              LXU = .TRUE.
              IF ( ISTA2 .EQ. ISTAT(4) ) THEN
                LSAUV = .TRUE.
                LDATE = .TRUE.
              ENDIF
              LMARQ = .TRUE.
            ELSE
              IF ( ISTA2 .EQ. ISTAT(4) ) THEN
                LSAUV = .TRUE.
              ENDIF
            ENDIF
          ENDIF
C
C ----- OPERATIONS LIBF
C       ===============
C ----- ON NE S'OCCUPE PAS DE LA MARQUE ASSOCIEE, ON FORCE LA
C ----- LIBERATION
C
        ELSE IF ( FONC .EQ. 'LIBF' ) THEN
          LLIBP = .TRUE.
          IF ( ISTA1 .EQ. ISTAT(2) ) THEN
            LXU = .TRUE.
            IF ( ISTA2 .EQ. ISTAT(4) ) THEN
              LAD   = .TRUE.
              LSAUV = .TRUE.
              LDATE = .TRUE.
            ENDIF
            LMARQ = .TRUE.
          ELSE
            IF ( ISTA2 .EQ. ISTAT(4) ) THEN
              LAD   = .TRUE.
              LSAUV = .TRUE.
            ELSE
              LLIBP = .TRUE.
            ENDIF
          ENDIF
        ENDIF
C
C ----- ACTUALISATION DES ATTRIBUTS
C
        IF ( LMARQ ) THEN
          IF ( KT(KIT+KK+ IIMAR) .EQ. 0 ) THEN
            IMARQ( IT(JIT+KK+ IIMAR)  ) = 0
            IADMAR = IMARQ( IT(JIT+KK+ IIMAR)+1)
            IF ( IADMAR.GT.0 ) THEN
              ISZON(JISZON+KDESMA+IADMAR-1) = 0
              IMARQ( IT(JIT+KK+IIMAR)+1)   = 0
            ENDIF
          ELSE
            ISZON( IT(JIT+KK+ IIMAR) ) = 0
            IADMAR = ISZON( IT(JIT+KK+ IIMAR)+1 )
            IF ( IADMAR.GT.0 ) THEN
              ISZON(JISZON+KDESMA+IADMAR-1) = 0
              ISZON( IT(JIT+KK+IIMAR)+1 ) = 0
            ENDIF
          ENDIF
          IF (LXU) ISZON(JISZON+IADMI-1) = ISTAT(1)
          IF (LAD) THEN
            IS                   = ISZON(JISZON+IADMI-4)
            ISZON(JISZON+IS-4)   = ISTAT(3)
          ENDIF
        ENDIF
        IF ( LSAUV .AND. IT(JIT+KK+ISAUV) .EQ. 1) THEN
          IF ( KT(KIT+KK+ IIADD) .EQ. 0 ) THEN
            IADDI(1) = IADD ( IT(JIT+KK+ IIADD)    )
            IADDI(2) = IADD ( IT(JIT+KK+ IIADD) + 1)
          ELSE
            IADDI(1) = ISZON ( IT(JIT+KK+ IIADD)     )
            IADDI(2) = ISZON ( IT(JIT+KK+ IIADD) + 1 )
          ENDIF
          IF ( KT(KIT+KK+ ILONO) .EQ. 0 ) THEN
            LONOI = LONO (IT(JIT+KK+ ILONO))  * LTYP(IT(JIT+KK +ILTYP))
          ELSE
            LONOI = ISZON (IT(JIT+KK+ ILONO))* LTYP(IT(JIT+KK +ILTYP))
          ENDIF
          IDCO = IT(JIT+KK + IIDCO )
          IDOS = IT(JIT+KK + IIDOS )
          CALL JXECRO ( IC , IADMI , IADDI , LONOI , IDCO, IDOS)
          IF ( KT(KIT+KK+ IIADD) .EQ. 0 ) THEN
            IADD ( IT(JIT+KK+ IIADD)    ) = IADDI(1)
            IADD ( IT(JIT+KK+ IIADD)+ 1 ) = IADDI(2)
          ELSE
            ISZON ( IT(JIT+KK+ IIADD)     ) = IADDI(1)
            ISZON ( IT(JIT+KK+ IIADD) + 1 ) = IADDI(2)
          ENDIF
        ENDIF
        IF ( LDATE ) DATE ( IT(JIT+KK+ IDATE) ) = DATEI
        IF ( LLIBP ) THEN
          CALL JJLIBP ( IADMI )
          IF ( KT(KIT+KK+ IIADM) .EQ. 0 ) THEN
            IADM ( IT(JIT+KK+ IIADM) ) = 0
          ELSE
            ISZON ( IT(JIT+KK+ IIADM) ) = 0
          ENDIF
        ENDIF
  100 CONTINUE
  101 CONTINUE
      IF ( IADIT .NE. 0 ) CALL JJLIBP ( IADIT )
      IF ( IASIG .NE. 0 ) CALL JJLIBP ( IASIG )
C FIN ------------------------------------------------------------------
      END
