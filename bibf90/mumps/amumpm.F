      SUBROUTINE AMUMPM(RANG,LDIST,KXMPS,KMONIT,IMPR,IFMUMP,ELI2LG,
     &                  TYPE,LMD,EPSMAT)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF AMUMPM MUMPS  DATE 03/01/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
! ==================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ==================================================================
C--------------------------------------------------------------
C BUT : REMPLIR LES OBJETS F90 DE MUMPS REPRESENTANT LA MATRICE A PARTIR
C       DE CELLE DE CODE_ASTER.
C
C IN  RANG   :   IN   : RANG DU PROCESSEUR.
C IN  LDIST  :  LOG   : LOGICAL MUMPS DISTRIBUE OR NOT
C IN  KXMPS  :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
C IN  KMONIT :  K24   : VECTEUR DE NOMS DES OBJ JEVEUX
C IN  IMPR   :  K14   : FLAG POUR IMPRESSION MATRICE
C IN  IFMUMP :   IN   : UNITE LOGIQUE POUR IMPRESSION FICHIER
C IN  ELI2LG :  LOG   : LOGICAL POUR NE LAISSER QU'1 LAGRANGE ACTIF
C IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
C IN  EPSMAT :   R8   : SEUIL DE FILTRAGE DES TERMES DE LA MATRICE
C---------------------------------------------------------------
! RESPONSABLE BOITEAU O.BOITEAU

      INTEGER      RANG,KXMPS,IFMUMP
      LOGICAL      LDIST,ELI2LG,LMD
      REAL*8       EPSMAT
      CHARACTER*1  TYPE
      CHARACTER*14 IMPR
      CHARACTER*24 KMONIT(12)

#ifdef _HAVE_MUMPS

C============================================================
      INCLUDE 'mpif.h'
      INCLUDE 'smumps_struc.h'
      INCLUDE 'cmumps_struc.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
C============================================================

      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (SMUMPS_STRUC) , TARGET  :: SMPS(NMXINS)
      TYPE (CMUMPS_STRUC) , TARGET  :: CMPS(NMXINS)
      TYPE (SMUMPS_STRUC) , POINTER :: SMPSK
      TYPE (CMUMPS_STRUC) , POINTER :: CMPSK
      TYPE (DMUMPS_STRUC) , TARGET  :: DMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (DMUMPS_STRUC) , POINTER :: DMPSK
      TYPE (ZMUMPS_STRUC) , POINTER :: ZMPSK
      INTEGER      JSMDI,NSMDI,JSMHC,NSMHC,JDELG,N1,N,NZ,NZ2,NVALE,
     &             JVALE,NLONG,JVALE2,NZLOC,JCOLG,KTERM,ILIGG,ITERM,
     &             IFM,NIV,K,SYM,IRET,JCOLL,ILIGL,JNULOGL,
     &             COLTMP,NBEQ,JNEQU,KZERO,IBID,IFILTR,NFILTR
      CHARACTER*1  ROUCS(NMXINS),PRECS(NMXINS)
      CHARACTER*4  KBID,ETAMS(NMXINS),ETAM
      CHARACTER*14 NONUS(NMXINS),NONU
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS),NOMAT,NOSOLV
      CHARACTER*24 KFILTR
      REAL*8       RAUX,RFILTR,RR4MAX,RAUX1,R4MAEM
      COMPLEX*16   CAUX
      LOGICAL      LMNSY,LTYPR,LNN,LFILTR
      COMMON /MUMPSS/ SMPS
      COMMON /MUMPSC/ CMPS
      COMMON /MUMPSD/ DMPS
      COMMON /MUMPSZ/ ZMPS
      COMMON /MUMPSH/ NONUS,NOMATS,NOSOLS,ETAMS,ROUCS,PRECS
      
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER*4 ZI4
      COMMON  /I4VAJE/ZI4(1)
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32, JEXNUM
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C       ------------------------------------------------
C        INITS
C       ------------------------------------------------
      RR4MAX=R4MAEM()
      LFILTR=.FALSE.
      KFILTR='&&AMUMPM.FILTRAGE'
      NFILTR=0
      NOMAT=NOMATS(KXMPS)
      NOSOLV=NOSOLS(KXMPS)
      NONU=NONUS(KXMPS)
      ETAM=ETAMS(KXMPS)
C --- REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
C --- DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
      IF (TYPE.EQ.'S') THEN
        SMPSK=>SMPS(KXMPS)
        LTYPR=.TRUE.
        SYM=SMPSK%SYM
      ELSE IF (TYPE.EQ.'C') THEN
        CMPSK=>CMPS(KXMPS)
        LTYPR=.FALSE.
        SYM=CMPSK%SYM
      ELSE IF (TYPE.EQ.'D') THEN
        DMPSK=>DMPS(KXMPS)
        LTYPR=.TRUE.
        SYM=DMPSK%SYM
      ELSE IF (TYPE.EQ.'Z') THEN
        ZMPSK=>ZMPS(KXMPS)
        LTYPR=.FALSE.
        SYM=ZMPSK%SYM
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      IF (LMD) CALL JEVEUO(NONU//'.NUML.NULG','L',JNULOGL)

C       ------------------------------------------------
C        LECTURE D'ADRESSES ET DE PARAMETRES PRELIMINAIRES
C       ------------------------------------------------
      IF (((RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
        CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
        CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
        CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
        CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
        IF (LMD) THEN
          CALL JEVEUO(NONU//'.NUML.DELG','L',JDELG)
          CALL JELIRA(NONU//'.NUML.DELG','LONMAX',N1,KBID)
        ELSE
          CALL JEVEUO(NONU//'.NUME.DELG','L',JDELG)
          CALL JELIRA(NONU//'.NUME.DELG','LONMAX',N1,KBID)
        ENDIF
        CALL JEVEUO(NONU//'.NUME.NEQU','L',JNEQU)
        NBEQ=ZI(JNEQU)
        CALL ASSERT(N1.EQ.NSMDI)
C --- CALCUL DE N
        N=NSMDI
C --- CALCUL DE NZ2
        NZ=ZI(JSMDI-1+N)
        CALL ASSERT(NZ.LE.NSMHC)
        NZ2=NZ
        IF (SYM.EQ.0) NZ2=2*NZ-N
        CALL JELIRA(NOMAT//'.VALM','NMAXOC',NVALE,KBID)
C --- LMNSY EST INDEPENDANT DE XMPSK%SYM POUR POUVOIR TRAITER
C --- DES CAS SYM EN MUMPS NON SYMETRIQUE
        IF (NVALE.EQ.1) THEN
          LMNSY=.FALSE.
        ELSE IF (NVALE.EQ.2) THEN
          LMNSY=.TRUE.
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

        CALL JEVEUO(JEXNUM(NOMAT//'.VALM',1),'L',JVALE)
        CALL JELIRA(JEXNUM(NOMAT//'.VALM',1),'LONMAX',NLONG,KBID)
        CALL ASSERT(NLONG.EQ.NZ)
        IF (LMNSY) THEN
          CALL JEVEUO(JEXNUM(NOMAT//'.VALM',2),'L',JVALE2)
          CALL JELIRA(JEXNUM(NOMAT//'.VALM',2),'LONMAX',NLONG,KBID)
          CALL ASSERT(NLONG.EQ.NZ)
        ENDIF

C ---- DETERMINATION DES TERMES DE REFERENCE POUR LE FILTRAGE
       IF (EPSMAT.GT.0.D0) THEN
         LFILTR=.TRUE.
         CALL JEEXIN(KFILTR,IRET)
         IF (IRET.NE.0) CALL JEDETR(KFILTR)
         CALL WKVECT(KFILTR,'V V R',N,IFILTR)
         IF (LTYPR) THEN
           DO K=1,N
             ZR(IFILTR-1+K)=EPSMAT*ABS(ZR(JVALE-1+ZI(JSMDI-1+K)))
           ENDDO
         ELSE
           DO K=1,N
             ZR(IFILTR-1+K)=EPSMAT*ABS(ZC(JVALE-1+ZI(JSMDI-1+K)))
           ENDDO
         ENDIF
       ELSE
         LFILTR=.FALSE.
       ENDIF
      ENDIF

C       ------------------------------------------------
C       DETERMINATION DU NBRE LOCAL DE TERMES PAR PROC: NZLOC
C       EN CENTRALISE (LDIST=.FALSE.): PROC 0 GERE TOUS LES TERMES POUR
C         LES FOURNIR A MUMPS. CE DERNIER ENSUITE LES REDISPATCHE PAR
C         PAQUETS SUR TOUS LES AUTRES PROCS.
C       EN DISTRIBUE (LDIST=.TRUE.): CHAQUE PROC FOURNIT LES TERMES
C         DONT IL A LA RESPONSABILITE.
C       ------------------------------------------------
      
      IF (((RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
        NZLOC=0
        JCOLL=1
        DO KTERM = 1, NZ
          IF (ZI(JSMDI-1+JCOLL).LT.KTERM) JCOLL=JCOLL+1
          ILIGL=ZI4(JSMHC-1+KTERM)
          IF (LFILTR) THEN
            RFILTR=ZR(IFILTR-1+ILIGL)+ZR(IFILTR-1+JCOLL)
          ELSE
            RFILTR=-1.D0
          ENDIF
          IF (LTYPR) THEN
            RAUX=ZR(JVALE-1+KTERM)
            IF (RAUX.NE.0.D0) THEN
              IF (ABS(RAUX).LT.RFILTR) THEN
                NFILTR=NFILTR+1
C                WRITE(IFM,*)'TERME FILTRE ',ILIGL,JCOLL,RAUX
              ELSE
                NZLOC=NZLOC+1
              ENDIF
            ENDIF
          ELSE
            CAUX=ZC(JVALE-1+KTERM)
            IF (CAUX.NE.(0.D0,0.D0)) THEN
              IF (ABS(CAUX).LT.RFILTR) THEN
                NFILTR=NFILTR+1
              ELSE
                NZLOC=NZLOC+1
              ENDIF
            ENDIF
          ENDIF
C -- SOUS LA DIAGONALE :
          IF ((SYM.EQ.0).AND.(ILIGL.NE.JCOLL)) THEN
            IF (LTYPR) THEN
              IF (LMNSY) RAUX=ZR(JVALE2-1+KTERM)
              IF (RAUX.NE.0.D0) THEN
                IF (ABS(RAUX).LT.RFILTR) THEN
                  NFILTR=NFILTR+1
                ELSE
                  NZLOC=NZLOC+1
                ENDIF
              ENDIF
            ELSE
              IF (LMNSY) CAUX=ZC(JVALE2-1+KTERM)
              IF (CAUX.NE.(0.D0,0.D0)) THEN
                IF (ABS(CAUX).LT.RFILTR) THEN
                  NFILTR=NFILTR+1
                ELSE
                  NZLOC=NZLOC+1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        NZ2=NZLOC
        IF (NIV.GE.2) 
     &    WRITE(IFM,*)'<AMUMPM> PROC/NZLOC/NFILTR',RANG,NZLOC,NFILTR
      ENDIF

C  -- POUR LAISSER LE PLUS DE MEMOIRE POSSIBLE A MUMPS.
C  -- MAIS CELA PEUT ETRE COUTEUX EN // AVEC DES ACCES DISQUES
C  -- LENTS.

      CALL JJLDYN(0,-1,IBID)

C       ------------------------------------------------
C       ALLOCATION DES OBJETS MUMPS F90
C       ------------------------------------------------
      IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN

        IF (LDIST) THEN
          IF (TYPE.EQ.'S') THEN
            IF (LMD) THEN
              SMPSK%N=NBEQ
            ELSE
              SMPSK%N=N
            ENDIF
            SMPSK%NZ_loc=NZ2
            ALLOCATE(SMPSK%IRN_loc(NZ2))
            ALLOCATE(SMPSK%JCN_loc(NZ2))
            ALLOCATE(SMPSK%A_loc(NZ2))
          ELSE IF (TYPE.EQ.'C') THEN
            CMPSK%N=N
            CMPSK%NZ_loc=NZ2
            ALLOCATE(CMPSK%IRN_loc(NZ2))
            ALLOCATE(CMPSK%JCN_loc(NZ2))
            ALLOCATE(CMPSK%A_loc(NZ2))
          ELSE IF (TYPE.EQ.'D') THEN
            IF (LMD) THEN
              DMPSK%N=NBEQ
            ELSE
              DMPSK%N=N
            ENDIF
            DMPSK%NZ_loc=NZ2
            ALLOCATE(DMPSK%IRN_loc(NZ2))
            ALLOCATE(DMPSK%JCN_loc(NZ2))
            ALLOCATE(DMPSK%A_loc(NZ2))
          ELSE IF (TYPE.EQ.'Z') THEN
            ZMPSK%N=N
            ZMPSK%NZ_loc=NZ2
            ALLOCATE(ZMPSK%IRN_loc(NZ2))
            ALLOCATE(ZMPSK%JCN_loc(NZ2))
            ALLOCATE(ZMPSK%A_loc(NZ2))
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ELSE
          IF (TYPE.EQ.'S') THEN
            SMPSK%N=N
            SMPSK%NZ=NZ2
            ALLOCATE(SMPSK%IRN(NZ2))
            ALLOCATE(SMPSK%JCN(NZ2))
            ALLOCATE(SMPSK%A(NZ2))
          ELSE IF (TYPE.EQ.'C') THEN
            CMPSK%N=N
            CMPSK%NZ=NZ2
            ALLOCATE(CMPSK%IRN(NZ2))
            ALLOCATE(CMPSK%JCN(NZ2))
            ALLOCATE(CMPSK%A(NZ2))
          ELSE IF (TYPE.EQ.'D') THEN
            DMPSK%N=N
            DMPSK%NZ=NZ2
            ALLOCATE(DMPSK%IRN(NZ2))
            ALLOCATE(DMPSK%JCN(NZ2))
            ALLOCATE(DMPSK%A(NZ2))
          ELSE IF (TYPE.EQ.'Z') THEN
            ZMPSK%N=N
            ZMPSK%NZ=NZ2
            ALLOCATE(ZMPSK%IRN(NZ2))
            ALLOCATE(ZMPSK%JCN(NZ2))
            ALLOCATE(ZMPSK%A(NZ2))
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF
            
C       ------------------------------------------------
C       REMPLISSAGE EFFECTIF DES TERMES DE LA MATRICE
C       ------------------------------------------------
        JCOLL=1
        ITERM=0
        DO KTERM=1,NZ

C --- PARTIE TRIANGULAIRE SUPERIEURE
          IF (ZI(JSMDI-1+JCOLL).LT.KTERM) JCOLL=JCOLL+1
          ILIGL=ZI4(JSMHC-1+KTERM)
          IF (LFILTR) THEN
            RFILTR=ZR(IFILTR-1+ILIGL)+ZR(IFILTR-1+JCOLL)
          ELSE
            RFILTR=-1.D0
          ENDIF
          LNN=.FALSE.
          IF (LTYPR) THEN
            RAUX=ZR(JVALE-1+KTERM)
            IF (RAUX.NE.0.D0) THEN
              IF (ABS(RAUX).LT.RFILTR) THEN
                NFILTR=NFILTR+1
              ELSE
                LNN=.TRUE.
              ENDIF
            ENDIF
          ELSE
            CAUX=ZC(JVALE-1+KTERM)
            IF (CAUX.NE.(0.D0,0.D0)) THEN
              IF (ABS(CAUX).LT.RFILTR) THEN
                NFILTR=NFILTR+1
              ELSE
                LNN=.TRUE.
              ENDIF
            ENDIF
          ENDIF
          IF (LMD) THEN
            ILIGG=ZI(JNULOGL+ILIGL-1)
            JCOLG=ZI(JNULOGL+JCOLL-1)
          ELSE
            ILIGG=ILIGL
            JCOLG=JCOLL
          ENDIF
          IF ( (SYM.NE.0).AND.(ILIGG.GE.JCOLG) ) THEN
            COLTMP=JCOLG
            JCOLG=ILIGG
            ILIGG=COLTMP
          ENDIF
          
C ---- TERME NON NUL
          IF (LNN) THEN
            ITERM=ITERM+1
            IF (TYPE.EQ.'S') THEN
              IF (ABS(RAUX).GT.RR4MAX) CALL ASSERT(.FALSE.)
            ELSE IF (TYPE.EQ.'C') THEN
              RAUX1=MAX(ABS(DBLE(CAUX)),ABS(DIMAG(CAUX)))
              IF (RAUX1.GT.RR4MAX) CALL ASSERT(.FALSE.)
            ENDIF
            IF (LDIST) THEN
              IF (TYPE.EQ.'S') THEN
                SMPSK%IRN_loc(ITERM)=ILIGG
                SMPSK%JCN_loc(ITERM)=JCOLG
                SMPSK%A_loc(ITERM)=RAUX
              ELSE IF (TYPE.EQ.'C') THEN
                CMPSK%IRN_loc(ITERM)=ILIGG
                CMPSK%JCN_loc(ITERM)=JCOLG
                CMPSK%A_loc(ITERM)=CAUX
              ELSE IF (TYPE.EQ.'D') THEN
                DMPSK%IRN_loc(ITERM)=ILIGG
                DMPSK%JCN_loc(ITERM)=JCOLG
                DMPSK%A_loc(ITERM)=RAUX
              ELSE IF (TYPE.EQ.'Z') THEN
                ZMPSK%IRN_loc(ITERM)=ILIGG
                ZMPSK%JCN_loc(ITERM)=JCOLG
                ZMPSK%A_loc(ITERM)=CAUX
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
            ELSE
              IF (TYPE.EQ.'S') THEN
                SMPSK%IRN(ITERM)=ILIGG
                SMPSK%JCN(ITERM)=JCOLG
                SMPSK%A(ITERM)=RAUX
              ELSE IF (TYPE.EQ.'C') THEN
                CMPSK%IRN(ITERM)=ILIGG
                CMPSK%JCN(ITERM)=JCOLG
                CMPSK%A(ITERM)=CAUX
              ELSE IF (TYPE.EQ.'D') THEN
                DMPSK%IRN(ITERM)=ILIGG
                DMPSK%JCN(ITERM)=JCOLG
                DMPSK%A(ITERM)=RAUX
              ELSE IF (TYPE.EQ.'Z') THEN
                ZMPSK%IRN(ITERM)=ILIGG
                ZMPSK%JCN(ITERM)=JCOLG
                ZMPSK%A(ITERM)=CAUX
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
            ENDIF
            IF (ELI2LG) THEN
C ------      ON ELIMINE LE DERNIER TERME DE A/A_loc SI LAG2
              KZERO=0
              IF (ZI(JDELG-1+ILIGL).EQ.-1) THEN
                IF (JCOLL.EQ.ILIGL) KZERO=1
                IF (ZI(JDELG-1+JCOLL).EQ.-2) KZERO=1
              ENDIF
              IF (ZI(JDELG-1+ILIGL).EQ.-2) THEN
                IF (JCOLL.NE.ILIGL) KZERO=1
              ENDIF
              IF (ZI(JDELG-1+JCOLL).EQ.-2) THEN
                IF (JCOLL.NE.ILIGL) KZERO=1
              ENDIF
              IF (KZERO.EQ.1)  ITERM=ITERM-1
C NSELLENET
C      IF(KZERO.EQ.0)WRITE(6,*)'K0 ',ILIGG,JCOLG,RAUX
C      IF(KZERO.EQ.1)WRITE(6,*)'K1 ',ILIGL,'- ',JCOLL,': ',RAUX
C            ELSE
C              WRITE(6,*)'K0 ',ILIGG,JCOLG,RAUX
C NSELLENET
            ENDIF
          ENDIF

C --- PARTIE TRIANGULAIRE INFERIEURE
          IF ((SYM.EQ.0).AND.(ILIGL.NE.JCOLL)) THEN
            LNN=.FALSE.
            IF (LTYPR) THEN
              IF (LMNSY) RAUX=ZR(JVALE2-1+KTERM)
              IF (RAUX.NE.0.D0) THEN
                IF (ABS(RAUX).LT.RFILTR) THEN
                  NFILTR=NFILTR+1
                ELSE
                  LNN=.TRUE.
                ENDIF
              ENDIF
            ELSE
              IF (LMNSY) CAUX=ZC(JVALE2-1+KTERM)
              IF (CAUX.NE.(0.D0,0.D0)) THEN
                IF (ABS(CAUX).LT.RFILTR) THEN
                  NFILTR=NFILTR+1
                ELSE
                  LNN=.TRUE.
                ENDIF
              ENDIF
            ENDIF
            IF (LNN) THEN
              ITERM=ITERM+1
              IF (LMD) THEN
                ILIGG=ZI(JNULOGL+ILIGL-1)
                JCOLG=ZI(JNULOGL+JCOLL-1)
              ELSE
                ILIGG=ILIGL
                JCOLG=JCOLL
              ENDIF
              IF (TYPE.EQ.'S') THEN
                IF (ABS(RAUX).GT.RR4MAX) CALL ASSERT(.FALSE.)
              ELSE IF (TYPE.EQ.'C') THEN
                RAUX1=MAX(ABS(DBLE(CAUX)),ABS(DIMAG(CAUX)))
                IF (RAUX1.GT.RR4MAX) CALL ASSERT(.FALSE.)
              ENDIF
              IF (LDIST) THEN
                IF (TYPE.EQ.'S') THEN
                  SMPSK%IRN_loc(ITERM)=JCOLG
                  SMPSK%JCN_loc(ITERM)=ILIGG
                  SMPSK%A_loc(ITERM)=RAUX
                ELSE IF (TYPE.EQ.'C') THEN
                  CMPSK%IRN_loc(ITERM)=JCOLG
                  CMPSK%JCN_loc(ITERM)=ILIGG
                  CMPSK%A_loc(ITERM)=CAUX
                ELSE IF (TYPE.EQ.'D') THEN
                  DMPSK%IRN_loc(ITERM)=JCOLG
                  DMPSK%JCN_loc(ITERM)=ILIGG
                  DMPSK%A_loc(ITERM)=RAUX
                ELSE IF (TYPE.EQ.'Z') THEN
                  ZMPSK%IRN_loc(ITERM)=JCOLG
                  ZMPSK%JCN_loc(ITERM)=ILIGG
                  ZMPSK%A_loc(ITERM)=CAUX
                ELSE
                  CALL ASSERT(.FALSE.)
                ENDIF
              ELSE
                IF (TYPE.EQ.'S') THEN
                  SMPSK%IRN(ITERM)=JCOLG
                  SMPSK%JCN(ITERM)=ILIGG
                  SMPSK%A(ITERM)=RAUX
                ELSE IF (TYPE.EQ.'C') THEN
                  CMPSK%IRN(ITERM)=JCOLG
                  CMPSK%JCN(ITERM)=ILIGG
                  CMPSK%A(ITERM)=CAUX
                ELSE IF (TYPE.EQ.'D') THEN
                  DMPSK%IRN(ITERM)=JCOLG
                  DMPSK%JCN(ITERM)=ILIGG
                  DMPSK%A(ITERM)=RAUX
                ELSE IF (TYPE.EQ.'Z') THEN
                  ZMPSK%IRN(ITERM)=JCOLG
                  ZMPSK%JCN(ITERM)=ILIGG
                  ZMPSK%A(ITERM)=CAUX
                ELSE
                  CALL ASSERT(.FALSE.)
                ENDIF
              ENDIF
              IF (ELI2LG) THEN
                IF (KZERO.EQ.1) ITERM=ITERM-1
C NSELLENET
C      IF(KZERO.EQ.0)WRITE(6,*)'K0 ',ILIGG,JCOLG,RAUX
C      IF(KZERO.EQ.1)WRITE(6,*)'K1 ',ILIGL,'- ',JCOLL,': ',RAUX
C            ELSE
C              WRITE(6,*)'K0 ',ILIGG,JCOLG,RAUX
C NSELLENET
              ENDIF
            ENDIF
          ENDIF
C ---FIN DE LA BOUCLE SUR NZ
        ENDDO
      
        CALL ASSERT(ITERM.LE.NZ2)
        NZ2=ITERM
        IF (LDIST) THEN
          IF (TYPE.EQ.'S') THEN
            SMPSK%NZ_loc=NZ2
          ELSE IF (TYPE.EQ.'C') THEN
            CMPSK%NZ_loc=NZ2
          ELSE IF (TYPE.EQ.'D') THEN
            DMPSK%NZ_loc=NZ2
          ELSE IF (TYPE.EQ.'Z') THEN
            ZMPSK%NZ_loc=NZ2
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ELSE
          IF (TYPE.EQ.'S') THEN
            SMPSK%NZ=NZ2
          ELSE IF (TYPE.EQ.'C') THEN
            CMPSK%NZ=NZ2
          ELSE IF (TYPE.EQ.'D') THEN
            DMPSK%NZ=NZ2
          ELSE IF (TYPE.EQ.'Z') THEN
            ZMPSK%NZ=NZ2
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF
        CALL ASSERT(ILIGL.EQ.N)
        CALL ASSERT(JCOLL.EQ.N)
        CALL JELIBE(NONU//'.SMOS.SMDI')
        CALL JELIBE(NONU//'.SMOS.SMHC')
        CALL JELIBE(JEXNUM(NOMAT//'.VALM',1))
        IF (LMNSY) CALL JELIBE(JEXNUM(NOMAT//'.VALM',2))
        CALL JELIBE(NONU//'.NUME.DELG')
        IF ( LMD ) THEN
          CALL JELIBE(NONU//'.NUML.DELG')
        ENDIF

        IF (NIV.GE.2) THEN
C --- TEST POUR EVITER LE MONITORING DES CMDES ECLATEES
C --- ET DE LDLT_SP
C     LES OBJETS TEMPORAIRES DE MONITORING SONT EFFACES A CHAQUE
C     FIN DE COMMANDE (NUM_DDL/FACTORISER/RESOUDRE)
          CALL JEEXIN(KMONIT(1),IRET)
          IF (IRET.NE.0) THEN
            CALL JEVEUO(KMONIT(1),'E',IBID)
            ZI(IBID+RANG)=NZ2
          ENDIF
        ENDIF

C       ------------------------------------------------
C       IMPRESSION DE LA MATRICE (SI DEMANDEE) :
C       ------------------------------------------------
        IF (IMPR(1:3).EQ.'OUI') THEN
          WRITE(IFMUMP,*)SYM,'   : SYM', RANG,'   : RANG'
          WRITE(IFMUMP,*)N,'   : N'
          IF (LDIST) THEN
            WRITE(IFMUMP,*)NZ2,'   : NZ_loc'
          ELSE
            WRITE(IFMUMP,*)NZ2,'   : NZ'
          ENDIF
          IF (TYPE.EQ.'S') THEN
            DO K=1,NZ2
              IF (LDIST) THEN
                WRITE(IFMUMP,*)SMPSK%IRN_loc(K),SMPSK%JCN_loc(K),
     &                       SMPSK%A_loc(K)
              ELSE
                WRITE(IFMUMP,*)SMPSK%IRN(K),SMPSK%JCN(K),SMPSK%A(K)
              ENDIF
            ENDDO
          ELSE IF (TYPE.EQ.'C') THEN
            DO K=1,NZ2
              IF (LDIST) THEN
                WRITE(IFMUMP,*)CMPSK%IRN_loc(K),CMPSK%JCN_loc(K),
     &                       CMPSK%A_loc(K)
              ELSE
                WRITE(IFMUMP,*)CMPSK%IRN(K),CMPSK%JCN(K),CMPSK%A(K)
              ENDIF
            ENDDO
          ELSE IF (TYPE.EQ.'D') THEN
            DO K=1,NZ2
              IF (LDIST) THEN
                WRITE(IFMUMP,*)DMPSK%IRN_loc(K),DMPSK%JCN_loc(K),
     &                       DMPSK%A_loc(K)
              ELSE
                WRITE(IFMUMP,*)DMPSK%IRN(K),DMPSK%JCN(K),DMPSK%A(K)
              ENDIF
            ENDDO
          ELSE IF (TYPE.EQ.'Z') THEN
            DO K=1,NZ2
              IF (LDIST) THEN
                WRITE(IFMUMP,*)ZMPSK%IRN_loc(K),ZMPSK%JCN_loc(K),
     &                       ZMPSK%A_loc(K)
              ELSE
                WRITE(IFMUMP,*)ZMPSK%IRN(K),ZMPSK%JCN(K),ZMPSK%A(K)
              ENDIF
            ENDDO
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
          IF (LDIST) THEN
            WRITE(IFMUMP,*) 'MUMPS FIN A_loc'
          ELSE
            WRITE(IFMUMP,*) 'MUMPS FIN A'
          ENDIF
        ENDIF
C FIN DU IF LDIST
      ENDIF

C --- NETTOYAGE VECTEUR TEMPORAIRE LOCAL
      CALL JEEXIN(KFILTR,IRET)
      IF (IRET.NE.0) CALL JEDETR(KFILTR)

      CALL JEDEMA()
#endif
      END
