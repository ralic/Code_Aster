      SUBROUTINE AMUMPM(RANG,LDIST,KXMPS,KMONIT,IMPR,IFMUMP,ELI2LG,
     &                  TYPE)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF AMUMPM MUMPS  DATE 18/05/2009   AUTEUR LEFEBVRE J-P.LEFEBVRE 
! ==================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D              WWW.CODE-ASTER.ORG
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
C IN  IMPR   :   K4   : FLAG POUR IMPRESSION MATRICE
C IN  IFMUMP :   IN   : UNITE LOGIQUE POUR IMPRESSION FICHIER
C IN  ELI2LG :  LOG   : LOGICAL POUR NE LAISSER QU'1 LAGRANGE ACTIF
C IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
C---------------------------------------------------------------
      INTEGER      RANG,KXMPS,IFMUMP
      LOGICAL      LDIST,ELI2LG
      CHARACTER*1  TYPE
      CHARACTER*4  IMPR
      CHARACTER*24 KMONIT(12)

#ifdef _HAVE_MUMPS

C============================================================
      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
C============================================================

      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (DMUMPS_STRUC) , TARGET  :: DMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (DMUMPS_STRUC) , POINTER :: XMPSK
      TYPE (ZMUMPS_STRUC) , POINTER :: YMPSK
      INTEGER      JSMDI,NSMDI,JSMHC,NSMHC,JDELG,N1,N,NZ,NZ2,NVALE,
     &             JVALE,NLONG,JVALE2,NZLOC,JCOL,KTERM,ILIG,ITERM,KZERO,
     &             IFM,NIV,K,SYM,IRET,IBID
      CHARACTER*1  ROUCS(NMXINS)
      CHARACTER*4  KBID,ETAMS(NMXINS),ETAM
      CHARACTER*14 NONUS(NMXINS),NONU
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS),NOMAT,NOSOLV
      REAL*8       RAUX
      COMPLEX*16   CAUX
      LOGICAL      LMNSY,LTYPR,LNN
      COMMON /SMUMPS1/ DMPS
      COMMON /SMUMPS2/ ZMPS
      COMMON /SMUMPS/  NONUS,NOMATS,NOSOLS,ETAMS,ROUCS
      
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
      NOMAT=NOMATS(KXMPS)
      NOSOLV=NOSOLS(KXMPS)
      NONU=NONUS(KXMPS)
      ETAM=ETAMS(KXMPS)
C --- REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
C --- DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
      IF (TYPE.EQ.'R') THEN
        XMPSK=>DMPS(KXMPS)
        LTYPR=.TRUE.
        SYM=XMPSK%SYM
      ELSE IF (TYPE.EQ.'C') THEN
        YMPSK=>ZMPS(KXMPS)
        LTYPR=.FALSE.
        SYM=YMPSK%SYM
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

C       ------------------------------------------------
C        LECTURE D'ADRESSES ET DE PARAMETRES PRELIMINAIRES
C       ------------------------------------------------
      IF (((RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
        CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
        CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
        CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
        CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
        CALL JEVEUO(NONU//'.NUME.DELG','L',JDELG)
        CALL JELIRA(NONU//'.NUME.DELG','LONMAX',N1,KBID)
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
      ENDIF

C       ------------------------------------------------
C       DETERMINATION DU NBRE LOCAL DE TERMES PAR PROC EN MODE DIST
C       NZLOC
C       ------------------------------------------------
      IF (LDIST) THEN
        NZLOC=0
        JCOL=1
        DO KTERM = 1, NZ
          IF (ZI(JSMDI-1+JCOL).LT.KTERM) JCOL=JCOL+1
          ILIG=ZI(JSMHC-1+KTERM)
          IF (LTYPR) THEN
            RAUX=ZR(JVALE-1+KTERM)
            IF (RAUX.NE.0.D0) NZLOC=NZLOC+1
          ELSE
            CAUX=ZC(JVALE-1+KTERM)
            IF (CAUX.NE.(0.D0,0.D0)) NZLOC=NZLOC+1
          ENDIF
C -- SOUS LA DIAGONALE :
          IF ((SYM.EQ.0).AND.(ILIG.NE.JCOL)) THEN
            IF (LTYPR) THEN
              IF (LMNSY) RAUX=ZR(JVALE2-1+KTERM)
              IF (RAUX.NE.0.D0) NZLOC=NZLOC+1
            ELSE
              IF (LMNSY) CAUX=ZC(JVALE2-1+KTERM)
              IF (CAUX.NE.(0.D0,0.D0)) NZLOC=NZLOC+1
            ENDIF
          ENDIF
        ENDDO
        NZ2=NZLOC
      ENDIF

C       ------------------------------------------------
C       ALLOCATION DES OBJETS MUMPS F90
C       ------------------------------------------------
      IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
C       -- POUR LAISSER LE PLUS DE MEMOIRE POSSIBLE A MUMPS:
        CALL JJLDYN(IBID)

        IF (LDIST) THEN
          IF (LTYPR) THEN
            XMPSK%N=N
            XMPSK%NZ_loc=NZ2
            ALLOCATE(XMPSK%IRN_loc(NZ2))
            ALLOCATE(XMPSK%JCN_loc(NZ2))
            ALLOCATE(XMPSK%A_loc(NZ2))
          ELSE
            YMPSK%N=N
            YMPSK%NZ_loc=NZ2
            ALLOCATE(YMPSK%IRN_loc(NZ2))
            ALLOCATE(YMPSK%JCN_loc(NZ2))
            ALLOCATE(YMPSK%A_loc(NZ2))
          ENDIF
        ELSE
          IF (LTYPR) THEN
            XMPSK%N=N
            XMPSK%NZ=NZ2
            ALLOCATE(XMPSK%IRN(NZ2))
            ALLOCATE(XMPSK%JCN(NZ2))
            ALLOCATE(XMPSK%A(NZ2))
          ELSE
            YMPSK%N=N
            YMPSK%NZ=NZ2
            ALLOCATE(YMPSK%IRN(NZ2))
            ALLOCATE(YMPSK%JCN(NZ2))
            ALLOCATE(YMPSK%A(NZ2))
          ENDIF
        ENDIF

C       ------------------------------------------------
C       REMPLISSAGE EFFECTIF DES TERMES DE LA MATRICE
C       ------------------------------------------------
        JCOL=1
        ITERM=0
        DO KTERM=1,NZ

C --- PARTIE TRIANGULAIRE SUPERIEURE
          IF (ZI(JSMDI-1+JCOL).LT.KTERM) JCOL=JCOL+1
          ILIG=ZI(JSMHC-1+KTERM)
          LNN=.FALSE.
          IF (LTYPR) THEN
            RAUX=ZR(JVALE-1+KTERM)
            IF (RAUX.NE.0.D0) LNN=.TRUE.
          ELSE
            CAUX=ZC(JVALE-1+KTERM)
            IF (CAUX.NE.(0.D0,0.D0)) LNN=.TRUE.
          ENDIF
          IF ((LDIST.AND.LNN).OR.(.NOT.LDIST)) THEN
            ITERM=ITERM+1
            IF (LDIST) THEN
              IF (LTYPR) THEN
                XMPSK%IRN_loc(ITERM)=ILIG
                XMPSK%JCN_loc(ITERM)=JCOL
                XMPSK%A_loc(ITERM)=RAUX
              ELSE
                YMPSK%IRN_loc(ITERM)=ILIG
                YMPSK%JCN_loc(ITERM)=JCOL
                YMPSK%A_loc(ITERM)=CAUX
              ENDIF
            ELSE
              IF (LTYPR) THEN
                XMPSK%IRN(ITERM)=ILIG
                XMPSK%JCN(ITERM)=JCOL
                XMPSK%A(ITERM)=RAUX
              ELSE
                YMPSK%IRN(ITERM)=ILIG
                YMPSK%JCN(ITERM)=JCOL
                YMPSK%A(ITERM)=CAUX
              ENDIF
            ENDIF
            IF (ELI2LG) THEN
C ------      ON ELIMINE LE DERNIER TERME DE A/A_loc SI LAG2
              KZERO=0
              IF (ZI(JDELG-1+ILIG).EQ.-1) THEN
                IF (JCOL.EQ.ILIG) KZERO=1
                IF (ZI(JDELG-1+JCOL).EQ.-2) KZERO=1
              ENDIF
              IF (ZI(JDELG-1+ILIG).EQ.-2) THEN
                IF (JCOL.NE.ILIG) KZERO=1
              ENDIF
              IF (ZI(JDELG-1+JCOL).EQ.-2) THEN
                IF (JCOL.NE.ILIG) KZERO=1
              ENDIF
              IF (KZERO.EQ.1)  ITERM=ITERM-1
            ENDIF
          ENDIF

C --- PARTIE TRIANGULAIRE INFERIEURE
          IF ((SYM.EQ.0).AND.(ILIG.NE.JCOL)) THEN
            LNN=.FALSE.
            IF (LTYPR) THEN
              IF (LMNSY) RAUX=ZR(JVALE2-1+KTERM)
              IF (RAUX.NE.0.D0) LNN=.TRUE.
            ELSE
              IF (LMNSY) CAUX=ZC(JVALE2-1+KTERM)
              IF (CAUX.NE.(0.D0,0.D0)) LNN=.TRUE.
            ENDIF
            IF ((LDIST.AND.LNN).OR.(.NOT.LDIST)) THEN
              ITERM=ITERM+1
              IF (LDIST) THEN
                IF (LTYPR) THEN
                  XMPSK%IRN_loc(ITERM)=JCOL
                  XMPSK%JCN_loc(ITERM)=ILIG
                  XMPSK%A_loc(ITERM)=RAUX
                ELSE
                  YMPSK%IRN_loc(ITERM)=JCOL
                  YMPSK%JCN_loc(ITERM)=ILIG
                  YMPSK%A_loc(ITERM)=CAUX
                ENDIF
              ELSE
                IF (LTYPR) THEN
                  XMPSK%IRN(ITERM)=JCOL
                  XMPSK%JCN(ITERM)=ILIG
                  XMPSK%A(ITERM)=RAUX
                ELSE
                  YMPSK%IRN(ITERM)=JCOL
                  YMPSK%JCN(ITERM)=ILIG
                  YMPSK%A(ITERM)=CAUX
                ENDIF
              ENDIF
              IF (ELI2LG) THEN
                IF (KZERO.EQ.1) ITERM=ITERM-1
              ENDIF
            ENDIF
          ENDIF
C ---FIN DE LA BOUCLE SUR NZ
        ENDDO

        CALL ASSERT(ITERM.LE.NZ2)
        NZ2=ITERM
        IF (LDIST) THEN
          IF (LTYPR) THEN
            XMPSK%NZ_loc=NZ2
          ELSE
            YMPSK%NZ_loc=NZ2
          ENDIF
        ELSE
          IF (LTYPR) THEN
            XMPSK%NZ=NZ2
          ELSE
            YMPSK%NZ=NZ2
          ENDIF
        ENDIF
        CALL ASSERT(ILIG.EQ.N)
        CALL ASSERT(JCOL.EQ.N)
        CALL JELIBE(NONU//'.SMOS.SMDI')
        CALL JELIBE(NONU//'.SMOS.SMHC')
        CALL JELIBE(JEXNUM(NOMAT//'.VALM',1))
        IF (LMNSY) CALL JELIBE(JEXNUM(NOMAT//'.VALM',2))
        CALL JELIBE(NONU//'.NUME.DELG')

        IF (NIV.GE.2) THEN
C --- TEST POUR EVITER LE MONITORING DES CMDES ECLATEES
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
        IF (IMPR.EQ.'OUI') THEN
          WRITE(IFMUMP,*)SYM,'   : SYM'
          WRITE(IFMUMP,*)N,'   : N'
          IF (LDIST) THEN
            WRITE(IFMUMP,*)NZ2,'   : NZ_loc'
          ELSE
            WRITE(IFMUMP,*)NZ2,'   : NZ'
          ENDIF
          IF (LTYPR) THEN
            DO K=1,NZ2
              IF (LDIST) THEN
                WRITE(IFMUMP,*)XMPSK%IRN_loc(K),XMPSK%JCN_loc(K),
     &                       XMPSK%A_loc(K)
              ELSE
                WRITE(IFMUMP,*)XMPSK%IRN(K),XMPSK%JCN(K),XMPSK%A(K)
              ENDIF
            ENDDO
          ELSE
            DO K=1,NZ2
              IF (LDIST) THEN
                WRITE(IFMUMP,*)YMPSK%IRN_loc(K),YMPSK%JCN_loc(K),
     &                       YMPSK%A_loc(K)
              ELSE
                WRITE(IFMUMP,*)YMPSK%IRN(K),YMPSK%JCN(K),YMPSK%A(K)
              ENDIF
            ENDDO
          ENDIF
          IF (LDIST) THEN
            WRITE(IFMUMP,*) 'MUMPS FIN A_loc'
          ELSE
            WRITE(IFMUMP,*) 'MUMPS FIN A'
          ENDIF
        ENDIF
C FIN DU IF LDIST
      ENDIF
      CALL JEDEMA()
#endif
      END
