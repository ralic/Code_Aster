      SUBROUTINE AMUMPR(ACTION,KXMPS,VECAS,SOLU,VCINE,NBSOL,IRET)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!&           CONFIGURATION MANAGEMENT OF EDF VERSION                   
!& MODIF AMUMPR MUMPS  DATE 19/05/2008   AUTEUR COURTOIS M.COURTOIS 
! ==================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D              WWW.CODE-ASTER.ORG
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
C
C IN : ACTION :
C     /'PRERES'  : POUR DEMANDER LA FACTORISATION
C     /'RESOUD'  : POUR DEMANDER LA DESCENTE/REMONTEE
C     /'DETR_MAT': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE MUMPS
C                  ASSOCIEE A UNE MATRICE
C     /'RESOUD_CONTACT' : RESOLUTIONS SIMULTANEES DU CONTACT
C
C IN : KXMPS (I)   : INDICE DE L'INSTANCE MUMPS DANS DMPS
C IN : VECAS (K19) : NOM DU SECOND MEMBRE
C            (SI ACTION=RESOUD)
C IN : SOLU  (K19) : NOM DE LA SOLUTION
C            (SI ACTION=RESOUD)
C IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
C            (SI ACTION=RESOUD)
C OUT : IRET (I) : CODE_RETOUR :
C            0 : OK
C            2 : MATRICE NUMERIQUEMENT SINGULIERE
C IN  : NBSOL  : NBRE DE SYSTEMES A RESOUDRE
C---------------------------------------------------------------
      CHARACTER*(*) ACTION
      CHARACTER*19  VECAS,SOLU,VCINE,NOSOLV
      INTEGER IRET,NBSOL,KXMPS

#ifdef _HAVE_MUMPS

C================================================================
      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
C================================================================
C --- DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C================================================================
      INTEGER NMXINS
C     -- LE CODE DE RETOUR POUR MPI DOIT ETRE UN INTEGER SUR 32 BITS
      PARAMETER (NMXINS=5)
      TYPE (DMUMPS_STRUC) , TARGET  :: DMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (DMUMPS_STRUC) , POINTER :: XMPSK
      INTEGER ISYM,N,NZ,NZ2,JSLVI,JSLVK,JSLVR,ISYMV,ISYMM,NNBSOL
      INTEGER I,K,JCOL,ILIG,KTERM,JVECAS,JSOLU,IERD,RANG
      INTEGER NLONG, JSMDI, NSMDI, JSMHC,NSMHC,JVALE,IBID,JVECA2
      INTEGER IDVALC,LMAT,ITERM,NVALE,JVALE2,IFMUMP,NBPROC,IAUX3
      INTEGER NIV,IFM,JREFA,JDELG,N1,KZERO,NZLOC,MONIT(9),IAUX1,IAUX2
      INTEGER*4 N4
      CHARACTER*1 ROUCS(NMXINS),ROUC
      CHARACTER*4 KBID,ETAMS(NMXINS),IMPR,ETAM,TYPM,KCODE
      CHARACTER*14 NU,NONUS(NMXINS),NONU
      CHARACTER*19 MATAS,NOMATS(NMXINS),NOMAT
      CHARACTER*19 NOSOLS(NMXINS)
      CHARACTER*24 SOLVAL,VECVAL,VCIVAL,KMONIT(9),VECVA2
      REAL*8 EPSMAX,VALR(2),RAUX,TEMPS(6),RMONIT(18),RDESQ
      COMPLEX*16 CBID
      LOGICAL LMNSY,LQUALI,ELI2LG,LDIST,LCONT
      COMMON /SMUMPS/ DMPS,ZMPS,NONUS,NOMATS,NOSOLS,ETAMS,ROUCS
C=================================================================
      CALL JEMARQ()

C     -- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LA BIBLIOTHEQUE MKL
C     -- CAR CES EXCEPTIONS NE SONT PAS JUSTIFIEES
      CALL MATFPE(-1)
      CALL INFNIV(IFM,NIV)
C      WRITE(IFM,*)'DEBUT AMUMPR'
C      CALL JXVERI('MESSAGE','DEBUT AMUMPR')

      IF (ACTION.EQ.'RESOUD_CONTACT') THEN
        LCONT=.TRUE.
      ELSE
        LCONT= .FALSE.
      ENDIF
      IF (.NOT.LCONT) THEN
        SOLVAL=SOLU(1:19)//'.VALE'
        VECVAL=VECAS(1:19)//'.VALE'
        VCIVAL=VCINE(1:19)//'.VALE'
      ELSE
        SOLVAL=VECAS
        VECVAL=VECAS
        VCIVAL=VCINE
      ENDIF
      VECVA2='&&AMUMPR.VECVA2'
      CALL ASSERT(KXMPS.GT.0)
      CALL ASSERT(KXMPS.LE.NMXINS)
      NOMAT=NOMATS(KXMPS)
      NOSOLV=NOSOLS(KXMPS)
      NONU=NONUS(KXMPS)
      ETAM=ETAMS(KXMPS)
      XMPSK=>DMPS(KXMPS)
      IRET=0

C     -- IMPR : PARAMETRE POUR IMPRIMER LA MATRICE OU LE SECOND MEMBRE
C               SUR L'UNITE IFMUMP (POUR RESOLUTION EXTERNE)
      IMPR='OUI'
      IMPR='NON'
      IFMUMP=17

C     RDESQ: TAUX DE DESEQUILIBRAGE MAX. POUR PROFILING ACTIVE AVEC INFO=2
      RDESQ=10.D0

C     CNTL(2)=1.D-13 POUR ETRE PLUS SUR DE FAIRE DES ITERATIONS DE RAFINEMENT
      XMPSK%CNTL(2) = 1.D-14


C     0. L'UTILISATEUR VEUT-IL UNE ESTIMATION DE LA QUALITE DE LA SOLUTION ? => LQUALI
C     --------------------------------------------------------------------------------
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      EPSMAX=ZR(JSLVR-1+2)
      LQUALI=(EPSMAX.GT.0.D0)

      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
C     -- POUR "ELIMINER" LE 2EME LAGRANGE :
      ELI2LG=ZK24(JSLVK-1+6)(1:3).EQ.'OUI'

C     -- MUMPS PARALLELE DISTRIBUE ?
      LDIST=.FALSE.
      LDIST=ZK24(JSLVK-1+7)(1:10).EQ.'DISTRIBUE_'
      RANG=XMPSK%MYID
      NBPROC=XMPSK%NPROCS

C POUR MONITORING
      IF (NIV.GE.2) THEN
        KMONIT(1)='&MUMPS.INFO.MAILLE'
        KMONIT(2)='&MUMPS.INFO.MEMOIRE'
        KMONIT(3)='&MUMPS.INFO.CPU.FACS'
        KMONIT(4)='&MUMPS.INFO.CPU.ANAL'
        KMONIT(5)='&MUMPS.INFO.CPU.FACN'
        KMONIT(6)='&MUMPS.INFO.CPU.CAEL'
        KMONIT(7)='&MUMPS.INFO.CPU.ASSE'
        KMONIT(8)='&MUMPS.INFO.CPU.SOLV'
        KMONIT(9)='&MUMPS.NB.MAILLE'
        CALL JEVEUO(KMONIT(1),'E',MONIT(1))
        CALL JEVEUO(KMONIT(2),'E',MONIT(2))
        CALL JEVEUO(KMONIT(3),'E',MONIT(3))
        CALL JEVEUO(KMONIT(4),'E',MONIT(4))
        CALL JEVEUO(KMONIT(5),'E',MONIT(5))
        CALL JEVEUO(KMONIT(6),'E',MONIT(6))
        CALL JEVEUO(KMONIT(7),'E',MONIT(7))
        CALL JEVEUO(KMONIT(8),'E',MONIT(8))
        CALL JEVEUO(KMONIT(9),'E',MONIT(9))
      ENDIF

C     1. PRERES :
      IF (ACTION.EQ.'PRERES') THEN
C     ------------------------------------------------

         XMPSK%COMM = MPI_COMM_WORLD

C        1.2 CALCUL DE ISYM  (XMPSK%SYM):
C        ISYM = 0 => NON-SYMETRIQUE
C        ISYM = 1 => SYMETRIQUE DEFINIE POSITIVE
C        ISYM = 2 => SYMETRIQUE  GENERAL

C        ISYMM DEDUIT DE LA MATRICE : NONSYM OU SYMGEN
         CALL JEVEUO(NOMAT//'.REFA','L',JREFA)
         TYPM=ZK24(JREFA-1+9)
         IF (TYPM.EQ.'MR') THEN
            ISYMM=0
         ELSE IF (TYPM.EQ.'MS') THEN
            ISYMM=2
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF

C        PRISE EN COMPTE DE LA VOLONTE DE L'UTILISATEUR
C        => ISYMV
         IF (ZK24(JSLVK-1+3).EQ.'NONSYM') THEN
            ISYMV=0
         ELSE IF (ZK24(JSLVK-1+3).EQ.'SYMDEF') THEN
            ISYMV=1
         ELSE IF (ZK24(JSLVK-1+3).EQ.'SYMGEN') THEN
            ISYMV=2
         ELSE IF (ZK24(JSLVK-1+3).EQ.'AUTO') THEN
            ISYMV=-1
         ELSE
            CALL ASSERT(.FALSE.)
         END IF

C STRATEGIE PRUDENTE ET CONSERVATIVE
C SI AUTO: NONSYM OU SYMGEN SUIVANT LA STRUCTURE DE LA MATRICE
C SINON, ON APPLIQUE LE CHOIX DE L'UTILISATEUR
         IF (ISYMV.EQ.-1) THEN
            ISYM=ISYMM
         ELSE IF (ISYMV.EQ.0) THEN
            ISYM=ISYMV
         ELSE
            IF (ISYMM.EQ.0) THEN
              CALL U2MESK('F','FACTOR_56',1,ZK24(JSLVK-1+3))
            ELSE
              ISYM=ISYMV
            END IF
         ENDIF


C        INITIALISATION DE L'OCCURENCE MUMPS KXMPS
C        ----------------
         XMPSK%SYM = ISYM
         XMPSK%PAR = 1
         XMPSK%JOB = -1
         CALL DMUMPS(XMPSK)
         RANG=XMPSK%MYID
         NBPROC=XMPSK%NPROCS

C        CHOIX ICNTL :
C        --------------
C        ICNTL(1,2,3) : UNITES LOGIQUES POUR LES MESSAGES
         XMPSK%ICNTL(1) = IFM
         XMPSK%ICNTL(2) = 0
         XMPSK%ICNTL(3) = 0

C        ICNTL(4) : NIVEAU DES MESSAGES (DEFAUT=1):
         XMPSK%ICNTL(4) = 1
C SEULEMENT POUR MONITORING AVANCE
C         IF (NIV.GT.1) THEN
C            XMPSK%ICNTL(4) = 2
C            XMPSK%ICNTL(3) = IFM
C         END IF
C        ICNTL(5) : FORMAT MATRICE   0 -> ASSEMBLEE
         XMPSK%ICNTL(5) = 0

C        ICNTL(6) : ALGORITHME DE SCALING
C        ICNTL(8) : PERMUTATION DES COLONNES
C        ICNTL(12)  DANS LE CAS SYMGEN, RAJOUT DE CONTRAINTES SUR LE
C                   RENUMEROTEUR
C        ATTENTION CES CHOIX SONT LIES
         IF (ZK24(JSLVK-1+2).EQ.'SANS') THEN
            XMPSK%ICNTL(6) = 0
            XMPSK%ICNTL(8) = 0
            XMPSK%ICNTL(12) = 1
         ELSE IF (ZK24(JSLVK-1+2).EQ.'AUTO') THEN
            XMPSK%ICNTL(6) = 7
            XMPSK%ICNTL(8) = 7
            XMPSK%ICNTL(12) = 0
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF

C        ICNTL(7) : ALGORITHME DE RENUMEROTATION
         IF (ZK24(JSLVK-1+4).EQ.'AMD') THEN
            XMPSK%ICNTL(7) = 0
         ELSE IF (ZK24(JSLVK-1+4).EQ.'AMF') THEN
            XMPSK%ICNTL(7) = 2
         ELSE IF (ZK24(JSLVK-1+4).EQ.'SCOTCH') THEN
            XMPSK%ICNTL(7) = 3
         ELSE IF (ZK24(JSLVK-1+4).EQ.'PORD') THEN
            XMPSK%ICNTL(7) = 4
         ELSE IF (ZK24(JSLVK-1+4).EQ.'METIS') THEN
            XMPSK%ICNTL(7) = 5
         ELSE IF (ZK24(JSLVK-1+4).EQ.'QAMD') THEN
            XMPSK%ICNTL(7) = 6
         ELSE IF (ZK24(JSLVK-1+4).EQ.'AUTO') THEN
            XMPSK%ICNTL(7) = 7
         ELSE
            CALL ASSERT(.FALSE.)
         ENDIF

C INITIALISATION EN DUR (EN DOUBLONS PAR RAPPORT A CALL DMUMPS JOB=-1)
C MAIS ON NE SAIT JAMAIS AVEC LES EVOLUTIONS DES INITS DU PACKAGE
C ET CELA PERMET DE SURCHARGER PLUS RAPIDEMENT POUR TESTER
C        ICNTL(9) : TYPE DE RESOLUTION: A OU AT
         XMPSK%ICNTL(9) = 1

C        ICNTL(10) : NB MAX ITERATIONS DE RAFFINEMENT :
         XMPSK%ICNTL(10) = 0
         IF (LQUALI)  XMPSK%ICNTL(10) = 4

C        ICNTL(11) : DECLENCHER LE RAFFINEMENT ITERATIF
C        IL FAUT METTRE ICNTL(11)=0 POUR AVOIR DE MEILLEURES "PERFS"
C        SURCOUT DE 10/20%
         XMPSK%ICNTL(11) = 0
         IF (LQUALI)  XMPSK%ICNTL(11) = 1

C        ICNTL(13) : CONTROLE DU PARALLELISME INDUIT PAR SCALAPACK
         XMPSK%ICNTL(13) = 0

C        ICNTL(14) % DE MEMOIRE SUPPL. POUR PIVOTAGE (DEFAUT:10)
         CALL JEVEUO(NOSOLV//'.SLVI','L',JSLVI)
         XMPSK%ICNTL(14) = ZI(JSLVI-1+2)

C        VALEURS POUR LA DETECTION DE NOYAU
         XMPSK%ICNTL(15)=0
         XMPSK%ICNTL(16)=0
         XMPSK%ICNTL(17)=0
         XMPSK%ICNTL(24)=0
         XMPSK%ICNTL(25)=0

C        VALEUR POUR LE PARALLELISME/DISTRIBUTION DU SECOND MEMBRE ET
C        SOLUTION
         IF (LDIST) THEN
           XMPSK%ICNTL(18)=3
         ELSE
           XMPSK%ICNTL(18)=0
         ENDIF
         XMPSK%ICNTL(20)=0
         XMPSK%ICNTL(21)=0

C        COMPLEMENT DE SCHUR
         XMPSK%ICNTL(19)=0
         XMPSK%ICNTL(26)=0

C        IN_CORE/OUT_OF_CORE
         XMPSK%ICNTL(22)=0
         XMPSK%ICNTL(23)=0

C        1.3 ALLOCATION ET REMPLISSAGE DE %IRN ET %ICN ET %A:
C        -------------------------------------------------------
C        LECTURE D'ADRESSES ET DE PARAMETRES PRELIMINAIRES
         IF (((RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN

            CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
            CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
            CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
            CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
            CALL JEVEUO(NONU//'.NUME.DELG','L',JDELG)
            CALL JELIRA(NONU//'.NUME.DELG','LONMAX',N1,KBID)
            CALL ASSERT(N1.EQ.NSMDI)
C           CALCUL DE N
            N=NSMDI
C           CALCUL DE NZ2
            NZ=ZI(JSMDI-1+N)
            CALL ASSERT(NZ.LE.NSMHC)
            NZ2=NZ
            IF (XMPSK%SYM.EQ.0) NZ2=2*NZ-N
            CALL JELIRA(NOMAT//'.VALM','NMAXOC',NVALE,KBID)
C LMNSY EST INDEPENDANT DE XMPSK%SYM POUR POUVOIR TRAITER DES CAS SYM
C EN MUMPS NON SYMETRIQUE
            IF (NVALE.EQ.1) THEN
               LMNSY=.FALSE.
            ELSE IF (NVALE.EQ.2) THEN
               LMNSY=.TRUE.
            ELSE
               CALL ASSERT(.FALSE.)
            ENDIF

            CALL JEVEUO(JEXNUM(NOMAT//'.VALM',1),'L',JVALE)
            CALL JELIRA(JEXNUM(NOMAT//'.VALM',1),'LONMAX',
     &                NLONG,KBID)
            CALL ASSERT(NLONG.EQ.NZ)
            IF (LMNSY) THEN
               CALL JEVEUO(JEXNUM(NOMAT//'.VALM',2),'L',JVALE2)
               CALL JELIRA(JEXNUM(NOMAT//'.VALM',2),'LONMAX',
     &                NLONG,KBID)
               CALL ASSERT(NLONG.EQ.NZ)
            ENDIF
         ENDIF
C        DETERMINATION DU NBRE LOCAL DE TERME PAR PROC EN MODE DIST
C        NZLOC
         IF (LDIST) THEN
            NZLOC=0
            JCOL=1
            DO KTERM = 1, NZ
              IF (ZI(JSMDI-1+JCOL).LT.KTERM) JCOL=JCOL+1
              ILIG=ZI(JSMHC-1+KTERM)
              RAUX=ZR(JVALE-1+KTERM)
              IF (RAUX.NE.0.D0) NZLOC=NZLOC+1
C              -- SOUS LA DIAGONALE :
              IF ((XMPSK%SYM.EQ.0).AND.(ILIG.NE.JCOL)) THEN
                 IF (LMNSY) RAUX=ZR(JVALE2-1+KTERM)
                 IF (RAUX.NE.0.D0) NZLOC=NZLOC+1
              ENDIF
            ENDDO
            NZ2=NZLOC
         ENDIF

C        WRITE(6,*)'NZ2 ',NZ2
C        ALLOCATION DES OBJETS MUMPS F90 ET LEUR REMPLISSAGE EFFECTIF
         IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
            XMPSK%N=N
            IF (LDIST) THEN
              XMPSK%NZ_loc=NZ2
              ALLOCATE(XMPSK%IRN_loc(NZ2))
              ALLOCATE(XMPSK%JCN_loc(NZ2))
              ALLOCATE(XMPSK%A_loc(NZ2))
            ELSE
              XMPSK%NZ=NZ2
              ALLOCATE(XMPSK%IRN(NZ2))
              ALLOCATE(XMPSK%JCN(NZ2))
              ALLOCATE(XMPSK%A(NZ2))
            ENDIF
            JCOL=1
            ITERM=0
            DO KTERM = 1, NZ
               IF (ZI(JSMDI-1+JCOL).LT.KTERM) JCOL=JCOL+1
               ILIG=ZI(JSMHC-1+KTERM)
               RAUX=ZR(JVALE-1+KTERM)
               IF ((LDIST.AND.(RAUX.NE.0.D0)).OR.(.NOT.LDIST)) THEN
                 ITERM=ITERM+1
                 IF (LDIST) THEN
                   XMPSK%IRN_loc(ITERM)=ILIG
                   XMPSK%JCN_loc(ITERM)=JCOL
                   XMPSK%A_loc(ITERM)=RAUX
                 ELSE
                   XMPSK%IRN(ITERM)=ILIG
                   XMPSK%JCN(ITERM)=JCOL
                   XMPSK%A(ITERM)=RAUX
                 ENDIF
                 IF (ELI2LG) THEN
C                ON ELIMINE LE DERNIER TERME DE A/A_loc SI LAG2
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

C              -- SOUS LA DIAGONALE :
               IF ((XMPSK%SYM.EQ.0).AND.(ILIG.NE.JCOL)) THEN
                 IF (LMNSY) RAUX=ZR(JVALE2-1+KTERM)
                 IF ((LDIST.AND.(RAUX.NE.0.D0)).OR.(.NOT.LDIST)) THEN
                   ITERM=ITERM+1
                   IF (LDIST) THEN
                     XMPSK%IRN_loc(ITERM)=JCOL
                     XMPSK%JCN_loc(ITERM)=ILIG
                     XMPSK%A_loc(ITERM)=RAUX
                   ELSE
                     XMPSK%IRN(ITERM)=JCOL
                     XMPSK%JCN(ITERM)=ILIG
                     XMPSK%A(ITERM)=RAUX
                   ENDIF
                   IF (ELI2LG) THEN
                     IF (KZERO.EQ.1) ITERM=ITERM-1
                   ENDIF
                 ENDIF
               ENDIF
C FIN DE LA BOUCLE SUR NZ
            ENDDO

C           WRITE(6,*)'ITERM ',ITERM

            CALL ASSERT(ITERM.LE.NZ2)
            NZ2=ITERM
            IF (LDIST) THEN
              XMPSK%NZ_loc=NZ2
            ELSE
              XMPSK%NZ=NZ2
            ENDIF
            CALL ASSERT(ILIG.EQ.N)
            CALL ASSERT(JCOL.EQ.N)
            CALL JELIBE(NONU//'.SMOS.SMDI')
            CALL JELIBE(NONU//'.SMOS.SMHC')
            CALL JELIBE(JEXNUM(NOMAT//'.VALM',1))
            IF (LMNSY) CALL JELIBE(JEXNUM(NOMAT//'.VALM',2))
            CALL JELIBE(NONU//'.NUME.DELG')

            IF (NIV.GE.2) THEN
              ZI(MONIT(1)+RANG)=NZ2
C             WRITE(6,*)'NBRE DE TERMES DE LA MATRICE ',NZ2
            ENDIF

C           IMPRESSION DE LA MATRICE (SI DEMANDEE) :
C           ----------------------------------------------
            IF (IMPR.EQ.'OUI') THEN
              WRITE(IFMUMP,*) XMPSK%SYM,'   : SYM'
              WRITE(IFMUMP,*) XMPSK%N,'   : N'
              IF (LDIST) THEN
                WRITE(IFMUMP,*) XMPSK%NZ_loc,'   : NZ_loc'
              ELSE
                WRITE(IFMUMP,*) XMPSK%NZ,'   : NZ'
              ENDIF
              DO K=1,NZ2
                IF (LDIST) THEN
                  WRITE(IFMUMP,*)XMPSK%IRN_loc(K),XMPSK%JCN_loc(K),
     &                           XMPSK%A_loc(K)
                ELSE
                  WRITE(IFMUMP,*)XMPSK%IRN(K),XMPSK%JCN(K),XMPSK%A(K)
                ENDIF
              ENDDO
              IF (LDIST) THEN
                WRITE(IFMUMP,*) 'MUMPS FIN A_loc'
              ELSE
                WRITE(IFMUMP,*) 'MUMPS FIN A'
              ENDIF
            ENDIF
C FIN DU IF LDIST
          ENDIF

C         1.4  ANALYSE :
C         -----------------------------

          IF (NIV.GE.2) THEN
            CALL UTTCPU(70,'INIT ',6,TEMPS)
            CALL UTTCPU(70,'DEBUT',6,TEMPS)
          ENDIF
          XMPSK%JOB = 1
          CALL DMUMPS(XMPSK)
          IF(RANG.EQ.0) CALL ASSERT(XMPSK%INFOG(1).EQ.0)

          IF (NIV.GE.2) THEN
            CALL UTTCPU(70,'FIN  ',6,TEMPS)
            ZR(MONIT(4)+RANG)=TEMPS(5)+TEMPS(6)
C            WRITE(6,*)'TEMPS ANALYSE MUMPS: ',RANG,TEMPS(5)+TEMPS(6)
            CALL UTTCPU(70,'INIT ',6,TEMPS)
            CALL UTTCPU(70,'DEBUT',6,TEMPS)
          ENDIF
C --- VERIFICATION DU RENUMEROTEUR EFFECTIVEMENT UTILISE
          IF(ZK24(JSLVK-1+4).NE.'AUTO' .AND.
     &        XMPSK%ICNTL(7).NE.XMPSK%INFOG(7)) THEN
              CALL U2MESS('A','FACTOR_50',1,ZK24(JSLVK-1+4))
          ENDIF

C        1.5  FACTORISATION NUMERIQUE :
C        -----------------------------

         XMPSK%JOB = 2
         CALL DMUMPS(XMPSK)
         IF (NIV.GE.2) THEN
           CALL UTTCPU(70,'FIN  ',6,TEMPS)
           ZR(MONIT(5)+RANG)=TEMPS(5)+TEMPS(6)
           ZI(MONIT(2)+RANG)=XMPSK%INFO(9)+XMPSK%INFO(10)
           CALL MUMMPI(4,IFM,NIV,KMONIT(1),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(2),NBPROC,IBID)
C           WRITE(6,*)'TEMPS FACTN MUMPS: ',RANG,TEMPS(5)+TEMPS(6)
         ENDIF
         IF (XMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
         ELSE IF (XMPSK%INFOG(1).EQ.-9) THEN
           CALL U2MESS('F','FACTOR_53')
         ELSE IF (XMPSK%INFOG(1).EQ.-10) THEN
           IRET=2
         ELSE IF (XMPSK%INFOG(1).EQ.-13) THEN
           CALL U2MESS('F','FACTOR_54')
         ELSE
           IF (XMPSK%INFOG(1).LT.0) THEN
             CALL U2MESI('F','FACTOR_55',1,XMPSK%INFOG(1))
           ELSE
             CALL U2MESI('A','FACTOR_55',1,XMPSK%INFOG(1))
           ENDIF
         ENDIF

         IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
            IF (.NOT.LQUALI) THEN
              IF (LDIST) THEN
                DEALLOCATE(XMPSK%A_loc)
                DEALLOCATE(XMPSK%IRN_loc)
                DEALLOCATE(XMPSK%JCN_loc)
              ELSE
                DEALLOCATE(XMPSK%A)
                DEALLOCATE(XMPSK%IRN)
                DEALLOCATE(XMPSK%JCN)
              ENDIF
            ENDIF
         ENDIF


C  2- RESOUD :
      ELSE IF (ACTION(1:6).EQ.'RESOUD') THEN
C     --------------------------------

         N=XMPSK%N
         NNBSOL=N*NBSOL
         N4=NNBSOL
         XMPSK%NRHS=NBSOL
         XMPSK%LRHS=N
         IF (((RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN

            IF (RANG.EQ.0) ALLOCATE(XMPSK%RHS(NNBSOL))
            CALL JEVEUO(VECVAL,'E',JVECAS)

C           -- PRISE EN COMPTE DU CONDITIONNEMENT DES LAGRANGES :
            CALL MTDSCR(NOMAT)
            CALL JEVEUO(NOMAT//'.&INT','E',LMAT)
            IF (RANG.EQ.0) THEN
              DO I=1,NBSOL
                CALL MRCONL(LMAT,N,'R',ZR(JVECAS+N*(I-1)),1)
              ENDDO
            ELSE
              CALL JEDUPO(VECVAL,'V',VECVA2,.FALSE.)
              CALL JERAZO(VECVAL,NNBSOL,1)
            ENDIF

C           -- PRISE EN COMPTE DES CHARGES CINEMATIQUES :
            CALL JEEXIN(VCIVAL,IERD)
            IF ( VCINE.NE.' '.AND.IERD.NE.0)THEN
               CALL JEVEUO(VCIVAL,'L',IDVALC)
               CALL JELIRA(VCIVAL,'TYPE',IBID,ROUC)
               CALL ASSERT(ROUC.EQ.'R')
               DO I=1,NBSOL
                 CALL CSMBGG(LMAT,ZR(JVECAS+N*(I-1)),ZR(IDVALC),
     &                       CBID,CBID,'R')
               ENDDO
            ENDIF

C        -- REDUCE DU SECOND MEMBRE AU PROC MAITRE
            IF (LDIST)
     &        CALL MUMMPI(4,IFM,NIV,VECVAL,NNBSOL,IBID)

C           -- COPIE DE VECAS DANS %RHS:
            IF (RANG.EQ.0) CALL DCOPY(N4,ZR(JVECAS),1,XMPSK%RHS,1)

C           -- ON RETABLIT VECVAL SUR LES PROCS != 0 :
            IF (RANG.GT.0) THEN
              CALL JEVEUO(VECVA2,'L',JVECA2)
              CALL JACOPO(NNBSOL,'R',JVECA2,JVECAS)
              CALL JEDETR(VECVA2)
            ENDIF
            CALL JELIBE(VECVAL)

C           IMPRESSION DU/DES SECONDS MEMBRES (SI DEMANDE) :
C           ----------------------------------------------
            IF (IMPR.EQ.'OUI') THEN
              DO K=1,NNBSOL
                WRITE(IFMUMP,*) XMPSK%RHS(K)
              ENDDO
              WRITE(IFMUMP,*) 'MUMPS FIN RHS'
            ENDIF
C FIN DU SI LDIST
         ENDIF

         IF (NIV.GE.2) THEN
           CALL UTTCPU(70,'INIT ',6,TEMPS)
           CALL UTTCPU(70,'DEBUT',6,TEMPS)
         ENDIF

         XMPSK%JOB = 3
         CALL DMUMPS(XMPSK)

         IF (NIV.GE.2) THEN
           CALL UTTCPU(70,'FIN  ',6,TEMPS)
           ZR(MONIT(8)+RANG)=TEMPS(5)+TEMPS(6)
C           WRITE(6,*)'TEMPS SOLVE MUMPS: ',RANG,TEMPS(5)+TEMPS(6)
         ENDIF

         IF (XMPSK%INFOG(1).EQ.0) THEN
C              -- C'EST OK
         ELSE IF ((XMPSK%INFOG(1).EQ.8).AND.(LQUALI)) THEN
           CALL U2MESI('A','FACTOR_62',1,XMPSK%ICNTL(10))
         ELSE IF (XMPSK%INFOG(1).LT.0) THEN
           CALL U2MESI('F','FACTOR_55',1,XMPSK%INFOG(1))
         ELSE IF (XMPSK%INFOG(1).EQ.4) THEN
C          -- PERMUTATION DE COLONNES, XMPSK%JCN MODIFIE VOLONTAIREMENT
C          -- PAR MUMPS. IL NE FAUT DONC PAS LE MANIPULER TEL QUE
C          -- PAS GRAVE POUR ASTER.
         ELSE
           CALL U2MESI('A','FACTOR_55',1,XMPSK%INFOG(1))
         ENDIF


C        -- CONTROLE DE L'ERREUR SUR LA SOLUTION :
         IF (LQUALI) THEN
           IF (XMPSK%RINFOG(9).GT.EPSMAX) THEN
             VALR(1)=XMPSK%RINFOG(9)
             VALR(2)=EPSMAX
             CALL U2MESR('F','FACTOR_57',2,VALR)
           ENDIF
         ENDIF

C        -- RECOPIE DE %RHS DANS SOLU
         CALL JEVEUO(SOLVAL,'E',JSOLU)
         IF (RANG.EQ.0) THEN
           CALL DCOPY(N4,XMPSK%RHS,1,ZR(JSOLU),1)
           DEALLOCATE(XMPSK%RHS)
         ENDIF

C        -- PRISE EN COMPTE DES LAGRANGES :
C        -- VALIDE QUE SUR PROC 0, MAIS C'EST OK CAR ON
C        -- BROADCAST LA SOLUTION APRES
         IF (RANG.EQ.0) THEN
           IF (ELI2LG) THEN
             CALL JEVEUO(NONU//'.NUME.DELG','L',JDELG)
             DO I=1,NBSOL
               DO K=1,N
                 IF (ZI(JDELG-1+K).EQ.-2) ZR(JSOLU-1+(I-1)*N+K)=0.D0
               ENDDO
             ENDDO
           ENDIF
           DO I=1,NBSOL
             CALL MRCONL(LMAT,N,'R',ZR(JSOLU+N*(I-1)),1)
           ENDDO
         ENDIF

C        -- BROADCAST DE SOLU A TOUS LES PROC
         CALL MUMMPI(1,IFM,NIV,SOLVAL,NNBSOL,IBID)
         CALL JELIBE(SOLVAL)

C AFFICHAGE DU MONITORING
         IF (NIV.GE.2) THEN
C COMMUNICATION DES DONNEES DU MONITORING
           CALL MUMMPI(4,IFM,NIV,KMONIT(3),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(4),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(5),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(6),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(7),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(8),NBPROC,IBID)
           IF (RANG.EQ.0) THEN
             WRITE(IFM,*)
             WRITE(IFM,*)'*********************************************'
     &                  //'*********************************'
             WRITE(IFM,*)'<MONITORING MUMPS >'
             WRITE(IFM,'(A19,I9)')' TAILLE DU SYSTEME ',XMPSK%N
             IF (LQUALI) THEN
               WRITE(IFM,'(A28,1PD11.4,1PD11.4)')'CONDITIONNEMENT'//
     &           '/ERREUR ALGO ',XMPSK%RINFOG(10),XMPSK%RINFOG(7)
               WRITE(IFM,'(A23,1PD11.4)')'ERREUR SUR LA SOLUTION ',
     &           XMPSK%RINFOG(9)
             ENDIF
             IAUX1=0
             IAUX2=0
             IAUX3=0
             WRITE(IFM,*)'RANG    '//' NBRE MAILLES    '//
     &         ' NBRE TERMES K    '//' LU FACTEURS'
             DO K=0,NBPROC-1
               WRITE(IFM,1001)K,ZI(MONIT(9)+K),ZI(MONIT(1)+K),
     &                        ZI(MONIT(2)+K)
               IAUX1=IAUX1+ZI(MONIT(9)+K)
               IAUX2=IAUX2+ZI(MONIT(1)+K)
               IAUX3=IAUX3+ZI(MONIT(2)+K)
             ENDDO
             WRITE(IFM,*)'--------------------------------------------'
     &          //'---------------'
             WRITE(IFM,1003)IAUX1,IAUX2,IAUX3
             WRITE(IFM,*)
             DO I=1,18
               RMONIT(I)=0.D0
             ENDDO
             WRITE(IFM,*)'TEMPS CPU+SYSTEME'
             WRITE(IFM,*)'RANG '//
     &        ' ASTER: FACS CALCUL_ELEM  ASSEMBLAGE '//
     &        '| MUMPS: ANALYSE  FACTO_NUM  RESOL'
             DO K=0,NBPROC-1
               WRITE(IFM,1002)K,ZR(MONIT(3)+K),ZR(MONIT(6)+K),
     &           ZR(MONIT(7)+K),ZR(MONIT(4)+K),ZR(MONIT(5)+K),
     &           ZR(MONIT(8)+K)
                 DO I=1,6
                   RMONIT(I)=RMONIT(I)+(ZR(MONIT(I+2)+K)/NBPROC)
                 ENDDO
             ENDDO
             WRITE(IFM,*)'------------------------------------------'//
     &                   '-----------------------------------'
             RAUX=0.D0
             DO I=1,6
               RAUX=RAUX+RMONIT(I)
             ENDDO
             DO I=1,6
               IF (RAUX.NE.0.D0)
     &           RMONIT(6+I)=100.D0*RMONIT(I)/RAUX
             ENDDO
             DO K=0,NBPROC-1
               DO I=1,6
                 RAUX=ZR(MONIT(I+2)+K)-RMONIT(I)
                 IF (DABS(RAUX).GT.DABS(RMONIT(12+I)))
     &             RMONIT(12+I)=RAUX
               ENDDO
             ENDDO
             DO I=1,6
               IF (RMONIT(I).NE.0.D0)
     &           RMONIT(12+I)=100.D0*RMONIT(12+I)/RMONIT(I)
             ENDDO
             WRITE(IFM,1004)RMONIT(1),RMONIT(4),RMONIT(5),RMONIT(2),
     &                   RMONIT(3),RMONIT(6)
             WRITE(IFM,*)'EN % : VALEUR RELATIVE ET DESEQUILIBRAGE MAX'
             WRITE(IFM,1005)RMONIT(7),RMONIT(10),RMONIT(11),RMONIT(8),
     &                   RMONIT(9),RMONIT(12)
             WRITE(IFM,1006)RMONIT(13),RMONIT(16),RMONIT(17),RMONIT(14),
     &                   RMONIT(15),RMONIT(18)
             WRITE(IFM,*)
             DO I=1,6
               RAUX=ABS(RMONIT(I+6)*RMONIT(I+12))/100
               VALR(1)=RDESQ
               IF (RAUX.GE.RDESQ) THEN
                 CALL U2MESR('I','FACTOR_63',1,VALR)
                 GOTO 998
               ENDIF
             ENDDO
  998        CONTINUE
             WRITE(IFM,*)'*********************************************'
     &                  //'*********************************'
 1001        FORMAT(' N ',I4,' :    ',I12,'    ',I12,'    ',I12)
 1002        FORMAT(' N ',I4,' :',1PD10.2,' ',1PD10.2,' ',1PD10.2,7X,
     &              1PD10.2,' ',1PD10.2,' ',1PD10.2)
 1003        FORMAT('TOTAL   : ',I15,' ',I15,' ',I15)
 1004        FORMAT('MOYENNE :',1PD10.2,' ',1PD10.2,' ',1PD10.2,7X,
     &              1PD10.2,' ',1PD10.2,' ',1PD10.2)
 1005        FORMAT('        :',1PD10.2,' ',1PD10.2,' ',1PD10.2,7X,
     &              1PD10.2,' ',1PD10.2,' ',1PD10.2)
 1006        FORMAT('        :',1PD10.2,' ',1PD10.2,' ',1PD10.2,7X,
     &              1PD10.2,' ',1PD10.2,' ',1PD10.2)
           ENDIF
C ON REINITIALISE LES VECTEURS DE MONITORING POUR L'EVENTUELLE
C RESOLUTION SUIVANTE (SURTOUT POUR MONIT(6) ET MONIT(7)
           DO I=1,NBPROC
             ZR(MONIT(3)+I-1)=0.D0
             ZR(MONIT(4)+I-1)=0.D0
             ZR(MONIT(5)+I-1)=0.D0
             ZR(MONIT(6)+I-1)=0.D0
             ZR(MONIT(7)+I-1)=0.D0
             ZR(MONIT(8)+I-1)=0.D0
           ENDDO
         ENDIF

C  3- DETR_MAT :
      ELSE IF (ACTION.EQ.'DETR_MAT') THEN
C     --------------------------------
         IF (NOMATS(KXMPS).NE.' ') THEN

           IF ((( RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
              CALL JEVEUO(NOSOLS(KXMPS)//'.SLVR','L',JSLVR)
              EPSMAX=ZR(JSLVR-1+2)
              LQUALI=(EPSMAX.GT.0.D0)

              IF (LQUALI) THEN
                IF (LDIST) THEN
                  DEALLOCATE(XMPSK%A_loc)
                  DEALLOCATE(XMPSK%IRN_loc)
                  DEALLOCATE(XMPSK%JCN_loc)
                ELSE
                  DEALLOCATE(XMPSK%A)
                  DEALLOCATE(XMPSK%IRN)
                  DEALLOCATE(XMPSK%JCN)
                ENDIF
              ENDIF
            ENDIF

            ETAMS(KXMPS)=' '
            NONUS(KXMPS)=' '
            NOMATS(KXMPS)=' '
            NOSOLS(KXMPS)=' '
            ROUCS(KXMPS)=' '

C           DESTROY THE INSTANCE (DEALLOCATE INTERNAL DATA STRUCTURES)
            XMPSK%JOB = -2
            CALL DMUMPS(XMPSK)
         ENDIF
      ENDIF

C     -- ON REACTIVE LA LEVEE D'EXCEPTION
      CALL MATFPE(1)
C      WRITE(IFM,*)'FIN AMUMPR'
C      CALL JXVERI('MESSAGE','FIN AMUMPR')
      CALL JEDEMA()

#endif
      END
