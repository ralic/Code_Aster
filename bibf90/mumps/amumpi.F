      SUBROUTINE  AMUMPI(OPTION,LQUALI,LDIST,KXMPS,TYPE)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF AMUMPI MUMPS  DATE 11/08/2009   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C BUT : ROUTINE DE PARAMETRAGE MUMPS POUR AMUMPR/C.
C
C IN  OPTION:   IN   : OPTION D'UTILISATION.
C IN  LQUALI:  LOG   : LOGICAL EN CAS DE CRITERE DE QUALITE
C IN  LDIST :  LOG   : LOGICAL MUMPS DISTRIBUE OR NOT
C IN  KXMPS :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
C IN  TYPE  :   K1   : TYPE DU POINTEUR R OU C
C---------------------------------------------------------------
      INTEGER      KXMPS,OPTION
      LOGICAL      LQUALI,LDIST
      CHARACTER*1  TYPE

#ifdef _HAVE_MUMPS

C============================================================
      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
C============================================================
      INTEGER    NICNTL,NCNTL
      PARAMETER (NICNTL=26,NCNTL=5)
      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (DMUMPS_STRUC) , TARGET  :: DMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (DMUMPS_STRUC) , POINTER :: XMPSK
      TYPE (ZMUMPS_STRUC) , POINTER :: YMPSK
      INTEGER      IFM,NIV,ICNTL(NICNTL),I,JREFA,ISYMM,JSLVK,ISYMV,ISYM,
     &             JSLVI,IVERS,N1,N3,N5,IISNAN
      INTEGER*4    KXMPS4,I4
      REAL*8       RVERS,CNTL(NCNTL)
      CHARACTER*1  ROUCS(NMXINS)
      CHARACTER*4  ETAMS(NMXINS),TYPM,ETAM
      CHARACTER*14 NONUS(NMXINS),NONU
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS),NOMAT,NOSOLV
      CHARACTER*80 NVERS
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
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C=================================================================
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C       ------------------------------------------------
C        INITS
C       ------------------------------------------------
      KXMPS4=KXMPS
      IF (TYPE.EQ.'R') THEN
        XMPSK=>DMPS(KXMPS4)
      ELSE IF (TYPE.EQ.'C') THEN
        YMPSK=>ZMPS(KXMPS4)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      NOMAT=NOMATS(KXMPS)
      NOSOLV=NOSOLS(KXMPS)
      NONU=NONUS(KXMPS)
      ETAM=ETAMS(KXMPS)
      CALL JEVEUO(NOMAT//'.REFA','L',JREFA)
      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
      CALL JEVEUO(NOSOLV//'.SLVI','L',JSLVI)

C       ------------------------------------------------
C        INITIALISATION SYM, PAR ET JOB POUR MUMPS
C       ------------------------------------------------
      IF (OPTION.EQ.0) THEN

        IF (TYPE.EQ.'R') THEN
          XMPSK%COMM = MPI_COMM_WORLD
        ELSE IF (TYPE.EQ.'C') THEN
          YMPSK%COMM = MPI_COMM_WORLD
        ENDIF

C ---   ISYM = 0 => NON-SYMETRIQUE
C ---   ISYM = 1 => SYMETRIQUE DEFINIE POSITIVE
C ---   ISYM = 2 => SYMETRIQUE  GENERAL
C ---   ISYMM DEDUIT DE LA MATRICE : NONSYM OU SYMGEN
        TYPM=ZK24(JREFA-1+9)
        IF (TYPM.EQ.'MR') THEN
          ISYMM=0
        ELSE IF (TYPM.EQ.'MS') THEN
          ISYMM=2
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

C ---  PRISE EN COMPTE DE LA VOLONTE DE L'UTILISATEUR
C ---  => ISYMV
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
        ENDIF

C --- STRATEGIE PRUDENTE ET CONSERVATIVE
C --- SI AUTO: NONSYM OU SYMGEN SUIVANT LA STRUCTURE DE LA MATRICE
C --- SINON, ON APPLIQUE LE CHOIX DE L'UTILISATEUR
        IF (ISYMV.EQ.-1) THEN
          ISYM=ISYMM
        ELSE IF (ISYMV.EQ.0) THEN
          ISYM=ISYMV
        ELSE
          IF (ISYMM.EQ.0) THEN
            CALL U2MESK('F','FACTOR_56',1,ZK24(JSLVK-1+3))
          ELSE
            ISYM=ISYMV
          ENDIF
        ENDIF

C ---   PARAMETRES D'INITIALISATION DE L'OCCURENCE MUMPS KXMPS
        I4=ISYM
        IF (TYPE.EQ.'R') THEN
          XMPSK%SYM = I4 
          XMPSK%PAR = 1
          XMPSK%JOB = -1
        ELSE
          YMPSK%SYM = I4
          YMPSK%PAR = 1
          YMPSK%JOB = -1
        ENDIF

C       ------------------------------------------------
C        INITIALISATION ICNTL POUR MUMPS
C       ------------------------------------------------
      ELSE IF (OPTION.EQ.2) THEN

C ---      INIT
        DO I=1,NICNTL
          ICNTL(I)=0
        ENDDO
        DO I=1,NCNTL
          CNTL(I)=0.D0
        ENDDO
        NVERS(1:80)=''
        IF (TYPE.EQ.'R') THEN
          NVERS(1:5)=XMPSK%VERSION_NUMBER
        ELSE IF (TYPE.EQ.'C') THEN
          NVERS(1:5)=YMPSK%VERSION_NUMBER
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
C       DECRYPTAGE (TRES FRUSTE) DU NUMERO DE VERSION DE MUMPS
        IVERS=0
        IF (NVERS.NE.' ') THEN
          READ(NVERS(1:1),*)N1
          READ(NVERS(3:3),*)N3
          READ(NVERS(5:5),*)N5
          IVERS=N1*100+N3*10+N5
C          WRITE(6,*)'TEST NUMERO VERSION MUMPS ',NVERS(1:5),IVERS
C         TESTS DE NAN
          RVERS=1.D0*IVERS
          CALL ASSERT(IISNAN(RVERS).EQ.0)
          CALL ASSERT((IVERS.GE.100).AND.(IVERS.LT.999))
          IF (IVERS.LT.473) CALL U2MESK('F','FACTOR_72',1,NVERS(1:5))
        ENDIF
        
C ---      MESSAGES/ALERTES MUMPS
        ICNTL(1) = IFM
        ICNTL(2) = 0
        ICNTL(3) = 0
        ICNTL(4) = 1
        IF (NIV.GE.2) THEN
C ---      ICNTL(4) = 1/ERROR MESSAGES ONLY 2/ERRORS, WARNINGS, 3 PUIS 4
           ICNTL(3) = IFM
           ICNTL(4) = 2
        ENDIF
C ---      FORMAT MATRICE
        ICNTL(5) = 0
C ---      PRETRAITEMENTS (SCALING/PERMUTATION)
        IF (ZK24(JSLVK-1+2).EQ.'SANS') THEN
          ICNTL(6) = 0
          ICNTL(8) = 0
          ICNTL(12) = 1
        ELSE IF (ZK24(JSLVK-1+2).EQ.'AUTO') THEN
          ICNTL(6) = 7
C--       A PARTIR DE LA V4.8.0 (77 AU LIEU DE 7)
          IF (IVERS.GE.480) THEN
            ICNTL(8) = 77
          ELSE
            ICNTL(8) = 7
          ENDIF
          ICNTL(12) = 0
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

C ---      RENUMEROTATION
        IF (ZK24(JSLVK-1+4).EQ.'AMD') THEN
          ICNTL(7) = 0
        ELSE IF (ZK24(JSLVK-1+4).EQ.'AMF') THEN
          ICNTL(7) = 2
        ELSE IF (ZK24(JSLVK-1+4).EQ.'SCOTCH') THEN
          ICNTL(7) = 3
        ELSE IF (ZK24(JSLVK-1+4).EQ.'PORD') THEN
          ICNTL(7) = 4
        ELSE IF (ZK24(JSLVK-1+4).EQ.'METIS') THEN
          ICNTL(7) = 5
        ELSE IF (ZK24(JSLVK-1+4).EQ.'QAMD') THEN
          ICNTL(7) = 6
        ELSE IF (ZK24(JSLVK-1+4).EQ.'AUTO') THEN
          ICNTL(7) = 7
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

C ---  INITIALISATION EN DUR (EN DOUBLONS VS CALL DMUMPS JOB=-1)
C ---  MAIS ON NE SAIT JAMAIS AVEC LES EVOLUTIONS DES INITS DU PACKAGE
C ---  ET CELA PERMET DE SURCHARGER PLUS RAPIDEMENT POUR TESTER

C ---      TYPE DE RESOLUTION: A OU AT
        ICNTL(9) = 1

C ---      RAFFINEMENT ITERATIF
        ICNTL(10) = 0
        CNTL(2)=10.D+99
        IF (LQUALI)  THEN
          IF (ZK24(JSLVK-1+11).EQ.'SANS') THEN
          ELSE IF (ZK24(JSLVK-1+11).EQ.'AUTO') THEN
            ICNTL(10)=4
            CNTL(2)=1.D-14
          ELSE IF (ZK24(JSLVK-1+11).EQ.'FORCE') THEN
            ICNTL(10)=10
            CNTL(2)=10.D-50
          ENDIF
        ENDIF
        ICNTL(11) = 0
        IF (LQUALI)  ICNTL(11) = 1

C ---      CONTROLE DU PARALLELISME INDUIT PAR SCALAPACK
        ICNTL(13) = 0

C ---      MEMOIRE SUPPL. POUR PIVOTAGE (DEFAUT:10)
        ICNTL(14) = ZI(JSLVI-1+2)

C ---      RECHERCHE DE NOYAU
        ICNTL(15)=0
        ICNTL(16)=0
        ICNTL(17)=0
        ICNTL(24)=0
        ICNTL(25)=0

C ---       PARALLELISME/DISTRIBUTION SECOND MEMBRE/SOLUTION
        IF (LDIST) THEN
          ICNTL(18)=3
        ELSE
          ICNTL(18)=0
        ENDIF
        ICNTL(20)=0
        ICNTL(21)=0

C ---      COMPLEMENT DE SCHUR
        ICNTL(19)=0
        ICNTL(26)=0

C ---   GESTION IN_CORE/OUT_OF_CORE
        
        IF (ZK24(JSLVK-1+9).EQ.'OUI') THEN
C ---     CAS OUT_OF_CORE (FACTO/RESOLUTION)
          ICNTL(22)=1
          
C ---    INCTL(23)=0 INDIQUE QUE MUMPS SE BASE SUR SES ESTIMATEURS
C ---    POUR ALLOUER L'ESPACE MEMOIRE REQUIS
C ---    UN UTILISATEUR AVANCE PEUT REMPLACER ICNTL(23)=0 PAR
C ---    ICNTL(23)=X, OU X EST LA TAILLE MAX EN MO
C ---    QUE MUMPS PEUT ALLOUER

          ICNTL(23)=0
C---    LE FICHIER PERMETTANT A MUMPS D EFFECTUER SON TRAITEMENT OOC
C---    EST CREE DANS LE REPETOIRE COURANT VIA LE PARAMETRE OOC_TMPDIR
C---    ON N'AJOUTE PAS DE PREFIXE AU FICHIER CREE PAR MUMPS

          IF (TYPE.EQ.'R') THEN
            XMPSK%OOC_TMPDIR='.'
          ELSE IF (TYPE.EQ.'C') THEN
            YMPSK%OOC_TMPDIR='.'
          ENDIF
        ELSE IF (ZK24(JSLVK-1+9)(1:3).EQ.'NON') THEN
C ---     CAS IN_CORE (FACTO/RESOLUTION)
          ICNTL(22)=0
          ICNTL(23)=0
        ELSE
C ---     CHOIX NON LICITE   
          CALL ASSERT(.FALSE.)
        ENDIF

C --- REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
C --- DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
        IF (TYPE.EQ.'R') THEN
          DO I=1,NICNTL
            I4=ICNTL(I)
            XMPSK%ICNTL(I)=I4
          ENDDO
          XMPSK%CNTL(2)=CNTL(2)
        ELSE IF (TYPE.EQ.'C') THEN
          DO I=1,NICNTL
            I4=ICNTL(I)
            YMPSK%ICNTL(I)=I4
          ENDDO
          YMPSK%CNTL(2)=CNTL(2)
        ENDIF

C       ------------------------------------------------
C        MAUVAISE OPTION
C       ------------------------------------------------
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      CALL JEDEMA()
#endif
      END
