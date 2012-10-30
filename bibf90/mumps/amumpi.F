      SUBROUTINE  AMUMPI(OPTION,LQUALI,LDIST,KXMPS,TYPE)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF AMUMPI MUMPS  DATE 29/10/2012   AUTEUR BOITEAU O.BOITEAU 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG 
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
C BUT : ROUTINE DE PARAMETRAGE MUMPS POUR AMUMPS/C/D/Z
C
C IN  OPTION:   IN   : OPTION D'UTILISATION.
C IN  LQUALI:  LOG   : LOGICAL EN CAS DE CRITERE DE QUALITE
C IN  LDIST :  LOG   : LOGICAL MUMPS DISTRIBUE OR NOT
C IN  KXMPS :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
C IN  TYPE  :   K1   : TYPE DU POINTEUR R OU C
C---------------------------------------------------------------
! RESPONSABLE BOITEAU O.BOITEAU

      INTEGER      KXMPS,OPTION
      LOGICAL      LQUALI,LDIST
      CHARACTER*1  TYPE

#ifdef _HAVE_MUMPS

C============================================================
      INCLUDE 'mpif.h'
      INCLUDE 'smumps_struc.h'
      INCLUDE 'cmumps_struc.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
      INCLUDE 'jeveux.h'
C============================================================
      INTEGER    NICNTL,NCNTL
      PARAMETER (NICNTL=40,NCNTL=15)
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
      INTEGER      IFM,NIV,ICNTL(NICNTL),I,JREFA,ISYMM,JSLVK,ISYMV,ISYM,
     &             JSLVI,N1,N3,N5,NPREC,IBID
      INTEGER*4    I4
      REAL*8       RVERS,CNTL(NCNTL),RR4MAX,R4MAEM,TMAX
      LOGICAL      LBID
      CHARACTER*1  ROUCS(NMXINS),PRECS(NMXINS),KBUFF
      CHARACTER*4  ETAMS(NMXINS),TYPM,ETAM
      CHARACTER*12 K12BID
      CHARACTER*14 NONUS(NMXINS),NONU
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS),NOMAT,NOSOLV
      CHARACTER*24 KVERS
      COMMON /MUMPSS/ SMPS
      COMMON /MUMPSC/ CMPS
      COMMON /MUMPSD/ DMPS
      COMMON /MUMPSZ/ ZMPS
      COMMON /MUMPSH/ NONUS,NOMATS,NOSOLS,ETAMS,ROUCS,PRECS
C=================================================================
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C       ------------------------------------------------
C        INITS
C       ------------------------------------------------
      RR4MAX=R4MAEM()
      
      IF (TYPE.EQ.'S') THEN
        SMPSK=>SMPS(KXMPS)
      ELSE IF (TYPE.EQ.'C') THEN
        CMPSK=>CMPS(KXMPS)
      ELSE IF (TYPE.EQ.'D') THEN
        DMPSK=>DMPS(KXMPS)
      ELSE IF (TYPE.EQ.'Z') THEN
        ZMPSK=>ZMPS(KXMPS)
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
      NPREC=ZI(JSLVI)

C       -----------------------------------------------------
C        INITIALISATION SYM, PAR ET JOB POUR MUMPS (CREATION)
C       -----------------------------------------------------
      IF (OPTION.EQ.0) THEN

        IF (TYPE.EQ.'S') THEN
          SMPSK%COMM = MPI_COMM_WORLD
        ELSE IF (TYPE.EQ.'C') THEN
          CMPSK%COMM = MPI_COMM_WORLD
        ELSE IF (TYPE.EQ.'D') THEN
          DMPSK%COMM = MPI_COMM_WORLD
        ELSE IF (TYPE.EQ.'Z') THEN
          ZMPSK%COMM = MPI_COMM_WORLD
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

C ---     ISYM = 0 => NON-SYMETRIQUE
C ---     ISYM = 1 => SYMETRIQUE DEFINIE POSITIVE
C ---     ISYM = 2 => SYMETRIQUE  GENERAL
C ---     ISYMM DEDUIT DE LA MATRICE : NONSYM OU SYMGEN
        TYPM=ZK24(JREFA-1+9)
        IF (TYPM.EQ.'MR') THEN
          ISYMM=0
        ELSE IF (TYPM.EQ.'MS') THEN
          ISYMM=2
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

C ---     PRISE EN COMPTE DE LA VOLONTE DE L'UTILISATEUR
C ---     => ISYMV
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

C ---     STRATEGIE PRUDENTE ET CONSERVATIVE
C ---     SI AUTO: NONSYM OU SYMGEN SUIVANT LA STRUCTURE DE LA MATRICE
C ---     SINON, ON APPLIQUE LE CHOIX DE L'UTILISATEUR
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

C ---     PARAMETRES D'INITIALISATION DE L'OCCURENCE MUMPS KXMPS
        I4=ISYM
        IF (TYPE.EQ.'S') THEN
          SMPSK%SYM = I4 
          SMPSK%PAR = 1
          SMPSK%JOB = -1
        ELSE IF (TYPE.EQ.'C') THEN
          CMPSK%SYM = I4 
          CMPSK%PAR = 1
          CMPSK%JOB = -1
        ELSE IF (TYPE.EQ.'D') THEN
          DMPSK%SYM = I4 
          DMPSK%PAR = 1
          DMPSK%JOB = -1
        ELSE IF (TYPE.EQ.'Z') THEN
          ZMPSK%SYM = I4
          ZMPSK%PAR = 1
          ZMPSK%JOB = -1
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

C       ------------------------------------------------------
C        INITIALISATION ICNTL/CNTL POUR MUMPS (ANALYSE +FACTO)
C       ------------------------------------------------------
      ELSE IF (OPTION.EQ.2) THEN

C ---     INIT
        DO I=1,NICNTL
          ICNTL(I)=0
        ENDDO
        DO I=1,NCNTL
          CNTL(I)=0.D0
        ENDDO
        
C ---     TEST DE COMPATIBILITE DE LA VERSION DE MUMPS
        CALL AMUMPU(3,TYPE,KXMPS,K12BID,IBID,LBID,KVERS)
        
C ---     MESSAGES/ALERTES MUMPS
        ICNTL(1) = IFM
        ICNTL(2) = 0
        ICNTL(3) = 0
        ICNTL(4) = 1
        IF (NIV.GE.2) THEN
C ---     ICNTL(4) = 1/ERROR MESSAGES ONLY 2/ERRORS, WARNINGS, 3 PUIS 4
           ICNTL(3) = IFM
           ICNTL(4) = 2
        ENDIF
C ---     FORMAT MATRICE
        ICNTL(5) = 0
C ---     PRETRAITEMENTS (SCALING/PERMUTATION)
        IF (ZK24(JSLVK-1+2).EQ.'SANS') THEN
          ICNTL(6) = 0
          ICNTL(8) = 0
          ICNTL(12) = 1
        ELSE IF (ZK24(JSLVK-1+2).EQ.'AUTO') THEN
          ICNTL(6) = 7
          ICNTL(8) = 77
          ICNTL(12) = 0
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

C ---     RENUMEROTATION
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

C ---     INITIALISATION EN DUR (EN DOUBLONS VS CALL DMUMPS JOB=-1)
C ---     MAIS ON NE SAIT JAMAIS AVEC LES EVOLUTIONS DES INITS DU PACKAGE
C ---     ET CELA PERMET DE SURCHARGER PLUS RAPIDEMENT POUR TESTER

C ---     TYPE DE RESOLUTION: A OU AT
        ICNTL(9) = 1

C ---     RAFFINEMENT ITERATIF ET ANALYSE QUALITE SOLUTION
C ---     PARAMETRES ACTIVES JUSTE AVANT SOLVE VIA AMUMPI OPTION=3
        ICNTL(10)=0
        CNTL(2)=0.D0
        ICNTL(11)=0

C ---     PARALLELISME INDUIT PAR SCALAPACK (VOIR NPREC PLUS BAS)
        ICNTL(13) = 0

C ---     MEMOIRE SUPPL. POUR PIVOTAGE (DEFAUT:20)
        ICNTL(14) = ZI(JSLVI-1+2)

C ---     PAS UTILISES
        ICNTL(15)=0
        ICNTL(16)=0
        ICNTL(17)=0

C --      DETECTION DE SINGULARITE/NOYAU  
        ICNTL(25)=0
        IF (NPREC.GE.0) THEN
          ICNTL(13)=1
          ICNTL(24)=1
          CNTL(3)=-10.D0**(-NPREC)
          CNTL(5)=1.D+6
        ELSE
          ICNTL(24)=0
          CNTL(3)=0.D0
          CNTL(5)=0.D0
        ENDIF     
          
C ---     PIVOTAGE STATIQUE DESACTIVE
        CNTL(4)=-1.D0

C ---     PARALLELISME/DISTRIBUTION SECOND MEMBRE/SOLUTION
        IF (LDIST) THEN
          ICNTL(18)=3
        ELSE
          ICNTL(18)=0
        ENDIF
        ICNTL(20)=0
        ICNTL(21)=0

C ---     GESTION MEMOIRE MUMPS
C ---     PARAMETRES ACTIVES APRES L'ANALYSE VIA AMUMPU OPTION=1

        ICNTL(22)=-999
        ICNTL(23)=-999
        IF (TYPE.EQ.'S') THEN
          SMPSK%OOC_TMPDIR='XXXX'
        ELSE IF (TYPE.EQ.'C') THEN
          CMPSK%OOC_TMPDIR='XXXX'
        ELSE IF (TYPE.EQ.'D') THEN
          DMPSK%OOC_TMPDIR='XXXX'
        ELSE IF (TYPE.EQ.'Z') THEN
          ZMPSK%OOC_TMPDIR='XXXX'
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF        

C ---     COMPLEMENT DE SCHUR
        ICNTL(19)=0
        ICNTL(26)=0

C ---     PARAMETRE POUR RESOLUTIONS SIMULTANEES
        ICNTL(27)=-8
 
C ---     ANALYSE SEQUENTIELLE
        ICNTL(28)=0
        ICNTL(29)=0
        
C ---     PAS DE CALCUL DE TERMES DE A-1
        ICNTL(30)=0
        
C ---     ON GARDER LA FACTO EN MEMOIRE POUR LE SOLVE
        ICNTL(31)=0

C ---     NON UTILISE
        ICNTL(32)=0
                
C ---     PAS DE CALCUL DU DETERMINANT
        ICNTL(33)=0

C ---     NON UTILISES
        DO I=34,NICNTL
          ICNTL(I)=0
        ENDDO
        DO I=6,NCNTL
          CNTL(I)=0.D0
        ENDDO     

C ---   REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
C ---   DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
        IF (TYPE.EQ.'S') THEN
          DO I=1,NICNTL
            SMPSK%ICNTL(I)=ICNTL(I)
          ENDDO
          DO I=2,NCNTL
            IF (ABS(CNTL(I)).GT.RR4MAX) CALL ASSERT(.FALSE.)
            SMPSK%CNTL(I)=CNTL(I)
          ENDDO
        ELSE IF (TYPE.EQ.'C') THEN
          DO I=1,NICNTL
            CMPSK%ICNTL(I)=ICNTL(I)
          ENDDO
          DO I=2,NCNTL
            IF (ABS(CNTL(I)).GT.RR4MAX) CALL ASSERT(.FALSE.)
            CMPSK%CNTL(I)=CNTL(I)
          ENDDO
        ELSE IF (TYPE.EQ.'D') THEN
          DO I=1,NICNTL
            DMPSK%ICNTL(I)=ICNTL(I)
          ENDDO
          DO I=2,NCNTL
            DMPSK%CNTL(I)=CNTL(I)
          ENDDO
        ELSE IF (TYPE.EQ.'Z') THEN
          DO I=1,NICNTL
            ZMPSK%ICNTL(I)=ICNTL(I)
          ENDDO
          DO I=2,NCNTL
            ZMPSK%CNTL(I)=CNTL(I)
          ENDDO
        ELSE
            CALL ASSERT(.FALSE.)
        ENDIF

C       ------------------------------------------------------
C        INITIALISATION ICNTL/CNTL POUR MUMPS (SOLVE)
C       ------------------------------------------------------
      ELSE IF (OPTION.EQ.3) THEN
C ---   POUR CMD ECLATEE RESOUDRE PRINCIPALEMENT

C ---     TEST DE COMPATIBILITE DE LA VERSION DE MUMPS
        CALL AMUMPU(3,TYPE,KXMPS,K12BID,IBID,LBID,KVERS)

C ---     MESSAGE/ALERTES MUMPS
        ICNTL(1) = IFM
        ICNTL(2) = 0
        ICNTL(3) = 0
        ICNTL(4) = 1
        IF (NIV.GE.2) THEN
C ---     ICNTL(4) = 1/ERROR MESSAGES ONLY 2/ERRORS, WARNINGS, 3 PUIS 4
           ICNTL(3) = IFM
           ICNTL(4) = 2
        ENDIF

C ---     RAFFINEMENT ITERATIF ET ETUDE DE LA QUALITE
        ICNTL(10)=0
        ICNTL(11)=0
        CNTL(2)=0.D0
        IF (LQUALI)  THEN
          IF (ZK24(JSLVK-1+11).EQ.'SANS') THEN
          ELSE IF (ZK24(JSLVK-1+11).EQ.'AUTO') THEN
            ICNTL(10)=4
            CNTL(2)=1.D-14
          ELSE IF (ZK24(JSLVK-1+11).EQ.'FORCE') THEN
            ICNTL(10)=10
            CNTL(2)=10.D-50
          ENDIF
          ICNTL(11)=1
        ENDIF
        IF (TYPE.EQ.'S') THEN
          SMPSK%ICNTL(1)=ICNTL(1)
          SMPSK%ICNTL(2)=ICNTL(2)
          SMPSK%ICNTL(3)=ICNTL(3)
          SMPSK%ICNTL(4)=ICNTL(4)
          SMPSK%ICNTL(10)=ICNTL(10)
          SMPSK%ICNTL(11)=ICNTL(11)
          SMPSK%CNTL(2)  =CNTL(2)
        ELSE IF (TYPE.EQ.'C') THEN
          CMPSK%ICNTL(1)=ICNTL(1)
          CMPSK%ICNTL(2)=ICNTL(2)
          CMPSK%ICNTL(3)=ICNTL(3)
          CMPSK%ICNTL(4)=ICNTL(4)
          CMPSK%ICNTL(10)=ICNTL(10)
          CMPSK%ICNTL(11)=ICNTL(11)
          CMPSK%CNTL(2)  =CNTL(2)
        ELSE IF (TYPE.EQ.'D') THEN
          DMPSK%ICNTL(1)=ICNTL(1)
          DMPSK%ICNTL(2)=ICNTL(2)
          DMPSK%ICNTL(3)=ICNTL(3)
          DMPSK%ICNTL(4)=ICNTL(4)
          DMPSK%ICNTL(10)=ICNTL(10)
          DMPSK%ICNTL(11)=ICNTL(11)
          DMPSK%CNTL(2)  =CNTL(2)
        ELSE IF (TYPE.EQ.'Z') THEN
          ZMPSK%ICNTL(1)=ICNTL(1)
          ZMPSK%ICNTL(2)=ICNTL(2)
          ZMPSK%ICNTL(3)=ICNTL(3)
          ZMPSK%ICNTL(4)=ICNTL(4)
          ZMPSK%ICNTL(10)=ICNTL(10)
          ZMPSK%ICNTL(11)=ICNTL(11)
          ZMPSK%CNTL(2)  =CNTL(2)
        ELSE
          CALL ASSERT(.FALSE.)
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
