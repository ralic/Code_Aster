      SUBROUTINE APETSR(ACTION,SOLVEU,NOMAT,VECAS,SOLU,VCINE)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APETSR PETSC  DATE 19/05/2008   AUTEUR COURTOIS M.COURTOIS 
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
C
C IN : ACTION :
C     /'PRERES'  : POUR DEMANDER LA FACTORISATION
C     /'RESOUD'  : POUR DEMANDER LA DESCENTE/REMONTEE
C     /'DETR_MAT': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE MUMPS
C                  ASSOCIEE A UNE MATRICE
C
C IN : NOMAT (K19) : NOM DE LA MATRICE
C IN : VECAS (K19) : NOM DU SECOND MEMBRE
C            (SI ACTION=RESOUD)
C IN : SOLU  (K19) : NOM DE LA SOLUTION
C            (SI ACTION=RESOUD)
C IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
C            (SI ACTION=RESOUD)
C
C
C
C---------------------------------------------------------------
      CHARACTER*(*) ACTION
      CHARACTER*19  VECAS,SOLU,VCINE,SOLVEU,NOMAT

#ifdef _HAVE_PETSC

C================================================================
#include "include/finclude/petsc.h"
#include "include/finclude/petscvec.h"
#include "include/finclude/petscmat.h"
#include "include/finclude/petscksp.h"
#include "include/finclude/petscpc.h"
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
      INTEGER*4 ZI4
      COMMON /I4VAJE/ZI4(1)
C================================================================
      INTEGER N,NZ,NZ2,ISYMM
      INTEGER JVECAS,JSOLU,IERD
      INTEGER*4 I,K,JCOL,ILIG,KTERM,NEQ,ITERM,NMAXIT,NBITER,INDIC,JTERM
      INTEGER NLONG, JSMDI, NSMDI, JSMHC,NSMHC,JVALM,IBID
      INTEGER IDVALC,LMAT,IRET,NVALM,JVALM2,IFM,NIV
      INTEGER JDXI, JDVAL1, JDVAL2,LTERM,JREFA
      INTEGER JSLVR,JSLVK,JSLVI
      CHARACTER*1 ROUC
      CHARACTER*4 KBID,TYPM
      CHARACTER*8 VARIAN,PRECON
      CHARACTER*16 IDXI, TRANS1, TRANS2
      CHARACTER*14 NU,NONU
      REAL*8 EPSMAX, VALM,VALE,rtol,atol,dtol,vnorm,vnormb
      COMPLEX*16 CBID
      LOGICAL LMNSY
      PARAMETER (IDXI  ='&&APETSR.IDXI___')
      PARAMETER (TRANS1='&&APETSR.TRANS1_')
      PARAMETER (TRANS2='&&APETSR.TRANS2_')
      INTEGER*4 ierr, rang, size, low, high, dlow
      INTEGER*4 NBO,NBD,NZDEB,NZFIN
      CHARACTER*16 IDXO, IDXD, IDXI2
      PARAMETER (IDXO  ='&&APETSR.IDXO___')
      PARAMETER (IDXD  ='&&APETSR.IDXD___')
      PARAMETER (IDXI2 ='&&APETSR.IDXI2__')
      INTEGER JIDXO,JIDXD, NBLLOC, JDXI2
C=================================================================
!  Variables:
!
!  A       - matrix that defines linear system
!  ksp    - KSP context
!  x, b - approx solution, RHS vectors
!
      Vec     x,b,resi,xgth
      Mat     A
      PC     pc
      KSP    ksp
C      PetscScalar  v
C      PetscErrorCode ierr
      PetscScalar  XX(1)
      PetscOffset xidx
C      IS       is1,is2
C      PetscViewer  viewer
      VecScatter   ctx
C=================================================================
      CALL JEMARQ()

C     -- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LA BIBLIOTHEQUE MKL
C     -- CAR CES EXCEPTIONS NE SONT PAS JUSTIFIEES
      CALL MATFPE(-1)

      CALL INFNIV(IFM,NIV)


      CALL JEVEUO(SOLVEU//'.SLVR','L',JSLVR)
      CALL JEVEUO(SOLVEU//'.SLVK','L',JSLVK)
      CALL JEVEUO(SOLVEU//'.SLVI','L',JSLVI)
      VARIAN=ZK24(JSLVK-1+3)
      PRECON=ZK24(JSLVK-1+6)

C      varian='BCGS'  ! ajacot suppr
C      precon='ILU'  ! ajacot suppr
C      write(6,*) 'ajacot varian,precon=',varian,precon



      IF (ACTION.EQ.'RESOUD') THEN
C     --------------------------------

C        ATTENTION : DANS PETSCINITIALIZE, ON FAIT UN
C        MPI_INIT ; IL FAUDRAIT FAIRE GAFFE A NE PAS
C        INTERFERER AVEC MUMPS!
C         CALL MPI_INIT(ierr)
C         write(6,*) 'toto ierr=',ierr
C         CALL ASSERT(ierr.EQ.0)
C         write(6,*) 'toto avant ierr=',ierr
C         call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
C         write(6,*) 'toto apres ierr=',ierr
C         CALL ASSERT(ierr.EQ.0)

C        ----- RECUPERE LE RANG DU PROCESSUS
	 CALL MPI_COMM_RANK(PETSC_COMM_WORLD, rang, ierr)
	 CALL MPI_COMM_SIZE(PETSC_COMM_WORLD, size, ierr)

C        1.2 CALCUL DE ISYM :
C        -------------------------------------------------------
C        ISYM = 0 => NON-SYMETRIQUE
C        ISYM = 1 => SYMETRIQUE DEFINIE POSITIVE
C        ISYM = 2 => SYMETRIQUE  GENERAL

C        ISYMM DEDUIT DE LA MATRICE :
C         CALL JEVEUO(NOMAT//'.REFA','L',JREFA)
C         TYPM=ZK24(JREFA-1+9)
C         IF (TYPM.EQ.'MR') THEN
C            ISYMM=0
C         ELSE IF (TYPM.EQ.'MS') THEN
C            ISYMM=2
C         ELSE
C            CALL ASSERT(.FALSE.)
C         END IF



C        1.1 LECTURE DES DIMENSIONS ET DU STOCKAGE DE LA MATRICE:
C        -------------------------------------------------------
         CALL DISMOI('F','NOM_NUME_DDL',NOMAT,'MATR_ASSE',IBID,
     &               NONU,IERD)
         CALL JEVEUO(NONU//'.SMOS.SMDI','L',JSMDI)
         CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
         CALL JEVEUO(NONU//'.SMOS.SMHC','L',JSMHC)
         CALL JELIRA(NONU//'.SMOS.SMHC','LONMAX',NSMHC,KBID)
         N=NSMDI
         NZ=ZI(JSMDI-1+N)
         CALL ASSERT(NZ.LE.NSMHC)

         NZ2=NZ
C        Dans PETSC, on stocke les matrices comme si elles
C        étaient non-symétriques
         NZ2=2*NZ-N

C        1.2 INITIALISATION DES OBJETS JEVEUX DE TRANSFERT:
C        -------------------------------------------------------
         CALL WKVECT(IDXI,'V V S',N,JDXI)
         CALL WKVECT(TRANS1,'V V R',N,JDVAL1)
         CALL WKVECT(TRANS2,'V V R',N,JDVAL2)
         
         CALL WKVECT(IDXI2,'V V S',N,JDXI2)

C        1.3 ALLOCATION/RECUPERATION DES OBJETS PETSC ET ASTER :
C        -------------------------------------------------------

C        on a NZ2 termes non nuls, on estime à 5*NZ2/N le
C        nombre de termes non nuls par ligne
C          tempo = INT(5*NZ2/N)
C          tempo2 = INT(tempo/size)
C          write(6,*) 'toto      tempo=',tempo
C          write(6,*) 'toto     tempo2=',tempo2
         NEQ = N
C         write(6,*) 'toto        NEQ=',NEQ

         call MatCreate(PETSC_COMM_WORLD,A,ierr)
         call MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,NEQ,NEQ,ierr)
         call MatSetType(A,MATMPIAIJ,ierr)
C        call MatSetType(A,MATSEQAIJ,ierr)

C          call VecCreate(PETSC_COMM_WORLD,b,ierr)
C          CALL ASSERT(ierr.EQ.0)
C          call VecSetSizes(b,PETSC_DECIDE,NEQ,ierr)
C          CALL ASSERT(ierr.EQ.0)
C          call VecSetFromOptions(b,ierr)
C          CALL ASSERT(ierr.EQ.0)

         CALL JELIRA(NOMAT//'.VALM','NMAXOC',NVALM,KBID)
         IF (NVALM.EQ.1) THEN
            LMNSY=.FALSE.
         ELSE IF (NVALM.EQ.2) THEN
            LMNSY=.TRUE.
         ELSE
            CALL ASSERT(.FALSE.)
         END IF

         CALL JEVEUO(JEXNUM(NOMAT//'.VALM',1),'L',JVALM)
         CALL JELIRA(JEXNUM(NOMAT//'.VALM',1),'LONMAX',
     &             NLONG,KBID)
         CALL ASSERT(NLONG.EQ.NZ)
         IF (LMNSY) THEN
            CALL JEVEUO(JEXNUM(NOMAT//'.VALM',2),'L',JVALM2)
            CALL JELIRA(JEXNUM(NOMAT//'.VALM',2),'LONMAX',
     &             NLONG,KBID)
            CALL ASSERT(NLONG.EQ.NZ)
         END IF


C        1.4 TRANSFERT DES OBJETS D'ASTER VERS PETSC :
C        -------------------------------------------------------
         
C              1.4.a PREALLOCATION : ON EVALUE LA MEMOIRE NECESSAIRE
C              -------------------------------------------------------
         
C low donne la premiere ligne stockee localement
C high donne la premiere ligne stockee par le processus (rang+1)
C ATTENTION ces indices commencent a zero (convention C de PETSc)
         call MatGetOwnershipRange(A,low,high,ierr)
C         call VecGetOwnershipRange(b,low,high,ierr) 
         
C nb de lignes que l'on stocke localement
         NBLLOC = high - low
         
C ces deux vecteurs sont les d_nnz et o_nnz a passer a PETSc
         CALL WKVECT(IDXO,'V V S',NBLLOC,JIDXO)
         CALL WKVECT(IDXD,'V V S',NBLLOC,JIDXD)

C initialisation des deux vecteurs         
         DO I = 1, NBLLOC
            ZI4(JIDXO-1+I) = 0
            ZI4(JIDXD-1+I) = 0
         END DO
         
         IF (low.EQ.0) THEN
            ZI4(JIDXO-1+1) = 0
            ZI4(JIDXD-1+1) = 1
         ELSE
            ZI4(JIDXO-1+1) = ZI(JSMDI+low) - ZI(JSMDI+low-1) - 1
            ZI4(JIDXD-1+1) = 1
         END IF

         NBO = 0
         NBD = 0

C on commence par s'occuper du nombres de NZ par ligne dans le bloc diagonal
         DO JCOL = low+1, high-1
            NZDEB = ZI(JSMDI+JCOL-1) + 1
            NZFIN = ZI(JSMDI+JCOL)
            DO K = NZDEB, NZFIN
               ILIG = ZI(JSMHC-1+K)
               IF (ILIG.LT.(low+1)) THEN
                  NBO = NBO + 1
               ELSE
                  NBD = NBD + 1
                  ZI4(JIDXD-1+(ILIG-low)) = ZI4(JIDXD-1+(ILIG-low)) + 1
               END IF
            END DO
            ZI4(JIDXD-1+(JCOL+1-low)) = ZI4(JIDXD-1+(JCOL+1-low)) + NBD 
     &                                                            - 1
            ZI4(JIDXO-1+(JCOL+1-low)) = ZI4(JIDXO-1+(JCOL+1-low)) + NBO
            NBD = 0
            NBO = 0
         END DO

C ensuite on complete le tableau du bloc hors diagonal         
         DO JCOL = high, NEQ-1
            NZDEB = ZI(JSMDI+JCOL-1) + 1
            NZFIN = ZI(JSMDI+JCOL)
            DO K = NZDEB, NZFIN
               ILIG = ZI(JSMHC-1+K)
               IF (ILIG.LT.(low+1)) THEN
                  CONTINUE
               ELSE IF (ILIG.LE.high) THEN
                  ZI4(JIDXO-1+(ILIG-low)) = ZI4(JIDXO-1+(ILIG-low)) + 1
               ELSE
                  GOTO 10
               END IF
            END DO
10          CONTINUE
         END DO

C          call MatMPIAIJSetPreallocation(A,PETSC_NULL_INTEGER,ZI4(JIDXD),
C      &                                    PETSC_NULL_INTEGER,ZI4(JIDXO),
C      &                                    ierr)
C          call MatSEQAIJSetPreallocation(A,PETSC_NULL_INTEGER,ZI4(JIDXD),
C      &                                    ierr)
         call MatCreateMPIAIJ(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE
     &                                    ,NEQ,NEQ,
     &                                    PETSC_NULL_INTEGER,ZI4(JIDXD),
     &                                    PETSC_NULL_INTEGER,ZI4(JIDXO),
     &                                    A,ierr)
C          call MatCreateSeqAIJ(PETSC_COMM_WORLD,NEQ,NEQ,
C      &                                    PETSC_NULL_INTEGER,ZI4(JIDXD),
C      &                                    A,ierr)
         CALL ASSERT(ierr.EQ.0)

C on n'oublie pas de detruire les tableaux apres avoir alloue correctement
         CALL JEDETR(IDXO)
         CALL JEDETR(IDXD)

C permet de dire a PETSc que les blocs fournis sont "tries" (plus rapide)
         call MatSetOption(A,MAT_ROWS_SORTED,ierr)
         call MatSetOption(A,MAT_COLUMNS_SORTED,ierr)
         
C              1.4.b ASSEMBLAGE : ON COPIE LES VALEURS DE LA MATRICE
C              -------------------------------------------------------

         LTERM=0
         ITERM=0
         JTERM=0

C cas ou on possede le premier bloc de lignes
         IF (low.EQ.0) THEN
            LTERM=LTERM+1
            call MatSetValue(A,0,0,ZR(JVALM),INSERT_VALUES,ierr)
            dlow=1
         ELSE
            dlow=0
         END IF

C on commence par s'occuper du nombres de NZ par ligne dans le bloc diagonal
         DO JCOL = low+dlow, high-1
            NZDEB = ZI(JSMDI+JCOL-1) + 1
            NZFIN = ZI(JSMDI+JCOL)
            DO K = NZDEB, NZFIN
               ILIG = ZI(JSMHC-1+K)
               JTERM=JTERM+1
               LTERM=LTERM+1
               IF (LMNSY) THEN
                  VALM=ZR(JVALM2-1+K)
                  ZR(JDVAL2+JTERM-1)=VALM
                  ZI4(JDXI2+JTERM-1)=ILIG-1
               ELSE
                  VALM=ZR(JVALM-1+K)
                  ZR(JDVAL2+JTERM-1)=VALM
                  ZI4(JDXI2+JTERM-1)=ILIG-1
               END IF
               IF (ILIG.GE.(low+1)) THEN
                  ITERM=ITERM+1
                  LTERM=LTERM+1
                  VALM=ZR(JVALM-1+K)
                  ZR(JDVAL1+ITERM-1)=VALM
                  ZI4(JDXI+ITERM-1)=ILIG-1
               END IF
            END DO
            JTERM=JTERM-1
            LTERM=LTERM-1
            call MatSetValues(A,ITERM,ZI4(JDXI),1,JCOL,
     &                    ZR(JDVAL1),INSERT_VALUES,ierr)
C            IF(JTERM.GE.1) THEN
                call MatSetValues(A,1,JCOL,JTERM,ZI4(JDXI2),
     &                      ZR(JDVAL2),INSERT_VALUES,ierr)
C            END IF
            ITERM=0
            JTERM=0
         END DO

C         write(6,*) 'toto      lterm=',LTERM

C ensuite on complete le tableau du bloc hors diagonal         
         DO JCOL = high, NEQ-1
            NZDEB = ZI(JSMDI+JCOL-1) + 1
            NZFIN = ZI(JSMDI+JCOL)
            DO K = NZDEB, NZFIN
               ILIG = ZI(JSMHC-1+K)
               IF (ILIG.LT.(low+1)) THEN
                  CONTINUE
               ELSE IF (ILIG.LE.high) THEN
                  ITERM=ITERM+1
                  LTERM=LTERM+1
                  VALM=ZR(JVALM-1+K)
                  ZR(JDVAL1+ITERM-1)=VALM
                  ZI4(JDXI+ITERM-1)=ILIG-1
               ELSE
                  GOTO 20
               END IF
            END DO
20          CONTINUE
            call MatSetValues(A,ITERM,ZI4(JDXI),1,JCOL,
     &                    ZR(JDVAL1),INSERT_VALUES,ierr)
            ITERM=0
         END DO

C        VERIFICATION DU TRANSFERT

C         CALL ASSERT(LTERM.EQ.NZ2)
C         write(6,*) 'toto      lterm=',LTERM
C         CALL ASSERT(ILIG.EQ.N)
C         CALL ASSERT(JCOL.EQ.N)
         
         CALL JELIBE(NONU//'.SMOS.SMDI')
         CALL JELIBE(NONU//'.SMOS.SMHC')
         CALL JELIBE(JEXNUM(NOMAT//'.VALM',1))
         IF (LMNSY) CALL JELIBE(JEXNUM(NOMAT//'.VALM',2))

         call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
C          call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
C          CALL ASSERT(ierr.EQ.0)

C        call MatView(A,PETSC_VIEWER_DRAW_WORLD,ierr)

CC        --- Impression de la matrice
C         call  PetscViewerBinaryOpen(PETSC_COMM_WORLD,
C     &                               '/home/pabhhhh/Matrix',
C     &                                PETSC_FILE_CREATE, viewer,ierr)
C         CALL ASSERT(ierr.EQ.0)
C
C         call MatView(A, viewer,ierr)
C         CALL ASSERT(ierr.EQ.0)
C         call PetscViewerDestroy(viewer,ierr)
C         CALL ASSERT(ierr.EQ.0)
C         call abort()
C         call PetscViewerPushFormat(PETSC_VIEWER_STDOUT_WORLD,
C     &     PETSC_VIEWER_ASCII_MATLAB, ierr)
C         CALL ASSERT(ierr.EQ.0)
C         call MatView(A, PETSC_VIEWER_STDOUT_WORLD, ierr)
C         CALL ASSERT(ierr.EQ.0)
C         call abort()

!!   --- Impression de la matrice
C         call PetscViewerASCIIOpen(PETSC_COMM_WORLD,
C     &                             '/home/desoza/matrix.output',
C     &                             viewer,ierr)
C
C         call MatView(A, viewer,ierr)
C         call PetscViewerDestroy(viewer,ierr)
C         call abort()

C        -- QUELQUES VERIFICATIONS :
C         CALL DISMOI('F','NOM_NUME_DDL',NOMAT,'MATR_ASSE',IBID,
C     &               NONU,IERD)
C         CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
C         N=NSMDI

         CALL JEVEUO(VECAS//'.VALE','L',JVECAS)
         CALL JELIRA(VECAS//'.VALE','LONMAX',NLONG,KBID)
         CALL ASSERT(N.EQ.NLONG)

C        -- PRISE EN COMPTE DU CONDITIONNEMENT DES LAGRANGES :
         CALL MTDSCR(NOMAT)
         CALL JEVEUO(NOMAT//'.&INT','L',LMAT)
         CALL MRCONL(LMAT,0,'R',ZR(JVECAS),1)

C        -- PRISE EN COMPTE DES CHARGES CINEMATIQUES :
         CALL JEEXIN(VCINE//'.VALE',IRET)
         IF ( VCINE.NE.' '.AND.IRET.NE.0)THEN
            CALL JEVEUO(VCINE//'.VALE','L',IDVALC)
            CALL JELIRA(VCINE//'.VALE','TYPE',IBID,ROUC)
            CALL ASSERT(ROUC.EQ.'R')
            CALL CSMBGG(LMAT,ZR(JVECAS),ZR(IDVALC),CBID,CBID,'R')
         END IF

         call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
         CALL ASSERT(ierr.EQ.0)

C         call MatView(A, PETSC_VIEWER_STDOUT_WORLD, ierr)

C        -- RECOPIE DE VECAS DANS RHS:
C          call VecCreate(PETSC_COMM_WORLD,b,ierr)
C          CALL ASSERT(ierr.EQ.0)
C          call VecSetSizes(b,PETSC_DECIDE,NEQ,ierr)
C          CALL ASSERT(ierr.EQ.0)
C C         call VecSetFromOptions(b,ierr)
C          call VecSetType(b,VECMPI,ierr)
         call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,NEQ,b,ierr)
         CALL ASSERT(ierr.EQ.0)
         call VecDuplicate(b,x,ierr)
         call VecDuplicate(b,resi,ierr)
         CALL ASSERT(ierr.EQ.0)
         
C          IF (rang.EQ.0) THEN
C          
C          DO K=1,NEQ
C             VALE=ZR(JVECAS-1+K)
C             call VecSetValues(b,1,K-1,VALE,INSERT_VALUES,ierr)
C C            CALL ASSERT(ierr.EQ.0)
C          END DO
C          
C          END IF

C          call VecGetOwnershipRange(b,low,high,ierr)
C          
C          DO K=low,high-1
C             VALE=ZR(JVECAS+K)
C             call VecSetValues(b,1,K,VALE,INSERT_VALUES,ierr)
C C            CALL ASSERT(ierr.EQ.0)
C          END DO         

         call VecGetArray(b,XX,xidx,ierr)

         DO K=1,NBLLOC
            XX(xidx+K)=ZR(JVECAS-1+low+K)
         END DO 

         call VecRestoreArray(b,XX,xidx,ierr)
                 
         CALL JELIBE(VECAS//'.VALE')

C          call VecAssemblyBegin(b,ierr)
C          call VecAssemblyEnd(b,ierr)

         

C       -- Create linear solver context  KSP :
C       ----------------------------------
        call KSPCreate(PETSC_COMM_WORLD,ksp,ierr)
        CALL ASSERT(ierr.EQ.0)
        call KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr)
        CALL ASSERT(ierr.EQ.0)


C       -- choix de la variante KSP :
C       -------------------------------
        if (varian.eq.'GMRES') THEN
           call KSPSetType(ksp,KSPGMRES,ierr)
           call assert(ierr.eq.0)
C          call KSPSetPreconditionerSide(ksp,PC_RIGHT,ierr)
        elseif (varian.eq.'PREONLY') THEN
           call KSPSetType(ksp,KSPPREONLY,ierr)
           call assert(ierr.eq.0)
        elseif (varian.eq.'BCGS') THEN
           call KSPSetType(ksp,KSPBCGS,ierr)
           call assert(ierr.eq.0)
        elseif (varian.eq.'CR') THEN
           call KSPSetType(ksp,KSPCR,ierr)
           call assert(ierr.eq.0)
        elseif (varian.eq.'CG') THEN
           call KSPSetType(ksp,KSPCG,ierr)
           call assert(ierr.eq.0)
        elseif (varian.eq.'BICG') THEN
           call KSPSetType(ksp,KSPBICG,ierr)
           call assert(ierr.eq.0)
        elseif (varian.eq.'CHEBYCHEV') THEN
           call KSPSetType(ksp,KSPCHEBYCHEV,ierr)
           call assert(ierr.eq.0)
        else
           call assert(.false.)
        endif
        CALL ASSERT(ierr.EQ.0)


C       -- choix du preconditionneur :
C       -------------------------------
        call KSPGetPC(ksp,pc,ierr)
        if (precon.eq.'ILU') THEN
           call PCSetType(pc,PCILU,ierr)
           call assert(ierr.eq.0)
C          call PCILUSetLevels(pc,300,ierr)
C          call PCILUSetMatOrdering(pc, MATORDERING_ND,ierr)
C          call PCILUSetUseDropTolerance(pc,1.e-10,0.1,1,ierr)
C          call PCILUReorderForNonzeroDiagonal(pc,1.D-11,ierr)
        elseif (precon.eq.'LU') THEN
           call PCSetType(pc,PCLU,ierr)
           call assert(ierr.eq.0)
        elseif (precon.eq.'EISENSTA') THEN
           call PCSetType(pc,PCEISENSTAT,ierr)
           call assert(ierr.eq.0)
        elseif (precon.eq.'SOR') THEN
           call PCSetType(pc,PCSOR,ierr)
           call assert(ierr.eq.0)
        elseif (precon.eq.'BJACOBI') THEN
           call PCSetType(pc,PCBJACOBI,ierr)
           call assert(ierr.eq.0)
        elseif (precon.eq.'JACOBI') THEN
           call PCSetType(pc,PCJACOBI,ierr)
           call assert(ierr.eq.0)
        else
           call assert(.false.)
        endif
        CALL ASSERT(ierr.EQ.0)


C        call KSPSetPreconditionerSide(ksp,PC_RIGHT)

C        CALL PCFactorSetZeroPivot(pc,1.D-50,ierr)
C        call KSPSetDiagonalScale(ksp, 1, ierr)
C        call PCFactorSetShiftNonzero(pc, PETSC_DECIDE,ierr)


C       -- paramètres numériques :
C       ----------------------------

C       -- nb iter max :
        if (ZI(JSLVI-1+2).EQ.-1) THEN
C          nmaxit=PETSC_DEFAULT_INTEGER
          nmaxit=1000
        ELSE
          nmaxit=ZI(JSLVI-1+2)
        ENDIF

C       -- rtol :
        if (ZR(JSLVR-1+2).EQ.-1.d0) THEN
          rtol=PETSC_DEFAULT_DOUBLE_PRECISION
        ELSE
          rtol=ZR(JSLVR-1+2)
        ENDIF


C       -- on force les défauts de PETSC ????? AJACOT ??
        rtol=PETSC_DEFAULT_DOUBLE_PRECISION
C        rtol=1.d-6
        atol=PETSC_DEFAULT_DOUBLE_PRECISION
        dtol=PETSC_DEFAULT_DOUBLE_PRECISION
        nmaxit=PETSC_DEFAULT_INTEGER


        call KSPSetTolerances(ksp,rtol,atol,dtol,nmaxit,ierr)

        call KSPSetFromOptions(ksp,ierr)

C      -- pour suivre le boulot de KSP ?
C      ----------------------------------
        if (niv.gt.2) then
            call KSPMonitorSet(ksp,KSPMonitorTrueResidualNorm,
     &               PETSC_NULL_OBJECT,PETSC_NULL_FUNCTION,ierr)
        endif



C      --  Resolution du systeme :
C      -----------------------------
        call KSPSolve(ksp,b,x,ierr)
        call KSPGetConvergedReason(ksp, indic, ierr)
        IF (indic.GT.0) THEN
           if (NIV.GT.1) WRITE(IFM,*) 'PETSC_CONVERGENCE OK'
        ELSE
           if (indic.EQ.-3) THEN
             WRITE(IFM,*) 'PETSC_CONVERGENCE ITEMAX'
C              CALL UTMESS('F',
C      &     'APETSR', 'ERREUR A LA RESOLUTION DU SYSTEME LINEAIRE')
             CALL ASSERT(.FALSE.)
          ELSE IF (indic.EQ.-4) THEN
             WRITE(IFM,*) 'PETSC_CONVERGENCE DIVERGENCE'
C              CALL UTMESS('F',
C      &     'APETSR', 'ERREUR A LA RESOLUTION DU SYSTEME LINEAIRE')
             CALL ASSERT(.FALSE.)
          ELSE IF (indic.EQ.-7) THEN
             WRITE(IFM,*) 'PETSC_CONVERGENCE NON_SYMETRIQUE'
C              CALL UTMESS('F',
C      &     'APETSR', 'ERREUR A LA RESOLUTION DU SYSTEME LINEAIRE')
             CALL ASSERT(.FALSE.)
          ELSE IF (indic.EQ.-8) THEN
             WRITE(IFM,*) 'PETSC_CONVERGENCE PC_INDEFINI'
C              CALL UTMESS('F',
C      &     'APETSR', 'ERREUR A LA RESOLUTION DU SYSTEME LINEAIRE')
             CALL ASSERT(.FALSE.)
          ELSE
             WRITE(IFM,*) 'PETSC_CONVERGENCE ERREUR'
C              CALL UTMESS('F',
C      &     'APETSR', 'ERREUR A LA RESOLUTION DU SYSTEME LINEAIRE')
             CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF

C        call KSPGetIterationNumber(ksp,nbiter,ierr)
C        WRITE(6,*) 'AJACOT PETSC CONVERGENCE EN : ',NBITER,' ITERATIONS'


        if (.false.) then
C          -- on vérifie que A*x=b :
           call VecNorm(b,NORM_2,vnormb,ierr)
           write(6,*) 'norme de b=',vnormb
           call assert(vnormb.gt.0.d0)

           call MatMult(A,x,resi,ierr)
           call VecAXPY(resi,-1.d0,b,ierr)
           call VecNorm(resi,NORM_2,vnorm,ierr)
           write(6,*) 'ajacot norme de Ax-b=',vnorm
           write(6,*) 'ajacot precision obtenue=',vnorm/vnormb
        endif





C        View solver info; we could instead use the option -ksp_view
C        call KSPView(ksp,PETSC_VIEWER_STDOUT_WORLD,ierr)
C        call MatView(A,PETSC_VIEWER_DRAW_WORLD,ierr)


C       --- ON DOIT RECONSTRUIRE LA SOLUTION SUR CHAQUE PROC
C       --- ON UTILISE DANS PETSC UN VECSCATTER

        call VecCreateSeq(PETSC_COMM_SELF,NEQ,xgth,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterCreateToAll(x,ctx,xgth,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterBegin(ctx,x,xgth,INSERT_VALUES,SCATTER_FORWARD,
     &                       ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterEnd(ctx,x,xgth,INSERT_VALUES,SCATTER_FORWARD,
     &                     ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterDestroy(ctx,ierr)
        CALL ASSERT(ierr.EQ.0)

C       -- RECOPIE DE %RHS DANS SOLU (PROCESSOR 0)
        call VecGetArray(xgth,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)

        CALL JEVEUO(SOLU//'.VALE','E',JSOLU)
        CALL JELIRA(SOLU//'.VALE','LONMAX',NLONG,KBID)
        CALL ASSERT(N.EQ.NLONG)
        DO K=1,NEQ
           ZR(JSOLU-1+K)=XX(K+xidx)
        END DO
        CALL MRCONL(LMAT,0,'R',ZR(JSOLU),1)
        CALL JELIBE(SOLU//'.VALE')
        
        call VecRestoreArray(xgth,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)

C       -- NETTOYAGE :

        call VecDestroy(xgth,ierr)
        CALL ASSERT(ierr.EQ.0)
        call KSPDestroy(ksp,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecDestroy(x,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecDestroy(b,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecDestroy(resi,ierr)
        CALL ASSERT(ierr.EQ.0)
        call MatDestroy(A,ierr)
        CALL ASSERT(ierr.EQ.0)

C        call PetscFinalize(ierr)

        CALL JEDETR(IDXI)
        CALL JEDETR(IDXI2)
        CALL JEDETR(TRANS1)
        CALL JEDETR(TRANS2)

      END IF

C     -- ON REACTIVE LA LEVEE D'EXCEPTION
      CALL MATFPE(1)

      CALL JEDEMA()

#endif
      END
