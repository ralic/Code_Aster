      SUBROUTINE APETSR(ACTION,KPTSC,RSOLU,VCINE)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APETSR PETSC  DATE 06/04/2010   AUTEUR TARDIEU N.TARDIEU 
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
      CHARACTER*(*) ACTION
      INTEGER       KPTSC
      REAL*8        RSOLU(*)
      CHARACTER*19  VCINE
!--------------------------------------------------------------
!
! IN : ACTION :
!     /'PRERES'  : POUR DEMANDER LA FACTORISATION DU PRECONDITIONNEUR
!     /'RESOUD'  : POUR DEMANDER LA RESOLUTION ITERATIVE
!     /'DETR_MAT': POUR DEMANDER LA DESTRUCTION DE L'INSTANCE PETSC
!                  ASSOCIEE A UNE MATRICE
!
! IN : KPTSC (I)   : INDICE DES INSTANCES PETSC DANS Ap,Kp
! VAR: RSOLU (R)   : EN ENTREE : VECTEUR SECOND MEMBRE (REEL)
!                    EN SORTIE : VECTEUR SOLUTION (REEL)
!             (SI ACTION=RESOUD)
! IN : VCINE (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
!            (SI ACTION=RESOUD)
!---------------------------------------------------------------

#ifdef _HAVE_PETSC

!================================================================
#include "include/finclude/petsc.h"
#include "include/finclude/petscvec.h"
#include "include/finclude/petscmat.h"
#include "include/finclude/petscksp.h"
#include "include/finclude/petscpc.h"
!================================================================
! --- DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
!================================================================
! DECLARATIONS DE VARIABLES
!
      INTEGER NMXINS
      PARAMETER (NMXINS=5)

      INTEGER   IFM,NIV,NSMDI,IERD,IBID,NMAXIT,PTSERR
      INTEGER   LMAT,IDVALC
      
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*14 NONU
      CHARACTER*4  KBID
      CHARACTER*1  ROUC

      REAL*8       DIVTOL

      COMPLEX*16   CBID
!
!     -- ON STOCKE AU PLUS NMXINS INSTANCES DE PETSC
!     -- PLUS PRECISEMENT LES MATRICES, SOLVEURS, ETC
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14 NONUS(NMXINS)
      Mat      Ap(NMXINS)
      KSP      Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
!=================================================================
!  Variables PETSc
!
!  A       - matrix that defines linear system
!  ksp    - KSP context
!  x, b - approx solution, RHS vectors
!
      PetscMPIInt rang,size,ier
      PetscInt    ierr,NEQ,low,high,i
      PetscInt    maxits
      PetscReal   rtol,atol,dtol
      Vec         b,xgth
      PetscScalar  XX(1)
      PetscOffset xidx
      VecScatter   ctx
      KSPConvergedReason indic
      Mat A
      KSP ksp
!=================================================================
      CALL JEMARQ()

!     -- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LA BIBLIOTHEQUE MKL
!     -- CAR CES EXCEPTIONS NE SONT PAS JUSTIFIEES
      CALL MATFPE(-1)

      CALL INFNIV(IFM,NIV)

!     -- ON RECUPERE DANS LE COMMON L'INSTANCE COURANTE
      NOMAT=NOMATS(KPTSC)
      NOSOLV=NOSOLS(KPTSC)
      NONU=NONUS(KPTSC)

      IF (ACTION.EQ.'PRERES') THEN
!     ----------------------------

!        1.1 RECUPERE LE RANG DU PROCESSUS:
!        ---------------------------------
         CALL MPI_COMM_RANK(PETSC_COMM_WORLD, rang, ier)
         CALL MPI_COMM_SIZE(PETSC_COMM_WORLD, size, ier)

!        1.2 LECTURE DES DIMENSIONS DE LA MATRICE:
!        ----------------------------------------

         CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
         NEQ=NSMDI

!        1.3 CREATION DE LA MATRICE PETSc:
!        --------------------------------

         call MatCreate(PETSC_COMM_WORLD,Ap(KPTSC),ierr)
         call MatSetSizes(Ap(KPTSC),PETSC_DECIDE,PETSC_DECIDE,NEQ,NEQ,
     &                                                            ierr)
         call MatSetType(Ap(KPTSC),MATMPIAIJ,ierr)

!        1.4 PREALLOCATION DE LA MATRICE PETSc:
!        -------------------------------------

         CALL APETS1(KPTSC)

!        1.5 COPIE DE LA MATRICE ASTER VERS LA MATRICE PETSc:
!        ---------------------------------------------------

         CALL APETS2(KPTSC)

!        1.6 ASSEMBLAGE DE LA MATRICE PETSc:
!        ----------------------------------

         call MatAssemblyBegin(Ap(KPTSC),MAT_FINAL_ASSEMBLY,ierr)
         call MatAssemblyEnd(Ap(KPTSC),MAT_FINAL_ASSEMBLY,ierr)
         CALL ASSERT(ierr.EQ.0)

!        1.7 CREATION DU PC PETSc (EXTRAIT DU KSP):
!        -----------------------------------------

         call KSPCreate(PETSC_COMM_WORLD,Kp(KPTSC),ierr)
         CALL ASSERT(ierr.EQ.0)
!  ATTENTION SI PB REPASSER EN DIFFERENT_NONZERO_PATTERN
         call KSPSetOperators(Kp(KPTSC),Ap(KPTSC),Ap(KPTSC),
     &                        SAME_NONZERO_PATTERN,ierr)
         CALL ASSERT(ierr.EQ.0)

         CALL APETS3(ACTION,KPTSC)


      ELSE IF (ACTION.EQ.'RESOUD') THEN
!     ---------------------------------

!        2.0 RECUPERATION DES POINTEURS:
!        ------------------------------

         A   = Ap(KPTSC)
         ksp = Kp(KPTSC)

!        2.1 PRETRAITEMENT DU SECOND MEMBRE:
!        ----------------------------------

!        -- PRISE EN COMPTE DU CONDITIONNEMENT DES LAGRANGES :
         CALL MTDSCR(NOMAT)
         CALL JEVEUO(NOMAT//'.&INT','L',LMAT)
         CALL MRCONL(LMAT,0,'R',RSOLU,1)

!        -- PRISE EN COMPTE DES CHARGES CINEMATIQUES :
         CALL JEEXIN(VCINE//'.VALE',IERD)
         IF ( VCINE.NE.' '.AND.IERD.NE.0) THEN
            CALL JEVEUO(VCINE//'.VALE','L',IDVALC)
            CALL JELIRA(VCINE//'.VALE','TYPE',IBID,ROUC)
            CALL ASSERT(ROUC.EQ.'R')
            CALL CSMBGG(LMAT,RSOLU,ZR(IDVALC),CBID,CBID,'R')
         END IF

!        2.2 CREATION DU VECTEUR PETSc:
!        -----------------------------

         CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
         NEQ=NSMDI
         call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,NEQ,b,ierr)
         CALL ASSERT(ierr.EQ.0)     

         call VecGetArray(b,XX,xidx,ierr)

         call VecGetOwnershipRange(b,low,high,ierr)

         DO i=1,high-low
            XX(xidx+i)=RSOLU(low+i)
         END DO 

         call VecRestoreArray(b,XX,xidx,ierr)

!        2.3 PARAMETRES DU KSP:
!        ---------------------

         CALL APETS4(KPTSC)

         CALL APETS3(ACTION,KPTSC)

!        2.4 RESOLUTION:
!        --------------

         call KSPSolve(ksp,b,b,ierr)
         IF (ierr.GT.0) then
               CALL U2MESS('F','PETSC_13')
         ENDIF
         call KSPGetConvergedReason(ksp, indic, ierr)
         IF (indic.LT.0) THEN
            call KSPGetTolerances(ksp,rtol,atol,dtol,maxits,ierr)
            IF (indic.EQ.KSP_DIVERGED_ITS) THEN
!              NOMBRE MAX D'ITERATIONS
               NMAXIT=maxits
               CALL U2MESI('F','PETSC_5',1,NMAXIT)
            ELSE IF (indic.EQ.KSP_DIVERGED_DTOL) THEN
!              DIVERGENCE
               DIVTOL = dtol
               CALL U2MESR('F','PETSC_6',1,DIVTOL)
            ELSE IF (indic.EQ.KSP_DIVERGED_BREAKDOWN) THEN
!              BREAKDOWN
               CALL U2MESS('F','PETSC_7')
            ELSE IF (indic.EQ.KSP_DIVERGED_BREAKDOWN_BICG) THEN
!              BREAKDOWN BiCG
               CALL U2MESS('F','PETSC_8')
            ELSE IF (indic.EQ.KSP_DIVERGED_NONSYMMETRIC) THEN
!              MATRICE NON SYMETRIQUE
               CALL U2MESS('F','PETSC_9')
            ELSE IF (indic.EQ.KSP_DIVERGED_INDEFINITE_PC) THEN
!              PRECONDITIONNEUR NON DEFINI
               CALL U2MESS('F','PETSC_10')
!            ELSE IF (indic.EQ.KSP_DIVERGED_INDEFINITE_MAT) THEN
            ELSE IF (indic.EQ.-10) THEN
!              MATRICE NON DEFINIE
               CALL U2MESS('F','PETSC_11')
            ELSE
!              AUTRE ERREUR
               PTSERR = indic
               CALL U2MESI('F','PETSC_12',1,PTSERR)
            ENDIF
         ENDIF

!        2.5 RECOPIE DE LA SOLUTION:
!        --------------------------

!       --- ON DOIT RECONSTRUIRE LA SOLUTION SUR CHAQUE PROC
!       --- ON UTILISE DANS PETSC UN VECSCATTER

        call VecCreateSeq(PETSC_COMM_SELF,NEQ,xgth,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterCreateToAll(b,ctx,xgth,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterBegin(ctx,b,xgth,INSERT_VALUES,SCATTER_FORWARD,
     &                       ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterEnd(ctx,b,xgth,INSERT_VALUES,SCATTER_FORWARD,
     &                     ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecScatterDestroy(ctx,ierr)
        CALL ASSERT(ierr.EQ.0)

!       -- RECOPIE DE %RHS DANS SOLU (PROCESSOR 0)
        call VecGetArray(xgth,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)

        DO i=1,NEQ
           RSOLU(i)=XX(xidx+i)
        END DO
        CALL MRCONL(LMAT,0,'R',RSOLU,1)
        
        call VecRestoreArray(xgth,XX,xidx,ierr)
        CALL ASSERT(ierr.EQ.0)


!        2.6 NETTOYAGE PETSc (VECTEURS):
!        ------------------------------

!       -- NETTOYAGE :

        call VecDestroy(xgth,ierr)
        CALL ASSERT(ierr.EQ.0)
        call VecDestroy(b,ierr)
        CALL ASSERT(ierr.EQ.0)

      ELSE IF (ACTION.EQ.'DETR_MAT') THEN
!     -----------------------------------

!        3.0 RECUPERATION DES POINTEURS:
!        ------------------------------

         A   = Ap(KPTSC)
         ksp = Kp(KPTSC)

!        3.1 NETTOYAGE PETSc (Mat et KSP):
!        --------------------------------

         IF (NOMAT.NE.' ') THEN

            call MatDestroy(A,ierr)
            CALL ASSERT(ierr.EQ.0)
            call KSPDestroy(ksp,ierr)
            CALL ASSERT(ierr.EQ.0)

            NOMATS(KPTSC)=' '
            NOSOLS(KPTSC)=' '
            NONUS(KPTSC)=' '

         END IF

      ELSE
          CALL ASSERT(.FALSE.)
      END IF

!     -- ON REACTIVE LA LEVEE D'EXCEPTION
      CALL MATFPE(1)

      CALL JEDEMA()

#endif

      END






