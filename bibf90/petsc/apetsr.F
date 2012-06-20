      SUBROUTINE APETSR(ACTION,KPTSC ,RSOLU ,VCINE )
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF APETSR PETSC  DATE 18/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
! RESPONSABLE DESOZA T.DESOZA
      INCLUDE 'jeveux.h'
      CHARACTER*(*) ACTION
      INTEGER       KPTSC
      REAL*8        RSOLU(*)
      CHARACTER*19  VCINE
!--------------------------------------------------------------
!
! IN  : ACTION :
!     /'DETR_MAT'  : POUR DEMANDER LA DESTRUCTION DE L'INSTANCE PETSC
!                    ASSOCIEE A UNE MATRICE
!     /'PRERES'    : POUR DEMANDER LA FACTORISATION DU PRECONDITIONNEUR
!     /'RESOUD'    : POUR DEMANDER LA RESOLUTION ITERATIVE
!
! IN  : KPTSC (I)  : INDICE DES INSTANCES PETSC DANS Ap,Kp
! I/O : RSOLU (R)  : EN ENTREE : VECTEUR SECOND MEMBRE (REEL)
!                    EN SORTIE : VECTEUR SOLUTION      (REEL)
!                   (SI ACTION=RESOUD)
! IN  : VCINE (K19): NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
!                   (SI ACTION=RESOUD)
!---------------------------------------------------------------

#ifdef _HAVE_PETSC

!================================================================
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/petscksp.h"
#include "finclude/petscpc.h"
!================================================================
!----------------------------------------------------------------
!     AU PLUS 5 MATRICES PETSC SONT GEREES A LA FOIS
      INTEGER      NMXINS
      PARAMETER   (NMXINS=5)
!
!     VARIABLES LOCALES
      INTEGER      IFM,NIV,NSMDI,IERD,IBID,NMAXIT,PTSERR
      INTEGER      LMAT,IDVALC,JSLVK,JSLVI,JSLVR
!
      CHARACTER*24 PRECON
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*14 NONU
      CHARACTER*4  KBID
      CHARACTER*1  ROUC
!
      REAL*8       DIVTOL,RESIPC
      COMPLEX*16   CBID
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*14  NONUS(NMXINS)
      Mat              Ap(NMXINS)
      KSP              Kp(NMXINS)
      COMMON /SPETSC/ Ap,Kp,NOMATS,NOSOLS,NONUS
!
!     COMMUN POUR LE PRECONDITIONNEUR SIMPLE PRECISION LDLT_SP
      CHARACTER*19 SPSOMU,SPMAT,SPSOLV
      Vec          xlocal,xglobal
      VecScatter   xscatt
      COMMON /LDLTSP/xlocal,xscatt,xglobal,SPSOMU,SPMAT,SPSOLV
!----------------------------------------------------------------
!     Variables PETSc
!
      PetscInt           ierr,neq,low,high,i
      PetscInt           maxits
      PetscReal          rtol,atol,dtol
      Vec                b,x,r,xgth
      PetscScalar        XX(1),ires,fres
      PetscOffset        xidx
      VecScatter         ctx
      KSPConvergedReason indic
      Mat                A
      KSP                ksp
!----------------------------------------------------------------
      CALL JEMARQ()

!     -- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LES MKL
      CALL MATFPE(-1)

      CALL INFNIV(IFM,NIV)

!     -- LECTURE DU COMMUN
      NOMAT  = NOMATS(KPTSC)
      NOSOLV = NOSOLS(KPTSC)
      NONU   =  NONUS(KPTSC)

!     -- PRECONDITIONNEUR UTILISE
      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
      PRECON = ZK24(JSLVK-1+2)

      IF (ACTION.EQ.'PRERES') THEN
!     ----------------------------

!        1.1 CREATION ET PREALLOCATION DE LA MATRICE PETSc :
!        ---------------------------------------------------

         CALL APETS1(KPTSC)

!        1.2 COPIE DE LA MATRICE ASTER VERS LA MATRICE PETSc :
!        -----------------------------------------------------

         CALL APETS2(KPTSC)

!        1.3 ASSEMBLAGE DE LA MATRICE PETSc :
!        ------------------------------------

         call MatAssemblyBegin(Ap(KPTSC),MAT_FINAL_ASSEMBLY,ierr)
         CALL ASSERT(ierr.EQ.0)
         call MatAssemblyEnd  (Ap(KPTSC),MAT_FINAL_ASSEMBLY,ierr)
         CALL ASSERT(ierr.EQ.0)

!        1.4 CREATION DU PRECONDITIONNEUR PETSc (EXTRAIT DU KSP) :
!        ---------------------------------------------------------

         call KSPCreate(PETSC_COMM_WORLD,Kp(KPTSC),ierr)
         CALL ASSERT(ierr.EQ.0)
!
         call KSPSetOperators(Kp(KPTSC),Ap(KPTSC),Ap(KPTSC),
     &                        DIFFERENT_NONZERO_PATTERN,ierr)
         CALL ASSERT(ierr.EQ.0)
!
         CALL APETS3(ACTION,KPTSC)

      ELSE IF (ACTION.EQ.'RESOUD') THEN
!     ---------------------------------

!        2.0 RECUPERATION DES POINTEURS DANS LE COMMUN :
!        -----------------------------------------------

         A   = Ap(KPTSC)
         ksp = Kp(KPTSC)

!        2.1 PRETRAITEMENT DU SECOND MEMBRE :
!        ------------------------------------

!        -- PRISE EN COMPTE DU CONDITIONNEMENT DES LAGRANGES :
         CALL MTDSCR(NOMAT)
         CALL JEVEUO(NOMAT//'.&INT','L',LMAT)
         CALL MRCONL('MULT',LMAT,0,'R',RSOLU,1)

!        -- PRISE EN COMPTE DES CHARGES CINEMATIQUES :
         CALL JEEXIN(VCINE//'.VALE',IERD)
         IF (IERD.NE.0) THEN
            CALL JEVEUO(VCINE//'.VALE','L',IDVALC)
            CALL JELIRA(VCINE//'.VALE','TYPE',IBID,ROUC)
            CALL ASSERT(ROUC.EQ.'R')
            CALL CSMBGG(LMAT,RSOLU,ZR(IDVALC),CBID,CBID,'R')
         END IF

!        2.2 CREATION DU VECTEUR PETSc :
!        -------------------------------

         CALL JELIRA(NONU//'.SMOS.SMDI','LONMAX',NSMDI,KBID)
         neq=NSMDI
         call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,neq,b,ierr)
         CALL ASSERT(ierr.EQ.0)

         call VecGetOwnershipRange(b,low,high,ierr)
         CALL ASSERT(ierr.EQ.0)
         call VecGetArray(b,XX,xidx,ierr)
         CALL ASSERT(ierr.EQ.0)

         DO i=1,high-low
            XX(xidx+i)=RSOLU(low+i)
         END DO

         call VecRestoreArray(b,XX,xidx,ierr)
         CALL ASSERT(ierr.EQ.0)

!        2.3 PARAMETRES DU KSP :
!        -----------------------

         CALL APETS4(KPTSC)

         CALL APETS3(ACTION,KPTSC)

!        2.4 RESOLUTION :
!        ----------------

         call VecDuplicate(b,x,ierr)
         CALL ASSERT(ierr.EQ.0)
         call KSPSolve(ksp,b,x,ierr)
         IF (ierr.GT.0) then
            CALL U2MESS('F','PETSC_13')
         ENDIF
         call KSPGetConvergedReason(ksp, indic, ierr)
         CALL ASSERT(ierr.EQ.0)
         IF (indic.LT.0) THEN
            call KSPGetTolerances(ksp,rtol,atol,dtol,maxits,ierr)
            CALL ASSERT(ierr.EQ.0)
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
            ELSE IF (indic.EQ.KSP_DIVERGED_INDEFINITE_MAT) THEN
!              MATRICE NON DEFINIE
               CALL U2MESS('F','PETSC_11')
            ELSE
!              AUTRE ERREUR
               PTSERR = indic
               CALL U2MESI('F','PETSC_12',1,PTSERR)
            ENDIF
         ENDIF

!        2.4b VERIFICATION DE LA SOLUTION :
!        ----------------------------------

!        -- DOIT-ON VERIFIER LE CRITERE EN NORME NON PRECONDITIONNEE ?
         CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
         RESIPC = ZR(JSLVR-1+4)

         IF (RESIPC.GE.0.D0) THEN
            call VecDuplicate(x,r,ierr)
            CALL ASSERT(ierr.EQ.0)
!           r = Ax
            call MatMult(A,x,r,ierr)
            CALL ASSERT(ierr.EQ.0)
!           r = b - Ax
            call VecAYPX(r,-1.D0,b,ierr)
            CALL ASSERT(ierr.EQ.0)
!           fres = ||r||_2
            call VecNorm(r,NORM_2,fres,ierr)
            CALL ASSERT(ierr.EQ.0)
!           ires = ||b||_2
            call VecNorm(b,NORM_2,ires,ierr)
            CALL ASSERT(ierr.EQ.0)
!
            call VecDestroy(r,ierr)
            CALL ASSERT(ierr.EQ.0)
!
            call KSPGetTolerances(ksp,rtol,atol,dtol,maxits,ierr)
            CALL ASSERT(ierr.EQ.0)
!
            IF (fres.GT.SQRT(rtol)*ires) THEN
               CALL U2MESS('F','PETSC_16')
            ENDIF
         ENDIF

!        2.5 RECOPIE DE LA SOLUTION :
!        ----------------------------

!        -- RECONSTRUCTION DE LA LA SOLUTION SUR CHAQUE PROC

         call VecScatterCreateToAll(x,ctx,xgth,ierr)
         CALL ASSERT(ierr.EQ.0)
         call VecScatterBegin(ctx,x,xgth,INSERT_VALUES,SCATTER_FORWARD,
     &                        ierr)
         CALL ASSERT(ierr.EQ.0)
         call VecScatterEnd(ctx,x,xgth,INSERT_VALUES,SCATTER_FORWARD,
     &                      ierr)
         CALL ASSERT(ierr.EQ.0)
         call VecScatterDestroy(ctx,ierr)
         CALL ASSERT(ierr.EQ.0)

!        -- RECOPIE DE DANS RSOLU
         call VecGetArray(xgth,XX,xidx,ierr)
         CALL ASSERT(ierr.EQ.0)

         DO i=1,neq
            RSOLU(i)=XX(xidx+i)
         END DO

         call VecRestoreArray(xgth,XX,xidx,ierr)
         CALL ASSERT(ierr.EQ.0)

!        -- REMISE A L'ECHELLE DES LAGRANGES
         CALL MRCONL('MULT',LMAT,0,'R',RSOLU,1)

!         2.6 NETTOYAGE PETSc (VECTEURS) :
!         --------------------------------

!        -- NETTOYAGE
         call VecDestroy(xgth,ierr)
         CALL ASSERT(ierr.EQ.0)
         call VecDestroy(b,ierr)
         CALL ASSERT(ierr.EQ.0)
         call VecDestroy(x,ierr)
         CALL ASSERT(ierr.EQ.0)

!        -- TRAITEMENT PARTICULIER DU PRECONDITIONNEUR LDLT_SP
         IF (PRECON .EQ. 'LDLT_SP') THEN
!           MENAGE
            SPSOMU = ZK24(JSLVK-1+3)
            CALL DETRSD('SOLVEUR',SPSOMU)
            SPSOMU = ' '
!
            call VecDestroy(xlocal,ierr)
            CALL ASSERT(ierr.EQ.0)
            call VecDestroy(xglobal,ierr)
            CALL ASSERT(ierr.EQ.0)
            call VecScatterDestroy(xscatt,ierr)
            CALL ASSERT(ierr.EQ.0)
            xlocal  = 0
            xglobal = 0
            xscatt  = 0
!           ON STOCKE LE NOMBRE D'ITERATIONS DU KSP
            call KSPGetIterationNumber(ksp,maxits,ierr)
            CALL ASSERT(ierr.EQ.0)
            NMAXIT = maxits
            CALL JEVEUO(NOSOLV//'.SLVI','E',JSLVI)
            ZI(JSLVI-1+5) = NMAXIT
         ENDIF


      ELSE IF (ACTION.EQ.'DETR_MAT') THEN
!     -----------------------------------

!        3.0 RECUPERATION DES POINTEURS :
!        --------------------------------

         A   = Ap(KPTSC)
         ksp = Kp(KPTSC)

!        3.1 NETTOYAGE PETSc :
!        ---------------------

!        -- DESTRUCTION DES OBJETS PETSC GENERAUX
         call MatDestroy(A,ierr)
         CALL ASSERT(ierr.EQ.0)
         call KSPDestroy(ksp,ierr)
         CALL ASSERT(ierr.EQ.0)

!        -- SUPRESSION DE L'INSTANCE PETSC
         NOMATS(KPTSC) = ' '
         NOSOLS(KPTSC) = ' '
          NONUS(KPTSC) = ' '
             Ap(KPTSC) =  0
             Kp(KPTSC) =  0

         IF (PRECON .EQ. 'LDLT_SP') THEN
!           MENAGE
            SPMAT  = ' '
            SPSOLV = ' '
         ENDIF

      ELSE
          CALL ASSERT(.FALSE.)
      END IF



!     -- ON REACTIVE LA LEVEE D'EXCEPTION
      CALL MATFPE(1)

      CALL JEDEMA()

#endif

      END

