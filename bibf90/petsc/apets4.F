      SUBROUTINE APETS4(KPTSC)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF APETS4 PETSC  DATE 23/11/2010   AUTEUR DESOZA T.DESOZA 
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
      INTEGER      KPTSC
!--------------------------------------------------------------
!
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

      INTEGER JSLVK,JSLVR,JSLVI
      INTEGER NMAXIT,IFM,NIV

      CHARACTER*24 ALGO
      CHARACTER*19 NOMAT,NOSOLV
      CHARACTER*14 NONU

      REAL*8 RESIRE

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
      PetscInt maxits,ierr
      PetscReal rtol,atol,dtol
      Mat A
      KSP ksp
!=================================================================
      CALL JEMARQ()

      CALL INFNIV(IFM,NIV)

!     -- ON RECUPERE DANS LE COMMON L'INSTANCE COURANTE
      NOMAT=NOMATS(KPTSC)
      NOSOLV=NOSOLS(KPTSC)
      NONU=NONUS(KPTSC)
      A=Ap(KPTSC)
      ksp=Kp(KPTSC)

      CALL JEVEUO(NOSOLV//'.SLVK','L',JSLVK)
      CALL JEVEUO(NOSOLV//'.SLVR','L',JSLVR)
      CALL JEVEUO(NOSOLV//'.SLVI','L',JSLVI)
      ALGO   = ZK24(JSLVK-1+6)
      RESIRE = ZR(JSLVR-1+2)
      NMAXIT = ZI(JSLVI-1+2)

!     -- choix de l'algo KSP:
!     ----------------------
! ("BCGS","BICG","CG","CR","GMRES","TFQMR",),defaut="CG" ),
      IF (ALGO.eq.'BCGS') THEN
         call KSPSetType(ksp,KSPBCGS,ierr)
      ELSEIF (ALGO.eq.'BICG') THEN
         call KSPSetType(ksp,KSPBICG,ierr)
      ELSEIF (ALGO.eq.'CG') THEN
         call KSPSetType(ksp,KSPCG,ierr)
         call KSPSetNormType(ksp,KSP_NORM_UNPRECONDITIONED,ierr)
      ELSEIF (ALGO.eq.'CR') THEN
         call KSPSetType(ksp,KSPCR,ierr)
      ELSEIF (ALGO.eq.'GMRES') THEN
         call KSPSetType(ksp,KSPGMRES,ierr)
      ELSEIF (ALGO.eq.'TFQMR') THEN
         call KSPSetType(ksp,KSPTFQMR,ierr)
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
      CALL ASSERT(ierr.EQ.0)

C     -- paramètres numériques :
C     ----------------------------

C     -- nb iter max :
      if (NMAXIT.LE.0) THEN
        maxits = PETSC_DEFAULT_INTEGER
      ELSE
        maxits = NMAXIT
      ENDIF

      rtol = RESIRE

      atol = PETSC_DEFAULT_DOUBLE_PRECISION
      dtol = PETSC_DEFAULT_DOUBLE_PRECISION

      call KSPSetTolerances(ksp,rtol,atol,dtol,maxits,ierr)

!      call KSPSetFromOptions(ksp,ierr)

C     - pour suivre le boulot de KSP ?
C     ---------------------------------
      IF (NIV.GE.2) THEN
         call KSPMonitorSet(ksp,KSPMonitorTrueResidualNorm,
     &               PETSC_NULL_OBJECT,PETSC_NULL_FUNCTION,ierr)
      ENDIF

      CALL JEDEMA()

#endif

      END
