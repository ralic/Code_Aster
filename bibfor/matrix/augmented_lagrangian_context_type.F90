!
! A saddle_point_context object is a container used to manage 
! a saddle point linear system 
! ( k_mat c_mat^T ) (x_1) = (b_1)
! ( c_mat 0       ) (x_2)   (b_2)
! For simplicity the saddle point linear system is embedded
! in the larger system 
! ( k_mat c_mat^T 0 ) (x_1) = (b_1)
! ( c_mat 0       0 ) (x_2)   (b_2)
! ( 0     0       Id) (x_3)   (b_3)
!
! It contains 
! - Index Sets necessary to extract data from the global (double Lagrange) Aster system
! - matrix data ( k_mat, c_mat )
! - vector workspace ( x_1, x_2, b_1, b_2 )
! 
module augmented_lagrangian_context_class
!
! COPYRIGHT (C) 2016 -  EDF R&D                WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! person_in_charge: natacha.bereux at edf.fr
! aslint: disable=C1308
!
use matrasse_module
use saddle_point_context_class
!
implicit none 
!
private 
#include "asterf.h"
#include "asterf_petsc.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/assert.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/conlag.h"
#include "asterfort/utmess.h"
!
!
type, public :: augmented_lagrangian_context_type
    !
#ifdef _HAVE_PETSC
    type(saddle_point_context_type), pointer :: sp_ctxt =>null()
    !
    ! Preconditioner data section  
    ! ===========================
    ! Matrix used to build block (1,1) of the preconditioner
    Mat :: m_mat
    ! Scaling coefficient
    PetscReal :: gamma 
    ! Preconditioner for the block of physical dofs
    PC :: pcphy
    !
#endif    
end type augmented_lagrangian_context_type
!
public :: new_augmented_lagrangian_context, free_augm_lagrangian_context
!
#ifdef _HAVE_PETSC 
PetscErrorCode :: ierr
!
contains 
!
!
function new_augmented_lagrangian_context( sp_ctxt  ) result ( ctxt )
    ! 
    type(saddle_point_context_type), target         :: sp_ctxt    
    type(augmented_lagrangian_context_type)         :: ctxt
    ! 
    ! Local variables
    !
    ctxt%sp_ctxt => sp_ctxt
    !
    ! Init data section 
    call set_precond_data( ctxt )
    !
end function new_augmented_lagrangian_context
!
!
! Init data needed by an augmented lagrangian preconditioner
!
subroutine set_precond_data( ctxt )
    !
    ! Dummy arguments 
    !
    type(augmented_lagrangian_context_type), intent(inout) :: ctxt
    !
    !
    ! Local variables
    !
    mpi_int :: mpicomm
    mpi_int :: rang, nbproc
    PetscInt :: niremp
    PetscScalar :: fillp
    PetscReal :: aster_petsc_default_real
    type(saddle_point_context_type), pointer :: sp_ctxt =>null()
    !
    ! TODO déterminer les bonnes valeurs 
    niremp=1
    fillp =1.0
    !
    ! Notation 
    sp_ctxt=>ctxt%sp_ctxt
    !
    ! Récupération du communicateur MPI
    call asmpi_comm('GET', mpicomm)
    ! Attention, la factorisation ICC ne fonctionne qu'en séquentiel 
    call asmpi_info(rank=rang, size=nbproc)
    if (rang > 1) then
      call utmess( 'F', 'PETSC_20')
    endif
    ctxt%gamma = sp_ctxt%alpha
    !
    ! Compute block of physical dofs m_mat 
    !
    ! m_mat = c^T c
#ifdef ASTER_PETSC_VERSION_LEQ_34
    aster_petsc_default_real = PETSC_DEFAULT_DOUBLE_PRECISION
#else
    aster_petsc_default_real = PETSC_DEFAULT_REAL
#endif
    call MatTransposeMatMult(sp_ctxt%c_mat,sp_ctxt%c_mat,                  &
   &   MAT_INITIAL_MATRIX,aster_petsc_default_real, ctxt%m_mat ,ierr)
    ASSERT( ierr == 0 )
    ! m_mat <- k_mat + gamma*m_mat
    call MatAYPX(ctxt%m_mat,ctxt%gamma,sp_ctxt%k_mat,DIFFERENT_NONZERO_PATTERN,ierr)
    ASSERT( ierr == 0 ) 
    call MatSetOption(ctxt%m_mat,MAT_SPD,PETSC_TRUE,ierr)
    ASSERT( ierr == 0 )
    !
    ! Define preconditionner pcphy, based on the incomplete factorization of m_mat
    !
    call PCCreate(mpicomm,ctxt%pcphy,ierr)
    ASSERT( ierr == 0 )
    call PCSetOperators( ctxt%pcphy, ctxt%m_mat, ctxt%m_mat, ierr )
    ASSERT( ierr == 0 )
    ! 
    call PCSetType(ctxt%pcphy,PCICC,ierr)
    ASSERT( ierr == 0 )
    call PCFactorSetLevels(ctxt%pcphy,niremp,ierr)
    ASSERT(ierr.eq.0)
    call PCFactorSetFill(ctxt%pcphy,fillp,ierr)
    ASSERT(ierr.eq.0)
    call PCFactorSetMatOrderingType(ctxt%pcphy,MATORDERINGNATURAL,ierr)
    ASSERT(ierr.eq.0)
    ! On pourrait aussi utiliser HYPRE pilut 
    !call PCSetType(ctxt%pcphy,PCHYPRE,ierr)
    !ASSERT( ierr == 0 )
    !call PCHYPRESetType(ctxt%pcphy,'pilut', ierr)
    !ASSERT(ierr.eq.0)
    ! Ou encore  mumps ...
!      call PCFactorSetMatSolverPackage(pcphy,MATSOLVERMUMPS,ierr)
!      call PCFactorSetUpMatSolverPackage(pcphy,ierr)
!      call PCFactorGetMatrix(pcphy,F,ierr)
!      call MatMumpsSetIcntl(F,7,2,ierr)
!      call MatMumpsSetIcntl(F,24,1,ierr)
!      call MatMumpsSetCntl(F,3,1.D-6,ierr)
     call PCSetUp(ctxt%pcphy,ierr)
     ASSERT(ierr.eq.0)
    !
end subroutine set_precond_data
!
subroutine free_augm_lagrangian_context( ctxt )
    !
    ! Dummy argument 
    !
    type( augmented_lagrangian_context_type ), intent(inout) :: ctxt
    !
    call MatDestroy( ctxt%m_mat, ierr )
    ASSERT( ierr == 0 )
    call PCDestroy( ctxt%pcphy, ierr )
    ASSERT( ierr == 0 ) 
    ! TODO compteur de référence 
    nullify( ctxt%sp_ctxt ) 
    !
end subroutine free_augm_lagrangian_context
!
#else
!
contains 
!
function new_augmented_lagrangian_context( sp_ctxt  ) result ( ctxt )
    type(saddle_point_context_type), target         :: sp_ctxt    
    type(augmented_lagrangian_context_type)         :: ctxt
end function new_augmented_lagrangian_context
!
subroutine free_augm_lagrangian_context( ctxt )
    type( augmented_lagrangian_context_type ), intent(inout) :: ctxt
end subroutine free_augm_lagrangian_context
#endif 
end module augmented_lagrangian_context_class
