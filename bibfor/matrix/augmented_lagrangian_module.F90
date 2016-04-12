module augmented_lagrangian_module 
!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
use saddle_point_context_class
use augmented_lagrangian_context_class
!
implicit none 
!
private 
#include "asterf.h"
#include "asterf_petsc.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/assert.h"
!
public :: augmented_lagrangian_setup, augmented_lagrangian_apply, augmented_lagrangian_destroy
!
#ifdef _HAVE_PETSC 
contains 
!
subroutine augmented_lagrangian_setup( al_pc , ierr )
    !
    use saddle_point_data_module, only : sp_context , sp_pc_context
    !
    PC, intent(inout)           :: al_pc
    PetscErrorCode, intent(out) :: ierr
    !
    sp_pc_context = new_augmented_lagrangian_context( sp_context )
    !
    ierr = 0 
    !
end subroutine augmented_lagrangian_setup
!
! PCApply operation used for the PCShell based on the Augmented Lagrangian Method
!
subroutine augmented_lagrangian_apply ( al_pc, x, y, ierr )
    !
    use saddle_point_data_module, only : sp_pc_context
    !
    ! Dummy arguments 
    !
    PC, intent(in)    :: al_pc
    Vec, intent(in)   :: x
    Vec, intent(inout):: y
    PetscErrorCode, intent(out) :: ierr
    !
    ! Local Variables 
    PetscReal :: beta
    type(saddle_point_context_type), pointer :: sp_ctxt => null()
    !
    ! Use work space and scatter context defined in the underlying saddle_point_context
    sp_ctxt => sp_pc_context%sp_ctxt
    !
    ! Load values of  x1, x2 from x : use SCATTER_REVERSE
    !
    call VecScatterBegin( sp_ctxt%scatter_to_phys, x, sp_ctxt%x1, &
   &  INSERT_VALUES, SCATTER_REVERSE, ierr )
    ASSERT( ierr == 0 )
    call VecScatterEnd( sp_ctxt%scatter_to_phys, x, sp_ctxt%x1, &
   & INSERT_VALUES, SCATTER_REVERSE, ierr )
    ASSERT( ierr == 0 )
    !
    call VecScatterBegin( sp_ctxt%scatter_to_lag1, x, sp_ctxt%x2, &
   & INSERT_VALUES, SCATTER_REVERSE, ierr )
    ASSERT( ierr == 0 )
    call VecScatterEnd( sp_ctxt%scatter_to_lag1, x, sp_ctxt%x2, &
   & INSERT_VALUES, SCATTER_REVERSE, ierr )
    ASSERT( ierr == 0 )
    !
    ! Apply Augmented Lgrangian preconditioner :
    ! Thèse Sylvain p. 113
    ! (y1) = ( LL^T  2 C^T     )^{-1} (x1) = ( L^{-T} L^{-1}  0      ) ( I  2*gamma*C^T ) (x1)
    ! (y2)   ( 0    -1/gamma*I )      (x2)   ( 0             -gamma*I) ( 0  I           ) (x2)
    !
    ! c'est à dire : y1 = L^{-T} L^{-1} (x1 + 2*gamma*C^T*x2)
    !                y2 = -gamma*x2 
    !
    ! y2 = -gamma*x2
    call VecCopy(sp_ctxt%x2, sp_ctxt%y2, ierr)
    ASSERT( ierr == 0 )
    beta = -sp_pc_context%gamma
    call VecScale(sp_ctxt%y2,beta, ierr)
    ASSERT( ierr == 0 )
    !
    ! x_1 = x1 + 2*gamma*C^T*x2 
    ! 
    ! xtmp = C^T x2
    call MatMultTranspose(sp_ctxt%c_mat, sp_ctxt%x2, sp_ctxt%xtmp, ierr )
    ASSERT( ierr == 0 )
    ! x1 = x1 + (2*gamma)* xtmp
    beta=2.0*sp_pc_context%gamma
    call VecAXPY( sp_ctxt%x1, beta, sp_ctxt%xtmp, ierr )
    ASSERT(ierr == 0) 
    !
    ! Apply pcphy 
    call PCApply( sp_pc_context%pcphy, sp_ctxt%x1, sp_ctxt%y1, ierr )
    ASSERT(ierr == 0 ) 
    !
    ! Load result to y : this time, use SCATTER_FORWARD
    !
    call VecScatterBegin( sp_ctxt%scatter_to_phys, sp_ctxt%y1, y, &
      &  INSERT_VALUES, SCATTER_FORWARD, ierr )
    ASSERT( ierr == 0 )
    call VecScatterEnd( sp_ctxt%scatter_to_phys, sp_ctxt%y1, y, &
      &  INSERT_VALUES, SCATTER_FORWARD, ierr )
    ASSERT( ierr == 0 )
    !
    call VecScatterBegin( sp_ctxt%scatter_to_lag1, sp_ctxt%y2, y, &
      &  INSERT_VALUES, SCATTER_FORWARD, ierr )
    ASSERT( ierr == 0 )
    call VecScatterEnd( sp_ctxt%scatter_to_lag1, sp_ctxt%y2, y, &
      &  INSERT_VALUES, SCATTER_FORWARD, ierr )
    ASSERT( ierr == 0 )
    !
end subroutine augmented_lagrangian_apply
!
subroutine augmented_lagrangian_destroy( al_pc, ierr )
    !
    use saddle_point_data_module, only : sp_pc_context
    !
    ! Dummy arguments 
    !
    PC, intent(inout)    :: al_pc
    PetscErrorCode, intent(out) :: ierr
    !
    call free_augm_lagrangian_context( sp_pc_context )
    !
    ierr = 0 
    !
end subroutine augmented_lagrangian_destroy
!
#else
contains 
!
subroutine augmented_lagrangian_setup( al_pc , ierr )
    integer, intent(inout):: al_pc
    integer, intent(out)  :: ierr
    ierr = 1
end subroutine augmented_lagrangian_setup
!
subroutine augmented_lagrangian_apply ( al_pc, x, y, ierr )
    integer, intent(in)    :: al_pc
    integer, intent(in)   :: x
    integer, intent(inout):: y
    integer, intent(out) :: ierr
    ierr = 1
end subroutine augmented_lagrangian_apply
!
subroutine augmented_lagrangian_destroy( al_pc, ierr )
    integer, intent(inout)    :: al_pc
    integer, intent(out) :: ierr
    ierr = 1
end subroutine augmented_lagrangian_destroy
#endif
end module augmented_lagrangian_module
