module lmp_module 
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
use lmp_context_class
!
implicit none 
!
private 
#include "asterf.h"
#include "asterf_petsc.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
!
public :: lmp_apply_right, lmp_update, lmp_destroy
!
!
#ifdef _HAVE_PETSC 
contains 
!
! lmp_update est appelé APRES la résolution (KSPSolve)
!
subroutine lmp_update( lmp_pc, ksp, ierr )
    !
    use lmp_data_module, only : lmp_context, lmp_is_setup, reac_lmp
    ! Dummy arguments 
    PC, intent(inout)           :: lmp_pc 
    KSP, intent(in)             :: ksp
    PetscErrorCode, intent(out) :: ierr
    ! Local variables
    PetscInt :: maxits 
    integer :: ifm, niv, si
    aster_logical :: verbose
    !
    call infniv( ifm, niv )
    verbose = niv == 2 
    
    call KSPGetIterationNumber(ksp, maxits, ierr)
    ASSERT( ierr == 0 ) 
    if ( verbose ) then  
        call utmess('I', 'PETSC_24' , si=to_aster_int(maxits))
    endif 
    if ( lmp_is_setup ) then 
       if ( maxits > reac_lmp ) then
       ! todo : il faudrait normalement détruire le LMP, mais cela fonctionne moins bien 
       ! à comprendre ... 
       !    call free_lmp_context( lmp_context )
       !    lmp_is_setup =.false.
        endif
        if ( verbose ) then  
               call utmess('I', 'PETSC_27' , si=reac_lmp )
        endif
    endif 
    if (.not. lmp_is_setup ) then 
       if ( maxits > reac_lmp ) then 
    ! On construit un nouveau lmp
       if ( verbose ) then
          call utmess( 'I', 'PETSC_26' )
       endif  
       lmp_context = new_lmp_context( )
       call build_lmp_context( ksp, lmp_context )
       lmp_is_setup = .true. 
       endif
    endif
    !
end subroutine lmp_update
!
! PCApply (Right) operation used for the PCShell based on the LMP Method
!----------------------------------------------------------------
!
!   APPLICATION DU LMP: y=(I+zz_1*yy_1^T)...(I+zz_lmp*yy_lmp^T) x
!
! ---------------------------------------------------------------
!
subroutine lmp_apply_right ( lmp_pc, x, y, ierr )
    !
    use lmp_data_module, only : lmp_context, lmp_is_setup
    !
    ! Dummy arguments 
    !
    PC, intent(in)    :: lmp_pc
    Vec, intent(in)   :: x
    Vec, intent(inout):: y
    PetscErrorCode, intent(out) :: ierr
    !
    ! Local Variables 
    PetscInt :: jj
    !
    ! --  Copie du vecteur d'entree
    call VecCopy(x, y, ierr)
    if ( lmp_is_setup ) then 
    ! --  sk=yy^T x
    do jj = 1,lmp_context%ritzeff
      call VecDot(lmp_context%yy(jj),y,lmp_context%sk(jj),ierr)
      ASSERT( ierr == 0 ) 
    enddo 
! --  y=I+zz*sk
    call VecMAXPY(y,lmp_context%ritzeff,lmp_context%sk,lmp_context%zz,ierr)
    ASSERT( ierr == 0 )
    endif
    !
end subroutine lmp_apply_right
!
subroutine lmp_destroy( lmp_pc, ierr )
    !
    use lmp_data_module, only : lmp_context, lmp_is_setup
    !
    ! Dummy arguments 
    !
    PC, intent(inout)    :: lmp_pc
    PetscErrorCode, intent(out) :: ierr
    !
    call free_lmp_context( lmp_context )
    lmp_is_setup = .false.
    !
    ierr = 0 
    !
end subroutine lmp_destroy
!
#else
contains 
!
subroutine lmp_update( lmp_pc, ksp, ierr )
    !
    ! Dummy arguments 
    integer  :: lmp_pc 
    integer  :: ksp
    integer  :: ierr
    lmp_pc = 0 
    ksp = 0 
    ierr = 0
    ASSERT(.false.)
end subroutine lmp_update
!
subroutine lmp_apply_right ( lmp_pc, x, y, ierr )
    integer :: lmp_pc
    integer :: x
    integer :: y
    integer :: ierr
    lmp_pc = 0 
    x = 0
    y = 0 
    ierr = 0
    ASSERT(.false.)
end subroutine lmp_apply_right
!
subroutine lmp_destroy( lmp_pc, ierr )
    integer :: lmp_pc
    integer :: ierr
    lmp_pc = 0 
    ierr = 0
    ASSERT( .false.)
end subroutine lmp_destroy
#endif
end module lmp_module
