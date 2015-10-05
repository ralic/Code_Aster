    subroutine matset_to_identity( mat_a , idrow_c, idcol_c )
      implicit none
!
! person_in_charge: natacha.bereux at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
!
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
!
      Mat, intent(inout)                                     :: mat_a
      PetscInt, dimension(:), intent(in), target, optional   :: idrow_c
      PetscInt, dimension(:), intent(in), target, optional   :: idcol_c
! ======================================================================
      PetscScalar, parameter :: rone=1.d0, rzero=0.d0
      PetscInt, parameter  :: ione=1
      PetscInt :: ncols
      PetscErrorCode :: ierr
      PetscInt :: nrow, ncol, nrow_a, ncol_a, ii, kk, ll, i_c, j_c  
      PetscInt, dimension(:), allocatable :: cols
      PetscScalar, dimension(:), allocatable :: vals
      PetscInt, dimension(:), pointer :: idrow_loc_c => null()
      PetscInt, dimension(:), pointer :: idcol_loc_c => null()
! ----------------------------------------------------------------------
!
! Vérifications 
    if (present( idrow_c )) then 
        ASSERT( size( idrow_c ) > 0 ) 
    endif
    if (present( idcol_c )) then 
        ASSERT( size( idcol_c ) > 0 ) 
    endif
! Taille de la matrice mat_a
    call MatGetSize(mat_a, nrow_a, ncol_a, ierr)
    ASSERT( ierr == 0 )
!
! Plages d'indices lignes & colonnes définissant
! le bloc de mat_a à réinitialiser
!
    if (present( idrow_c )) then 
      nrow = size( idrow_c )
      idrow_loc_c => idrow_c
    else
      nrow =nrow_a 
      allocate( idrow_loc_c( nrow_a ) , stat = ierr )
      ASSERT( ierr == 0 ) 
      do ii = 1, nrow_a
        idrow_loc_c( ii ) = ii - 1 
      enddo 
    endif
   if (present( idcol_c )) then
      ncol = size( idcol_c )
      idcol_loc_c => idcol_c
    else
      ncol =ncol_a 
      allocate( idcol_loc_c( ncol_a ) , stat = ierr )
      ASSERT( ierr == 0 ) 
      do ii = 1, nrow_a
        idcol_loc_c( ii ) = ii - 1 
      enddo
    endif     
!
!  Si on ne précise pas quelles colonnes sont à remettre à jour, c'est facile, il suffit 
!  de faire appel à MatZeroRows 
!     
    if (.not. present( idcol_c )) then
      call MatSetOption(mat_a,MAT_KEEP_NONZERO_PATTERN, petsc_true, ierr)
      call MatZeroRows(mat_a, nrow, idrow_loc_c, 1.d0, petsc_null_object, petsc_null_object, ierr)
      call MatSetOption(mat_a,MAT_KEEP_NONZERO_PATTERN, petsc_false, ierr)
    else
!
! Sinon, on traite la matrice ligne par ligne 
! Pour chaque ligne il faut vérifier si l'insertion demandée est 
! compatible avec la structure creuse et mettre à 0/1 les termes 
! concernés 
!
    allocate( cols( ncol_a ) , stat = ierr )
    ASSERT( ierr == 0 ) 
    allocate( vals( ncol_a ) , stat = ierr )
    ASSERT( ierr == 0 ) 
    do  kk=1 , nrow
        i_c= idrow_loc_c( kk ) 
        call MatGetRow( mat_a, i_c, ncols, cols, vals, ierr )
! TODO optimiser la détermination des indices 
        do ll = 1, ncol
            j_c = idcol_loc_c( ll ) 
            if ( any( j_c == cols ) ) then
                if ( i_c == j_c ) then 
                ! 1 sur la diagonale 
                   call MatSetValues(mat_a, ione ,[i_c], &
                      ione, [j_c], [rone], &
                      INSERT_VALUES, ierr)
                else
                ! 0 ailleurs
                   call MatSetValues(mat_a, ione ,[i_c], &
                      ione, [j_c], [rzero], &
                      INSERT_VALUES, ierr)
                endif
            endif
         enddo
      enddo
    endif
    call MatAssemblyBegin( mat_a, MAT_FINAL_ASSEMBLY, ierr )
    call MatAssemblyEnd( mat_a, MAT_FINAL_ASSEMBLY, ierr ) 
!
! Libération de la mémoire 
!
    if ( associated ( idrow_loc_c ) ) then 
        deallocate( idrow_loc_c ) 
        nullify(  idrow_loc_c )
    endif
    if ( associated ( idcol_loc_c ) ) then 
        deallocate( idcol_loc_c ) 
        nullify(  idcol_loc_c )
    endif
    if ( allocated( cols ) ) then 
        deallocate( cols )
    endif
    if ( allocated( vals ) ) then 
        deallocate( vals )
    endif
!
#else
      integer :: mat_a
      integer, dimension(:), intent(inout) :: idrow_c, idcol_c
      ASSERT(.false.)
#endif
  end subroutine matset_to_identity
