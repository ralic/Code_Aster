subroutine compress_sparse_pattern(a)
    implicit none
! person_in_charge: natacha.bereux at edf.fr
! aslint: disable=W0104
!
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
#include "asterfort/infniv.h"
!
!--------------------------------------------------------------
! BUT : - optimiser le profil d'une matrice A en enlevant les termes 
!       numériquement nuls du profil de A
!       A garde la même taille (maxna) mais le nombre de termes non-nuls
!       décroît
!
! INOUT  : A de taille ma x na 
!---------------------------------------------------------------
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"

    Mat, intent(inout)  :: a
!    
!================================================================
!
!     VARIABLES LOCALES
!
    integer :: ifm, niv
    aster_logical :: verbose
    mpi_int :: mpicomm    
    PetscInt :: ii, jj, ma, na, passe, ncols_a
    PetscErrorCode :: ierr
    PetscInt :: inz, nanz, nnzmax_a, nnz_a
    PetscInt, parameter :: ione = 1 , izero = 0 
    PetscInt, dimension(:), allocatable :: nnz_acompressed, cols
    PetscReal, dimension(:), allocatable :: vals
    Mat :: acompressed
!----------------------------------------------------------------
   call asmpi_comm('GET_WORLD', mpicomm)
   call infniv(ifm, niv)
   verbose=(niv == 2)
!
    call MatGetSize( a, ma, na, ierr) 
    ASSERT(ierr == 0 )
!
! On filtre le profil de A pour ne garder que les termes numériquement 
! non-nuls
!
!   -- nnzmax_a is the maximum number of nonzeros in any row of A  
    nnzmax_a=na
!   call MatSeqAIJGetMaxRowNonzeros( a, nnzmax_a, ierr )
!   -- allocations
   allocate( nnz_acompressed(ma), stat = ierr )
   ASSERT( ierr == 0 )      
   allocate( cols( nnzmax_a ), stat = ierr )
   ASSERT( ierr == 0 )  
   allocate( vals( nnzmax_a ), stat = ierr )
   ASSERT( ierr == 0 )  
!
!   -- filtrage de A => Acompressed en deux passes  
!      passe 1: on compte, 
!      passe 2: on remplit 
   do passe = 1, 2 
!   
   nnz_a = 0 
   if ( passe == 2 ) then 
    call MatCreateSeqAIJ( mpicomm , ma, na, petsc_default_integer, &
    nnz_acompressed, acompressed, ierr ) 
    ASSERT(ierr == 0 )
   endif
   !
   do ii = 1, ma
    call MatGetRow( a, to_petsc_int(ii-1) ,ncols_a, cols, vals, ierr )
    nnz_a = nnz_a + ncols_a
    if ( passe == 1 ) then 
      nnz_acompressed( ii ) = count ( abs(vals(1:ncols_a)) > r8prem() )
    else if ( passe == 2 ) then 
      do jj = 1, ncols_a 
         if ( abs(vals( jj )) > r8prem() ) then
            call MatSetValues( acompressed, ione, [to_petsc_int(ii-1)], &
                ione, [cols(jj)], [vals(jj)] , INSERT_VALUES, ierr )
         endif
      enddo
    endif 
    call MatRestoreRow( a, to_petsc_int(ii-1) ,ncols_a, cols, vals, ierr )
   enddo                        
   !   
   enddo
   call MatAssemblyBegin(acompressed, MAT_FINAL_ASSEMBLY, ierr)
   call MatAssemblyEnd(acompressed, MAT_FINAL_ASSEMBLY, ierr)
!
!  On détruit A 
    call MatDestroy(a, ierr)
!  On copie Acompressed en A
    call MatDuplicate( acompressed, MAT_COPY_VALUES, a, ierr )
! On détruit Acompressed
    call MatDestroy(acompressed, ierr)
!
    if (verbose) then
        write(ifm,*) " -- COMPRESSION DU STOCKAGE MATRICIEL "
        write(ifm,*) "    TAILLE DE LA MATRICE : ",ma," LIGNE(S) , ", na," COLONNE(S)" 
        write(ifm,*) "    EN ENTREE, NNZ:     : ", nnz_a
        write(ifm,*) "    EN SORTIE, NNZ:     : ", sum( nnz_acompressed )
    endif
!
! Libération de la mémoire
!
    deallocate( cols , stat = ierr )
    ASSERT( ierr == 0 )
    deallocate( vals , stat = ierr )
    ASSERT( ierr == 0 )
    deallocate( nnz_acompressed , stat = ierr )
    ASSERT( ierr == 0 )
 
#else
    integer, intent(in)  :: a
    ASSERT(.false.)
#endif
!
end subroutine
