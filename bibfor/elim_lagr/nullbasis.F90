    subroutine nullbasis( mat_c, mat_z, nbnvco, nvco_c)
!
      implicit none
!
! person_in_charge: natacha.bereux at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/check_nullbasis.h" 
#include "asterfort/elg_nllspc.h"
#include "asterfort/infniv.h"
#include "asterfort/matset_to_identity.h"
#include "asterfort/skeleton_of_nullbasis.h" 
!
!----------------------------------------------------------------
!
!     Construction de la base du noyau de la matrice mat_c  
!     En sortie mat_z contient les vecteurs de la base du noyau. 
!     La construction s'effectue ligne par ligne de mat_c
!     A chaque nouvelle ligne (= nouvelle contrainte) 
!     on met à jour mat_z de façon à ce que 
!     ses colonnes soient orthogonales à cette nouvelle contrainte, 
!     tout en restant orthogonales aux lignes précédentes. 
!   
!     Pour certaines lignes, la mise à jour de mat_z peut échouer
!     => on dit que la contrainte concernée est "non vérifiée"
!     En sortie  
!     nbnvco est le nombre de contraintes non vérifiées 
!     et nvco_c contient les indices (en convention C) des lignes 
!     de mat_c qui contiennent les contraintes non vérifiées.  
!     
! ---------------------------------------------------------------
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
!
      Mat, intent(in)                       :: mat_c
      Mat, intent(out)                      :: mat_z 
      PetscInt, intent(out)                 :: nbnvco
      PetscInt, dimension(:), intent(inout) :: nvco_c
!
!================================================================
!
      real(kind=8) :: eps
      integer :: ifm, niv
      aster_logical :: verbose, is_ok
      aster_logical, dimension(:), allocatable :: is_constrained
      mpi_int :: mpicomm
      Vec :: vec_c, vec_cz, c_elim, c_free, cz_elim
      Mat :: z_elim_elim
      IS :: is_elim, is_free
      PetscOffset :: xx_i, nn_i
! Mimics a C pointer
      PetscScalar, dimension(1) :: xx_v
      PetscInt :: nn_is(1)
      PetscInt :: nnz, ncons, nfree, nelim
      PetscInt :: mc, nc, ic, ii, jj, kk, ncol
      PetscInt :: nnzmax_row, loop
      PetscInt :: maxloc_free_c
      PetscErrorCode :: ierr
      PetscInt, dimension(:), allocatable :: col_c, colnorm_c
      PetscScalar, dimension(:), allocatable :: val, valnorm
      PetscInt, dimension(:), allocatable ::  idfree_c, idelim_c
      PetscScalar :: norm_v, norm_row, valmax
      PetscScalar, parameter :: tol=1.d-10
      PetscScalar, parameter :: rone=1.d0, rzero=0.d0
      PetscInt, parameter  :: ione=1
      PetscScalar, dimension(:,:), allocatable  :: nullbasis_of_vec 
    !----------------------------------------------------------------------
      eps=r8prem()
      call asmpi_comm('GET_WORLD', mpicomm)
      call infniv(ifm, niv)
      verbose=(niv == 2)
!
!   La matrice des contraintes mat_c est de taille mc x nc   
!   - mc est le nombre de contraintes, 
!   - nc est le nombre de degrés de liberté "physiques"
    call MatGetSize(mat_c, mc, nc, ierr)
    ASSERT(ierr == 0 ) 
    if (verbose) then
        write(ifm,*),' -- Numerical assembly of Z, such that CZ = 0 '
    endif
!    Allocation de la matrice mat_z (noyau de la matrice mat_c)
!    On précalcule le profil de mat_z 
!    et on remplit mat_z de zéros (si on n'affecte pas de valeur
!    à un terme d'une matrice Petsc et qu'on appelle un 
!    MatAssembly, le terme est supprimé du profil de la matrice)  
    call skeleton_of_nullbasis( mat_c, mat_z, nnzmax_row ) 
!   Sert à conserver la structure creuse lors des appels à MatZeroRows
    call MatSetOption(mat_z,MAT_KEEP_NONZERO_PATTERN,PETSC_TRUE, ierr) 
    call matset_to_identity( mat_z ) 
!
    allocate( val(nc), stat = ierr)
    ASSERT( ierr == 0 )
    allocate( col_c ( nc ), stat = ierr )
    ASSERT( ierr == 0 )   
    allocate( valnorm( nc), stat = ierr ) 
    ASSERT( ierr == 0 )   
    allocate( colnorm_c( nnzmax_row ), stat = ierr ) 
    ASSERT( ierr == 0 )
    allocate( idfree_c( nnzmax_row ), stat = ierr )
    ASSERT( ierr == 0 )
    allocate(idelim_c(nnzmax_row), stat = ierr )
    ASSERT( ierr == 0 )
    allocate(nullbasis_of_vec(nnzmax_row, nnzmax_row), stat=ierr) 
    ASSERT( ierr == 0 )
!   is_constrained(ii)=true si le degré de liberté ii 
!   est fixé par une contrainte déjà éliminée
    allocate(is_constrained (nc), stat = ierr) 
    ASSERT( ierr == 0) 
    is_constrained(:)=.false.
!
!--------------------------------!
!--                            --!
!-- Boucle sur les contraintes --!
!--                            --!
!--------------------------------!
!
!   Number of non-verified constraints
    nbnvco = 0 
    nvco_c(:) = 0  
!
    do loop = 1, 2
    do ic = 1, mc
    if (verbose) write(ifm,*),' '
    if (verbose) write(ifm,*),' '
    if (verbose) write(ifm,'(A14,I3)'),' CONTRAINTE : ',ic
    if (verbose) write(ifm,*),' '
!
! On lit la ligne ic de la matrice C 
!
    call MatGetRow(mat_c, to_petsc_int(ic-1), ncol, col_c, val,&
            ierr)
!
! Copie, normalisation et filtrage des termes nuls de C(ic,:) 
! => contrainte courante (colnorm_c & valnorm)
! 
    norm_row = sqrt( dot_product(val(1:ncol), val(1:ncol)) )
! TODO 
    ASSERT( norm_row > eps ) 
    nnz = 0
    do ii = 1, ncol  
        if ( abs(val( ii )/norm_row)  > eps ) then 
          nnz = nnz + 1 
          valnorm( nnz )   = val ( ii )/norm_row
          colnorm_c( nnz ) = col_c( ii) 
        endif
    enddo 
! Libération de la ligne ic de la matrice C
    call MatRestoreRow( mat_c, to_petsc_int(ic-1), ncol, col_c, val,&
            ierr)
!
! On copie la contrainte courante dans un vecteur PETSc
!   
    call VecCreateSeq( mpicomm, nc, vec_c, ierr)
    call VecSetValues( vec_c, nnz, colnorm_c, valnorm, INSERT_VALUES, ierr )
    call VecAssemblyBegin( vec_c, ierr)
    call VecAssemblyEnd( vec_c, ierr)
! 
! La contrainte courante est-elle déjà orthogonale à Z ? 
!
    is_ok = check_nullbasis( vec_c, mat_z, tol )
    if ( is_ok )  then
      if (verbose) write(ifm,*),' CONTRAINTE DEJA VERIFIEE'
! il n'y a rien à faire, on passe à la contrainte suivante 
      else
! Sinon, on enrichit mat_z pour qu'elle soit orthogonale à 
! la contrainte courante        
! 
! >>>>>>
! Cas 1 : la contrainte a un seul terme non-nul
! <<<<<<
    if ( nnz == 1 ) then 
! on vérifie que l'unique degré de liberté impliqué n'est pas 
! déjà contraint (sinon, comme la contrainte n'est pas orthogonale 
! à Z, il y a une incompatibilité)
      ASSERT(.not. is_constrained ( colnorm_c( nnz ) + 1 ) )
! la contrainte est colinéaire à un vecteur de la base standard 
! => ce vecteur n'est pas dans la base du noyau, on l'enlève de Z  
      is_constrained ( colnorm_c( nnz ) + 1 ) = .true.
      call MatSetValues( mat_z, ione, [colnorm_c(nnz)] , ione,&
                [colnorm_c(nnz)], [rzero],&
                INSERT_VALUES, ierr)
       if (verbose) write(ifm,*),'CONTRAINTE N° ', ic, ' BIEN ELIMINEE - '
    else
! On traite ce cas seulement au second parcours (loop=2)
    if (loop == 2) then 
! >>>>>> 
! Cas 2:  la contrainte a plusieurs termes non-nuls 
! <<<<<<
! On partitionne les indices des termes non-nuls 
! en deux ensembles : 
! ceux qui ont été impliqués dans une contrainte précédemment 
! éliminée et ceux que l'on n'a jamais rencontré
! 
    ncons = 0 
    nfree = 0 
    valmax = 0.d0
    maxloc_free_c = 0 
!       
    do jj = 1, nnz
        if ( is_constrained( colnorm_c( jj ) + 1 ) ) then 
          ncons = ncons + 1 
        else 
          nfree = nfree + 1 
          idfree_c( nfree ) = colnorm_c( jj )
          if ( abs(valnorm( jj )) > abs(valmax) ) then 
          valmax =  valnorm( jj )
          maxloc_free_c =   colnorm_c( jj )
          endif
        endif
    enddo
    ASSERT( nfree + ncons == nnz ) 
    call ISCreateGeneral(mpicomm, nfree,idfree_c(1:nfree),                   &
    &                     petsc_copy_values, is_free, ierr)
    
!
! ----------------------------------------------------------------------------
! Les degrés de liberté impliqués dans la contrainte n'ont jamais 
! été rencontrés auparavant 
    if ( ncons == 0 ) then 
! ----------------------------------------------------------------------------
! on construit une base de la contrainte courante
! stockée dans une matrice dense.  
        call elg_nllspc( to_aster_int(nnz), valnorm(1:nnz), nullbasis_of_vec(1:nnz, 1:nnz) )
        
! On met à zéro les termes diagonaux (Z est initialisée à l'identité)
        do ii = 1, nnz
            call MatSetValues(mat_z, ione, [colnorm_c(ii)] , ione,&
                    [colnorm_c(ii)], [rzero],&
                    INSERT_VALUES, ierr)
        enddo
! On insère dans mat_z les termes non-nuls de nullbasis_of_vec
           do jj = 1, nnz 
            do ii = 1, nnz 
              if ( abs(nullbasis_of_vec(ii,jj)) > eps ) then 
                call MatSetValues(mat_z, ione, [colnorm_c(ii)] , ione,&
                    [colnorm_c(jj)], [nullbasis_of_vec(ii,jj)],&
                    INSERT_VALUES, ierr)
              endif
            enddo
        enddo
        call MatAssemblyBegin(mat_z, MAT_FINAL_ASSEMBLY, ierr)
        call MatAssemblyEnd(mat_z, MAT_FINAL_ASSEMBLY, ierr)
!
! La contrainte a-t-elle été bien eliminee ? 
!
      is_ok = check_nullbasis( vec_c, mat_z, tol )
!      
      if ( .not. is_ok )  then
!--
!-- On ne devrait, normalement, jamais passer par la
!--    => voir note cas NBLIB < NBNZ
!--
        if (verbose) write(ifm,*),'CAS NCONS = 0'
        if (verbose) write(ifm,*),'CONTRAINTE N° ', ic, ' MAL ELIMINEE - '
        nbnvco=nbnvco+1
        nvco_c(nbnvco) = ic -1                     
!--
!-- On reinitialise Z_free_free  à l'identité 
!--
        call matset_to_identity( mat_z, idfree_c, idfree_c )
!
      else
! La contrainte courante vec_c a été éliminée 
! (vec_c est orthogonal à mat_z  ) 
! il reste à enregistrer les indices des degrés de liberté impliqués 
! dans cette contrainte 
          is_constrained( idfree_c(1: nfree) +1 ) = .true.
!
          if (verbose) write(ifm,*),'CAS NCONS = 0'
          if (verbose) write(ifm,*),'CONTRAINTE N° ', ic, ' BIEN ELIMINEE - '
      endif
! ---------------------------------------------------------------
    else 
! ncons > 0 et nfree > 0
! la contrainte courante implique à la fois des degrés de liberté 
! "constrained" déjà rencontrés  au cours des éliminations 
! précédentes et des degrés de liberté "free", encore jamais 
! rencontrés. 
    if ( abs(valmax) > 1.d-10 ) then 
!--
!-- Partie Z(free,elim) = -C(ic,free) \ (C(ic,cons) * Z(cons,elim)) 
!--
!
! is_elim = index set désignant les indices colonnes de C qui sont impliqués dans 
! une contrainte déjà éliminée.
      nelim = 0 
      idelim_c(:)= 0
      do ii=1, nc
        if ( is_constrained( ii )) then
        nelim = nelim +1  
        idelim_c( nelim ) = ii - 1
        endif
      enddo   
      call ISCreateGeneral(mpicomm, to_petsc_int(nelim), idelim_c(1:nelim),&
                      PETSC_USE_POINTER, is_elim, ierr)
! Tri de is_elim  
      call ISSort(is_elim, ierr)
! On extrait de la contrainte courante le vecteur correspondant à is_elim 
      call VecGetSubVector(vec_c, is_elim, c_elim, ierr)
! Et de la matrice Z la sous-matrice Z_elim_elim correspondant à is_elim x is_elim 
      call MatGetSubMatrix(mat_z, is_elim, is_elim, MAT_INITIAL_MATRIX, z_elim_elim,&
                      ierr)
! Calcul de cz_elim = c_elim * z_elim_elim
#ifdef ASTER_PETSC_VERSION_LEQ_35
      call MatGetVecs(z_elim_elim, PETSC_NULL_OBJECT, cz_elim, ierr)
#else
      call MatCreateVecs( z_elim_elim, PETSC_NULL_OBJECT, cz_elim, ierr)
#endif
      ASSERT( ierr == 0 ) 
      call MatMultTranspose(z_elim_elim, c_elim, cz_elim, ierr)
      call MatDestroy(z_elim_elim, ierr)
      call VecDestroy(c_elim, ierr)
! 

! Le bloc Z_free_elim doit satisfaire le pb (sous-déterminé)
! c_free * Z_free_elim = -cz_elim
! Soit maxloc_free l'indice colonne du plus grand terme de c_free 
! Z_free_elim(maxloc_free,:) = -cz_elim / c(ic,maxloc_free)

      call VecScale(cz_elim, -1./valmax, ierr)
! Insertion des valeurs de cz_elim à la ligne imax de Z 
      call VecGetArray(cz_elim, xx_v, xx_i, ierr)
      call ISGetIndices(is_elim, nn_is, nn_i, ierr)
      call MatSetValues(mat_z, ione, [maxloc_free_c], to_petsc_int(nelim), nn_is(nn_i+1),&
                xx_v(xx_i+1), INSERT_VALUES, ierr)
      call ISRestoreIndices(is_elim, nn_is, nn_i, ierr)
      call ISDestroy(is_elim, ierr)
      call VecRestoreArray(cz_elim, xx_v, xx_i, ierr)
      call VecDestroy(cz_elim, ierr)
!
!--
!-- Construction du bloc Z_free_free 
!--
! On extrait de la contrainte courante le vecteur correspondant à is_free
      call VecGetSubVector(vec_c, is_free, c_free, ierr)
      call VecGetArray(c_free, xx_v, xx_i, ierr)
! 
      call elg_nllspc( to_aster_int(nfree), xx_v(xx_i+1: xx_i+nfree), &
      nullbasis_of_vec(1:nfree, 1:nfree) )
      call VecRestoreArray(c_free, xx_v, xx_i, ierr)
!
! Insertion dans mat_z
! On met à zéro les termes diagonaux (Z est initialisée à l'identité)
        do ii = 1, nfree
            call MatSetValues(mat_z, ione, [idfree_c(ii)] , ione,&
                    [idfree_c(ii)], [rzero],&
                    INSERT_VALUES, ierr)
        enddo
      do ii = 1, nfree
        do jj = 1, nfree
         if ( abs( nullbasis_of_vec(ii,jj) ) > eps ) then 
          call MatSetValues(mat_z, ione, [ idfree_c(ii)], &
               ione, [idfree_c(jj)], &
               [nullbasis_of_vec(ii,jj)], INSERT_VALUES, ierr)
         endif           
           end do
      end do
!
     call MatAssemblyBegin(mat_z, MAT_FINAL_ASSEMBLY, ierr)
     call MatAssemblyEnd(mat_z, MAT_FINAL_ASSEMBLY, ierr)
!
!
! La contrainte a-t-elle été bien eliminee ? 
!
     is_ok = check_nullbasis( vec_c, mat_z, tol )
!
     if ( .not. is_ok )  then
!
     if (verbose) write(ifm,*),'CAS NCONS > 0 ET NFREE > 0 '
     if (verbose) write(ifm,*),'CONTRAINTE N° ', ic, ' MAL ELIMINEE - '
     nbnvco=nbnvco+1
     nvco_c(nbnvco) = ic -1
!--
!-- On réinitialise la sous-matrice Z_free_free à l'identité 
!--
    call matset_to_identity( mat_z, idfree_c, idfree_c )
!
!-- On réinitialise aussi Z_free_elim à zéro 
!
    call MatSetValues(mat_z, ione, [maxloc_free_c], nelim, idelim_c(1:nelim),&
                (/( rzero, jj=1,nelim)/), INSERT_VALUES, ierr )
    call MatAssemblyBegin(mat_z, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(mat_z, MAT_FINAL_ASSEMBLY, ierr)
!
! La contrainte courante vec_c a été éliminée 
      else
! On marque les degrés de liberté "free" comme désormais "constrained"
        is_constrained( idfree_c(1: nfree) +1 ) = .true.
!
        if (verbose) write(ifm,*),'CAS NCONS > 0 ET NFREE > 0'
        if (verbose) write(ifm,*),'CONTRAINTE N° ',ic,' BIEN ELIMINEE - '
      endif
!--
!-- On ne peut pas eliminer correctement la contrainte -> On sort
!--
! if valmax < 1.d-10
    else 
      nbnvco=nbnvco+1
      nvco_c(nbnvco) = ic -1
      if (verbose) then
          write(ifm,*),'LA CONTRAINTE N° ', ic,' N''A PAS PU ETRE ELIMINEE'
      endif 
    endif 
   ! if ( c_is_orth_to_z ) 
   endif 
endif
!  
  endif
  call MatAssemblyBegin(mat_z, MAT_FINAL_ASSEMBLY, ierr)
  call MatAssemblyEnd(mat_z, MAT_FINAL_ASSEMBLY, ierr)
 endif
enddo
! Boucle loop
enddo
!
!
! Libération de la mémoire
deallocate( is_constrained, stat=ierr)
ASSERT( ierr == 0 )
deallocate( idfree_c, stat=ierr)
ASSERT( ierr == 0 )
deallocate( idelim_c, stat=ierr)
ASSERT( ierr == 0 )
deallocate( nullbasis_of_vec, stat=ierr)
ASSERT( ierr == 0 )
deallocate( valnorm, stat=ierr)
ASSERT( ierr == 0 )
deallocate( val, stat=ierr)
ASSERT( ierr == 0 )
deallocate( col_c, stat=ierr)
ASSERT( ierr == 0 )
deallocate( colnorm_c, stat=ierr)
ASSERT( ierr == 0 )

#else
      integer :: mat_c, mat_z 
      integer :: nbnvco
      integer, dimension(:), intent(inout) :: nvco_c
      ASSERT(.false.)
#endif
  end subroutine nullbasis
