module elim_lagr_comp_module
!
!!----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!----------------------------------------------------------------
!
! person_in_charge: natacha.bereux at edf.fr
! aslint:disable=C1308
!
use elim_lagr_context_class
use elim_lagr_data_module
use petsc_data_module 
use saddle_point_context_class
!
implicit none 
!
private 
#include "asterf.h"
#include "asterf_petsc.h"
#include "asterc/getres.h"
#include "asterc/r8prem.h"
#include "asterfort/apalmc.h"
#include "asterfort/apmamc.h"
#include "asterfort/apmain.h"
#include "asterfort/assert.h"
#include "asterfort/compress_sparse_pattern.h"
#include "asterfort/extract_nonzero_col.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/nullbasis.h"
#include "asterfort/utmess.h"
!
public ::  build_elim_lagr_context
!
contains 
!
#ifdef _HAVE_PETSC 
subroutine build_elim_lagr_context( full_matas )
   ! 
    ! Dummy arguments 
    character(len=19), intent(in) :: full_matas 
    ! 
    ! Local variables
    !
    integer :: iret, ii
    integer :: kptsc, kptscr 
    character(len=3) :: matd
    character(len=19) :: kbid
    PetscInt  ::   is_array(1)
    PetscOffset ::  i_is
    integer, dimension(:), allocatable :: idphys_c  
    type(saddle_point_context_type)    :: sp_ctxt
    type(elim_lagr_context_type), pointer :: elg_ctxt => null()
    PetscErrorCode :: ierr  
    !    
    ! Retrouve l'identifiant de l'objet elim_lagr_context associe 
    ! a la matrice full_matas 
    call elg_gest_data('CHERCHE', full_matas, ' ' , ' ')
    ! Alias vers cet objet
    elg_ctxt => elg_context( ke )  
    ! On recherche egalement l'identifiant de la matrice PETSc 
    ! associée à full_matas 
    kptsc = get_mat_id( full_matas )  
    !
    ! Pour l'instant on interdit les matrices distribuées
    call dismoi('MATR_DISTRIBUEE', full_matas, 'MATR_ASSE', repk=matd)
    ASSERT( matd == 'NON' ) 
    !
    ! -------------------------------------------------------------------
    ! Creation d'un clone PETSc de la matrice (matr_asse) definissant 
    ! le systeme aster complet (avec doubles multiplicateurs de Lagrange) 
    ! -------------------------------------------------------------------
    ! La matrice est deja enregistree et possede l'id kptsc  
    ! On prealloue la matrice PETSc correspondante
    call apalmc(kptsc)
    ! On copie les valeurs de la matr_asse dans la matrice PETSc
    call apmamc(kptsc)
    ! et on assemble 
    call MatAssemblyBegin(ap(kptsc), MAT_FINAL_ASSEMBLY, ierr)
    ASSERT(ierr==0)
    call MatAssemblyEnd(ap(kptsc), MAT_FINAL_ASSEMBLY, ierr)
    ASSERT(ierr==0)
    !
    ! Parfois (si matrice de masse ou amortissement), il faut aller chercher 
    ! les relations lineaires dans une autre matrice 
    !
    kptscr = kptsc 
    if ( elg_ctxt%k_matas /= " ") then  
       kptscr = get_mat_id( elg_ctxt%k_matas ) 
    ASSERT( kptscr /= 0 )  
    endif
    !
    ! -------------------------------------------------------------------
    ! Calcul d'un saddle_point_context 
    ! -------------------------------------------------------------------
    !
    sp_ctxt = new_saddle_point_context( full_matas, ap(kptsc), ap(kptscr) )
    !  et on libere le clone PETSc de full_matas 
    kbid = repeat(" ",19)
    call  apmain('DETR_MAT', kptsc, [0.d0], kbid, 0, iret)
    ! -------------------------------------------------------------------
    ! Remplissage de elg_ctxt
    ! -------------------------------------------------------------------
    !
    ! on utilise sp_ctxt pour definir les matrices ctrans et matb 
    call MatConvert( sp_ctxt%k_mat, MATSAME, MAT_INITIAL_MATRIX, &
         elg_ctxt%matb, ierr)
    ASSERT( ierr == 0 ) 
    call MatTranspose(sp_ctxt%c_mat, MAT_INITIAL_MATRIX, &
        elg_ctxt%ctrans , ierr )
    ASSERT( ierr == 0 ) 
    elg_ctxt%nphys = sp_ctxt%nphys
    elg_ctxt%nlag = sp_ctxt%nlag1
    !
    ! -- necessaire pour tfinal
    ! idphys_c(k) = indice (C) global du kième ddl physique. 
    allocate( idphys_c( elg_ctxt%nphys ) )
    idphys_c(:) = 0 
    call ISGetIndices(sp_ctxt%is_phys,is_array,i_is,ierr)
    ASSERT(ierr == 0 ) 
    !
    do ii=1, elg_ctxt%nphys
        idphys_c(ii)= is_array(i_is+ii)
    enddo 
    call ISRestoreIndices(sp_ctxt%is_phys,is_array,i_is,ierr)
    ASSERT(ierr == 0 )
    ! -------------------------------------------------------------------
    ! Liberation de la memoire
    ! -------------------------------------------------------------------
    call free_saddle_point_context( sp_ctxt )
    !
    call build_tfinal( idphys_c, elg_ctxt )  
    !
    !   -- Projection T'*(MatB*T) :
    !   -----------------------------
    call MatPtAP(elg_ctxt%matb, elg_ctxt%tfinal, MAT_INITIAL_MATRIX, 1.d0, &
        elg_ctxt%kproj, ierr)
    ASSERT( ierr == 0 )
   !
   deallocate( idphys_c )
   !
end subroutine build_elim_lagr_context
!
!  -- Allocation et remplissage de Tfinal, 
!     qui servira pour la projection de la matrice 
!
subroutine build_tfinal( idphys_c, elg_ctxt ) 
    ! Dummy arguments 
    integer, dimension(:), intent(in) :: idphys_c 
    type(elim_lagr_context_type), intent(inout) :: elg_ctxt
     
    ! Local variables
    character(len=16) :: concep, nomcmd
    character(len=8)  :: k8b
    aster_logical :: verbose
    integer :: i1, j1, restart, ifm, niv
    real(kind=8) :: eps
    PetscInt :: nbelig
    PetscInt :: nbnvco, i 
    PetscInt, parameter  :: izero=0, ione = 1
    PetscScalar, parameter :: rzero=0.d0, rone = 1.d0
    PetscInt, dimension(:), allocatable :: non_verified_cons_c
    PetscInt, dimension(:), pointer :: icolnz_c 
    PetscReal :: aster_petsc_default_real  
    PetscReal, dimension(:), allocatable :: norms_c, norms_ct
    PetscErrorCode :: ierr 
    Mat :: mat_c, mat_ct, mat_tmp, mat_t
    IS :: isall, isnvco
    mpi_int :: mpicomm
    !
    call asmpi_comm('GET_WORLD', mpicomm)
    call infniv(ifm, niv)
    verbose= ( niv == 2 )
    !
#ifdef ASTER_PETSC_VERSION_LEQ_34
    aster_petsc_default_real = PETSC_DEFAULT_DOUBLE_PRECISION
#else
    aster_petsc_default_real = PETSC_DEFAULT_REAL
#endif 
    !
    ! Allocation et initialisation de T à l'identite
    call MatDuplicate( elg_ctxt%matb, MAT_DO_NOT_COPY_VALUES, &
        &     elg_ctxt%tfinal, ierr )
    ASSERT( ierr == 0 ) 
    call MatZeroRows( elg_ctxt%tfinal, elg_ctxt%nphys, &
       (/ (i, i=0, elg_ctxt%nphys-1)/), &
     &    rone, PETSC_NULL_OBJECT, PETSC_NULL_OBJECT, ierr ) 
    ASSERT( ierr == 0 ) 
!
!----------------------------------------------------!
!--                                                --!
!-- On boucle tant que les colonnes de Tfinal ne   --!
!-- sont pas orthogonales à toutes les contraintes --!
!-- ie à toutes les lignes de C                    --!
!--                                                --!
!----------------------------------------------------!
!
!
!   -- C est initialisée en transposant Ctrans 
!
    call MatTranspose(elg_ctxt%ctrans, MAT_INITIAL_MATRIX, mat_c, ierr)
    ASSERT( ierr == 0 )
! Number of non-verified constraints
    nbnvco  = elg_ctxt%nlag
    allocate( non_verified_cons_c( elg_ctxt%nlag ), stat = ierr )
    ASSERT( ierr == 0 ) 
    non_verified_cons_c(:)= izero
! Nombre de tours de boucle   
    restart = 0 
    do while (( nbnvco > 0 ).and.( restart < elg_ctxt%nlag ))
!
!
! -- Calcul de T, telle que CT=0 
!    en sortie : nbnvco, non_verified_cons_c caractérisent
!    les contraintes qui n'ont pas pu être prises en compte 
!    à ce tour de boucle 
!
      call nullbasis(mat_c, mat_t, nbnvco, non_verified_cons_c) 
!
! -- si toutes les contraintes ne sont pas vérifiées, 
!    on prépare les données pour le tour de boucle 
!    suivant
      if (nbnvco >  0) then
        restart=restart+1
        if (verbose) write(ifm,'(A14,I3)') 'restart #',restart
        if (verbose) write(ifm,'(A3,I3,A26)') ' * ',nbnvco, ' CONTRAINTES NON VERIFIEES'
!
        if (verbose) write(ifm,*) 'NEW NUMBER    ','OLD NUMBER    '
        do j1 = 1, nbnvco
            if (verbose) write(ifm,*) j1,' - ', non_verified_cons_c(j1) + 1
        end do
!   
! CT <- C*T (stockée dans mat_tmp)
        call MatMatMult(mat_c, mat_t, MAT_INITIAL_MATRIX, &
             aster_petsc_default_real, mat_tmp, ierr)
        ASSERT(ierr.eq.0)
        call MatDestroy(mat_c, ierr)
        ASSERT( ierr == 0 )
! On extrait de CT la sous-matrice des contraintes qui n'ont pas été éliminées
! On nomme C cette nouvelle matrice des contraintes 
        call ISCreateGeneral(mpicomm, nbnvco, non_verified_cons_c, PETSC_USE_POINTER,&
                             isnvco, ierr) 
        ASSERT( ierr == 0 )
        call ISCreateStride(mpicomm, elg_ctxt%nphys, izero, ione, &
            isall, ierr)
        ASSERT( ierr == 0 )  
        call MatGetSubMatrix(mat_tmp, isnvco, isall, MAT_INITIAL_MATRIX, mat_c,&
                             ierr)
        ASSERT( ierr == 0 )
        call MatDestroy(mat_tmp, ierr)
        ASSERT( ierr == 0 )
        call ISDestroy( isnvco, ierr)
        ASSERT( ierr == 0 )
        call ISDestroy( isall, ierr)
        ASSERT( ierr == 0 )
        
! T2 = TFinal 
        call MatDuplicate(elg_ctxt%tfinal, MAT_COPY_VALUES, mat_tmp, ierr)
        ASSERT( ierr == 0 )
 
        call MatDestroy(elg_ctxt%tfinal, ierr)
        ASSERT( ierr == 0 )
! TFinal = T2 * T 
        call MatMatMult(mat_tmp, mat_t, MAT_INITIAL_MATRIX, aster_petsc_default_real,&
                        elg_ctxt%tfinal, ierr)
        ASSERT(ierr==0)
        call MatDestroy(mat_t, ierr)
        ASSERT( ierr == 0 )
      endif
    enddo
!
!   -- Derniere projection pour obtenir Tfinal
!
    call MatDuplicate(elg_ctxt%tfinal, MAT_COPY_VALUES, mat_tmp, ierr)
    ASSERT( ierr == 0 )
    call MatDestroy(elg_ctxt%tfinal, ierr)
    ASSERT( ierr == 0 )
    call MatMatMult(mat_tmp, mat_t, MAT_INITIAL_MATRIX, aster_petsc_default_real,&
         elg_ctxt%tfinal, ierr)
    ASSERT( ierr == 0 )
    call MatDestroy(mat_tmp, ierr)
    ASSERT( ierr == 0 ) 
    call MatDestroy(mat_c, ierr)
    ASSERT( ierr == 0 )
    call MatDestroy(mat_t, ierr)
    ASSERT( ierr == 0 )
!
!
!   -- Verif de la qualite de la base
!   ----------------------------------
 
    allocate( norms_c( elg_ctxt%nlag ) , stat = ierr ) 
    allocate( norms_ct( elg_ctxt%nlag ) , stat = ierr ) 
!   -- calcul de la norme de chaque ligne de C 
    call MatGetColumnNorms(elg_ctxt%ctrans, norm_2, norms_c, ierr)
    ASSERT( ierr == 0 )
!
!   -- calcul de T^T C^T = (CT)^T
!-- Changement de version PETSc 3.2 -> 3.3 
!   Renamed MatMatMultTranspose() for C=A^T*B to MatTransposeMatMult()
#ifdef ASTER_PETSC_VERSION_LEQ_32
    call MatMatMultTranspose(elg_ctxt%tfinal, elg_ctxt%ctrans, MAT_INITIAL_MATRIX, &
                                     PETSC_DEFAULT_DOUBLE_PRECISION, mat_tmp, ierr)
#else 
    call MatTransposeMatMult(elg_ctxt%tfinal,elg_ctxt%ctrans,MAT_INITIAL_MATRIX, &
                           aster_petsc_default_real, mat_tmp,ierr)
#endif  
!
    ASSERT( ierr == 0 )
!     -- calcul de la norme de chaque ligne de CT 
    call MatGetColumnNorms(mat_tmp, norm_2, norms_ct, ierr)
    ASSERT( ierr == 0 )
    if (verbose) write(ifm,*) ' '
    if (verbose) write(ifm,*) ' '
    if (verbose) write(ifm,*) '|C.T|/|C| apres elimination :'
    do i1 = 1, elg_ctxt%nlag
       if ( norms_c(i1) > eps ) then 
        if (verbose) write(ifm, '(A11,I3,A3,E11.4)') 'CONTRAINTE ', i1, ' : ',&
                   norms_ct(i1)/norms_c(i1) 
       endif
    end do
    !
    call MatDestroy( mat_tmp, ierr)
    ASSERT( ierr == 0 )
!
!   -- on "retasse" la matrice Tfinal :
!   -----------------------------------
!
    call MatDuplicate( elg_ctxt%tfinal, MAT_COPY_VALUES, mat_tmp, ierr)
    ASSERT( ierr == 0 )
    call compress_sparse_pattern( mat_tmp )
    call extract_nonzero_col( mat_tmp, elg_ctxt%tfinal, icolnz_c)
    call MatDestroy( mat_tmp, ierr )
    ASSERT( ierr == 0 )
!    
!   Le tableau elg_ctxt%indred est dans le common elim_lagr.h 
!   il n'est pas forcément désalloué à la fin de la commande courante
!   -> allocation avec allocate (et pas AS_ALLOCATE)
    allocate( elg_ctxt%indred(size( icolnz_c ) ), stat = ierr ) 
    ASSERT( ierr == 0 ) 
!   indred contient la correspondance entre un indice colonne de 
!   Tfinal et la numérotation des degrés de liberté.
!   (indices Fortran)
    elg_ctxt%indred(:) = idphys_c(icolnz_c(:) + 1)+1 

!
!  -- Vérification du nombre de contraintes éliminées
!  --------------------------------------------------
    if (verbose) then 
        write(ifm,100) size(icolnz_c(:))
100  format(  " Taille du système réduit (projeté sur T=Ker(C)) :", I8 )
    endif 
! 
!    Nombre de contraintes éliminées = nb de lignes de C qui sont bien 
!    orthogonales à toutes les colonnes de TFinal
!    c'est à dire nombre de lignes de CT de norme (quasi)nulle
     nbelig=count(norms_ct/norms_c < 10*r8prem())
!
    if (verbose) then
        write(ifm,*) 'On a ',elg_ctxt%nphys+2*elg_ctxt%nlag ,&
    &   ' ddls (y compris les double-Lagrange).'
        write(ifm,*) 'On a ',elg_ctxt%nphys ,' ddls "physiques".'
        write(ifm,*) 'On avait ',elg_ctxt%nlag,' contraintes'
        write(ifm,*) 'On a ete capable d''en eliminer ',nbelig
    endif
    ASSERT(nbelig.le.elg_ctxt%nlag)
!
    if (nbelig .eq. 0) call utmess('F', 'ELIMLAGR_10', sk=elg_ctxt%full_matas)
!
    if (elg_ctxt%nlag .ne. nbelig) then
!       Si la condition ci-dessus n'est pas respectée,
!       c'est que A n'est pas de rang maximum. Il faut regarder de plus près
!       et se préoccuper de la cohérence du second membre (c).
!
        call getres(k8b, concep, nomcmd)
!       -- dans la commande ELIM_LAGR, c==0, on peut donc continuer
        if (nomcmd .ne. 'ELIM_LAGR') then
! développement à faire
            ASSERT(.false.)
        endif
    endif
!
end subroutine build_tfinal
!
#else
subroutine build_elim_lagr_context( full_matas )
    character(len=19), intent(in) :: full_matas
    ASSERT( .false. )  
end subroutine build_elim_lagr_context
#endif 
end module elim_lagr_comp_module
