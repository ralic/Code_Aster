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
! - matrix data ( k_mat, c_mat, d_mat )
! - vector workspace ( x_1, x_2, x_3, b_1, b_2, b_3 )
! 
module saddle_point_context_class
!
! COPYRIGHT (C) 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
! aslint:disable=C1308
!
use matrasse_module
!
implicit none 
!
private
#include "asterf.h"
#include "asterf_petsc.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/conlag.h"
#include "asterfort/utmess.h"
!
!
type, public :: saddle_point_context_type
#ifdef _HAVE_PETSC
    !
    ! Double Lagrange scaling coefficient (from .conl)
    real(kind=8) :: alpha 
    !
    ! Index Sets section 
    ! ==================
    ! Flag : true if Index Sets (IS) have been setup
    aster_logical :: is_setup = .false.
    ! Index set of physical degrees of freedom 
    ! in PETSc matrix (of the whole double Lagrange system)
    IS :: is_phys
    ! Number of physical degrees of freedom 
    integer :: nphys
    !
    ! Index set of Lagrange 1 multipliers 
    ! in PETSc matrix (of the whole double Lagrange system)
    IS :: is_lag1
    ! Number of Lagrange 1 multipliers 
    integer :: nlag1 
    ! Index set of Lagrange 2 multipliers 
    ! in PETSc matrix (of the whole double Lagrange system)
    IS :: is_lag2
    ! Number of Lagrange 2 multipliers 
    integer :: nlag2
    ! 
    ! Matrix Data section
    ! ============
    aster_logical :: data_setup = .false.
    ! Stiffness Matrix
    Mat :: k_mat 
    ! Constraint Matrix 
    ! c_mat is obtained from is_lag1 x is_phys 
    ! whereas d_mat is obtained from is_lag2 x is_phys
    ! They are identical on a single proc, but they have different parallel layout
    Mat :: c_mat,  d_mat 
    !
    ! Work Space
    ! ==========
    aster_logical :: work_setup = .false.
    Vec :: x1, x2, x3, xtmp
    Vec :: y1, y2, y3 
    ! Scatter tools
    ! =============
    aster_logical :: scatter_setup = .false.
    VecScatter :: scatter_to_phys, scatter_to_lag1, scatter_to_lag2
    !
#endif 
end type saddle_point_context_type
!
public :: new_saddle_point_context, free_saddle_point_context
!
#ifdef _HAVE_PETSC
!
PetscErrorCode  :: ierr
!
contains 
!
function new_saddle_point_context( full_matas, a_mat, ak_mat ) result ( ctxt )
    ! 
    character(len=19), intent(in)                :: full_matas
    Mat, intent(in)                              :: a_mat
    Mat, intent(in), optional                    :: ak_mat 
    type(saddle_point_context_type)              :: ctxt
    ! 
    ! Local variables
    !
    real(kind=8) :: un_sur_alpha
    !
    ! On récupère l'inverse du paramètre alpha
    call conlag( full_matas, un_sur_alpha )
    ctxt%alpha = 1.d0/un_sur_alpha
    !
    call set_is( full_matas, ctxt )
    ASSERT( ctxt%is_setup ) 
    if  ( present (ak_mat) ) then 
    ! Les relations lineaires sont stockees dans 
    ! la matrice ak_mat 
      call set_matrix_data( a_mat, ak_mat, ctxt )
    else 
      call set_matrix_data( a_mat, a_mat, ctxt )
    endif 
    ASSERT( ctxt%data_setup )
    call set_workspace( ctxt )
    ASSERT( ctxt%work_setup )
    call set_scatter( a_mat, ctxt )
    ASSERT( ctxt%scatter_setup )
    !
end function new_saddle_point_context
!
! This routine initializes the Index sets section of the context object
!
subroutine set_is( matas , ctxt )
    !
    character(len=19), intent(in)                  :: matas
    type(saddle_point_context_type), intent(inout) :: ctxt 
    !
    ! Local variables 
    PetscInt :: nphys, nlag1, nlag2 
    !
    ASSERT( .not. ctxt%is_setup )
    !
    ! Degrés de liberté physiques :
    ! =============================
    ctxt%is_phys = build_is_for_type_of_dof( matas, physical_dof )
    call ISGetSize(ctxt%is_phys , nphys, ierr )
    ASSERT( ierr == 0 ) 
    ctxt%nphys = to_aster_int( nphys )
    !
    ! Lagrange 1 :
    ! ============
    ctxt%is_lag1 = build_is_for_type_of_dof( matas, lagrange1_dof )
    call ISGetSize(ctxt%is_lag1 , nlag1, ierr )
    ASSERT( ierr == 0 ) 
    ctxt%nlag1 = to_aster_int( nlag1 ) 
    !
    ! Lagrange 2 :
    ! ============
    ctxt%is_lag2 = build_is_for_type_of_dof( matas, lagrange2_dof )
    call ISGetSize(ctxt%is_lag2 , nlag2, ierr )
    ASSERT( ierr == 0 ) 
    ctxt%nlag2 = to_aster_int( nlag2 )
    ! Cette étape est terminée 
    ctxt%is_setup = .true.
    !
end subroutine set_is
!
! This routine initializes the data section of the context object 
! The IS section is supposed to be OK 
! a_mat contains the (double Lagrange) matrix from which k_mat and 
! c_mat are extracted. 
subroutine set_matrix_data( a_mat, ak_mat,  ctxt)
    !
    Mat, intent(in)                                :: a_mat
    Mat, intent(in)                                :: ak_mat 
    type(saddle_point_context_type), intent(inout) :: ctxt 
    !
    ASSERT( ctxt%is_setup )
    ASSERT( .not. ctxt%data_setup )
    ! Définition du bloc k_mat : nouvelle matrice PETSc contenant 
    ! les interactions des ddls "physiques" du modèle
    call MatGetSubMatrix(a_mat, ctxt%is_phys, ctxt%is_phys, &
    MAT_INITIAL_MATRIX, ctxt%k_mat, ierr)
    ASSERT(ierr == 0)
    !
    ! Définition du bloc c_mat des contraintes: nouvelle matrice PETSc
    ! contenant les interactions des ddls lagrange1 (lignes) avec les ddls
    ! physiques (colonnes)
    ! Attention ! il faut peut-être utiliser la matrice de rigidité
    !   
    call MatGetSubMatrix(ak_mat, ctxt%is_lag1, ctxt%is_phys, &
    MAT_INITIAL_MATRIX, ctxt%c_mat, ierr)
    ASSERT(ierr == 0)
    !
    call MatGetSubMatrix(ak_mat, ctxt%is_lag2, ctxt%is_phys, &
    MAT_INITIAL_MATRIX, ctxt%d_mat, ierr)
    ASSERT(ierr == 0)
    !
    ctxt%data_setup = .true.
    !
end subroutine set_matrix_data
!
! This routine initializes the workspace section of the context object 
!
subroutine set_workspace( ctxt )
    !
    ! Dummy arguments 
    !
    type(saddle_point_context_type), intent(inout) :: ctxt 
    !
    ASSERT( ctxt%data_setup )
    ASSERT( .not. ctxt%work_setup )
    !
#ifdef ASTER_PETSC_VERSION_LEQ_35
    call MatGetVecs( ctxt%k_mat, ctxt%x1, ctxt%y1, ierr )
#else
    call MatCreateVecs( ctxt%k_mat, ctxt%x1, ctxt%y1, ierr )
#endif 
    ASSERT( ierr == 0 ) 
    call VecDuplicate( ctxt%x1, ctxt%xtmp, ierr )
    ASSERT( ierr == 0 ) 
    call MatCreateVecs( ctxt%c_mat, PETSC_NULL_OBJECT, ctxt%x2, ierr )
    ASSERT( ierr == 0 ) 
    call VecDuplicate( ctxt%x2, ctxt%y2, ierr )
    ASSERT( ierr == 0 )
    call MatCreateVecs( ctxt%d_mat, PETSC_NULL_OBJECT, ctxt%x3, ierr )
    ASSERT( ierr == 0 ) 
    call VecDuplicate( ctxt%x3, ctxt%y3, ierr )
    ASSERT( ierr == 0 ) 
    !
    ctxt%work_setup = .true. 
    ! On n'a plus besoin de d_mat 
    call MatDestroy( ctxt%d_mat, ierr ) 
    ASSERT( ierr == 0 ) 
    !
end subroutine set_workspace
!
subroutine set_scatter( a_mat, ctxt ) 
    !
    ! Dummy arguments 
    !
    Mat, intent(in)                                :: a_mat
    type(saddle_point_context_type), intent(inout) :: ctxt
    !
    ! Local Variables
    Vec :: x
    !
    ASSERT( ctxt%is_setup ) 
    ASSERT( ctxt%work_setup )
    ASSERT( .not. ctxt%scatter_setup )
    !
#ifdef ASTER_PETSC_VERSION_LEQ_35
    call MatGetVecs( a_mat, x, PETSC_NULL_OBJECT, ierr ) 
#else
    call MatCreateVecs( a_mat, x, PETSC_NULL_OBJECT, ierr )
#endif  
    ASSERT( ierr == 0 )
    call VecScatterCreate(ctxt%x1,PETSC_NULL_OBJECT,x, ctxt%is_phys, ctxt%scatter_to_phys, ierr)
    ASSERT( ierr == 0 ) 
    call VecScatterCreate(ctxt%x2,PETSC_NULL_OBJECT,x, ctxt%is_lag1, ctxt%scatter_to_lag1, ierr)
    ASSERT( ierr == 0 ) 
    call VecScatterCreate(ctxt%x3,PETSC_NULL_OBJECT,x, ctxt%is_lag2, ctxt%scatter_to_lag2, ierr)
    ASSERT( ierr == 0 ) 
    !
    call VecDestroy( x, ierr )
    ASSERT( ierr == 0 )
    !
    ctxt%scatter_setup = .true.
    !
end subroutine set_scatter 
!
!
subroutine free_saddle_point_context( ctxt )
    !
    ! Dummy argument 
    !
    type( saddle_point_context_type ), intent(inout) :: ctxt
    !
    call ISDestroy( ctxt%is_phys, ierr )
    ASSERT( ierr == 0 )
    call ISDestroy( ctxt%is_lag1, ierr )
    ASSERT( ierr == 0 )
    call ISDestroy( ctxt%is_lag2, ierr )
    ASSERT( ierr == 0 )
    call MatDestroy( ctxt%k_mat, ierr )
    ASSERT( ierr == 0 )
    call MatDestroy( ctxt%c_mat, ierr )
    ASSERT( ierr == 0 )
    call VecDestroy( ctxt%x1, ierr )
    ASSERT( ierr == 0 )
    call VecDestroy( ctxt%x2, ierr )
    ASSERT( ierr == 0 )
    call VecDestroy( ctxt%x3, ierr )
    ASSERT( ierr == 0 )
    call VecDestroy( ctxt%xtmp, ierr )
    ASSERT( ierr == 0 )
    call VecDestroy( ctxt%y1, ierr )
    ASSERT( ierr == 0 )
    call VecDestroy( ctxt%y2, ierr )
    ASSERT( ierr == 0 )
    call VecDestroy( ctxt%y3, ierr )
    ASSERT( ierr == 0 )
    call VecScatterDestroy( ctxt%scatter_to_phys, ierr ) 
    ASSERT( ierr == 0 )
    call VecScatterDestroy( ctxt%scatter_to_lag1, ierr ) 
    ASSERT( ierr == 0 )
    call VecScatterDestroy( ctxt%scatter_to_lag2, ierr ) 
    ASSERT( ierr == 0 )
    !
end subroutine free_saddle_point_context
!
! 
function build_is_for_type_of_dof( matas, type_of_dof ) result ( is )
    ! Dummy arguments
    character(len=19)   :: matas 
    integer, intent(in) :: type_of_dof
    IS                  :: is
    ! Local variables 
    mpi_int :: mpicomm, rank, nbproc
    integer :: ndof, pass, ii, jerr  
    PetscInt :: istart, iend, my_ndof
    integer(kind=4),dimension(:), pointer :: idof => null()
    PetscInt,dimension(:), allocatable    :: my_idof 
    Vec :: vtmp 
    !
    ! Récupération du communicateur MPI
    call asmpi_comm('GET', mpicomm)
    call asmpi_info(rank=rank, size=nbproc)
    !
    ! Détermination des indices (Fortran) dans la matrice aster 
    idof => get_indices_of_dofs( type_of_dof, matas )
    ndof = size(idof)
    ! Le vecteur d'indices idof contient les indices globaux des ddls 
    ! physiques. Il a la même taille et contient les mêmes
    ! valeurs sur tous les processeurs participant au calcul. 
    !
    ! Passage indices Fortran -> Indices C 
    idof(:) = idof(:) - 1
    !
    ! Détermination de istart et iend 
    if ( nbproc == 1 ) then
        ! on n'a rien à faire 
        istart = 0 
        iend = ndof
    else 
    ! Sur plusieurs procs 
    ! l'index set retourné 
    ! par cette fonction a la même répartition 
    ! parallèle qu'un vecteur PETSc de taille ndof   
        call VecCreateMPI(mpicomm, PETSC_DECIDE, ndof, vtmp, ierr )
        ASSERT( ierr == 0 ) 
        call VecGetOwnerShipRange( vtmp, istart, iend, ierr )
        ASSERT( ierr == 0 )
        call VecDestroy( vtmp, ierr ) 
        ASSERT( ierr == 0 ) 
    endif 
    ! Sur chaque processeur, l'IS  possède les degrés de liberté 
    ! listés dans my_idof
    do pass = 1, 2
        my_ndof = 0  
        do ii = istart, iend-1
                my_ndof = my_ndof + 1 
                if ( pass == 2 ) then 
                    my_idof( my_ndof ) = idof ( ii + 1 )
                endif  
        enddo
        if ( pass == 1 ) then 
            allocate( my_idof( my_ndof ), stat = jerr ) 
            ASSERT( jerr == 0 )
        endif
    enddo
    !
    ! Construction d'un Index Set (IS) PETSc à partir du vecteur d'indices C
    call ISCreateGeneral(mpicomm, my_ndof, my_idof, &
        PETSC_COPY_VALUES, is, ierr )
    ASSERT(ierr == 0 )
    if (associated( idof ) ) then 
       deallocate( idof ) 
    endif 
    !
    if (allocated( my_idof ) ) then 
       deallocate( my_idof ) 
    endif 
    ! 
end function build_is_for_type_of_dof
!
#else
! Si on ne compile pas avec PETSc, il faut quand même définir les 
! interfaces des routines publiques 
contains 
!
function new_saddle_point_context( matas, a_mat ) result ( ctxt )
    ! 
    character(len=19), intent(in)                :: matas
    integer, intent(in)                          :: a_mat
    type(saddle_point_context_type)              :: ctxt
end function 
!
subroutine set_auglag_precond( ctxt )
    type(saddle_point_context_type), intent(inout) :: ctxt
end subroutine
!
subroutine free_saddle_point_context( ctxt )
    type( saddle_point_context_type ), intent(inout) :: ctxt
end subroutine
!
#endif 
end module saddle_point_context_class
