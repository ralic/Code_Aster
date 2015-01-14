subroutine extract_nonzero_col(a, acnz, icolnz_c)
    implicit none
! person_in_charge: natacha.bereux at edf.fr
! aslint: disable=W0104
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! BUT : creer une matice creuse ACNZ formée des colonnes non-nulles de A
!       A et ACNZ ont le même nombre de termes non-nuls
! RQ : on se base sur la norme des colonnes pour sélectionner les colonnes
!      à garder
!
! IN : A de taille ma x na
! OUT : ACNZ de taille ma x nacnz
!---------------------------------------------------------------
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"

    Mat, intent(in)  :: a
    Mat, intent(out) :: acnz
    PetscInt, dimension(:), pointer :: icolnz_c
!
!================================================================
!
!     VARIABLES LOCALES
!
    integer :: ifm, niv
    aster_logical :: verbose
    mpi_int :: mpicomm
    PetscInt :: inz, ii
    PetscInt :: first, step, ma, na, nacnz, ierr
    PetscInt, parameter :: ione = 1 , izero = 0
    IS :: isall, isnz
    PetscReal, dimension(:), allocatable :: norms
!----------------------------------------------------------------
   call asmpi_comm('GET_WORLD', mpicomm)
   call infniv(ifm, niv)
   verbose=(niv == 2)
!
    call MatGetSize( a, ma, na, ierr)
    ASSERT(ierr == 0 )
!
    allocate(norms( na ), stat = ierr)
    ASSERT(ierr == 0 )
    call MatGetColumnNorms( a, norm_2, norms, ierr)
     ASSERT( ierr == 0 )
!   -- nombre de colonnes nonnulles dans Atmp
    nacnz = count( norms > r8prem() )
!   -- icolnz :indices C des colonnes non nulles de de A
    allocate( icolnz_c( nacnz), stat = ierr )
    ASSERT( ierr == 0 )
!
    inz = 0
    do ii=1, na
        if ( norms( ii ) > r8prem() ) then
         inz = inz + 1
         icolnz_c( inz ) = ii-1
        endif
    enddo

!
!   -- isnz index set des  colonnes non-nulles de Atmp (et de A)
    call ISCreateGeneral(mpicomm, nacnz, icolnz_c, PETSC_USE_POINTER,&
                         isnz, ierr)
!   -- isall index set de toutes les lignes de Atmp
    call ISCreateStride( mpicomm, ma, izero, ione, isall, ierr )
!   -- extraction des colonnes non-nulles de Atmp => Anz
    call MatGetSubMatrix( a, isall, isnz , MAT_INITIAL_MATRIX, &
        acnz, ierr)
!
    if (verbose) then
        call MatGetSize( acnz, ma, nacnz, ierr)
        write(ifm,*) " -- EXTRACTION DES COLONNES NON-NULLES  "
        write(ifm,*) "    EN ENTREE, NB COLONNES: ", na
        write(ifm,*) "    EN SORTIE, NB COLONNES: ", nacnz
    endif
!
! Libération de la mémoire
!
    call ISDestroy(isnz, ierr)
    call ISDestroy(isall, ierr)
    deallocate( norms, stat = ierr )
    ASSERT(ierr == 0 )

#else
    integer, intent(in)  :: a
    integer, intent(out)  :: acnz
    integer, dimension(:), pointer :: icolnz_c
    ASSERT(.false.)
    acnz = 0
#endif
!
end subroutine
