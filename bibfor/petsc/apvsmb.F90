subroutine apvsmb(kptsc, lmd, rsolu)
!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    implicit none
! person_in_charge: natacha.bereux at edf.fr
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: kptsc
    aster_logical :: lmd
    real(kind=8) :: rsolu(*)
!----------------------------------------------------------------
!
!  CREATION ET REMPLISSAGE DU SECOND MEMBRE
!
!----------------------------------------------------------------
#ifdef _HAVE_PETSC
!
#include "asterf_petsc.h"
!
!     VARIABLES LOCALES
    integer :: nsmdi, rang, nbproc, jnequ, jnequl
    integer :: iloc, iglo, nloc, nglo, ndprop
    integer :: bs, i, neq1, neq2, fictif, ieq1, ieq2, k
    integer, dimension(:), pointer         :: nulg => null()
    integer, dimension(:), pointer         :: nlgp => null(), pddl => null()
    integer(kind=4), dimension(:), pointer :: ig_petsc_c => null()
    integer(kind=4), pointer :: new_ieq(:) => null()
    integer(kind=4), pointer :: old_ieq(:) => null()

    mpi_int :: mpicomm
!
    character(len=14) :: nonu
    character(len=19) :: nomat, nosolv

    real(kind=8), dimension(:), pointer :: val => null()
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: low2, high2
    PetscErrorCode ::  ierr
    PetscScalar :: xx(1)
    PetscOffset :: xidx
    mpi_int :: mrank, msize
!----------------------------------------------------------------
    call jemarq()

!
!   -- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicomm)
!
!     -- LECTURE DU COMMUN
    nomat = nomat_courant
    nonu = nonu_courant
    bs = tblocs(kptsc)
    fictif = fictifs(kptsc)
    ASSERT(bs.ge.1)
    nosolv = nosols(kptsc)

!
    if (lmd) then
        ASSERT(fictif.eq.0)
        call asmpi_info(rank=mrank, size=msize)
        rang = to_aster_int(mrank)
        nbproc = to_aster_int(msize)
        call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
        call jeveuo(nonu//'.NUML.NEQU', 'L', jnequl)
        call jeveuo(nonu//'.NUML.NULG', 'L', vi=nulg)
        call jeveuo(nonu//'.NUML.NLGP', 'L', vi=nlgp)
        call jeveuo(nonu//'.NUML.PDDL', 'L', vi=pddl)
        nloc = zi(jnequl)
        nglo = zi(jnequ)
!       Nombre de ddls m'appartenant (pour PETSc)
        ndprop = count( pddl(1:nloc) == rang )
!
        call VecCreate(mpicomm, b, ierr)
        ASSERT(ierr.eq.0)
        call VecSetBlockSize(b, to_petsc_int(bs), ierr)
        ASSERT(ierr.eq.0)
        call VecSetSizes(b, to_petsc_int(ndprop), to_petsc_int(nglo), ierr)
        ASSERT(ierr.eq.0)
        call VecSetType(b, VECMPI, ierr)
        ASSERT(ierr.eq.0)
!
        AS_ALLOCATE( vi4=ig_petsc_c, size=nloc )
        AS_ALLOCATE( vr=val, size=nloc )
        do iloc = 1, nloc
            ! Indice global PETSc (convention C)
            ig_petsc_c( iloc ) = nlgp( iloc ) - 1
            ! Indice global Aster (convention F)
            iglo               = nulg( iloc )
            val( iloc )        = rsolu( iglo )
        end do
        call VecSetValues(b, to_petsc_int(nloc), ig_petsc_c, val, ADD_VALUES, ierr)
        call VecAssemblyBegin(b, ierr)
        ASSERT(ierr.eq.0)
        call VecAssemblyEnd(b, ierr)
        ASSERT(ierr.eq.0)
        !
        AS_DEALLOCATE( vi4=ig_petsc_c )
        AS_DEALLOCATE( vr=val )
!
    else
        call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
        neq1=nsmdi
        if (fictif.eq.0) then
            neq2=neq1
            allocate(new_ieq(neq2))
            allocate(old_ieq(neq2))
            do k=1,neq2
                new_ieq(k)=k
                old_ieq(k)=k
            enddo
        else
            new_ieq => new_ieqs(kptsc)%pi4
            old_ieq => old_ieqs(kptsc)%pi4
        endif
        neq2=size(old_ieq)


        ASSERT(mod(neq2,bs).eq.0)
!
!       -- allocation de b :
        call VecCreate(mpicomm, b, ierr)
        ASSERT(ierr.eq.0)
        call VecSetBlockSize(b, to_petsc_int(bs), ierr)
        ASSERT(ierr.eq.0)
        call VecSetSizes(b, PETSC_DECIDE, to_petsc_int(neq2), ierr)
        ASSERT(ierr.eq.0)
        call VecSetType(b, VECMPI, ierr)
        ASSERT(ierr.eq.0)
!       -- on met le vecteur a zero a cause des ddls fictifs :
        call VecSet(b, 0.d0, ierr)
        ASSERT(ierr.eq.0)
!
!
!       -- calcul de b=RSOLU :
!       ------------------------------------------------
        call VecGetOwnershipRange(b, low2, high2, ierr)
        call VecGetArray(b, xx, xidx, ierr)
        ASSERT(ierr.eq.0)
!
        do i = 1, high2-low2
            ieq2=low2+i
            ieq1=old_ieq(ieq2)
            if (ieq1.gt.0) xx(xidx+i)=rsolu(ieq1)
        end do
        call VecRestoreArray(b, xx, xidx, ierr)
        ASSERT(ierr.eq.0)

        if (fictif.eq.0) then
            deallocate(new_ieq)
            deallocate(old_ieq)
        endif

    endif



    call jedema()
!
#else
    integer :: idummy
    aster_logical :: ldummy
    real(kind=8) :: rdummy
    idummy = kptsc
    ldummy = lmd
    rdummy = rsolu(1)
#endif
!
end subroutine
