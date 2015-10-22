subroutine apsolu(kptsc, lmd, rsolu)
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
! aslint: disable=C1513
! cf. issue23375
!
    implicit none
! person_in_charge: natacha.bereux at edf.fr
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/mrconl.h"
    integer :: kptsc
    aster_logical :: lmd
    real(kind=8) :: rsolu(*)
!----------------------------------------------------------------
!
!  RECOPIE DE LA SOLUTION
!
!----------------------------------------------------------------
#ifdef _HAVE_PETSC
!
#include "asterf_petsc.h"
!
!     VARIABLES LOCALES
    integer :: jnequ, jnequl, jnuglp, jnugl, jprddl, nloc, nglo, rang
    integer :: nbproc, lmat, neq1,neq2, fictif, bs,ieq1,ieq2,k
    integer :: iloc, iglo
    integer, dimension(:), pointer :: nlgp => null(), nulg=> null(), prddl =>null()
    integer(kind=4), pointer :: new_ieq(:) => null()
    integer(kind=4), pointer :: old_ieq(:) => null()
!
    character(len=14) :: nonu
    character(len=19) :: nomat
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: i, neqg, neql, nuglpe, high2, low2
    PetscErrorCode ::  ierr
    PetscScalar :: xx(1)
    PetscOffset :: xidx
    VecScatter :: ctx
    Vec :: xgth
    mpi_int :: mrank, msize
!----------------------------------------------------------------
    call jemarq()
!
!     -- LECTURE DU COMMUN
    nomat = nomat_courant
    nonu = nonu_courant
    bs = tblocs(kptsc)
    fictif = fictifs(kptsc)
!
    call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
    neqg = zi(jnequ)
    !
    if (lmd) then
!
        call asmpi_info(rank=mrank, size=msize)
        rang = to_aster_int(mrank)
        nbproc = to_aster_int(msize)
!
        call jeveuo(nonu//'.NUML.NLGP', 'L', vi=nlgp)
        call jeveuo(nonu//'.NUML.NULG', 'L', vi=nulg)
        call jeveuo(nonu//'.NUML.NEQU', 'L', jnequl)
        call jeveuo(nonu//'.NUML.PDDL', 'L', vi=prddl)


!
        nloc = zi(jnequl)
        nglo = neqg
        neql = nloc
!
        do iloc = 1, nglo
            rsolu(iloc)=0.d0
        enddo
!
        call VecGetOwnershipRange(x, low2, high2, ierr)
        ASSERT(ierr.eq.0)
!
!       -- RECOPIE DE DANS RSOLU
        call VecGetArray(x, xx, xidx, ierr)
        ASSERT(ierr.eq.0)
!
        do iloc = 1, nloc
            if ( prddl(iloc) .eq. rang ) then
                nuglpe= nlgp(iloc)
                iglo= nulg(iloc)
                rsolu(iglo)=xx(xidx+nuglpe-low2)
            endif
        enddo
!
        call asmpi_comm_vect('MPI_SUM', 'R', nbval=nglo, vr=rsolu)
!
        call VecRestoreArray(x, xx, xidx, ierr)
        ASSERT(ierr.eq.0)
!
    else
        call jelira(nonu//'.SMOS.SMDI', 'LONMAX', neq1)
        ASSERT(neq1.eq.neqg)
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
!
!       -- RECONSTRUCTION DE LA LA SOLUTION SUR CHAQUE PROC
        call VecScatterCreateToAll(x, ctx, xgth, ierr)
        ASSERT(ierr.eq.0)
        call VecScatterBegin(ctx, x, xgth, INSERT_VALUES, SCATTER_FORWARD,&
                             ierr)
        ASSERT(ierr.eq.0)
        call VecScatterEnd(ctx, x, xgth, INSERT_VALUES, SCATTER_FORWARD,&
                           ierr)
        ASSERT(ierr.eq.0)
        call VecScatterDestroy(ctx, ierr)
        ASSERT(ierr.eq.0)
!
!       -- RECOPIE DE XX DANS RSOLU
        call VecGetArray(xgth, xx, xidx, ierr)
        ASSERT(ierr.eq.0)

! Pour corriger issue23375
! Probleme possible du compilateur Intel sur athosdev :
! Version 14.0.2.144 Build 20140120
!DIR$ NOVECTOR
        do ieq1 = 1, neq1
            ieq2=new_ieq(ieq1)
            rsolu(ieq1)=xx(xidx+ieq2)
        end do
!
        call VecRestoreArray(xgth, xx, xidx, ierr)
        ASSERT(ierr.eq.0)
!
!       -- NETTOYAGE
        call VecDestroy(xgth, ierr)
        ASSERT(ierr.eq.0)
        if (fictif.eq.0) then
            deallocate(new_ieq)
            deallocate(old_ieq)
        endif
!
    endif
!
    call jeveuo(nomat//'.&INT', 'L', lmat)
!
!     -- REMISE A L'ECHELLE DES LAGRANGES DANS LA SOLUTION
    call mrconl('MULT', lmat, 0, 'R', rsolu, 1)
!
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
