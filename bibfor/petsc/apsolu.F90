subroutine apsolu(kptsc, lmd, rsolu)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
! person_in_charge: thomas.de-soza at edf.fr
#include "asterf_config.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mpicm0.h"
#include "asterfort/mpicm1.h"
#include "asterfort/mrconl.h"
    integer :: kptsc
    logical :: lmd
    real(kind=8) :: rsolu(*)
!----------------------------------------------------------------
!
!  RECOPIE DE LA SOLUTION
!
!----------------------------------------------------------------
#ifdef _HAVE_PETSC
!
#include "aster_petsc.h"
!
!     VARIABLES LOCALES
    integer :: jnequ, jnequl, jnuglp, jnugl, jprddl, nloc, nglo, rang
    integer :: nbproc, iaux, numglo, ibid, lmat
!
    character(len=14) :: nonu
    character(len=19) :: nomat
    complex(kind=8) :: cbid
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: i, neqg, neql, nuglpe, high, low, ierr
    PetscScalar :: xx(1)
    PetscOffset :: xidx
    VecScatter :: ctx
    Vec :: xgth
!----------------------------------------------------------------
    call jemarq()
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nonu = nonus(kptsc)
!
    if (lmd) then
!
        call mpicm0(rang, nbproc)
!
        call jeveuo(nonu//'.NUML.NLGP', 'L', jnuglp)
        call jeveuo(nonu//'.NUML.NULG', 'L', jnugl)
        call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
        call jeveuo(nonu//'.NUML.NEQU', 'L', jnequl)
        call jeveuo(nonu//'.NUML.PDDL', 'L', jprddl)
!
        nloc = zi(jnequl)
        nglo = zi(jnequ)
        neqg = nglo
        neql = nloc
!
        do iaux = 1, nglo
            rsolu(iaux)=0.d0
        enddo
!
        call VecGetOwnershipRange(x, low, high, ierr)
        call assert(ierr.eq.0)
!
!       -- RECOPIE DE DANS RSOLU
        call VecGetArray(x, xx, xidx, ierr)
        call assert(ierr.eq.0)
!
        do iaux = 1, nloc
            if (zi(jprddl-1+iaux) .eq. rang) then
                nuglpe=zi(jnuglp-1+iaux)
                numglo=zi(jnugl-1+iaux)
                rsolu(numglo)=xx(xidx+nuglpe-low)
            endif
        enddo
!
        call mpicm1('MPI_SUM', 'R', nglo, ibid, ibid,&
                    rsolu, cbid)
!
        call VecRestoreArray(x, xx, xidx, ierr)
        call assert(ierr.eq.0)
!
    else
!
!       -- RECONSTRUCTION DE LA LA SOLUTION SUR CHAQUE PROC
        call VecScatterCreateToAll(x, ctx, xgth, ierr)
        call assert(ierr.eq.0)
        call VecScatterBegin(ctx, x, xgth, INSERT_VALUES, SCATTER_FORWARD,&
                             ierr)
        call assert(ierr.eq.0)
        call VecScatterEnd(ctx, x, xgth, INSERT_VALUES, SCATTER_FORWARD,&
                           ierr)
        call assert(ierr.eq.0)
        call VecScatterDestroy(ctx, ierr)
        call assert(ierr.eq.0)
!
!       -- RECOPIE DE DANS RSOLU
        call VecGetArray(xgth, xx, xidx, ierr)
        call assert(ierr.eq.0)
        call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
        neqg = zi(jnequ)
        do i = 1, neqg
            rsolu(i)=xx(xidx+i)
        end do
!
        call VecRestoreArray(xgth, xx, xidx, ierr)
        call assert(ierr.eq.0)
!
!       -- NETTOYAGE
        call VecDestroy(xgth, ierr)
        call assert(ierr.eq.0)
!
    endif
!
    call jeveuo(nomat//'.&INT', 'L', lmat)
!
!     -- REMISE A L'ECHELLE DES LAGRANGES DANS LA SOLUTION
    call mrconl('MULT', lmat, 0, 'R', rsolu,&
                1)
!
    call jedema()
!
#endif
!
end subroutine
