subroutine apvsmb(kptsc, lmd, rsolu)
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
! person_in_charge: thomas.desoza at edf.fr
    include 'jeveux.h'
    include 'asterfort/apbloc.h'
    include 'asterfort/assert.h'
    include 'asterfort/comcou.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mpicm0.h'
    include 'asterfort/wkvect.h'
    integer :: kptsc
    logical :: lmd
    real(kind=8) :: rsolu(*)
!----------------------------------------------------------------
!
!  CREATION ET REMPLISSAGE DU SECOND MEMBRE
!
!----------------------------------------------------------------
#ifdef _HAVE_PETSC
!
#   include "aster_petsc.h"
!
!     VARIABLES LOCALES
    integer :: nsmdi, tbloc, rang, nbproc, jnequ, jnequl, jnugl, jnuglp
    integer :: jprddl, nloc, nglo, ndprop, jcoll, jindic, jvaleu, numglo
    integer(kind=4) :: mpicou
!
    character(len=4) :: kbid
    character(len=14) :: nonu
    character(len=19) :: nomat, nosolv
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: low, high, bs, i, neq, ierr
    PetscScalar :: xx(1)
    PetscOffset :: xidx
!----------------------------------------------------------------
    call jemarq()
!---- COMMUNICATEUR MPI DE TRAVAIL
    mpicou=comcou(1)
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
!
!     -- TAILLE DES BLOCS
    call apbloc(nomat, nosolv, tbloc)
    bs = abs(tbloc)
!
    if (lmd) then
        call mpicm0(rang, nbproc)
        call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
        call jeveuo(nonu//'.NUML.NEQU', 'L', jnequl)
        call jeveuo(nonu//'.NUML.NULG', 'L', jnugl)
        call jeveuo(nonu//'.NUML.NLGP', 'L', jnuglp)
        call jeveuo(nonu//'.NUML.PDDL', 'L', jprddl)
        nloc = zi(jnequl)
        nglo = zi(jnequ)
!
        ndprop = 0
        do jcoll = 0, nloc-1
            if (zi(jprddl+jcoll) .eq. rang) ndprop = ndprop+1
        end do
!
        call VecCreate(mpicou, b, ierr)
        call assert(ierr.eq.0)
        call VecSetBlockSize(b, bs, ierr)
        call assert(ierr.eq.0)
        call VecSetSizes(b, ndprop, nglo, ierr)
        call assert(ierr.eq.0)
        call VecSetType(b, VECMPI, ierr)
        call assert(ierr.eq.0)
!
        call wkvect('&&APVSMB.INDICES', 'V V S', nloc, jindic)
        call wkvect('&&APVSMB.VALEURS', 'V V R', nloc, jvaleu)
        do jcoll = 0, nloc-1
            zi4(jindic+jcoll) = zi(jnuglp+jcoll)-1
            numglo = zi(jnugl+jcoll)
            zr(jvaleu+jcoll) = rsolu(numglo)
        end do
        call VecSetValues(b, nloc, zi4(jindic), zr(jvaleu), ADD_VALUES,&
                          ierr)
        call jedetr('&&APVSMB.INDICES')
        call jedetr('&&APVSMB.VALEURS')
        call VecAssemblyBegin(b, ierr)
        call assert(ierr.eq.0)
        call VecAssemblyEnd(b, ierr)
        call assert(ierr.eq.0)
    else
        call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi, kbid)
        neq=nsmdi
!
        call VecCreate(mpicou, b, ierr)
        call assert(ierr.eq.0)
        call VecSetBlockSize(b, bs, ierr)
        call assert(ierr.eq.0)
        call VecSetSizes(b, PETSC_DECIDE, neq, ierr)
        call assert(ierr.eq.0)
        call VecSetType(b, VECMPI, ierr)
        call assert(ierr.eq.0)
!
        call VecGetOwnershipRange(b, low, high, ierr)
        call assert(ierr.eq.0)
        call VecGetArray(b, xx, xidx, ierr)
        call assert(ierr.eq.0)
!
        do i = 1, high-low
            xx(xidx+i)=rsolu(low+i)
        end do
!
        call VecRestoreArray(b, xx, xidx, ierr)
        call assert(ierr.eq.0)
    endif
!
    call jedema()
!
#endif
!
end subroutine
