subroutine apalmd(kptsc)
!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
! aslint:disable=C1308
use petsc_data_module

    implicit none

#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/apbloc.h"
#include "asterfort/asmpi_comm_point.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: kptsc
!----------------------------------------------------------------
!
!  CREATION DE LA MATRICE PETSC (INSTANCE NUMERO KPTSC)
!  PREALLOCATION DANS LE CAS MATR_DISTRIBUEE
!
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
#include "asterf_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: rang, nbproc, jnbjoi, nbjoin, jnequ, jnequl, jnugll
    integer :: nsmdi, nsmhc, nz, jprddl, nloc, nglo, jcoll
    integer :: jsmdi, jsmhc, jidxd, jidxo, ndprop, procol, jidxdc, jidxoc
    integer :: k, nzdeb, nzfin, jcolg, iligl, jaux
    integer :: prolig, iligg, iaux, numpro, jjoint, jvaleu, numloc
    integer :: lgenvo, numglo, comple
    mpi_int :: mpicou
!
!
    character(len=4) :: chnbjo
    character(len=14) :: nonu
    character(len=16) :: idxo, idxd, idxoc, idxdc, cpysol
    character(len=19) :: nomat, nosolv
    character(len=24) :: nojoin
!
    parameter   (idxo  ='&&APALMD.IDXO___')
    parameter   (idxd  ='&&APALMD.IDXD___')
    parameter   (idxoc ='&&APALMD.IDXOC__')
    parameter   (idxdc ='&&APALMD.IDXDC__')
    parameter   (cpysol='&&APALMD.COPYSOL')
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: low, high
    PetscErrorCode ::  ierr
    integer :: neql, neqg, bs 
    Vec :: tmp
    mpi_int :: mrank, msize
!----------------------------------------------------------------
    call jemarq()
!
!   -- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
!
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
!
!     -- LECTURE DU COMMUN
    nomat = nomat_courant
    nonu = nonu_courant
    nosolv = nosols(kptsc)
!
    call jeveuo(nonu//'.NUML.JOIN', 'L', jnbjoi)
    call jelira(nonu//'.NUML.JOIN', 'LONMAX', nbjoin)
!
    call jeveuo(nonu//'.SMOS.SMDI', 'L', jsmdi)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc)
    nz = zi(jsmdi-1+nsmdi)
!
    call apbloc(kptsc)
    bs=tblocs(kptsc)
    ASSERT(bs.ge.1)
    ASSERT(fictifs(kptsc).eq.0)
!
    call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
    call jeveuo(nonu//'.NUML.NEQU', 'L', jnequl)
    call jeveuo(nonu//'.NUML.NLGP', 'L', jnugll)
    call jeveuo(nonu//'.NUML.PDDL', 'L', jprddl)
!
    nloc = zi(jnequl)
    nglo = zi(jnequ)
    neqg = nglo
    neql = nloc
!
    ndprop = 0
!
    do jcoll = 1, nloc
        procol = zi(jprddl-1+jcoll)
        if (procol .eq. rang) ndprop = ndprop+1
    end do
    call VecCreateMPI(mpicou, to_petsc_int(ndprop), to_petsc_int(neqg), tmp, ierr)
    ASSERT(ierr.eq.0)
!
    call VecGetOwnershipRange(tmp, low, high, ierr)
    ASSERT(ierr.eq.0)
    call VecDestroy(tmp, ierr)
    ASSERT(ierr.eq.0)
!
    call wkvect(idxd, 'V V S', ndprop, jidxd)
    call wkvect(idxo, 'V V S', ndprop, jidxo)
    call wkvect(idxdc, 'V V S', nloc, jidxdc)
    call wkvect(idxoc, 'V V S', nloc, jidxoc)
!
    jcolg = zi(jnugll)
    if (zi(jprddl) .eq. rang) then
        zi4(jidxd+jcolg-low-1) = zi4(jidxd+jcolg-low-1)+1
    else
        zi4(jidxdc) = zi4(jidxdc)+1
    endif
!
!     ON COMMENCE PAR NOTER DDL PAR DDL LE NOMBRE DE TERMES POSSEDES
!     ET CEUX QU'IL FAUDRA ENVOYER AUX AUTRES PROCESSEURS
    do jcoll = 2, nloc
        nzdeb = zi(jsmdi+jcoll-2) + 1
        nzfin = zi(jsmdi+jcoll-1)
        procol = zi(jprddl+jcoll-1)
        jcolg = zi(jnugll+jcoll-1)
        do k = nzdeb, nzfin
            iligl = zi4(jsmhc-1+k)
            prolig = zi(jprddl-1+iligl)
            iligg = zi(jnugll-1+iligl)
!         SOIT LA COLONNE ET LA LIGNE APPARTIENNENT AU PROC COURANT
!         AUQUEL CAS, ON S'EN PREOCCUPE POUR L'ALLOCATION
            if (procol .eq. rang .and. prolig .eq. rang) then
                zi4(jidxd+iligg-low-1) = zi4(jidxd+iligg-low-1)+1
                if (iligg .ne. jcolg) then
                    zi4(jidxd+jcolg-low-1) = zi4(jidxd+jcolg-low-1)+1
                endif
!           SOIT ILS N'APPARTIENNENT PAS AU PROC COURANT TOUS LES
!         DEUX, DANS CE CAS ON LES OUBLIE
            else if (procol.ne.rang.and.prolig.ne.rang) then
                if (procol .eq. prolig) then
                    zi4(jidxdc+iligl-1) = zi4(jidxdc+iligl-1)+1
                    if (iligg .ne. jcolg) then
                        zi4(jidxdc+jcoll-1) = zi4(jidxdc+jcoll-1)+1
                    endif
                else
                    zi4(jidxoc+iligl-1) = zi4(jidxoc+iligl-1)+1
                    if (iligg .ne. jcolg) then
                        zi4(jidxoc+jcoll-1) = zi4(jidxoc+jcoll-1)+1
                    endif
                endif
!         SOIT L'UN DES DEUX APPARTIENT AU PROC COURANT
!         DANS CE CAS, ON LE COMPTE POUR L'ALLOCATION
!         OU ON PREVIENT L'AUTRE PROC
            else
                if (procol .eq. rang) then
                    zi4(jidxo+jcolg-low-1) = zi4(jidxo+jcolg-low-1)+1
                    zi4(jidxoc+iligl-1) = zi4(jidxoc+iligl-1)+1
                else
                    zi4(jidxo+iligg-low-1) = zi4(jidxo+iligg-low-1)+1
                    zi4(jidxoc+jcoll-1) = zi4(jidxoc+jcoll-1)+1
                endif
            endif
        end do
    end do
!
    do iaux = 1, nbjoin
        numpro=zi(jnbjoi+iaux-1)
        if (numpro .ne. -1) then
            call codent(iaux, 'G', chnbjo)
!
            nojoin=nonu//'.NUML.'//chnbjo
            call jeveuo(nojoin, 'L', jjoint)
            call jelira(nojoin, 'LONMAX', lgenvo)
            if (lgenvo .gt. 0) then
                call wkvect(cpysol, 'V V S', lgenvo, jvaleu)
!
                if (numpro .gt. rang) then
                    call asmpi_comm_point('MPI_RECV', 'I4', numpro, iaux, nbval=lgenvo,&
                                          vi4=zi4(jvaleu))
                    do jaux = 1, lgenvo
                        numloc=zi(jjoint+jaux-1)
                        numglo=zi(jnugll+numloc-1)
                        zi4(jidxo+numglo-low-1)=zi4(jidxo+numglo-low-1)+zi4(jvaleu+jaux-1)
                    enddo
!
                    call asmpi_comm_point('MPI_RECV', 'I4', numpro, iaux, nbval=lgenvo,&
                                          vi4=zi4(jvaleu))
                    do jaux = 1, lgenvo
                        numloc=zi(jjoint+jaux-1)
                        numglo=zi(jnugll+numloc-1)
                        zi4(jidxd+numglo-low-1)=zi4(jidxd+numglo-low-1)+zi4(jvaleu+jaux-1)
                    enddo
                else if (numpro.lt.rang) then
                    do jaux = 1, lgenvo
                        numloc=zi(jjoint+jaux-1)
                        numglo=zi(jnugll+numloc-1)
                        zi4(jvaleu+jaux-1)=zi4(jidxoc+numloc-1)
                    enddo
                    call asmpi_comm_point('MPI_SEND', 'I4', numpro, iaux, nbval=lgenvo,&
                                          vi4=zi4(jvaleu))
!
                    do jaux = 1, lgenvo
                        numloc=zi(jjoint+jaux-1)
                        numglo=zi(jnugll+numloc-1)
                        zi4(jvaleu+jaux-1)=zi4(jidxdc+numloc-1)
                    enddo
                    call asmpi_comm_point('MPI_SEND', 'I4', numpro, iaux, nbval=lgenvo,&
                                          vi4=zi4(jvaleu))
                else
                    ASSERT(.false.)
                endif
                call jedetr(cpysol)
            endif
        endif
    enddo
!
    comple=nglo-ndprop
    do iaux = 1, ndprop
        zi4(jidxd+iaux-1)=min(zi4(jidxd+iaux-1),ndprop)
        zi4(jidxo+iaux-1)=min(zi4(jidxo+iaux-1),comple)
    enddo
!
    call MatCreate(mpicou, ap(kptsc), ierr)
    ASSERT(ierr.eq.0)
    call MatSetSizes(ap(kptsc), to_petsc_int(ndprop), to_petsc_int(ndprop), &
                     to_petsc_int(neqg), to_petsc_int(neqg), ierr)
    ASSERT(ierr.eq.0)
    call MatSetType(ap(kptsc), MATMPIAIJ, ierr)
    ASSERT(ierr.eq.0)
! 
#ifndef ASTER_PETSC_VERSION_LEQ_32 
!     AVEC PETSc >= 3.3
!     IL FAUT APPELER MATSETBLOCKSIZE *AVANT* MAT*SETPREALLOCATION 
    call MatSetBlockSize(ap(kptsc), to_petsc_int(bs), ierr)
    ASSERT(ierr.eq.0)
#endif 
    !
    call MatMPIAIJSetPreallocation(ap(kptsc), PETSC_NULL_INTEGER, zi4(jidxd),&
                                       PETSC_NULL_INTEGER, zi4(jidxo), ierr)
    ASSERT(ierr.eq.0)
    !
#ifdef ASTER_PETSC_VERSION_LEQ_32
!     AVEC PETSc <= 3.2
!     LE BS DOIT ABSOLUMENT ETRE DEFINI ICI
      call MatSetBlockSize(ap(kptsc), to_petsc_int(bs), ierr)
      ASSERT(ierr.eq.0)
!     RQ : A PARTIR DE LA VERSION V 3.3 IL DOIT PRECEDER LA PREALLOCATION
#endif 
!
    call jedetr(idxd)
    call jedetr(idxo)
    call jedetr(idxdc)
    call jedetr(idxoc)
!
    call jedema()
!
#else
    integer :: idummy
    idummy = kptsc
#endif
!
end subroutine
