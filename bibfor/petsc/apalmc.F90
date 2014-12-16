subroutine apalmc(kptsc)
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
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/apbloc.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
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
!  PREALLOCATION DANS LE CAS GENERAL
!
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
#include "asterf_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: rang, nbproc
    integer :: nsmdi, nsmhc, nz, bs, nblloc2, fictif
    integer :: jidxd, jidxo
    integer :: i, k, ilig1, jcol1, ilig2,jcol2,nbo, nbd, nzdeb, nzfin, nbterm
    mpi_int :: mpicou
    integer(kind=4), pointer :: new_ieq(:) => null()
    integer(kind=4), pointer :: old_ieq(:) => null()
    integer, pointer :: smdi(:) => null()
    integer(kind=4), pointer :: smhc(:) => null()
!
    character(len=19) :: nomat, nosolv
    character(len=16) :: idxo, idxd
    character(len=14) :: nonu
!
    parameter   (idxo  ='&&APALMC.IDXO___')
    parameter   (idxd  ='&&APALMC.IDXD___')
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: low2, high2, low1, high1,ierr
    integer :: neq, neq2
    Vec :: vtmp
    Mat :: a
    mpi_int :: mrank, msize
!----------------------------------------------------------------
    call jemarq()
!
!   -- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
!
    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', vi4=smhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc)
    neq = nsmdi
    nz = smdi(neq)
!
    call apbloc(kptsc)
    bs=tblocs(kptsc)

    ASSERT(bs.ge.1)

    fictif=fictifs(kptsc)
    if (fictif.eq.1) then
        new_ieq => new_ieqs(kptsc)%pi4
        old_ieq => old_ieqs(kptsc)%pi4
        ASSERT(size(new_ieq).eq.neq)
        neq2=size(old_ieq)
        ASSERT(neq2.gt.neq)
    else
        neq2=neq
        allocate(new_ieq(neq))
        allocate(old_ieq(neq))
        do k=1,neq
            new_ieq(k)=k
            old_ieq(k)=k
        enddo
    endif
    ASSERT(mod(neq2,bs).eq.0)

!
!     -- RECUPERE LE RANG DU PROCESSUS ET LE NB DE PROCS
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
!
!     low2  DONNE LA PREMIERE LIGNE STOCKEE LOCALEMENT
!     high2 DONNE LA PREMIERE LIGNE STOCKEE PAR LE PROCESSUS DE (RANG+1)
!     *ATTENTION* CES INDICES COMMENCENT A ZERO (CONVENTION C DE PETSc)
!
!     ON EST OBLIGE DE PASSER PAR UN VECTEUR TEMPORAIRE CONSTRUIT
!     PAR MORCEAUX POUR OBTENIR LE BON DECOUPAGE PAR BLOC
    call VecCreate(mpicou, vtmp, ierr)
    ASSERT(ierr.eq.0)
    call VecSetBlockSize(vtmp, to_petsc_int(bs), ierr)
    ASSERT(ierr.eq.0)
    call VecSetSizes(vtmp, PETSC_DECIDE, to_petsc_int(neq2), ierr)
    ASSERT(ierr.eq.0)
    call VecSetType(vtmp, VECMPI, ierr)
    ASSERT(ierr.eq.0)
!
    call VecGetOwnershipRange(vtmp, low2, high2, ierr)
    ASSERT(ierr.eq.0)
    call VecDestroy(vtmp, ierr)
    ASSERT(ierr.eq.0)

!   -- NB DE LIGNES QUE L'ON STOCKE LOCALEMENT
    nblloc2 = high2 - low2

!   -- CES DEUX VECTEURS SONT LES D_NNZ ET O_NNZ A PASSER A PETSc
    call wkvect(idxo, 'V V S', nblloc2, jidxo)
    call wkvect(idxd, 'V V S', nblloc2, jidxd)



!   -- On commence par s'occuper du nombre de nz par ligne dans le bloc diagonal
!      Indices C : jcol2
!      Indices F : jcol1, ilig1, ilig2
!   -----------------------------------------------------------------------------
    do jcol2 = low2, high2-1
        jcol1=old_ieq(jcol2+1)
        nbo = 0
        nbd = 0
        if (jcol1.gt.0) then
!           -- jcol2 est un vrai ddl :
            if (jcol1.eq.1) then
                nzdeb = 1
            else
                ASSERT(jcol1.ge.2)
                nzdeb = smdi(jcol1-1) + 1
            endif
            nzfin = smdi(jcol1)
            do k = nzdeb, nzfin
                ilig1 = smhc(k)
                ilig2 = new_ieq(ilig1)
                if (ilig2 .lt. (low2+1)) then
                    nbo = nbo + 1
                else
                    nbd = nbd + 1
                    zi4(jidxd-1+(ilig2-low2)) = zi4(jidxd-1+(ilig2-low2)) + 1
                endif
            end do
        else
!           -- jcol2 est un ddl fictif, on ne stocke que le terme diagonal "1" :
            ilig2=jcol2+1
            nbo=0
            nbd=1
            zi4(jidxd-1+(ilig2-low2)) = zi4(jidxd-1+(ilig2-low2)) + 1
        endif
        zi4(jidxd-1+(jcol2+1-low2)) = zi4(jidxd-1+(jcol2+1-low2)) + nbd - 1
        zi4(jidxo-1+(jcol2+1-low2)) = zi4(jidxo-1+(jcol2+1-low2)) + nbo
    end do


!   -- Ensuite on complete le tableau du bloc hors diagonal
!      Indices C : jcol2
!      Indices F : jcol1, ilig1, ilig2
!   ---------------------------------------------------------
    do jcol2 = high2, neq2-1
        jcol1=old_ieq(jcol2+1)
!       -- les ddls fictifs n'ont pas de termes dans ce bloc :
        if (jcol1.eq.0) cycle

        ASSERT(jcol1.ge.2)
        nzdeb = smdi(jcol1-1) + 1
        nzfin = smdi(jcol1)
        do k = nzdeb, nzfin
            ilig1 = smhc(k)
            ilig2 = new_ieq(ilig1)
            if (ilig2 .lt. (low2+1)) then
                continue
            else if (ilig2.le.high2) then
                zi4(jidxo-1+(ilig2-low2)) = zi4(jidxo-1+(ilig2-low2)) + 1
            else
                exit
            endif
        end do
    end do
!
    call MatCreate(mpicou, a, ierr)
    ASSERT(ierr.eq.0)
    call MatSetSizes(a, to_petsc_int(nblloc2), to_petsc_int(nblloc2), &
                     to_petsc_int(neq2), to_petsc_int(neq2),&
                     ierr)
    ASSERT(ierr.eq.0)
!
#ifndef ASTER_PETSC_VERSION_LEQ_32
!   AVEC PETSc >= 3.3
!   IL FAUT APPELER MATSETBLOCKSIZE *AVANT* MAT*SETPREALLOCATION
    call MatSetBlockSize(a, to_petsc_int(bs), ierr)
    ASSERT(ierr.eq.0)
#endif

    if (nbproc .eq. 1) then
        call MatSetType(a, MATSEQAIJ, ierr)
        ASSERT(ierr.eq.0)
        call MatSEQAIJSetPreallocation(a, PETSC_NULL_INTEGER, zi4( jidxd), ierr)
        ASSERT(ierr.eq.0)
    else
        call MatSetType(a, MATMPIAIJ, ierr)
        ASSERT(ierr.eq.0)
        call MatMPIAIJSetPreallocation(a, PETSC_NULL_INTEGER, zi4( jidxd),&
                                       PETSC_NULL_INTEGER, zi4(jidxo), ierr)
        ASSERT(ierr.eq.0)
    endif
!
#ifdef ASTER_PETSC_VERSION_LEQ_32
!      LE BS DOIT ABSOLUMENT ETRE DEFINI ICI
    call MatSetBlockSize(a, to_petsc_int(bs), ierr)
    ASSERT(ierr.eq.0)
!   RQ : A PARTIR DE LA VERSION V 3.3 IL DOIT PRECEDER LA PREALLOCATION
#endif
!
    ap(kptsc)=a


!   -- menage :
!   -----------
    call jedetr(idxo)
    call jedetr(idxd)
    if (fictif.eq.0) then
        deallocate(new_ieq)
        deallocate(old_ieq)
    endif

    call jedema()
!
#else
    integer :: idummy
    idummy = kptsc
#endif
!
end subroutine
