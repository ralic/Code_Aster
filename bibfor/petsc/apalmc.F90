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
    integer :: nsmdi, nsmhc, nz, tbloc, nblloc
    integer :: jsmdi, jsmhc, jidxd, jidxo
    integer :: i, k, ilig, jcol, nbo, nbd, nzdeb, nzfin
    mpi_int :: mpicou
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
    PetscInt :: low, high, ierr
    integer :: neq, bs
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
    call jeveuo(nonu//'.SMOS.SMDI', 'L', jsmdi)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc)
    neq = nsmdi
    nz = zi(jsmdi-1+nsmdi)
!
    call apbloc(nomat, nosolv, tbloc)
    bs = abs(tbloc)
!
!     -- RECUPERE LE RANG DU PROCESSUS ET LE NB DE PROCS
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
!
!     low DONNE LA PREMIERE LIGNE STOCKEE LOCALEMENT
!     high DONNE LA PREMIERE LIGNE STOCKEE PAR LE PROCESSUS DE (RANG+1)
!     *ATTENTION* CES INDICES COMMENCENT A ZERO (CONVENTION C DE PETSc)
!
!     ON EST OBLIGE DE PASSER PAR UN VECTEUR TEMPORAIRE CONSTRUIT
!     PAR MORCEAUX POUR OBTENIR LE BON DECOUPAGE PAR BLOC
    call VecCreate(mpicou, vtmp, ierr)
    ASSERT(ierr.eq.0)
    call VecSetBlockSize(vtmp, bs, ierr)
    ASSERT(ierr.eq.0)
    call VecSetSizes(vtmp, PETSC_DECIDE, neq, ierr)
    ASSERT(ierr.eq.0)
    call VecSetType(vtmp, VECMPI, ierr)
    ASSERT(ierr.eq.0)
!
    call VecGetOwnershipRange(vtmp, low, high, ierr)
    ASSERT(ierr.eq.0)
    call VecDestroy(vtmp, ierr)
    ASSERT(ierr.eq.0)
!
!     NB DE LIGNES QUE L'ON STOCKE LOCALEMENT
    nblloc = high - low
!
!     CES DEUX VECTEURS SONT LES D_NNZ ET O_NNZ A PASSER A PETSc
    call wkvect(idxo, 'V V S', nblloc, jidxo)
    call wkvect(idxd, 'V V S', nblloc, jidxd)
!
!     INITIALISATION DES DEUX VECTEURS
    do i = 1, nblloc
        zi4(jidxo-1+i) = 0
        zi4(jidxd-1+i) = 0
    end do
!
    if (low .eq. 0) then
        zi4(jidxo-1+1) = 0
        zi4(jidxd-1+1) = 1
    else
        zi4(jidxo-1+1) = zi(jsmdi+low) - zi(jsmdi+low-1) - 1
        zi4(jidxd-1+1) = 1
    endif
!
    nbo = 0
    nbd = 0
!
!     ON COMMENCE PAR S'OCCUPER DU NOMBRE DE NZ PAR LIGNE
!     DANS LE BLOC DIAGONAL
    do jcol = low+1, high-1
        nzdeb = zi(jsmdi+jcol-1) + 1
        nzfin = zi(jsmdi+jcol)
        do k = nzdeb, nzfin
            ilig = zi4(jsmhc-1+k)
            if (ilig .lt. (low+1)) then
                nbo = nbo + 1
            else
                nbd = nbd + 1
                zi4(jidxd-1+(ilig-low)) = zi4(jidxd-1+(ilig-low)) + 1
            endif
        end do
        zi4(jidxd-1+(jcol+1-low)) = zi4(jidxd-1+(jcol+1-low)) + nbd - 1
        zi4(jidxo-1+(jcol+1-low)) = zi4(jidxo-1+(jcol+1-low)) + nbo
        nbd = 0
        nbo = 0
    end do
!
!     ENSUITE ON COMPLETE LE TABLEAU DU BLOC HORS DIAGONAL
    do jcol = high, neq-1
        nzdeb = zi(jsmdi+jcol-1) + 1
        nzfin = zi(jsmdi+jcol)
        do k = nzdeb, nzfin
            ilig = zi4(jsmhc-1+k)
            if (ilig .lt. (low+1)) then
                continue
            else if (ilig.le.high) then
                zi4(jidxo-1+(ilig-low)) = zi4(jidxo-1+(ilig-low)) + 1
            else
                exit
            endif
        end do
    end do
!
    call MatCreate(mpicou, a, ierr)
    ASSERT(ierr.eq.0)
    call MatSetSizes(a, nblloc, nblloc, neq, neq,&
                     ierr)
    ASSERT(ierr.eq.0)
!
#ifndef ASTER_PETSC_VERSION_32 
!   AVEC PETSc >= 3.3
!   IL FAUT APPELER MATSETBLOCKSIZE *AVANT* MAT*SETPREALLOCATION
    call MatSetBlockSize(a, bs, ierr)
    ASSERT(ierr.eq.0)
#endif 
    
    if (nbproc .eq. 1) then
        call MatSetType(a, MATSEQAIJ, ierr)
        ASSERT(ierr.eq.0)
        call MatSEQAIJSetPreallocation(a, int(PETSC_NULL_INTEGER), zi4( jidxd), ierr)
        ASSERT(ierr.eq.0)
    else
        call MatSetType(a, MATMPIAIJ, ierr)
        ASSERT(ierr.eq.0)
        call MatMPIAIJSetPreallocation(a, int(PETSC_NULL_INTEGER), zi4( jidxd),&
                                       int(PETSC_NULL_INTEGER), zi4(jidxo), ierr)
        ASSERT(ierr.eq.0)
    endif
!
#ifdef ASTER_PETSC_VERSION_32 
!     LE BS DOIT ABSOLUMENT ETRE DEFINI ICI
      call MatSetBlockSize(a, bs, ierr)
      ASSERT(ierr.eq.0)
!     RQ : A PARTIR DE LA VERSION V 3.3 IL DOIT PRECEDER LA PREALLOCATION
#endif 
!      
      ap(kptsc)=a
!
!     ON N'OUBLIE PAS DE DETRUIRE LES TABLEAUX
!     APRES AVOIR ALLOUE CORRECTEMENT
    call jedetr(idxo)
    call jedetr(idxd)
!
    call jedema()
!
#else
    integer :: idummy
    idummy = kptsc
#endif
!
end subroutine
