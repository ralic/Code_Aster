subroutine appcrs(kptsc, lmd)
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
! person_in_charge: natacha.bereux at edf.fr
! aslint:disable=C1308
use petsc_data_module
    implicit none
    
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/crsmsp.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: kptsc
    aster_logical :: lmd
!----------------------------------------------------------------
!
!  CREATION DU PRECONDITIONNEUR PETSC (INSTANCE NUMERO KPTSC)
!  PHASE DE RESOLUTION (RESOUD)
!
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
#include "asterfort/ldsp1.h"
#include "asterfort/ldsp2.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: rang, nbproc
    integer :: jslvk, jslvr, jslvi, jnequ, jnequl, jprddl, jcoll, nloc
    integer :: niremp, nsmdi
    mpi_int :: mpicou
!
    character(len=24) :: precon
    character(len=19) :: nomat, nosolv
    character(len=14) :: nonu
!
    real(kind=8) :: fillin
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscErrorCode ::  ierr
    integer :: fill, neq, ndprop
    PetscReal :: fillp
    Mat :: a
    KSP :: ksp, kspp
    PC :: pc, pcp
    mpi_int :: mrank, msize
!----------------------------------------------------------------
    call jemarq()
!
!   -- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
!
!     -- LECTURE DU COMMUN
    nomat = nomat_courant
    nonu = nonu_courant
    nosolv = nosols(kptsc)
    a = ap(kptsc)
    ksp = kp(kptsc)
!
    call jeveuo(nosolv//'.SLVK', 'L', jslvk)
    call jeveuo(nosolv//'.SLVR', 'L', jslvr)
    call jeveuo(nosolv//'.SLVI', 'L', jslvi)
    precon = zk24(jslvk-1+2)
    fillin = zr(jslvr-1+3)
    niremp = zi(jslvi-1+4)
!
    fill = niremp
    fillp = fillin
!
!     -- RECUPERE LE RANG DU PROCESSUS ET LE NB DE PROCS
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
!
!     -- CAS PARTICULIER (LDLT_INC/SOR)
!     -- CES PC NE SONT PAS PARALLELISES
!     -- ON UTILISE DONC DES VERSIONS PAR BLOC
!     ----------------------------------------
    if ((precon.eq.'LDLT_INC') .or. (precon.eq.'SOR')) then
        if (nbproc .gt. 1) then
            kspp=ksp
            call KSPGetPC(kspp, pcp, ierr)
            ASSERT(ierr.eq.0)
            call PCSetType(pcp, PCBJACOBI, ierr)
            ASSERT(ierr.eq.0)
            call KSPSetUp(kspp, ierr)
            ASSERT(ierr.eq.0)
            call PCBJacobiGetSubKSP(pcp, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, ksp, ierr)
            ASSERT(ierr.eq.0)
        else
            goto 999
        endif
    endif
!
!     -- choix du preconditionneur :
!     -------------------------------
    call KSPGetPC(ksp, pc, ierr)
    ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    if (precon .eq. 'LDLT_INC') then
        call PCSetType(pc, PCILU, ierr)
        ASSERT(ierr.eq.0)
        call PCFactorSetLevels(pc, to_petsc_int(fill), ierr)
        ASSERT(ierr.eq.0)
        call PCFactorSetFill(pc, fillp, ierr)
        ASSERT(ierr.eq.0)
        call PCFactorSetMatOrderingType(pc, MATORDERINGNATURAL, ierr)
        ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    else if (precon.eq.'LDLT_SP') then
!        CREATION SOLVEUR BIDON SIMPLE PRECISION
        spsomu = zk24(jslvk-1+3)(1:19)
        call crsmsp(spsomu, nomat, 0)
!        CREATION DES VECTEURS TEMPORAIRES UTILISES DANS LDLT_SP
        if (lmd) then
            call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
            call jeveuo(nonu//'.NUML.NEQU', 'L', jnequl)
            call jeveuo(nonu//'.NUML.PDDL', 'L', jprddl)
            nloc=zi(jnequl)
            neq=zi(jnequ)
            ndprop = 0
            do jcoll = 0, nloc-1
                if (zi(jprddl+jcoll) .eq. rang) ndprop = ndprop+1
            end do
!
            ASSERT(xlocal.eq.0)
            call VecCreateMPI(mpicou, to_petsc_int(ndprop), to_petsc_int(neq), xlocal, ierr)
        else
            call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
            neq=nsmdi
!
            ASSERT(xlocal.eq.0)
            call VecCreateMPI(mpicou, PETSC_DECIDE, to_petsc_int(neq), xlocal, ierr)
        endif
        ASSERT(ierr.eq.0)
!
        ASSERT(xscatt.eq.0)
        ASSERT(xglobal.eq.0)
        call VecScatterCreateToAll(xlocal, xscatt, xglobal, ierr)
        ASSERT(ierr.eq.0)
!-----------------------------------------------------------------------
    else if (precon.eq.'SOR') then
        call PCSetType(pc, PCSOR, ierr)
        ASSERT(ierr.eq.0)
    endif
!-----------------------------------------------------------------------
!
!     CREATION EFFECTIVE DES PRECONDITIONNEURS RETARDES
    if ((precon.eq.'LDLT_INC') .or. (precon.eq.'SOR')) then
        call PCSetUp(pc, ierr)
        if (ierr .ne. 0) then
            call utmess('F', 'PETSC_14')
        endif
    endif
!
999 continue
!
    call jedema()
!
#else
    integer :: idummy
    aster_logical :: ldummy
    idummy = kptsc
    ldummy = lmd
#endif
!
end subroutine
