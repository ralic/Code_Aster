subroutine apksp(kptsc)
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
#include "asterf.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: kptsc
!----------------------------------------------------------------
!
!  CREATION DU SOLVEUR DE KRYLOV PETSC (INSTANCE NUMERO KPTSC)
!
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
#include "asterf_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: nmaxit, ifm, niv
!
    character(len=24) :: algo
    character(len=19) :: nomat, nosolv
    character(len=14) :: nonu
!
    real(kind=8) :: resire
    real(kind=8), pointer :: slvr(:) => null()
    character(len=24), pointer :: slvk(:) => null()
    integer, pointer :: slvi(:) => null()
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: ierr
    PetscInt :: maxits
    PetscReal :: rtol, atol, dtol, aster_petsc_default_real
    Mat :: a
    KSP :: ksp
!=================================================================
    call jemarq()
!
    call infniv(ifm, niv)
!
#ifdef ASTER_PETSC_VERSION_LEQ_34
    aster_petsc_default_real = PETSC_DEFAULT_DOUBLE_PRECISION
#else
    aster_petsc_default_real = PETSC_DEFAULT_REAL
#endif 
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
    a = ap(kptsc)
    ksp = kp(kptsc)
!
    call jeveuo(nosolv//'.SLVK', 'L', vk24=slvk)
    call jeveuo(nosolv//'.SLVR', 'L', vr=slvr)
    call jeveuo(nosolv//'.SLVI', 'L', vi=slvi)
    algo = slvk(6)
    resire = slvr(2)
    nmaxit = slvi(2)
!
!     -- choix de l'algo KSP :
!     ------------------------
    if (algo .eq. 'CG') then
        call KSPSetType(ksp, KSPCG, ierr)
        ASSERT(ierr.eq.0)
    else if (algo.eq.'CR') then
        call KSPSetType(ksp, KSPCR, ierr)
        ASSERT(ierr.eq.0)
    else if (algo.eq.'GMRES') then
        call KSPSetType(ksp, KSPGMRES, ierr)
        ASSERT(ierr.eq.0)
    else if (algo.eq.'GCR') then
        call KSPSetType(ksp, KSPGCR, ierr)
        ASSERT(ierr.eq.0)
    else if (algo.eq.'FGMRES') then
        call KSPSetType(ksp, KSPFGMRES, ierr)
        ASSERT(ierr.eq.0)
    else
        ASSERT(.false.)
    endif
    ASSERT(ierr.eq.0)
!
!     -- paramètres numériques :
!     --------------------------
!
!     -- nb iter max :
    if (nmaxit .le. 0) then
        maxits = PETSC_DEFAULT_INTEGER
    else
        maxits = nmaxit
    endif
!
    rtol = resire
    atol = aster_petsc_default_real
    dtol = aster_petsc_default_real
!
    call KSPSetTolerances(ksp, rtol, atol, dtol, maxits, ierr)
    ASSERT(ierr.eq.0)
!
!     - pour suivre les itérations de Krylov
!     --------------------------------------
    if (niv .ge. 2) then
        call KSPMonitorSet(ksp, KSPMonitorTrueResidualNorm, PETSC_NULL_OBJECT,&
                           PETSC_NULL_FUNCTION, ierr)
        ASSERT(ierr.eq.0)
    endif
!
    call jedema()
!
#endif
!
end subroutine
