subroutine apksp(kptsc)
    implicit none
!           CONFIGURATION MANAGEMENT OF EDF VERSION
! ==================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ==================================================================
! person_in_charge: thomas.desoza at edf.fr
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: kptsc
!----------------------------------------------------------------
!
!  CREATION DU SOLVEUR DE KRYLOV PETSC (INSTANCE NUMERO KPTSC)
!
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
!================================================================
# include "finclude/petscsys.h"
# include "finclude/petscvec.h"
# include "finclude/petscmat.h"
# include "finclude/petscksp.h"
# include "finclude/petscpc.h"
!================================================================
!----------------------------------------------------------------
!     AU PLUS 5 MATRICES PETSC SONT GEREES A LA FOIS
    integer :: nmxins
    parameter   (nmxins=5)
!
!     VARIABLES LOCALES
    integer :: jslvk, jslvr, jslvi
    integer :: nmaxit, ifm, niv
!
    character(len=24) :: algo
    character(len=19) :: nomat, nosolv
    character(len=14) :: nonu
!
    real(kind=8) :: resire
!
!     COMMUN DE SAUVEGARDE DES INSTANCES
    character(len=19) :: nomats(nmxins), nosols(nmxins)
    character(len=14) :: nonus(nmxins)
    Mat :: ap(nmxins)
    KSP :: kp(nmxins)
    Vec :: b, x
    common /spetsc/ ap,kp,b,x,nomats,nosols,nonus
!=================================================================
!     Variables PETSc
    PetscInt :: maxits, ierr
    PetscReal :: rtol, atol, dtol
    Mat :: a
    KSP :: ksp
!=================================================================
    call jemarq()
!
    call infniv(ifm, niv)
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
    a = ap(kptsc)
    ksp = kp(kptsc)
!
    call jeveuo(nosolv//'.SLVK', 'L', jslvk)
    call jeveuo(nosolv//'.SLVR', 'L', jslvr)
    call jeveuo(nosolv//'.SLVI', 'L', jslvi)
    algo = zk24(jslvk-1+6)
    resire = zr(jslvr-1+2)
    nmaxit = zi(jslvi-1+2)
!
!     -- choix de l'algo KSP :
!     ------------------------
    if (algo .eq. 'CG') then
        call KSPSetType(ksp, KSPCG, ierr)
        call assert(ierr.eq.0)
    else if (algo.eq.'CR') then
        call KSPSetType(ksp, KSPCR, ierr)
        call assert(ierr.eq.0)
    else if (algo.eq.'GMRES') then
        call KSPSetType(ksp, KSPGMRES, ierr)
        call assert(ierr.eq.0)
    else if (algo.eq.'GCR') then
        call KSPSetType(ksp, KSPGCR, ierr)
        call assert(ierr.eq.0)
    else if (algo.eq.'BCGS') then
        call KSPSetType(ksp, KSPBCGS, ierr)
        call assert(ierr.eq.0)
    else
        call assert(.false.)
    endif
    call assert(ierr.eq.0)
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
    atol = PETSC_DEFAULT_DOUBLE_PRECISION
    dtol = PETSC_DEFAULT_DOUBLE_PRECISION
!
    call KSPSetTolerances(ksp, rtol, atol, dtol, maxits,&
                          ierr)
    call assert(ierr.eq.0)
!
!     - pour suivre les itérations de Krylov
!     --------------------------------------
    if (niv .ge. 2) then
        call KSPMonitorSet(ksp, KSPMonitorTrueResidualNorm, PETSC_NULL_OBJECT,&
                           PETSC_NULL_FUNCTION, ierr)
        call assert(ierr.eq.0)
    endif
!
    call jedema()
!
#endif
!
end subroutine
