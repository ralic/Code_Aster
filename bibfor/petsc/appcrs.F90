subroutine appcrs(kptsc, lmd)
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
#include "asterfort/comcou.h"
#include "asterfort/crsmsp.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ldsp1.h"
#include "asterfort/ldsp2.h"
#include "asterfort/mpicm0.h"
#include "asterfort/u2mess.h"
    integer :: kptsc
    logical :: lmd
!----------------------------------------------------------------
!
!  CREATION DU PRECONDITIONNEUR PETSC (INSTANCE NUMERO KPTSC)
!  PHASE DE RESOLUTION (RESOUD)
!
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "aster_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: rang, nbproc
    integer :: jslvk, jslvr, jslvi, jnequ, jnequl, jprddl, jcoll, nloc
    integer :: niremp, nsmdi
    integer(kind=4) :: mpicou
!
    character(len=24) :: precon
    character(len=19) :: nomat, nosolv
    character(len=14) :: nonu
    character(len=4) :: kbid
!
    real(kind=8) :: fillin
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: fill, ierr, neq, ndprop
    PetscReal :: fillp
    Mat :: a
    KSP :: ksp, kspp
    PC :: pc, pcp
!----------------------------------------------------------------
    call jemarq()
!---- COMMUNICATEUR MPI DE TRAVAIL
    mpicou=comcou(1)
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
    precon = zk24(jslvk-1+2)
    fillin = zr(jslvr-1+3)
    niremp = zi(jslvi-1+4)
!
    fill = niremp
    fillp = fillin
!
!     -- RECUPERE LE RANG DU PROCESSUS ET LE NB DE PROCS
    call mpicm0(rang, nbproc)
!
!     -- CAS PARTICULIER (LDLT_INC/SOR)
!     -- CES PC NE SONT PAS PARALLELISES
!     -- ON UTILISE DONC DES VERSIONS PAR BLOC
!     ----------------------------------------
    if ((precon.eq.'LDLT_INC') .or. (precon.eq.'SOR')) then
        if (nbproc .gt. 1) then
            kspp=ksp
            call KSPGetPC(kspp, pcp, ierr)
            call assert(ierr.eq.0)
            call PCSetType(pcp, PCBJACOBI, ierr)
            call assert(ierr.eq.0)
            call KSPSetUp(kspp, ierr)
            call assert(ierr.eq.0)
            call PCBJacobiGetSubKSP(pcp, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, ksp, ierr)
            call assert(ierr.eq.0)
        else
            goto 999
        endif
    endif
!
!     -- choix du preconditionneur :
!     -------------------------------
    call KSPGetPC(ksp, pc, ierr)
    call assert(ierr.eq.0)
!-----------------------------------------------------------------------
    if (precon .eq. 'LDLT_INC') then
        call PCSetType(pc, PCILU, ierr)
        call assert(ierr.eq.0)
        call PCFactorSetLevels(pc, fill, ierr)
        call assert(ierr.eq.0)
        call PCFactorSetFill(pc, fillp, ierr)
        call assert(ierr.eq.0)
        call PCFactorSetMatOrderingType(pc, MATORDERINGNATURAL, ierr)
        call assert(ierr.eq.0)
!-----------------------------------------------------------------------
    else if (precon.eq.'LDLT_SP') then
!        CREATION SOLVEUR BIDON SIMPLE PRECISION
        spsomu = zk24(jslvk-1+3)
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
            call assert(xlocal.eq.0)
            call VecCreateMPI(mpicou, ndprop, neq, xlocal, ierr)
        else
            call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi, kbid)
            neq=nsmdi
!
            call assert(xlocal.eq.0)
            call VecCreateMPI(mpicou, PETSC_DECIDE, neq, xlocal, ierr)
        endif
        call assert(ierr.eq.0)
!
        call assert(xscatt.eq.0)
        call assert(xglobal.eq.0)
        call VecScatterCreateToAll(xlocal, xscatt, xglobal, ierr)
        call assert(ierr.eq.0)
!-----------------------------------------------------------------------
    else if (precon.eq.'SOR') then
        call PCSetType(pc, PCSOR, ierr)
        call assert(ierr.eq.0)
    endif
!-----------------------------------------------------------------------
!
!     CREATION EFFECTIVE DES PRECONDITIONNEURS RETARDES
    if ((precon.eq.'LDLT_INC') .or. (precon.eq.'SOR')) then
        call PCSetUp(pc, ierr)
        if (ierr .ne. 0) then
            call u2mess('F', 'PETSC_14')
        endif
    endif
!
999  continue
!
    call jedema()
!
#endif
!
end subroutine
