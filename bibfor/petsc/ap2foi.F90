subroutine ap2foi(kptsc, mpicou, nosolv, lmd, indic,&
                  its)
    implicit none
! person_in_charge: jacques.pellet at edf.fr

!
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
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterf_petsc.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/apksp.h"
#include "asterfort/appcpr.h"
#include "asterfort/appcrs.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jedema.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpu.h"

!
!--------------------------------------------------------------
! But :
!  * recalculer un nouveau pre-conditionneur pour LDLT_SP
!  * faire une resolution avec ce nouveau preconditionneur
!---------------------------------------------------------------
!
#ifdef _HAVE_PETSC

!
    integer :: kptsc
    mpi_int :: mpicou
    character(len=19) :: nosolv
    aster_logical :: lmd
    KSPConvergedReason :: indic
    PetscInt :: its
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer, dimension(:), pointer :: slvi => null()
    PetscInt :: ierr
    KSP :: ksp
!----------------------------------------------------------------

!
!   -- bascule pour la mesure du temps CPU : RESOUD -> PRERES :
    call uttcpu('CPU.RESO.5', 'FIN', ' ')
    call uttcpu('CPU.RESO.4', 'DEBUT', ' ')
!
!
!   -- avant de refabriquer une matrice de preconditionnement,
!      il faut reinitialiser quelques variables :
!   -----------------------------------------------------------
    spmat=' '
    spsolv=' '
    call KSPDestroy(kp(kptsc), ierr)
    call KSPCreate(mpicou, kp(kptsc), ierr)
    ksp=kp(kptsc)
    ASSERT(ierr.eq.0)
#ifdef ASTER_PETSC_VERSION_LEQ_34
    call KSPSetOperators(kp(kptsc), ap(kptsc), ap(kptsc), DIFFERENT_NONZERO_PATTERN, ierr)
#else
    call KSPSetOperators(kp(kptsc), ap(kptsc), ap(kptsc), ierr)
#endif
    ASSERT(ierr.eq.0)
!
!   slvi(5) = nombre d'itérations pour atteindre la convergence du solveur linéaire.
!   si :
!   - slvi(5) = 0 (on résout pour la première fois),
!   - slvi(5) > reac_precond (la résolution linéaire précédente a demandé
!                            "trop" d'itérations),
!   alors il faut effectuer le calcul du préconditionneur LDLT_SP (voir pcmump)
!
    call jeveuo(nosolv//'.SLVI', 'E', vi=slvi)
    slvi(5) = 0 
!
!   -- calcul du nouveau preconditionneur :
!   ---------------------------------------
    call appcpr(kptsc)
!
!   -- 2eme resolution :
!   ---------------------
    call VecDestroy(xlocal, ierr)
    call VecDestroy(xglobal, ierr)
    call VecScatterDestroy(xscatt, ierr)
    xlocal=0
    xglobal=0
    xscatt=0
    call apksp(kptsc)
    call appcrs(kptsc, lmd)
    call KSPSolve(ksp, b, x, ierr)
    ASSERT(ierr.eq.0)
    call KSPGetConvergedReason(ksp, indic, ierr)
    call KSPGetIterationNumber(ksp, its, ierr)


!
!
!   -- bascule pour la mesure du temps CPU : PRERES -> RESOUD :
    call uttcpu('CPU.RESO.4', 'FIN', ' ')
    call uttcpu('CPU.RESO.5', 'DEBUT', ' ')

!
#else
    integer :: kptsc
    integer :: mpicou
    character(len=19) :: nosolv
    aster_logical :: lmd
    integer :: indic
    integer :: its

!
    character(len=1) :: kdummy
    integer :: idummy
    kdummy = nosolv(1:1)
    idummy = kptsc
    idummy = mpicou
    idummy = indic
    if (lmd) idummy = its
#endif
!
end subroutine

