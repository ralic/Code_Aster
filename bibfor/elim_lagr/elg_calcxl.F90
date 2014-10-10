subroutine elg_calcxl(x1, vlag)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
!----------------------------------------------------------------
! Calcul des coefficients de Lagrange pour ELIM_LAGR='OUI'
!     IN  : Vec X1    (solution sur les dds physiques)
!
!     OUT : Vec VLAG  (solution sur les dds Lagrange "1")
!
!     On utilise la méthode des moindres carrés (LSQR dans PETSc)
!     Rq: La méthode originale utilise une factorisation QR
!     préalable de A. Le calcul des multiplicateurs s'effectue alors
!     par des descentes remontées:
!
!     Les coefficients de Lagrange (1 et 2) sont
!     calculés par :
!       L = (R'*R) \ A*(b - B*x)
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
#include "asterfort/elg_allocvr.h"
!
    Vec :: x1, vlag
    mpi_int :: mpicomm
    KSP :: ksp
    PC :: pc
!
!================================================================
    integer :: ifm, niv
    real(kind=8) :: norm
    aster_logical :: info
    PetscInt :: n1, n2, n3
    PetscInt :: ierr
    PetscScalar :: neg_one
    PetscOffset :: xidxay, xidxl
    Mat :: cct
    Vec :: bx, y, ay, xtmp, xlag
    PetscInt :: its
    PetscReal :: aster_petsc_default_real

!----------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
    info=niv.eq.2
!
#ifdef ASTER_PETSC_VERSION_LEQ_34
    aster_petsc_default_real = PETSC_DEFAULT_DOUBLE_PRECISION
#else
    aster_petsc_default_real = PETSC_DEFAULT_REAL
#endif
!
!     -- dimensions :
!       n1 : # ddls physiques
!       n2 : # lagranges "1"
!     ----------------------------------------------------------
    call MatGetSize(melim(ke)%ctrans, n1, n2, ierr)
!
!     -- vérifs :
    call VecGetSize(x1, n3, ierr)
    ASSERT(n3.eq.n1)
    call VecGetSize(vlag, n3, ierr)
    ASSERT(n3.eq.n2)
!
!
!     -- calcul de BX = B*x :
    call VecDuplicate(x1, bx, ierr)
    call MatMult(melim(ke)%matb, x1, bx, ierr)
!
!
!     -- calcul de Y = b - B*x :
    call VecDuplicate(melim(ke)%vecb, y, ierr)
    call VecCopy(melim(ke)%vecb, y, ierr)
    call VecAXPY(y, neg_one, bx, ierr)
!
!     -- calcul de AY = A*Y
    call elg_allocvr(ay, int(n2))
    call MatMultTranspose(melim(ke)%ctrans, y, ay, ierr)
!
!     -- résolution : z = (R*R') \ AY :
!
    call asmpi_comm('GET', mpicomm)
!
!   Initialisation du solveur PETSc : on utilise LSQR
!   qui calcule la solution aux moindres carrés d'un système linéaire sur-déterminé
!   C^T Vlag = AY
!   ksp = solver context
    call KSPCreate(mpicomm, ksp, ierr)
!
!   Calcul de C C^T (utilisée pour construire le préconditionneur)

#ifdef ASTER_PETSC_VERSION_LEQ_32
    call MatMatMultTranspose(melim(ke)%ctrans, melim(ke)%ctrans, MAT_INITIAL_MATRIX, &
         PETSC_DEFAULT_DOUBLE_PRECISION, cct, ierr)
#else
    call MatTransposeMatMult(melim(ke)%ctrans, melim(ke)%ctrans, MAT_INITIAL_MATRIX, &
         aster_petsc_default_real, cct, ierr)
#endif
    ASSERT( ierr==0 )
!   Set linear solver : LSQR
    call KSPSetType(ksp, KSPLSQR, ierr)
!   Set linear system
#ifdef ASTER_PETSC_VERSION_LEQ_34
    call KSPSetOperators(ksp, melim(ke)%ctrans, cct, SAME_PRECONDITIONER, ierr)
#else
    call KSPSetOperators(ksp, melim(ke)%ctrans, cct, ierr)
#endif
!   Solve linear system  C^T * Vlag = AY
    call KSPSolve( ksp, y, vlag, ierr)
    if (info) then
      call VecDuplicate(y, xtmp , ierr)
      call MatMult(melim(ke)%ctrans, vlag, xtmp, ierr)
      call VecAXPY(xtmp,neg_one,y ,ierr)
      call VecNorm(xtmp,norm_2,norm,ierr)
      call KSPGetIterationNumber(ksp,its,ierr)
      call VecDuplicate(vlag, xlag , ierr)
      call MatMultTranspose(melim(ke)%ctrans, xtmp, xlag, ierr)
      call VecNorm(xlag,norm_2,norm,ierr)

      write(6,100) norm,its
  100 format('CALCXL: Norm of error = ',e11.4,',  Number of iterations = ',i5)
      call VecDestroy(xtmp, ierr)
      call VecDestroy(xlag, ierr)
    endif
!
!     -- ménage :
    call VecDestroy(bx, ierr)
    call VecDestroy(y, ierr)
    call VecDestroy(ay, ierr)
    call KSPDestroy(ksp, ierr)
    call MatDestroy(cct, ierr)
!
    call jedema()
!
#else
    integer :: x1, vlag
    vlag = x1
    ASSERT(.false.)
#endif
!
end subroutine
