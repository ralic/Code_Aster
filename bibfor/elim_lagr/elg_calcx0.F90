subroutine elg_calcx0()
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
# include "jeveux.h"
# include "asterfort/assert.h"
# include "asterfort/jedema.h"
# include "asterfort/jemarq.h"
# include "asterc/asmpi_comm.h"
!----------------------------------------------------------------
!
!     Résolution de x0 = A \ c par la méthode des moindres carrés.
!     (appel d'une méthode native de PETSc) 
!  
!     Rq: La méthode originale de résolution est basée sur une 
!         factorisation QR préalable de A. La solution x0 
!         est obtenue par:   
!       z   = (L'*L) \  c  (par descente / remontée)
!       x0  = A' * z
!       avec : 
!     * Mat A'  : matrice des contraintes linéaires
!       A' utilisée est ELIMLG/Ctrans
!     * Mat L'  : matrice triangulaire supérieure
!       L' utilisée est ELIMLG/RCt
!       Remarque : L est telle que : A*A'=L'*L
!
!     * Vec VecC (second membre c)
!       VecC utilisé est ELIMLG/VecC
!     * Vec Vx0  (solution)
!       Vx0 utilisé est ELIMLG/Vx0
!----------------------------------------------------------------
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
!================================================================
    Vec :: vbid1
    Mat :: c
    KSP :: ksp
    PC  :: pc
    mpi_int :: mpicomm
    PetscInt :: its, ierr
    real(kind=8) :: norm
    PetscScalar ::  neg_one
    logical :: info 
!----------------------------------------------------------------
    neg_one = -1.d0
    info    = .true. 
    call jemarq()
    !
!   -- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicomm)
!
!   C = transpose(Ctrans)
    call MatTranspose(melim(ke)%ctrans, MAT_INITIAL_MATRIX, c, ierr)
!   Résolution de C Vx0 = Vec C (PETSc, solveur "least square" LSQR)
!   Init solver context : ksp 
    call KSPCreate(mpicomm, ksp, ierr)
!   Choose LSQR solver
    call KSPSetType(ksp, KSPLSQR, ierr)
!   Set the linear system matrix 
    call KSPSetOperators(ksp, c, c, SAME_PRECONDITIONER, ierr)
!   No precond : c is rectangular, Petsc default preconditioner ILU won't work
    call KSPGetPC(ksp,pc,ierr)
    call PCSetType(pc,PCNONE, ierr)
!   Solve 
    call KSPSolve( ksp, melim(ke)%vecc, melim(ke)%vx0, ierr)
!
!     -- Calcul de ||A*x0 - c||
!     ---------------------------
    if (info) then
      call VecDuplicate(melim(ke)%vecc, vbid1, ierr)
      call MatMult(c, melim(ke)%vx0, vbid1, ierr)
      call VecAXPY(vbid1,neg_one,melim(ke)%vecc ,ierr)
      call VecNorm(vbid1,norm_2,norm,ierr)
      call KSPGetIterationNumber(ksp,its,ierr)
      write(6,100) norm,its
  100 format('CALCX0: Norm of error = ',e11.4,',  Number of iterations = ',i5)
      call VecDestroy(vbid1, ierr)
    endif
!
    call MatDestroy( c, ierr )
    call KSPDestroy( ksp, ierr)

!
    call jedema()
!
#else
    ASSERT(.false.)
#endif
!
end subroutine
