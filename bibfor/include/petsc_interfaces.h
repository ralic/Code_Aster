#ifndef PETSC_INTERFACES_H
#define PETSC_INTERFACES_H
!
! aslint: disable=W1510
!
! Il n'est pas facile de definir les interfaces des external :
! aslint: disable=C1002
!
! personne_in_charge: jacques.pellet at edf.fr
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "aster_types.h"
!
interface
!
!----------------------------------------------------------------
!    Définition des interfaces des routines PETSc
!----------------------------------------------------------------
!
#define PETSC_INT_SCAL_IN integer
#define PETSC_INT_SCAL_OUT PetscInt
!
    subroutine ISCreateGeneral(comm, n, idx, mode, is,&
                               ierr)
        mpi_int comm ! MPI_Comm
        PETSC_INT_SCAL_IN n
        PetscInt idx (*) ! PetscInt
        PetscCopyMode mode ! PetscCopyMode
        IS is ! IS
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine ISDestroy(is, ierr)
        IS is ! IS
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPCreate(comm, inksp, ierr)
        mpi_int comm ! MPI_Comm
        KSP inksp ! KSP
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPDestroy(ksp, ierr)
        KSP ksp ! KSP
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPGetConvergedReason(ksp, reason, ierr)
        KSP ksp ! KSP
        KSPConvergedReason reason ! KSPConvergedReason
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPGetIterationNumber(ksp, its, ierr)
        KSP ksp ! KSP
        PETSC_INT_SCAL_OUT its
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPGetPC(ksp, pc, ierr)
        KSP ksp ! KSP
        PC pc ! PC
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPGetTolerances(ksp, rtol, abstol, dtol, maxits,&
                                ierr)
        KSP ksp ! KSP
        PetscReal rtol ! PetscReal
        PetscReal abstol ! PetscReal
        PetscReal dtol ! PetscReal
        PETSC_INT_SCAL_OUT maxits
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPMonitorSet(ksp, monitor, mctx, mdistroy, ierr)
        KSP ksp ! KSP
        external monitor
        integer :: mctx
        external mdistroy
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPSetOperators(ksp, Amat, Pmat, flag, ierr)
        KSP ksp ! KSP
        Mat Amat ! Mat
        Mat Pmat ! Mat
        MatStructure flag ! MatStructure
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPSetTolerances(ksp, rtol, abstol, dtol, maxits,&
                                ierr)
        KSP ksp ! KSP
        PetscReal rtol ! PetscReal
        PetscReal abstol ! PetscReal
        PetscReal dtol ! PetscReal
        PETSC_INT_SCAL_IN maxits
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPSetType(ksp, type, ierr)
        KSP ksp
        character(len=*) :: type
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPSetUp(ksp, ierr)
        KSP ksp ! KSP
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine KSPSolve(ksp, b, x, ierr)
        KSP ksp ! KSP
        Vec b ! Vec
        Vec x ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatAssemblyBegin(mat, type, ierr)
        Mat mat ! Mat
        MatAssemblyType type ! MatAssemblyType
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatAssemblyEnd(mat, type, ierr)
        Mat mat ! Mat
        MatAssemblyType type ! MatAssemblyType
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatCreate(comm, A, ierr)
        mpi_int comm ! MPI_Comm
        Mat A ! Mat
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatCreateSeqAIJ(comm, m, n, nz, nnz,&
                               a, ierr)
        mpi_int comm ! MPI_Comm
        PETSC_INT_SCAL_IN m
        PETSC_INT_SCAL_IN n
        PETSC_INT_SCAL_IN nz
        PetscInt nnz(*)
        Mat a ! Mat
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatDestroy(A, ierr)
        Mat A ! Mat
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatDuplicate(mat, op, M, ierr)
        Mat mat ! Mat
        MatDuplicateOption op ! MatDuplicateOption
        Mat M ! Mat
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatGetColumnNorms(A, type, norms, ierr)
        Mat A ! Mat
        NormType type ! NormType
        PetscReal norms ! PetscReal
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatGetOwnershipRange(mat, m, n, ierr)
        Mat mat ! Mat
        PETSC_INT_SCAL_OUT m
        PETSC_INT_SCAL_OUT n
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatGetRow(mat, ilig, nterm, irow, vrow,&
                         ierr)
        Mat mat ! Mat
        PETSC_INT_SCAL_IN ilig
        PETSC_INT_SCAL_OUT nterm
        PetscInt irow (*)
        PetscScalar vrow (*)
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatGetSize(mat, m, n, ierr)
        Mat mat ! Mat
        PETSC_INT_SCAL_OUT m
        PETSC_INT_SCAL_OUT n
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatGetSubMatrix(mat, isrow, iscol, cll, newmat,&
                               ierr)
        Mat mat ! Mat
        IS isrow ! IS
        IS iscol ! IS
        MatReuse cll ! MatReuse
        Mat newmat ! Mat
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatGetValues(mat, m, idxm, n, idxn,&
                            v, ierr)
        Mat mat ! Mat
        PETSC_INT_SCAL_IN m
        PetscInt idxm (*) ! PetscInt
        PETSC_INT_SCAL_IN n
        PetscInt idxn (*) ! PetscInt
        PetscScalar v (*) ! PetscScalar
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatMatMult(A, B, scall, fill, C,&
                          ierr)
        Mat A ! Mat
        Mat B ! Mat
        MatReuse scall ! MatReuse
        PetscReal fill ! PetscReal
        Mat C ! Mat
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatMatMultTranspose(A, B, scall, fill, C,&
                                   ierr)
        Mat A ! Mat
        Mat B ! Mat
        MatReuse scall ! MatReuse
        PetscReal fill ! PetscReal
        Mat C ! Mat
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatMPIAIJSetPreallocation(A, d_nz, d_nnz, o_nz, o_nnz,&
                                         ierr)
        Mat A
        PETSC_INT_SCAL_IN d_nz
        PetscInt d_nnz (*)
        PETSC_INT_SCAL_IN o_nz
        PetscInt o_nnz (*)
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatMult(mat, x, y, ierr)
        Mat mat ! Mat
        Vec x ! Vec
        Vec y ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatMultTranspose(mat, x, y, ierr)
        Mat mat ! Mat
        Vec x ! Vec
        Vec y ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatPtAP(A, P, scall, fill, C,&
                       ierr)
        Mat A ! Mat
        Mat P ! Mat
        MatReuse scall ! MatReuse
        PetscReal fill ! PetscReal
        Mat C ! Mat
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatRestoreRow(mat, row, ncols, cols, vals,&
                             ierr)
        Mat mat
        PETSC_INT_SCAL_IN row
        PETSC_INT_SCAL_IN ncols
        PetscInt cols(*)
        PetscScalar vals(*)
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatSEQAIJSetPreallocation(B, nz, nnz, ierr)
        Mat B
        PETSC_INT_SCAL_IN nz
        PetscInt nnz (*)
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatSetBlockSize(mat, bs, ierr)
        Mat mat ! Mat
        PETSC_INT_SCAL_IN bs
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatSetSizes(A, m, n, mupper, nupper,&
                           ierr)
        Mat A ! Mat
        PETSC_INT_SCAL_IN m
        PETSC_INT_SCAL_IN n
        PETSC_INT_SCAL_IN mupper
        PETSC_INT_SCAL_IN nupper
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatSetType(mat, type, ierr)
        Mat mat
        character(len=*) :: type
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatSetValue(m, row, col, value, addv,&
                           ierr)
        Mat m ! Mat
        PETSC_INT_SCAL_IN row
        PETSC_INT_SCAL_IN col
        PetscScalar value ! PetscScalar
        InsertMode addv ! InsertMode
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatSetValues(mat, m, idxm, n, idxn,&
                            v, addv, ierr)
        Mat mat ! Mat
        PETSC_INT_SCAL_IN m
        PetscInt idxm (*) ! PetscInt
        PETSC_INT_SCAL_IN n
        PetscInt idxn (*) ! PetscInt
        PetscScalar v (*) ! PetscScalar
        InsertMode addv ! InsertMode
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatTranspose(mat, reuse, B, ierr)
        Mat mat ! Mat
        MatReuse reuse ! MatReuse
        Mat B ! Mat
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatView(mat, inviewer, ierr)
        Mat mat
        PetscViewer inviewer
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine MatZeroRows(mat, nbrow, rows, diag, x,&
                           b, ierr)
        Mat mat ! Mat
        PETSC_INT_SCAL_IN nbrow
        PetscInt rows(*)
        PetscScalar diag
        Vec x
        Vec b
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PCBJacobiGetSubKSP(pc, n_local, first_local, ksp, ierr)
        PC pc
        PetscInt n_local
        PetscInt first_local
        KSP      ksp
!
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PCFactorSetFill(pc, fill, ierr)
        PC pc ! PC
        PetscReal fill ! PetscReal
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PCFactorSetLevels(pc, levels, ierr)
        PC pc ! PC
        PETSC_INT_SCAL_IN levels
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PCFactorSetMatOrderingType(pc, ordering, ierr)
        PC pc
        character(len=*) :: ordering
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PCSetFromOptions(pc, ierr)
        PC pc ! PC
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PCSetType(pc, type, ierr)
        PC pc
        character(len=*) :: type
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PCSetUp(pc, ierr)
        PC pc ! PC
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PCShellSetApply(pc, sub, ierr)
        PC pc
        external sub
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PCShellSetSetUp(pc, sub, ierr)
        PC pc
        external sub
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PetscFinalize(ierr)
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PetscInitialize(X, ierr)
        character(len=*) :: X
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PetscInitializeFortran(ierr)
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine PetscOptionsSetValue(iname, value, ierr)
        character(len=*) :: iname
        character(len=*) :: value
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecAssemblyBegin(vec, ierr)
        Vec vec ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecAssemblyEnd(vec, ierr)
        Vec vec ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecAXPY(y, alpha, x, ierr)
        Vec y ! Vec
        PetscScalar alpha ! PetscScalar
        Vec x ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecAYPX(y, alpha, x, ierr)
        Vec y ! Vec
        PetscScalar alpha ! PetscScalar
        Vec x ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecCopy(x, y, ierr)
        Vec x ! Vec
        Vec y ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecCreate(comm, vec, ierr)
        mpi_int comm ! MPI_Comm
        Vec vec ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecCreateMPI(comm, n, nupper, v, ierr)
        mpi_int comm ! MPI_Comm
        PetscInt n
        PETSC_INT_SCAL_IN nupper
        Vec v ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecCreateSeq(comm, n, v, ierr)
        mpi_int comm ! MPI_Comm
        PETSC_INT_SCAL_IN n
        Vec v ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecDestroy(v, ierr)
        Vec v ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecDuplicate(v, newv, ierr)
        Vec v ! Vec
        Vec newv ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecGetArray(vec, x_array, i_x, ierr)
        Vec vec
        PetscScalar x_array(*)
        PetscOffset i_x
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecGetOwnershipRange(x, m, n, ierr)
        Vec x ! Vec
        PETSC_INT_SCAL_OUT m
        PETSC_INT_SCAL_OUT n
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecGetSize(x, size, ierr)
        Vec x ! Vec
        PETSC_INT_SCAL_OUT size
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecNorm(x, type, val, ierr)
        Vec x ! Vec
        NormType type ! NormType
        PetscReal val ! PetscReal
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecRestoreArray(vec, x_array, i_x, ierr)
        Vec vec
        PetscScalar x_array(*)
        PetscOffset i_x
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecScatterBegin(inctx, x, y, addv, mode,&
                               ierr)
        VecScatter inctx ! VecScatter
        Vec x ! Vec
        Vec y ! Vec
        InsertMode addv ! InsertMode
        ScatterMode mode ! ScatterMode
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecScatterCreateToAll(vin, ctx, vout, ierr)
        Vec vin ! Vec
        VecScatter ctx ! VecScatter
        Vec vout ! Vec
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecScatterDestroy(ctx, ierr)
        VecScatter ctx
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecScatterEnd(ctx, x, y, addv, mode,&
                             ierr)
        VecScatter ctx ! VecScatter
        Vec x ! Vec
        Vec y ! Vec
        InsertMode addv ! InsertMode
        ScatterMode mode ! ScatterMode
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecSet(x, alpha, ierr)
        Vec x ! Vec
        PetscScalar alpha ! PetscScalar
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecSetBlockSize(v, bs, ierr)
        Vec v ! Vec
        PETSC_INT_SCAL_IN bs
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecSetSizes(v, n, nupper, ierr)
        Vec v ! Vec
        PetscInt n
        PETSC_INT_SCAL_IN nupper
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecSetType(vec, type, ierr)
        Vec vec
        character(len=*) :: type
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecSetValues(x, ni, ix, y, iora,&
                            ierr)
        Vec x ! Vec
        PETSC_INT_SCAL_IN ni
        PetscInt ix (*) ! PetscInt
        PetscScalar y (*) ! PetscScalar
        InsertMode iora ! InsertMode
        PETSC_INT_SCAL_OUT ierr
    end subroutine
! -------------
!
    subroutine VecView(vec, inviewer, ierr)
        Vec vec
        PetscViewer inviewer
        PETSC_INT_SCAL_OUT ierr
    end subroutine
!
end interface
#endif
