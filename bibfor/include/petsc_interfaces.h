#ifndef PETSC_INTERFACES_H
#define PETSC_INTERFACES_H
!
!
! Il n'est pas facile de definir les interfaces des external :
! aslint: disable=C1002
!
! personne_in_charge: jacques.pellet at edf.fr
!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
!
interface
!
!----------------------------------------------------------------
!    Définition des interfaces des routines PETSc
!----------------------------------------------------------------
!
!
    subroutine ISCreateGeneral(comm, n, idx, mode, is,&
                               ierr)
        mpi_int comm
        PetscInt n
        PetscInt idx (*)
        PetscCopyMode mode ! PetscCopyMode
        IS is ! IS
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine ISCreateStride(comm, n, first, step, is, &
                                ierr)
        mpi_int  comm
        PetscInt n 
        PetscInt first
        PetscInt step
        IS       is
        PetscErrorCode, intent(out) ::  ierr
    end subroutine 
! -------------
!
    subroutine ISDestroy(is, ierr)
        IS       is 
        PetscErrorCode, intent(out) ::  ierr
    end subroutine

! -------------
!
    subroutine ISGetIndices( is, is_array, i_is, ierr )
       IS             is
       PetscInt       is_array(1)
       PetscOffset    i_is
       PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine ISRestoreIndices( is, is_array, i_is, ierr )
       IS          is
       PetscInt    is_array(1)
       PetscOffset i_is
       PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine ISSort( is, ierr )
       IS          is
       PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPCreate(comm, inksp, ierr)
        mpi_int comm
        KSP inksp ! KSP
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPDestroy(ksp, ierr)
        KSP ksp ! KSP
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPGetConvergedReason(ksp, reason, ierr)
        KSP ksp ! KSP
        KSPConvergedReason reason ! KSPConvergedReason
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPGetIterationNumber(ksp, its, ierr)
        KSP ksp ! KSP
        PetscInt its
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPGetPC(ksp, pc, ierr)
        KSP ksp ! KSP
        PC pc ! PC
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPGetTolerances(ksp, rtol, abstol, dtol, maxits,&
                                ierr)
        KSP ksp ! KSP
        PetscReal rtol
        PetscReal abstol
        PetscReal dtol
        PetscInt maxits
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPMonitorSet(ksp, monitor, mctx, mdistroy, ierr)
        KSP ksp ! KSP
        external monitor
        integer :: mctx
        external mdistroy
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
#ifdef ASTER_PETSC_VERSION_LEQ_34
    subroutine KSPSetOperators(ksp, Amat, Pmat, flag, ierr)
#else
    subroutine KSPSetOperators(ksp, Amat, Pmat, ierr)
#endif
        KSP ksp ! KSP
        Mat Amat
        Mat Pmat
#ifdef ASTER_PETSC_VERSION_LEQ_34
        MatStructure flag
#endif
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPSetTolerances(ksp, rtol, abstol, dtol, maxits,&
                                ierr)
        KSP ksp ! KSP
        PetscReal rtol
        PetscReal abstol
        PetscReal dtol
        PetscInt maxits
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPSetType(ksp, type, ierr)
        KSP ksp
        character(len=*) :: type
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPSetUp(ksp, ierr)
        KSP ksp ! KSP
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine KSPSolve(ksp, b, x, ierr)
        KSP ksp ! KSP
        Vec b
        Vec x
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatAssemblyBegin(mat, type, ierr)
        Mat mat
        MatAssemblyType type
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatAssemblyEnd(mat, type, ierr)
        Mat mat
        MatAssemblyType type
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatCreate(comm, A, ierr)
        mpi_int comm
        Mat A
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatNullSpaceCreateRigidBody (coords, sp, ierr)
    Vec                :: coords
    MatNullSpace       :: sp
    PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatNullSpaceDestroy(sp, ierr)
    MatNullSpace       :: sp
    PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatCreateSeqAIJ(comm, m, n, nz, nnz,&
                               a, ierr)
        mpi_int comm
        PetscInt m
        PetscInt n
        PetscInt nz
        PetscInt nnz(*)
        Mat a
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatDestroy(A, ierr)
        Mat A
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatDuplicate(mat, op, M, ierr)
        Mat mat
        MatDuplicateOption op
        Mat M
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatGetColumnNorms(A, type, norms, ierr)
        Mat A
        NormType type
        PetscReal norms(*)
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatGetOwnershipRange(mat, m, n, ierr)
        Mat mat
        PetscInt m
        PetscInt n
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatGetRow(mat, ilig, nterm, irow, vrow,&
                         ierr)
        Mat mat
        PetscInt ilig
        PetscInt nterm
        PetscInt irow (*)
        PetscScalar vrow (*)
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatGetSize(mat, m, n, ierr)
        Mat mat
        PetscInt m
        PetscInt n
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatGetSubMatrix(mat, isrow, iscol, cll, newmat,&
                               ierr)
        Mat mat
        IS isrow ! IS
        IS iscol ! IS
        MatReuse cll
        Mat newmat
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatGetValues(mat, m, idxm, n, idxn,&
                            v, ierr)
        Mat mat
        PetscInt m
        PetscInt idxm (*)
        PetscInt n
        PetscInt idxn (*)
        PetscScalar v (*)
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatGetVecs( mat, right, left, ierr )
        Mat mat
        Vec  right, left
        PetscErrorCode, intent(out) ::  ierr
    end subroutine MatGetVecs
! -------------
!
    subroutine MatMatMult(A, B, scall, fill, C,&
                          ierr)
        Mat A
        Mat B
        MatReuse scall
        PetscReal fill
        Mat C
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
     subroutine MatMatTransposeMult(A, B, scall, fill, C ,ierr)
       Mat A
       Mat B
       MatReuse scall
       PetscReal fill
       Mat C
       PetscErrorCode, intent(out) ::  ierr
     end subroutine

! -------------
!
    subroutine MatMatMultTranspose(A, B, scall, fill, C, ierr)
       Mat A
       Mat B
       MatReuse scall
       PetscReal fill
       Mat C
       PetscErrorCode, intent(out) ::  ierr
     end subroutine
! -------------
!
    subroutine MatMPIAIJSetPreallocation(A, d_nz, d_nnz, o_nz, o_nnz,&
                                         ierr)
        Mat A
        PetscInt d_nz
        PetscInt d_nnz (*)
        PetscInt o_nz
        PetscInt o_nnz (*)
        PetscErrorCode, intent(out) ::  ierr
    end subroutine

! -------------
!
    subroutine MatMult(mat, x, y, ierr)
        Mat mat
        Vec x
        Vec y
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatMultTranspose(mat, x, y, ierr)
        Mat mat
        Vec x
        Vec y
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatPtAP(A, P, scall, fill, C,&
                       ierr)
        Mat A
        Mat P
        MatReuse scall
        PetscReal fill
        Mat C
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatRestoreRow(mat, row, ncols, cols, vals,&
                             ierr)
        Mat mat
        PetscInt row
        PetscInt ncols
        PetscInt cols(*)
        PetscScalar vals(*)
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatSetNearNullSpace(mat, sp, ierr)
        Mat   mat
        MatNullSpace  sp
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatSEQAIJSetPreallocation(B, nz, nnz, ierr)
        Mat B
        PetscInt nz
        PetscInt nnz (*)
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatSetBlockSize(mat, bs, ierr)
        Mat mat
        PetscInt bs
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
    subroutine MatSetOption(mat, op, flg, ierr)
        Mat mat
        PetscInt op
        PetscBool flg
        PetscErrorCode, intent(out) ::  ierr
    end subroutine 
! -------------
!
    subroutine MatSetSizes(A, m, n, mupper, nupper,&
                           ierr)
        Mat A
        PetscInt m
        PetscInt n
        PetscInt mupper
        PetscInt nupper
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatSetType(mat, type, ierr)
        Mat mat
        character(len=*) :: type
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatSetValue(m, row, col, value, addv,&
                           ierr)
        Mat m
        PetscInt row
        PetscInt col
        PetscScalar value
        InsertMode addv
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatSetValues(mat, m, idxm, n, idxn,&
                            v, addv, ierr)
        Mat mat
        PetscInt m
        PetscInt idxm (*)
        PetscInt n
        PetscInt idxn (*)
        PetscScalar v (*)
        InsertMode addv
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatTranspose(mat, reuse, B, ierr)
        Mat mat
        MatReuse reuse
        Mat B
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatTransposeMatMult(A, B, scall, fill, C,&
                                   ierr)
        Mat A
        Mat B
        MatReuse scall
        PetscReal fill
        Mat C
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatView(mat, inviewer, ierr)
        Mat mat
        PetscViewer inviewer
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine MatZeroRows(mat, nbrow, rows, diag, x,&
                           b, ierr)
        Mat mat
        PetscInt nbrow
        PetscInt rows(*)
        PetscScalar diag
        Vec x
        Vec b
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PCGAMGSetNSmooths( pc, nsmooth, ierr)
        PC                 :: pc
        PetscInt           :: nsmooth
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
    subroutine PCBJacobiGetSubKSP(pc, n_local, first_local, ksp, ierr)
        PC pc
        PetscInt n_local
        PetscInt first_local
        KSP      ksp
!
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PCFactorSetFill(pc, fill, ierr)
        PC pc ! PC
        PetscReal fill
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PCFactorSetLevels(pc, levels, ierr)
        PC pc ! PC
        PetscInt levels
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PCFactorSetMatOrderingType(pc, ordering, ierr)
        PC pc
        character(len=*) :: ordering
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PCSetFromOptions(pc, ierr)
        PC pc ! PC
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PCSetType(pc, type, ierr)
        PC pc
        character(len=*) :: type
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PCSetUp(pc, ierr)
        PC pc ! PC
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PCShellSetApply(pc, sub, ierr)
        PC pc
        external sub
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PCShellSetSetUp(pc, sub, ierr)
        PC pc
        external sub
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PetscFinalize(ierr)
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PetscInitialize(X, ierr)
        character(len=*) :: X
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PetscInitializeFortran(ierr)
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PetscOptionsSetValue(iname, value, ierr)
        character(len=*) :: iname
        character(len=*) :: value
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine PetscPushErrorHandler(handler, ctx, ierr)
      external handler
      PetscInt ctx
      PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecAssemblyBegin(vec, ierr)
        Vec vec
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecAssemblyEnd(vec, ierr)
        Vec vec
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecAXPY(y, alpha, x, ierr)
        Vec y
        PetscScalar alpha
        Vec x
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecAYPX(y, alpha, x, ierr)
        Vec y
        PetscScalar alpha
        Vec x
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecCopy(x, y, ierr)
        Vec x
        Vec y
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecCreate(comm, vec, ierr)
        mpi_int comm
        Vec vec
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecCreateMPI(comm, n, nupper, v, ierr)
        mpi_int comm
        PetscInt n
        PetscInt nupper
        Vec v
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecCreateSeq(comm, n, v, ierr)
        mpi_int comm
        PetscInt n
        Vec v
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecDestroy(v, ierr)
        Vec v
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecDuplicate(v, newv, ierr)
        Vec v
        Vec newv
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecGetArray(vec, x_array, i_x, ierr)
        Vec vec
        PetscScalar x_array(*)
        PetscOffset i_x
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecGetOwnershipRange(x, m, n, ierr)
        Vec x
        PetscInt m
        PetscInt n
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecGetSize(x, size, ierr)
        Vec x
        PetscInt size
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
   subroutine VecGetSubVector( x, is, y, ierr )
        Vec x
        IS is
        Vec y
        PetscErrorCode, intent(out) ::  ierr
   end subroutine
! -------------
!
    subroutine VecNorm(x, type, val, ierr)
        Vec x
        NormType type
        PetscReal val
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecRestoreArray(vec, x_array, i_x, ierr)
        Vec vec
        PetscScalar x_array(*)
        PetscOffset i_x
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecScale( x, alpha, ierr )
      Vec x
      PetscScalar alpha
      PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecScatterBegin(inctx, x, y, addv, mode,&
                               ierr)
        VecScatter inctx
        Vec x
        Vec y
        InsertMode addv
        ScatterMode mode
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecScatterCreateToAll(vin, ctx, vout, ierr)
        Vec vin
        VecScatter ctx
        Vec vout
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecScatterDestroy(ctx, ierr)
        VecScatter ctx
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecScatterEnd(ctx, x, y, addv, mode,&
                             ierr)
        VecScatter  ctx
        Vec x
        Vec y
        InsertMode addv
        ScatterMode mode
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecSet(x, alpha, ierr)
        Vec x
        PetscScalar alpha
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecSetBlockSize(v, bs, ierr)
        Vec v
        PetscInt bs
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecSetSizes(v, n, nupper, ierr)
        Vec v
        PetscInt n
        PetscInt nupper
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecSetType(vec, type, ierr)
        Vec vec
        character(len=*) :: type
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecSetValues(x, ni, ix, y, iora,&
                            ierr)
        Vec x
        PetscInt ni
        PetscInt ix (*)
        PetscScalar y (*)
        InsertMode iora
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
! -------------
!
    subroutine VecView(vec, inviewer, ierr)
        Vec vec
        PetscViewer inviewer
        PetscErrorCode, intent(out) ::  ierr
    end subroutine
!
end interface
#endif
