subroutine elg_allocvr(vect1, n1)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
! aslint: disable=W0104
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
!--------------------------------------------------------------
! BUT : allouer un vecteur PETSc réel de longueur n1
!---------------------------------------------------------------
#include "asterc/asmpi_comm.h"
#include "asterfort/utmess.h"
!
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
    Vec :: vect1
    integer :: n1
!
!================================================================
    PetscInt :: ierr
    integer :: bs
    mpi_int :: mpicomm
!----------------------------------------------------------------
    bs=1
    call asmpi_comm('GET_WORLD', mpicomm)
    call VecCreate(mpicomm, vect1, ierr)
    call VecSetBlockSize(vect1, to_petsc_int(bs), ierr)
    call VecSetType(vect1, VECSEQ, ierr)
    call VecSetSizes(vect1, PETSC_DECIDE, to_petsc_int(n1), ierr)
#else
    integer :: vect1, n1
    call utmess('F', 'ELIMLAGR_1')
#endif
end subroutine
