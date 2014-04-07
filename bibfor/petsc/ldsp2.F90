subroutine ldsp2(pc, x1, y, ierr)
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
!
#include "asterf.h"
#include "asterfort/amumph.h"
#include "asterfort/assert.h"
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
    complex(kind=8) :: cbid
    integer :: iret
!----------------------------------------------------------------
!     Variables PETSc
    PC :: pc
    Vec :: x1, y
    PetscInt :: ierr
!
    PetscScalar :: xx(1)
    PetscOffset :: xidx
!----------------------------------------------------------------
!
! --  COPIE DU VECTEUR D'ENTREE, CAR ERREUR S'IL EST TRANSFORME
    call VecCopy(x1, xlocal, ierr)
    ASSERT(ierr.eq.0)
!
! --  RECUPERATION DES VALEURS DU VECTEUR SUR LES DIFFERENTS PROCS
    call VecScatterBegin(xscatt, xlocal, xglobal, INSERT_VALUES, SCATTER_FORWARD, ierr)
    ASSERT(ierr.eq.0)
    call VecScatterEnd(xscatt, xlocal, xglobal, INSERT_VALUES, SCATTER_FORWARD, ierr)
    ASSERT(ierr.eq.0)
!
    call VecGetArray(xglobal, xx, xidx, ierr)
    ASSERT(ierr.eq.0)
!
! --  APPEL A LA ROUTINE DE PRECONDITIONNEMENT (DESCENTE/REMONTEE)
    cbid = dcmplx(0.d0, 0.d0)
    call amumph('RESOUD', spsomu, spmat, xx(xidx+1), [cbid],&
                ' ', 1, iret, .true.)
!
! --  ENVOI DES VALEURS DU VECTEUR SUR LES DIFFERENTS PROCS
    call VecRestoreArray(xglobal, xx, xidx, ierr)
    ASSERT(ierr.eq.0)
    call VecScatterBegin(xscatt, xglobal, y, INSERT_VALUES, SCATTER_REVERSE, ierr)
    ASSERT(ierr.eq.0)
    call VecScatterEnd(xscatt, xglobal, y, INSERT_VALUES, SCATTER_REVERSE, ierr)
    ASSERT(ierr.eq.0)
!
    ierr=iret
!
#else
!
!     DECLARATION BIDON POUR ASSURER LA COMPILATION
    integer :: pc, x1, y, ierr
!
#endif
!
end subroutine
