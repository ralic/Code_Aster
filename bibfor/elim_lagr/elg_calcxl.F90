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
# include "jeveux.h"
# include "asterfort/assert.h"
# include "asterfort/jedema.h"
# include "asterfort/jemarq.h"
!----------------------------------------------------------------
! Calcul des coefficients de Lagrange pour ELIM_LAGR='OUI'
!     IN  : Vec X1    (solution sur les dds physiques)
!
!     OUT : Vec VLAG  (solution sur les dds Lagrange "1")
!
!     Les coefficients de Lagrange (1 et 2) sont
!     calculés par :
!       L = (R'*R) \ A*(b - B*x)
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
# include "asterfort/elg_allocvr.h"
# include "asterfort/elg_resodr.h"
!
    Vec :: x1, vlag
!
!================================================================
    Mat :: ilt
    PetscInt :: n1, n2, n3
    PetscInt :: ierr
    PetscScalar :: xx(1), m1
    PetscOffset :: xidxay, xidxl
    Vec :: bx, y, ay
!----------------------------------------------------------------
    call jemarq()
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
    m1=-1.d0
    call VecDuplicate(melim(ke)%vecb, y, ierr)
    call VecCopy(melim(ke)%vecb, y, ierr)
    call VecAXPY(y, m1, bx, ierr)
!
!     -- calcul de AY = A*Y
    call elg_allocvr(ay, int(n2))
    call MatMultTranspose(melim(ke)%ctrans, y, ay, ierr)
!
!
!     -- résolution : z = (R*R') \ AY :
    call VecGetArray(ay, xx, xidxay, ierr)
    call VecGetArray(vlag, xx, xidxl, ierr)
    ilt=melim(ke)%rct
    call elg_resodr(ilt, xx(xidxay+1), xx(xidxl+1))
    call VecRestoreArray(ay, xx, xidxay, ierr)
    call VecRestoreArray(vlag, xx, xidxl, ierr)
!
!
!     -- ménage :
    call VecDestroy(bx, ierr)
    call VecDestroy(y, ierr)
    call VecDestroy(ay, ierr)
!
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
