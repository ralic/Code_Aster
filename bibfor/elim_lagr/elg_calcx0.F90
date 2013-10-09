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
!----------------------------------------------------------------
!
!     Resolution de x0 = A \ c par :
!       z   = (L'*L) \  c  (par descente / remontée)
!       x0  = A' * z
!
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
# include "asterfort/elg_resodr.h"
!
!================================================================
    integer(kind=8) :: ilt
    PetscInt :: ierr
    PetscInt :: n1, n2
    PetscScalar :: xxc(1), xxz(1)
    PetscOffset :: xidxc, xidxz
    Mat :: l, lt
    Vec :: vecz, vbid1
!----------------------------------------------------------------
    call jemarq()
!
    call MatGetSize(melim(ke)%ctrans, n1, n2, ierr)
!
! juste une copie de "pointeur" pour la lisibilit
    lt=melim(ke)%rct
!
!     -- On transpose Lt pour permettre un acces par ligne pour L:
    call MatTranspose(lt, MAT_INITIAL_MATRIX, l, ierr)
!
!
!     -- Résolution de z   = (L'*L) \  c
!     ------------------------------------
    ilt=lt
    call VecDuplicate(melim(ke)%vecc, vecz, ierr)
    call VecGetArray(melim(ke)%vecc, xxc, xidxc, ierr)
    call VecGetArray(vecz, xxz, xidxz, ierr)
    call elg_resodr(ilt, xxc(xidxc+1), xxz(xidxz+1))
    call VecRestoreArray(melim(ke)%vecc, xxc, xidxc, ierr)
    call VecRestoreArray(vecz, xxz, xidxz, ierr)
!
!
!     Vx0=A'*VZ :
!     ---------------
    call MatMult(melim(ke)%ctrans, vecz, melim(ke)%vx0, ierr)
!
!
!     -- Vérification que A*x0=c
!     ---------------------------
    if (.true.) then
        call VecDuplicate(vecz, vbid1, ierr)
        call MatMultTranspose(melim(ke)%ctrans, melim(ke)%vx0, vbid1, ierr)
        call VecDestroy(vbid1, ierr)
    endif
!
    call MatDestroy(l, ierr)
    call VecDestroy(vecz, ierr)
!
    call jedema()
!
#else
    ASSERT(.false.)
#endif
!
end subroutine
