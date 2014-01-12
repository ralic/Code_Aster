subroutine elg_resodr(lt, c, z)
    implicit none
! aslint: disable=W0104
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
# include "asterfort/assert.h"
# include "asterfort/utmess.h"
#ifdef _HAVE_PETSC
# include "elim_lagr.h"
    Mat :: lt
    real(kind=8) :: c(*), z(*)
!----------------------------------------------------------------
!
!  Resolution de :
!     z   = (L'*L) \  c  (par descente / remontée)
!     input : Mat L'     : matrice triangulaire supérieure
!     input : R8(:)  c   : second membre
!     output: R8(:)  z   : solution
!  Remarque : z est alloué avant l'appel à elg_resodr
!             son contenu est calculé par elg_resodr
!----------------------------------------------------------------
!
!================================================================
    integer(kind=4), allocatable :: irow(:)
    real(kind=8), allocatable :: vrow(:), y(:)
!================================================================
    PetscInt :: ierr
    PetscInt :: nterm
    PetscInt :: n1, n2
    PetscInt :: ilig, jcol, kterm, n
    PetscScalar :: s
    Mat :: l
!----------------------------------------------------------------
!
!     Les objets de type Mat et Vec ne sont en réalité que des
!     entiers contenant des adresses (INTEGER*8)
!
!     -- dimensions :
!       n    : dimension de la matrice triangulaire Lt
!     ----------------------------------------------------------
    call MatGetSize(lt, n1, n2, ierr)
    ASSERT(n1.ge.n2)
    n=n2
!
!
!     -- On transpose Lt pour permettre un acces par ligne pour L:
    call MatTranspose(lt, MAT_INITIAL_MATRIX, l, ierr)
!
!
!     -- Allocation de 3 vecteurs de travail : y, irow, vrow :
!     --------------------------------------------------------------
    allocate(y(n))
    allocate(irow(n))
    allocate(vrow(n))
!
!
!     -- Etape 1 :
!        Calcul de y = L \ c  (descente)
!     ------------------------------
!     -- calcul de y(1) :
    call MatGetRow(l, 0, nterm, irow(1), vrow(1),&
                   ierr)
    ASSERT(nterm.eq.1)
    ASSERT(irow(1).eq.0)
    y(1)=c(1)/vrow(1)
    call MatRestoreRow(l, 0, int(nterm), irow(1), vrow(1),&
                       ierr)
!
!     -- calcul de y(2:n) :
    do ilig = 2, n
        call MatGetRow(l, ilig-1, nterm, irow(1), vrow(1),&
                       ierr)
        ASSERT(nterm.le.n)
!       -- le terme diagonal est obligatoire.
!          c'est le dernier de la ligne.
        if (irow(nterm)+1 .ne. ilig) then
            call utmess('F', 'ELIMLAGR_8')
        endif
        s=c(ilig)
        do kterm = 1, nterm-1
            jcol=irow(kterm)+1
            ASSERT(jcol.ge.1)
!         -- L est triangulaire inferieure :
            ASSERT(jcol.le.ilig)
            s=s-vrow(kterm)*y(jcol)
        enddo
        y(ilig)=s/vrow(nterm)
        call MatRestoreRow(l, ilig-1, int(nterm), irow(1), vrow(1),&
                           ierr)
    enddo
!
!
!     -- Etape 2 :
!        Calcul de z = L' \ y  (remontee)
!     ------------------------------------
!
!     -- calcul de z(n) :
    call MatGetRow(lt, n-1, nterm, irow(1), vrow(1),&
                   ierr)
    ASSERT(nterm.eq.1)
    ASSERT(irow(1).eq.n-1)
    z(n)=y(n)/vrow(1)
    call MatRestoreRow(lt, n-1, int(nterm), irow(1), vrow(1),&
                       ierr)
!
!     -- calcul de z(n-1:1) :
    do ilig = n-1, 1, -1
        call MatGetRow(lt, ilig-1, nterm, irow(1), vrow(1),&
                       ierr)
        ASSERT(irow(1)+1.eq.ilig)
        s=y(ilig)
        do kterm = 2, nterm
            jcol=irow(kterm)+1
!         -- Lt est triangulaire superieure :
            ASSERT(jcol.gt.ilig)
            s=s-vrow(kterm)*z(jcol)
        enddo
        ASSERT(irow(1)+1.eq.ilig)
        z(ilig)=s/vrow(1)
        call MatRestoreRow(lt, ilig-1, int(nterm), irow(1), vrow(1),&
                           ierr)
    enddo
!
!
    call MatDestroy(l, ierr)
    deallocate(irow)
    deallocate(vrow)
    deallocate(y)
!
!
#else
    integer :: lt
    real(kind=8) :: c(*), z(*)
    ASSERT(.false.)
#endif
!
end subroutine
