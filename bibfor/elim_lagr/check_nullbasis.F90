function check_nullbasis( vec_c, mat_z, tol ) result ( is_ok )
!
implicit none
!
! person_in_charge: natacha.bereux at edf.fr
! aslint:disable=C1308
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/infniv.h"
#include "asterfort/assert.h"
!
!----------------------------------------------------------------
!
!     Vérification de l'orthogonalité de la base stockée dans
!     mat_z par rapport au vecteur vec_c
!
! ---------------------------------------------------------------
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
! Dummy arguments
   Vec, intent(in)                    :: vec_c
   Mat, intent(in)                    :: mat_z
   PetscScalar, optional, intent(in)  :: tol
   aster_logical                      :: is_ok
! Local variables
   integer                  :: ifm, niv
   Vec                      :: vec_cz
   PetscErrorCode           :: ierr
   PetscScalar              :: tol_loc, norm
   PetscScalar, parameter   :: tolmax=1.d-10
   aster_logical            :: verbose
!
   call infniv(ifm, niv)
   verbose=(niv == 2)
!
  if (present(tol)) then
     tol_loc = tol
  else
     tol_loc = tolmax
  endif
! Allocation d'un vecteur de travail pour calculer le produit vec_c * mat_z
#ifdef ASTER_PETSC_VERSION_LEQ_35
  call MatGetVecs(mat_z, vec_cz, PETSC_NULL_OBJECT, ierr)
#else
  call MatCreateVecs( mat_z, vec_cz, PETSC_NULL_OBJECT, ierr)
#endif
  ASSERT(ierr == 0)
  call MatMultTranspose(mat_z, vec_c, vec_cz, ierr)
  call VecNorm(vec_cz, norm_2, norm, ierr)
!
  if (verbose) write(ifm,*),'   |C(IC,:).T|=', norm
  if (verbose) write(ifm,*),' '
!
  is_ok = (norm < tol_loc)
!
! Libération de la mémoire
  call VecDestroy(vec_cz, ierr)
!
#else
    integer, intent(in)         :: vec_c, mat_z
    real, optional, intent(in)  :: tol
    aster_logical               :: is_ok
    is_ok = .false.
#endif
end function check_nullbasis
