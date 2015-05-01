! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "asterf_petsc.h"

interface
   subroutine matset_to_identity( mat_a, idrow_c, idcol_c )
# ifdef _HAVE_PETSC
       Mat, intent(inout) :: mat_a
       PetscInt, dimension(:), intent(in), target, optional   :: idrow_c
       PetscInt, dimension(:), intent(in), target, optional   :: idcol_c
# else
       integer, intent(inout) :: mat_a
       integer, dimension(:), intent(in), target, optional   :: idrow_c
       integer, dimension(:), intent(in), target, optional   :: idcol_c
# endif
   end subroutine matset_to_identity
end interface
