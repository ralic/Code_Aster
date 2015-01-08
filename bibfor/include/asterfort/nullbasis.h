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
#include "asterf_petsc.h"
!
interface
    subroutine nullbasis( mat_c, mat_z, nbnvco, nvco_c)
# ifdef _HAVE_PETSC
        Mat, intent(in)                      :: mat_c
        Mat, intent(out)                     :: mat_z
        PetscInt, intent(out)                 :: nbnvco
        PetscInt, dimension(:), intent(inout) :: nvco_c
# else
        integer, intent(in)    :: mat_c
        integer, intent(inout) :: mat_z
        integer, intent(out)                 :: nbnvco
        integer, dimension(:), intent(inout) :: nvco_c
# endif
    end subroutine nullbasis
end interface
