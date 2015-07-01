subroutine mmvalp_scal(nb_dim   , elem_type, elem_nbno, ksi1, ksi2,&
                       vale_node, vale_poin)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/mmnonf.h"
!
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: nb_dim
    character(len=8), intent(in) :: elem_type
    integer, intent(in) :: elem_nbno
    real(kind=8), intent(in) :: ksi1
    real(kind=8), intent(in) :: ksi2
    real(kind=8), intent(in) :: vale_node(*)
    real(kind=8), intent(out) :: vale_poin
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Utility
!
! Continue method - Interpolate ONE component at point in given element
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_dim           : dimension of element
! In  elem_type        : type of element
! In  elem_nbno        : number of nodes
! In  ksi1             : first parametric coordinate of the point 
! In  ksi2             : second parametric coordinate of the point 
! In  vale_node        : value of components at nodes
! Out vale_poin        : value of components at point
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: shape_func(9)
    integer :: i_node
!
! --------------------------------------------------------------------------------------------------
!
    vale_poin = 0.d0
    ASSERT(elem_nbno.le.9)
!
! - Shape functions
!
    call mmnonf(nb_dim    , elem_nbno, elem_type, ksi1, ksi2,&
                shape_func)
!
! - Compute
!
    do i_node = 1, elem_nbno
        vale_poin = shape_func(i_node)*vale_node(i_node) + vale_poin
    end do
!
end subroutine
