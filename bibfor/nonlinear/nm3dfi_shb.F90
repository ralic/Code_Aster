subroutine nm3dfi_shb(nno, poids, def, sigma, vectu)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
implicit none
!
! aslint: disable=W1504
!
    integer, intent(in) :: nno
    real(kind=8), intent(in) :: poids
    real(kind=8), intent(in) :: def(6,nno,3)
    real(kind=8), intent(in) :: sigma(6)
    real(kind=8), intent(inout) :: vectu(3,nno)
!
! Non linear  3D isoparametric elements Force Internal
!
!
! Evaluation of internal forces for 3D isoparametric and solid-shell elements.
!
!
! IN  nno      number of nodes in element
! IN  poids    Gauss point weight
! IN  def        product F . DFF
!                           F is the tensor of gradient transformation
!                           DFF are the shape function derivatives
! IN  sigma    Cauchy stress (with root square 2 for shear components)
! INOUT  vectu     internal forces
!
    integer :: i_node
!
! ......................................................................
!
    do i_node = 1, nno
       vectu(1,i_node) = vectu(1,i_node)+ poids*&
                                          (def(1,i_node,1)*sigma(1)+&
                                           def(2,i_node,1)*sigma(2)+&
                                           def(3,i_node,1)*sigma(3)+&
                                           def(4,i_node,1)*sigma(4)+&
                                           def(5,i_node,1)*sigma(5)+&
                                           def(6,i_node,1)*sigma(6))
       vectu(2,i_node) = vectu(2,i_node)+ poids*&
                                          (def(1,i_node,2)*sigma(1)+&
                                           def(2,i_node,2)*sigma(2)+&
                                           def(3,i_node,2)*sigma(3)+&
                                           def(4,i_node,2)*sigma(4)+&
                                           def(5,i_node,2)*sigma(5)+&
                                           def(6,i_node,2)*sigma(6))
       vectu(3,i_node) = vectu(3,i_node)+ poids*&
                                          (def(1,i_node,3)*sigma(1)+&
                                           def(2,i_node,3)*sigma(2)+&
                                           def(3,i_node,3)*sigma(3)+&
                                           def(4,i_node,3)*sigma(4)+&
                                           def(5,i_node,3)*sigma(5)+&
                                           def(6,i_node,3)*sigma(6))
    end do
!
end subroutine
