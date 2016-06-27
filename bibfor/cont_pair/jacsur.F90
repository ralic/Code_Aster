subroutine jacsur(elem_coor, elem_nbnode, elem_code, elem_dime,&
                  ksi1     , ksi2       , jacobian , dire_norm)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/apnorm.h"
#include "asterfort/mmdonf.h"
!
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
!
    real(kind=8), intent(in) :: elem_coor(3,9)
    integer, intent(in) :: elem_nbnode
    character(len=8), intent(in) :: elem_code
    integer, intent(in) :: elem_dime
    real(kind=8), intent(in) :: ksi1
    real(kind=8), intent(in) :: ksi2
    real(kind=8), intent(out) :: jacobian
    real(kind=8), intent(out) :: dire_norm(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Compute jacobian
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_coor        : coordinates of nodes for current element
! In  elem_nbnode      : number of node for current element
! In  elem_code        : code of current element
! In  elem_dime        : dimension of current element
! In  ksi1             : first parametric coordinate of the point
! In  ksi2             : second parametric coordinate of the point
! Out jacobian         : surfacic jacobian of element
! Out dire_norm        : normal direction of element
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_node
    real(kind=8) :: jcs1(3), jcs2(3)
    real(kind=8) :: dxdk, dydk, dzdk
    real(kind=8) :: aux(3)
    real(kind=8) :: dff(2,9)
!
! --------------------------------------------------------------------------------------------------
!
    dire_norm(1:3) = 0.d0
    jacobian       = 0.d0
!
! - Derivated shape function
!
    call mmdonf(elem_dime, elem_nbnode, elem_code, ksi1, ksi2,&
                dff)
!
! - Compute normal direction of element
!
    call apnorm(elem_nbnode, elem_code, elem_dime, elem_coor,&
                ksi1       , ksi2     , dire_norm)
!
! - Compute surfacic jacobian of element
!
    if( (elem_dime-1) .eq. 2 ) then
        jcs1(1:3) = 0.d0
        jcs2(1:3) = 0.d0
        do i_node = 1, elem_nbnode
            jcs1(1)=jcs1(1)+dff(1,i_node)*elem_coor(1,i_node)
            jcs1(2)=jcs1(2)+dff(1,i_node)*elem_coor(2,i_node)
            jcs1(3)=jcs1(3)+dff(1,i_node)*elem_coor(3,i_node)
            jcs2(1)=jcs2(1)+dff(2,i_node)*elem_coor(1,i_node)
            jcs2(2)=jcs2(2)+dff(2,i_node)*elem_coor(2,i_node)
            jcs2(3)=jcs2(3)+dff(2,i_node)*elem_coor(3,i_node)
        end do
        aux(1)=jcs1(2)*jcs2(3)-jcs1(3)*jcs2(2)
        aux(2)=jcs1(3)*jcs2(1)-jcs1(1)*jcs2(3)
        aux(3)=jcs1(1)*jcs2(2)-jcs1(2)*jcs2(1)    
        jacobian=sqrt(aux(1)**2+aux(2)**2+aux(3)**2)
    elseif ((elem_dime-1) .eq. 1) then
        dxdk = 0.d0
        dydk = 0.d0
        dzdk = 0.d0
        do i_node = 1, elem_nbnode
            dxdk = dxdk + elem_coor(1,i_node)*dff(1,i_node)
            dydk = dydk + elem_coor(2,i_node)*dff(1,i_node)
            if (elem_dime .eq. 3) then
                dzdk = dzdk + elem_coor(3,i_node)*dff(1,i_node)
            end if
        end do
        jacobian = sqrt(dxdk**2+dydk**2+dzdk**2)
    else
        ASSERT(.false.)
    endif
!
end subroutine
