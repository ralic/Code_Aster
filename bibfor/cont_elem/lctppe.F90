subroutine lctppe(side      , elem_dime , l_axis     ,&
                  nb_node   , elem_coor , elem_code  ,&
                  gauss_coor, shape_func, shape_dfunc,&
                  jaco_init , jaco_upda , norm)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/mmfonf.h"
#include "asterfort/mmmjac.h"
#include "asterfort/reerel.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmtang.h"
#include "asterfort/mmnewd.h"
#include "asterfort/mmnewt.h"
#include "asterfort/assert.h"
#include "asterfort/nmepsi.h"
#include "asterfort/dfdmip.h"
#include "asterfort/jevech.h"
#include "asterfort/utmess.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: side
    integer, intent(in) :: elem_dime
    aster_logical, intent(in) :: l_axis
    integer, intent(in) :: nb_node
    real(kind=8), intent(in) :: elem_coor(elem_dime,nb_node)
    character(len=8), intent(in) :: elem_code   
    real(kind=8), intent(in) :: gauss_coor(2)
    real(kind=8), intent(out) :: shape_func(9)
    real(kind=8), intent(out) :: shape_dfunc(2, 9)
    real(kind=8), intent(out) :: jaco_init 
    real(kind=8), intent(out) :: jaco_upda
    real(kind=8), intent(out) :: norm(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact (LAC) - Elementary computations
!
! Compute geometric quantities for contact matrix
!
! --------------------------------------------------------------------------------------------------
!
! In  side             : side (master or slave)
! In  elem_dime        : dimension of elements
! In  l_axis           : .true. for axisymmetric element
! In  nb_node          : number of nodes 
! In  elem_coor        : updated coordinates of element
! In  elem_code        : code of element
! In  gauss_coor       : coordinates of current integration point
! Out shape_func       : shape functions at integration point
! Out shape_dfunc      : derivatives of shape functions at integration point
! Out jaco_init        : initial jacobian at integration point
! Out jaco_upda        : updated jacobian at integration point
! Out norm             : normal at integration point
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: shape_ddfunc(3, 9), jcs1(3), jcs2(3), aux(3), elem_coot(3,9)
    real(kind=8) :: tau1(3), tau2(3), dxdk, dydk, dzdk, r
    integer :: jv_geom ,i_node, i_dime, shift
!
! --------------------------------------------------------------------------------------------------
!
    do i_node = 1, nb_node
        shape_func(i_node)    = 0.d0
        shape_dfunc(1,i_node) = 0.d0
        shape_dfunc(2,i_node) = 0.d0
    end do
    norm(1:3) = 0.d0
    tau1(1:3) = 0.d0
    tau2(1:3) = 0.d0
    jaco_init = 0.d0 
    jaco_upda = 0.d0
!
! - Shift in list of nodes depending on side (master or slave)
!
    if (side .eq. 'Slave' ) then
        shift = 0
    elseif (side.eq.'Master') then
        shift = nb_node*elem_dime
    else
        ASSERT(.false.)
    end if
!
! - Change shape of vector for coordinates of element
!
    do i_node = 1, nb_node
        do i_dime = 1, elem_dime
            elem_coot(i_dime,i_node) = elem_coor(i_dime,i_node)
        end do
    end do
!
! - Get shape functions
!
    call mmfonf(elem_dime    , nb_node      , elem_code,&
                gauss_coor(1), gauss_coor(2),&
                shape_func   , shape_dfunc  , shape_ddfunc)
!
! - Compute normal
!
    call mmtang(elem_dime, nb_node, elem_coot, shape_dfunc, tau1,&
                tau2)
    call mmnorm(elem_dime, tau1, tau2, norm)
    if (side.eq.'Master') then
        norm(1:3)=-norm(1:3)
    end if
!
! - Compute updated surfacic jacobian of element
!
    if (elem_dime .eq. 3) then
        jcs1(1:3)=0.d0
        jcs2(1:3)=0.d0
        do i_node = 1, nb_node
            jcs1(1)=jcs1(1)+shape_dfunc(1,i_node)*elem_coot(1,i_node)
            jcs1(2)=jcs1(2)+shape_dfunc(1,i_node)*elem_coot(2,i_node)
            jcs1(3)=jcs1(3)+shape_dfunc(1,i_node)*elem_coot(3,i_node)
            jcs2(1)=jcs2(1)+shape_dfunc(2,i_node)*elem_coot(1,i_node)
            jcs2(2)=jcs2(2)+shape_dfunc(2,i_node)*elem_coot(2,i_node)
            jcs2(3)=jcs2(3)+shape_dfunc(2,i_node)*elem_coot(3,i_node)
        end do
        aux(1)=jcs1(2)*jcs2(3)-jcs1(3)*jcs2(2)
        aux(2)=jcs1(3)*jcs2(1)-jcs1(1)*jcs2(3)
        aux(3)=jcs1(1)*jcs2(2)-jcs1(2)*jcs2(1)    
        jaco_upda=sqrt(aux(1)**2+aux(2)**2+aux(3)**2)
    elseif (elem_dime .eq. 2) then
        dxdk=0.d0
        dydk=0.d0
        dzdk=0.d0
        do i_node = 1, nb_node
            dxdk = dxdk + elem_coot(1,i_node)*shape_dfunc(1,i_node)
            dydk = dydk + elem_coot(2,i_node)*shape_dfunc(1,i_node)
            if (elem_dime .eq. 3) then
                dzdk = dzdk + elem_coot(3,i_node)*shape_dfunc(1,i_node)
            end if
        end do
        jaco_upda = sqrt(dxdk**2+dydk**2+dzdk**2)
        if (l_axis) then
            r = 0.d0
            do i_node = 1, nb_node
                r = r + elem_coot(1,i_node)*shape_func(i_node)
            end do
            if (r .le. r8prem()) then
                r=1.d-7
                call utmess('A', 'CONTACT2_14')
            endif
            jaco_upda = jaco_upda*abs(r)
        endif
    else
        ASSERT(.false.)
    endif
!
! - Compute initial surfacic jacobian of element
! 
    call jevech('PGEOMER', 'L', jv_geom)
    call mmmjac(elem_code, jv_geom+shift, shape_func, shape_dfunc, l_axis, &
                nb_node  , elem_dime    , jaco_init)  
!
end subroutine
