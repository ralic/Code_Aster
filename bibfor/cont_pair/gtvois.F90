subroutine gtvois(v_connex  , v_connex_lcum, list_elem, nb_elem   , elem_nume, elem_code,&
                  v_conx_inv, v_inv_lcum   , nb_neigh , list_neigh)
                 
!
implicit none
!
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utlisi.h"
#include "asterfort/jelira.h"
#include "asterfort/assert.h"
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
    integer, pointer, intent(in) :: v_connex(:)
    integer, pointer, intent(in) :: v_connex_lcum(:)
    integer, pointer, intent(in) :: v_conx_inv(:)
    integer, pointer, intent(in) :: v_inv_lcum(:)
    integer, intent(in) :: nb_elem
    integer, intent(in) :: list_elem(nb_elem)
    integer, intent(in) :: elem_nume
    character(len=8), intent(in) :: elem_code
    integer, intent(in) :: nb_neigh
    integer, intent(out) :: list_neigh(4)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Find element' neighbours of current element
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  conx_inve        : name of object for inverse connectivity
! In  nb_elem          : number of elements
! In  list_elem        : list of elements
! In  elem_nume        : index in mesh datastructure of current element
! In  elem_code        : code of current element
! In  nb_neigh         : number of neigbours for current element
! Out list_neigh       : list of index of element's neighbours of current element
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_node, list_node(4), node_1, node_2
    integer :: node_nbelem_1, node_nbelem_2
    integer :: nb_find, elem_find(2)
    integer :: list_node_next(4)
    integer :: i_node, i_neigh, nb_dime
    integer :: a(nb_elem) , b(nb_elem), i
!
! --------------------------------------------------------------------------------------------------
!
    list_neigh(1:4) = 0   
!
! - Get list of nodes of current element
!
    if (elem_code .eq. 'SE2' .or. elem_code .eq. 'SE3') then
        nb_node = 2
        nb_dime = 1 
    elseif (elem_code .eq. 'TR3' .or. elem_code .eq. 'TR6') then
        nb_node = 3
        nb_dime = 2
    elseif (elem_code .eq. 'QU4' .or. elem_code .eq. 'QU8' .or. elem_code .eq. 'QU9') then
        nb_node = 4
        nb_dime = 2
    else
        ASSERT(.false.)
    end if   
    do i_node = 1, nb_node
        list_node(i_node) = v_connex(v_connex_lcum(elem_nume)-1+i_node)
    end do
!
! - Set index of next nodes
!
    do i_node = 2, nb_node
        list_node_next(i_node-1) = i_node
    end do
    list_node_next(nb_node) = 1
!
! - Find neighbours
!
    if (nb_dime.eq.2) then
        do i_neigh = 1,nb_neigh
            nb_find = 0
            node_1  = list_node(i_neigh)
            node_2  = list_node(list_node_next(i_neigh))
            node_nbelem_1=v_inv_lcum(node_1+1) - v_inv_lcum(node_1)
            node_nbelem_2=v_inv_lcum(node_2+1) - v_inv_lcum(node_2)
            do i= 1 , node_nbelem_1
                a(i)=v_conx_inv(v_inv_lcum(node_1)-1+i)
            end do
            do i= 1 , node_nbelem_2
                b(i)=v_conx_inv(v_inv_lcum(node_2)-1+i)
            end do
            call utlisi('INTER'   , a, node_nbelem_1,b, node_nbelem_2,&
                        elem_find , 2            , nb_find)
            ASSERT(nb_find .le. 2)
            ASSERT(nb_find .ge. 1)
            if (nb_find .eq. 2) then
                if (list_elem(elem_find(1)) .eq. elem_nume) then
                    list_neigh(i_neigh) = list_elem(elem_find(2))
                else
                    list_neigh(i_neigh) = list_elem(elem_find(1))
                end if    
            end if
        end do
    elseif (nb_dime .eq. 1) then
        do i_neigh = 1,nb_neigh
            nb_find = 0
            node_1  = list_node(i_neigh)
            node_nbelem_1=v_inv_lcum(node_1+1) - v_inv_lcum(node_1)          
            ASSERT(node_nbelem_1 .le. 2)
            if (node_nbelem_1 .eq. 2) then
                if (list_elem(v_conx_inv(v_inv_lcum(node_1))) .eq. elem_nume) then
                    list_neigh(i_neigh) = list_elem(v_conx_inv(v_inv_lcum(node_1)+1))
                else
                    list_neigh(i_neigh) = list_elem(v_conx_inv(v_inv_lcum(node_1)))
                end if    
            end if
        end do
    else
        ASSERT(.false.)
    end if
!
end subroutine
