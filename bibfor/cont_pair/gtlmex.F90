subroutine gtlmex(v_cninv, v_cninv_lcum, nume_node_cl, nb_elem_mast, list_elem_mast ,&
                  list_el_ma_ax, nb_el_ma_ax)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"

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
    integer, pointer, intent(in) :: v_cninv(:) 
    integer, pointer, intent(in) :: v_cninv_lcum(:)
    integer, intent(in) :: nume_node_cl
    integer, intent(in) :: nb_elem_mast
    integer, intent(in) :: list_elem_mast(nb_elem_mast)
    integer, intent(out) :: list_el_ma_ax(nb_elem_mast)
    integer, intent(out) :: nb_el_ma_ax 
    
!
! -------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Get list of master element to seek
!
! -------------------------------------------------------------------------------------------------
!
! In  v_cninv(:)         : inverse connectivity
! In  v_cninv_lcum(:)    : inverse connectivity loncum
! In  nume_node_cl       : support node to build the list of master element attached with
! In  nb_elem_mast       : number of master element
! In  list_elem_mast     : list of master element
! Out list_el_ma_ax      : list of master element to seek
! Out nb_el_ma_ax        : number of element in the list

!
! -------------------------------------------------------------------------------------------------
!
    integer :: i_elem
!
! -------------------------------------------------------------------------------------------------
!
!
! - List construction
!  
     nb_el_ma_ax=v_cninv_lcum(nume_node_cl+1)-v_cninv_lcum(nume_node_cl)          
     do i_elem=1, nb_el_ma_ax
        list_el_ma_ax(i_elem)=list_elem_mast(v_cninv(v_cninv_lcum(nume_node_cl)-1+i_elem))
     end do
!
! - Print check
!
    !write(*,*)"LIST_MA_AUX:",list_el_ma_ax(:)
!
end subroutine    
