subroutine nmsui3(sd_prnt      , field_disc, nb_elem  , nb_node      , nb_poin       ,&
                  nb_spoi      , nb_cmp    , type_extr, type_extr_cmp, type_extr_elem,&
                  list_elem    , work_node , work_elem, field        , field_s       ,&
                  i_dof_monitor)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmsuiy.h"
#include "asterfort/sdmpic.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: nb_node
    integer, intent(in) :: nb_elem
    integer, intent(in) :: nb_poin
    integer, intent(in) :: nb_spoi
    integer, intent(in) :: nb_cmp
    character(len=24), intent(in) :: list_elem
    character(len=19), intent(in) :: field
    character(len=4), intent(in) :: field_disc
    character(len=24), intent(in) :: field_s
    character(len=24), intent(in) :: sd_prnt
    character(len=8), intent(in) :: type_extr
    character(len=8), intent(in) :: type_extr_elem
    character(len=8), intent(in) :: type_extr_cmp
    character(len=19), intent(in) :: work_node
    character(len=19), intent(in) :: work_elem
    integer, intent(inout) :: i_dof_monitor
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear operators - DOF monitor
!
! Print monitored values in table
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_prnt          : datastructure for print informations
! In  nb_node          : number of nodes
! In  nb_elem          : number of elements
! In  nb_poin          : number of points (Gauss)
! In  nb_spoi          : number of subpoints
! In  nb_cmp           : number of components
! In  list_elem        : name of object contains list of elements
! In  field            : name of field
! In  field_disc       : localization of field (discretization: NOEU or ELGA)
! In  field_s          : name of reduced field (CHAM_ELEM_S)
! In  type_extr        : type of extraction
! In  type_extr_elem   : type of extraction by element
! In  type_extr_cmp    : type of extraction for components
! In  work_node        : working vector to save node values
! In  work_elem        : working vector to save element values
! IO  i_dof_monitor    : index of current monitoring
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_node, i_elem, i_poin, i_spoi, i_cmp
    integer :: nb_poin_elem, nb_spoi_elem, elem_nume, iret
    real(kind=8) :: vale_r
    integer :: nb_cmp_r, nb_poin_r, nb_spoi_r, nb_node_r, nb_elem_r
    integer :: nb_poin_e , nb_spoi_e
    integer, pointer :: cesd(:) => null()
    integer, pointer :: v_list_elem(:) => null()
    real(kind=8), pointer :: v_work_node(:) => null()
    real(kind=8), pointer :: v_work_elem(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Convert to reduced field
!
    if (field_disc .eq. 'ELGA') then
        call jeexin(field_s, iret)
        if (iret .eq. 0) then
            call sdmpic('CHAM_ELEM', field)
            call celces(field, 'V', field_s)
        endif
        call jeveuo(field_s(1:19)//'.CESD', 'L', vi=cesd)
    endif
!
! - Number of nodes for loop
!
    if (field_disc .eq. 'NOEU') then
        if (type_extr .eq. 'VALE') then
            nb_node_r = nb_node
        elseif ((type_extr.eq.'MIN').or.&
                (type_extr.eq.'MAX').or.&
                (type_extr.eq.'MAXI_ABS').or.&
                (type_extr.eq.'MINI_ABS').or.&
                (type_extr.eq.'MOY')) then
            nb_node_r = 1
        else
            ASSERT(.false.)
        endif
    endif
!
! - Number of elements for loop
!
    if (field_disc .eq. 'ELGA') then
        if (type_extr .eq. 'VALE') then
            nb_elem_r = nb_elem
        elseif ((type_extr.eq.'MIN').or.&
                (type_extr.eq.'MAX').or.&
                (type_extr.eq.'MAXI_ABS').or.&
                (type_extr.eq.'MINI_ABS').or.&
                (type_extr.eq.'MOY')) then
            nb_elem_r = 1
        else
            ASSERT(.false.)
        endif
    endif
!
! - Number for components for loop
!
    if (type_extr_cmp .eq. ' ') then
        nb_cmp_r = nb_cmp
    else
        nb_cmp_r = 1
    endif
!
! - For node discretization
!
    if (field_disc .eq. 'NOEU') then
        call jeveuo(work_node, 'L', vr = v_work_node)
        do i_node = 1, nb_node_r
            do i_cmp = 1, nb_cmp_r
                vale_r   = v_work_node(i_cmp+nb_cmp*(i_node-1))
                call nmsuiy(sd_prnt, vale_r, i_dof_monitor)
            end do
        end do
    endif
!
! - For element discretization
!
    if (field_disc .eq. 'ELGA') then
        call jeveuo(work_elem, 'L', vr = v_work_elem)
        call jeveuo(list_elem, 'L', vi = v_list_elem)
!
        do i_elem = 1, nb_elem_r
!
! --------- Current element
!
            elem_nume = v_list_elem(i_elem)
!
! --------- Real number of point/subpoint for current element
!
            nb_poin_elem = cesd(1+5+4*(elem_nume-1))
            nb_spoi_elem = cesd(1+5+4*(elem_nume-1)+1)
!
! --------- Check
!
            nb_poin_e = nb_poin
            nb_spoi_e = nb_spoi
            if (nb_poin_e .gt. nb_poin_elem) nb_poin_e = nb_poin_elem
            if (nb_spoi_e .gt. nb_spoi_elem) nb_spoi_e = nb_spoi_elem
!
! --------- Number for points/subpoints for loop
!
            if (type_extr_elem .eq. 'VALE') then
                nb_poin_r = nb_poin_e
                nb_spoi_r = nb_spoi_e
            else
                nb_poin_r = 1
                nb_spoi_r = 1
            endif
!
            do i_poin = 1, nb_poin_r
                do i_spoi = 1, nb_spoi_r
                    do i_cmp = 1, nb_cmp_r
                        vale_r   = v_work_elem(nb_cmp*nb_poin*nb_spoi*(i_elem-1)+&
                                               nb_poin*nb_spoi*(i_cmp-1)+&
                                               nb_spoi*(i_poin-1)+&
                                               (i_spoi-1)+1)
                        call nmsuiy(sd_prnt, vale_r, i_dof_monitor)
                    end do
                end do
            end do
        end do
    endif
!
end subroutine
