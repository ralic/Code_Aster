subroutine nmext2(mesh         , field    , nb_cmp  , nb_node  , type_extr,&
                  type_extr_cmp, list_node, list_cmp, work_node)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmexti.h"
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
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_node
    integer, intent(in) :: nb_cmp
    character(len=8), intent(in) :: type_extr
    character(len=8), intent(in) :: type_extr_cmp
    character(len=24), intent(in) :: list_node
    character(len=24), intent(in) :: list_cmp
    character(len=19), intent(in) :: field
    character(len=19), intent(in) :: work_node
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Extract value(s) at nodes and store them in working vectors
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  field            : name of field
! In  nb_node          : number of nodes
! In  nb_cmp           : number of components
! In  work_node        : working vector to save node values
! In  list_node        : name of object contains list of nodes
! In  list_cmp         : name of object contains list of components
! In  type_extr        : type of extraction
! In  type_extr_cmp    : type of extraction for components
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_para_maxi
    parameter    (nb_para_maxi=20)
    real(kind=8) :: vale_resu(nb_para_maxi)
!
    integer :: i_node, i_node_work, node_nume
    character(len=8) :: node_name
    integer :: i_vale, nb_vale
    real(kind=8) :: valr, val2r
    real(kind=8), pointer :: v_work_node(:) => null()
    integer, pointer :: v_list_node(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jeveuo(work_node, 'E', vr = v_work_node)
    ASSERT(nb_cmp.le.nb_para_maxi)
!
! - List of nodes
!
    call jeveuo(list_node, 'L', vi = v_list_node)
!
! - Loop on nodes
!
    do i_node = 1, nb_node
!
! ----- Current node
!
        node_nume = v_list_node(i_node)
        call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume), node_name)
!
! ----- Extract value(s) at node
!
        call nmexti(node_name, field    , nb_cmp, list_cmp, type_extr_cmp,&
                    nb_vale   , vale_resu)
!
! ----- Select index in working vector
!
        if (type_extr .eq. 'VALE') then
            i_node_work = i_node
        else
            i_node_work = 1
        endif
!
! ----- Save values in working vector
!
        do i_vale = 1, nb_vale
            valr  = v_work_node(i_vale+nb_cmp*(i_node_work-1))
            val2r = vale_resu(i_vale)
            if (type_extr .eq. 'VALE') then
                v_work_node(i_vale+nb_cmp*(i_node_work-1)) = val2r
            else if (type_extr.eq.'MIN') then
                v_work_node(i_vale+nb_cmp*(i_node_work-1)) = min(val2r,valr)
            else if (type_extr.eq.'MAX') then
                v_work_node(i_vale+nb_cmp*(i_node_work-1)) = max(val2r,valr)
            else if (type_extr.eq.'MAXI_ABS') then
                v_work_node(i_vale+nb_cmp*(i_node_work-1)) = max(abs(val2r), abs(valr))
            else if (type_extr.eq.'MINI_ABS') then
                v_work_node(i_vale+nb_cmp*(i_node_work-1)) = min(abs(val2r), abs(valr))
            else if (type_extr.eq.'MOY') then
                v_work_node(i_vale+nb_cmp*(i_node_work-1)) = valr+val2r
            else
                ASSERT(.false.)
            endif
        end do
    end do
!
! - For mean value
!
    if (type_extr .eq. 'MOY') then
        i_node_work = 1
        do i_vale = 1, nb_vale
            valr = v_work_node(i_vale+nb_cmp*(i_node_work-1))
            if (nb_node .ne. 0) then
                v_work_node(i_vale+nb_cmp*(i_node_work-1)) = valr/nb_node
            endif
        end do
    endif
!
end subroutine
