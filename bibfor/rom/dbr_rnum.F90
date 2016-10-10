subroutine dbr_rnum(ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexnom.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/infniv.h"
#include "asterfort/jelira.h"
#include "asterfort/uttrir.h"
#include "asterfort/romLineicIndexList.h"
#include "asterfort/romLineicIndexSurf.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(ROM_DS_Empi), intent(inout) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Renumbering
!
! Create numbering of nodes for the lineic model
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: niv, ifm
    type(ROM_DS_LineicNumb) :: ds_line
    integer :: nb_node, nb_slice, i_node, nb_node_grno
    real(kind=8) :: tole_node
    character(len=8) :: axe_line = ' ', mesh= ' '
    character(len=24) :: surf_num = ' '
    integer          , pointer :: v_grno(:) => null()
    real(kind=8)     , pointer :: v_coor(:) => null()
    real(kind=8)     , pointer :: v_coor_x(:) => null()
    real(kind=8)     , pointer :: v_coor_y(:) => null()
    real(kind=8)     , pointer :: v_coor_z(:) => null()
    real(kind=8)     , pointer :: v_coor_w(:) => null()
    real(kind=8)     , pointer :: v_coor_p(:) => null()
    real(kind=8)     , pointer :: v_coor_1(:) => null()
    real(kind=8)     , pointer :: v_coor_2(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
       call utmess('I', 'ROM5_12')
    endif
!
! - Get parameters
!
    mesh      = ds_empi%mesh
    axe_line  = ds_empi%axe_line
    surf_num  = ds_empi%surf_num
    ds_line   = ds_empi%ds_lineic
    nb_node   = ds_empi%nb_node
    tole_node = ds_line%tole_node
!
! - Allocate pointers for lineic objects
!
    nb_slice = nb_node
    AS_ALLOCATE(vi = ds_line%v_nume_pl, size = nb_node)
    AS_ALLOCATE(vi = ds_line%v_nume_sf, size = nb_node)
!
! - Get coordinates of nodes
!
    call jeveuo(mesh//'.COORDO    .VALE', 'L', vr = v_coor)
    AS_ALLOCATE(vr = v_coor_x, size = nb_node)
    AS_ALLOCATE(vr = v_coor_y, size = nb_node)
    AS_ALLOCATE(vr = v_coor_z, size = nb_node)
    AS_ALLOCATE(vr = v_coor_w, size = nb_node)
    do i_node = 1, nb_node
        v_coor_x(i_node) = v_coor(1+3*(i_node-1)+0)
        v_coor_y(i_node) = v_coor(1+3*(i_node-1)+1)
        v_coor_z(i_node) = v_coor(1+3*(i_node-1)+2)
    enddo
!
! - Get coordinates of nodes for one slice
!
    call jelira(jexnom(mesh//'.GROUPENO',surf_num), 'LONUTI', nb_node_grno)
    call jeveuo(jexnom(mesh//'.GROUPENO',surf_num), 'L'     , vi = v_grno)
    AS_ALLOCATE(vr = v_coor_1, size = nb_node_grno)
    AS_ALLOCATE(vr = v_coor_2, size = nb_node_grno)
!
! - In case of lineic model, we must create a new numbering for the nodes on mesh
!
    if (axe_line .eq. 'OX') then
        v_coor_w(1:nb_node) = v_coor_x(1:nb_node)
        call uttrir(nb_slice, v_coor_w, tole_node)
        AS_ALLOCATE(vr = v_coor_p, size = nb_slice)
        v_coor_p(1:nb_slice) = v_coor_w(1:nb_slice)

        call romLineicIndexList(nb_node  , v_coor_x , nb_slice, v_coor_p,&
                                ds_line%v_nume_pl, tole_node)
        
        do i_node = 1, nb_node_grno
            v_coor_1(i_node) = v_coor(1+3*(v_grno(i_node)-1)+1)
            v_coor_2(i_node) = v_coor(1+3*(v_grno(i_node)-1)+2)
        enddo
        call romLineicIndexSurf(nb_node     , v_coor_y , v_coor_z,&
                                nb_node_grno, v_coor_1 , v_coor_2,&
                                ds_line%v_nume_sf, tole_node)
    elseif (axe_line .eq. 'OY') then
        v_coor_w(1:nb_node) = v_coor_y(1:nb_node)
        call uttrir(nb_slice, v_coor_w, tole_node)
        AS_ALLOCATE(vr=v_coor_p, size=nb_slice)
        v_coor_p(1:nb_slice) = v_coor_w(1:nb_slice)

        call romLineicIndexList(nb_node  , v_coor_y , nb_slice, v_coor_p,&
                                ds_line%v_nume_pl, tole_node)

        do i_node = 1, nb_node_grno
            v_coor_1(i_node) = v_coor(1+3*(v_grno(i_node)-1)+2)
            v_coor_2(i_node) = v_coor(1+3*(v_grno(i_node)-1)+0)
        enddo
        call romLineicIndexSurf(nb_node     , v_coor_z , v_coor_x,&
                                nb_node_grno, v_coor_1 , v_coor_2,&
                                ds_line%v_nume_sf, tole_node)    
    elseif (axe_line .eq. 'OZ') then
        v_coor_w(1:nb_node) = v_coor_z(1:nb_node)
        call uttrir(nb_slice, v_coor_w, tole_node)
        AS_ALLOCATE(vr=v_coor_p, size=nb_slice)
        v_coor_p(1:nb_slice) = v_coor_w(1:nb_slice)

        call romLineicIndexList(nb_node  , v_coor_z , nb_slice, v_coor_p,&
                                ds_line%v_nume_pl, tole_node)

        do i_node = 1, nb_node_grno
            v_coor_1(i_node) = v_coor(1+3*(v_grno(i_node)-1)+0)
            v_coor_2(i_node) = v_coor(1+3*(v_grno(i_node)-1)+1)
        enddo
        call romLineicIndexSurf(nb_node     , v_coor_x , v_coor_y,&
                                nb_node_grno, v_coor_1 , v_coor_2,&
                                ds_line%v_nume_sf, tole_node)
    else
        ASSERT(.false.)
    endif
!
! - Clean
!
    AS_DEALLOCATE(vr = v_coor_p)
    AS_DEALLOCATE(vr = v_coor_x)
    AS_DEALLOCATE(vr = v_coor_y)
    AS_DEALLOCATE(vr = v_coor_z)
    AS_DEALLOCATE(vr = v_coor_w)
    AS_DEALLOCATE(vr = v_coor_1)
    AS_DEALLOCATE(vr = v_coor_2)
!
! - Save 
!
    ds_line%nb_slice  = nb_slice
    ds_empi%ds_lineic = ds_line
!
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_13', si = nb_slice)
    endif
!
end subroutine
