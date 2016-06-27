subroutine aplcfb(mesh        , newgeo        , sdappa      , i_zone        , pair_tole,&
                  nb_elem_mast, list_elem_mast, nb_elem_slav, list_elem_slav, &
                  nb_pair_zone, list_pair_zone)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8nnem.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/apcoor.h"
#include "asterfort/prjint.h"
#include "asterfort/gapint.h"
#include "asterfort/jecroc.h"
#include "asterfort/clpoma.h"
#include "asterfort/assert.h"
#include "asterfort/apdcma.h"
#include "asterfort/aprtpe.h"
#include "asterfort/apsave_pair.h"
#include "asterfort/apsave_patch.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
! aslint: disable=W1306
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: newgeo
    character(len=19), intent(in) :: sdappa
    integer, intent(in) :: i_zone
    real(kind=8), intent(in) :: pair_tole
    integer, intent(in) :: nb_elem_mast
    integer, intent(in) :: list_elem_mast(nb_elem_mast)
    integer, intent(in) :: nb_elem_slav
    integer, intent(in) :: list_elem_slav(nb_elem_slav)
    integer, intent(inout) :: nb_pair_zone
    integer, pointer, intent(inout) :: list_pair_zone(:)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Pairing by "brute" force
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
! In  sdappa           : name of pairing datastructure
! In  i_zone           : index of contact zone
! In  pair_tole        : tolerance for pairing
! In  nb_elem_mast     : number of master elements on current zone
! In  nb_elem_slav     : number of slave elements on current zone
! In  list_elem_mast   : name of datastructure for list of master elements on current zone
! In  list_elem_slav   : name of datastructure for list of slave elements on current zone
! IO  nb_pair_zone     : number of contact elements
! IO  list_pair_zone   : list of contact elements
!
! --------------------------------------------------------------------------------------------------
!
    integer :: list_pair(nb_elem_mast)
    integer :: elem_slav_nbnode, elem_slav_nume, elem_slav_dime
    integer :: elem_mast_nbnode, elem_mast_nume, elem_mast_dime
    integer :: nb_pair, nb_poin_inte
    integer :: i_elem_slav, i_elem_mast, i_elin_mast, i_node, i_elin_slav, i_dime
    integer :: patch_indx, patch_jdec, patch_nume
    character(len=8) :: elem_mast_code, elem_slav_code
    character(len=24) :: sdappa_gapi, sdappa_coef
    real(kind=8), pointer :: v_sdappa_gapi(:) => null()
    real(kind=8), pointer :: v_sdappa_coef(:) => null()
    real(kind=8) :: inte_weight, gap_moy, patch_weight_c(nb_elem_mast), patch_weight_t(nb_elem_mast)
    real(kind=8) :: total_weight, elem_slav_weight, poin_inte(32)
    real(kind=8) :: elin_mast_coor(27), elin_slav_coor(27)
    real(kind=8) :: elem_mast_coor(27), elem_slav_coor(27)
    integer :: elin_mast_nbsub, elin_mast_sub(8,4), elin_mast_nbnode(8)
    integer :: elin_slav_nbsub, elin_slav_sub(8,4), elin_slav_nbnode(8)
    character(len=8) :: elin_mast_code, elin_slav_code, elem_slav_type, elem_mast_type
    integer :: jv_geom, elem_type_nume
    integer, pointer :: v_mesh_comapa(:) => null()
    integer, pointer :: v_mesh_patch(:) => null()
    integer, pointer :: v_mesh_typmail(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    patch_weight_c(1:nb_elem_mast) = 0.d0
    patch_weight_t(1:nb_elem_mast) = 0.d0
!
! - Access to pairing datastructures
!
    sdappa_gapi = sdappa(1:19)//'.GAPI'
    sdappa_coef = sdappa(1:19)//'.COEF'
    call jeveuo(sdappa_gapi, 'E', vr = v_sdappa_gapi)
    call jeveuo(sdappa_coef, 'E', vr = v_sdappa_coef)
!
! - Access to updated geometry
!
    call jeveuo(newgeo(1:19)//'.VALE', 'L', jv_geom)
!
! - Access to mesh
!
    call jeveuo(mesh//'.TYPMAIL', 'L', vi = v_mesh_typmail)
    call jeveuo(jexnum(mesh//'.PATCH',1), 'L', vi = v_mesh_patch)
    call jeveuo(mesh//'.COMAPA','L', vi = v_mesh_comapa)
    patch_jdec = v_mesh_patch(2*(i_zone-1)+1)-1
!
! - Loop on slave elements
!
    do i_elem_slav = 1, nb_elem_slav
!
! ----- Current slave element
!
        elem_slav_nume = list_elem_slav(i_elem_slav)
        elem_type_nume = v_mesh_typmail(elem_slav_nume)
        call jenuno(jexnum('&CATA.TM.NOMTM', elem_type_nume), elem_slav_type)
!
! ----- Get informations about element
!
        call apcoor(mesh          , jv_geom       , elem_slav_type  ,&
                    elem_slav_nume, elem_slav_coor, elem_slav_nbnode,&
                    elem_slav_code, elem_slav_dime)
!
! ----- Get current patch
!
        patch_indx = v_mesh_comapa(elem_slav_nume)
        patch_nume = patch_indx+1-patch_jdec
!
! ----- Compute weight of element
!
        call clpoma(elem_slav_dime  , elem_slav_code, elem_slav_coor, elem_slav_nbnode,&
                    elem_slav_weight)
!
! ----- Total weight for patch
!                    
        patch_weight_t(patch_nume) = patch_weight_t(patch_nume) + elem_slav_weight
!
! ----- Cut element in linearized sub-elements
!
        call apdcma(elem_slav_code, elin_slav_sub, elin_slav_nbnode, elin_slav_nbsub)
!
! ----- Loop on master elements
!
        nb_pair = 0
        do i_elem_mast = 1, nb_elem_mast
!
            total_weight=0.d0 
!
! --------- Current master element
!
            elem_mast_nume = list_elem_mast(i_elem_mast)
            elem_type_nume = v_mesh_typmail(elem_mast_nume)
            call jenuno(jexnum('&CATA.TM.NOMTM', elem_type_nume), elem_mast_type)
!
! --------- Get informations about element
!
            call apcoor(mesh          , jv_geom       , elem_mast_type  ,&
                        elem_mast_nume, elem_mast_coor, elem_mast_nbnode,&
                        elem_mast_code, elem_mast_dime)
!
! --------- Cut element in linearized sub-elements
!
            call apdcma(elem_mast_code, elin_mast_sub, elin_mast_nbnode, elin_mast_nbsub)
!
! --------- Loop on linearized master sub-elements
!
            do i_elin_mast = 1, elin_mast_nbsub
!
! ------------- Code for current linearized master sub-element
!
                if (elin_mast_nbnode(i_elin_mast) .eq. 2) then
                    elin_mast_code = 'SE2' 
                elseif (elin_mast_nbnode(i_elin_mast) .eq. 3) then
                    elin_mast_code = 'TR3'
                else
                    ASSERT(.false.)
                end if
!
! ------------- Get coordinates for current linearized master sub-element
!
                do i_node = 1, elin_mast_nbnode(i_elin_mast)
                    do i_dime = 1,elem_slav_dime
                         elin_mast_coor(3*(i_node-1)+i_dime) = &
                            elem_mast_coor(3*(elin_mast_sub(i_elin_mast,i_node)-1)+i_dime)
                    end do
                end do
!
! ------------- Loop on linearized slave sub-elements
!
                do i_elin_slav=1, elin_slav_nbsub
!
! ----------------- Code for current linearized slave sub-element
!
                    if (elin_slav_nbnode(i_elin_slav) .eq. 2) then
                        elin_slav_code = 'SE2'
                    elseif (elin_slav_nbnode(i_elin_slav) .eq. 3) then
                        elin_slav_code = 'TR3'
                    else
                        ASSERT(.false.)
                    endif
!
! ----------------- Coordinates for current linearized slave sub-element
!
                    do i_node = 1, elin_slav_nbnode(i_elin_slav)
                        do i_dime = 1, elem_slav_dime
                            elin_slav_coor(3*(i_node-1)+i_dime) =&
                                elem_slav_coor(3*(elin_slav_sub(i_elin_slav,i_node)-1)+i_dime)
                        end do
                    end do
!
! ----------------- Projection/intersection of elements in slave parametric space     
!
                    call prjint(pair_tole     , elem_slav_dime,&
                                elin_slav_coor, elin_slav_nbnode(i_elin_slav), elin_slav_code,&
                                elin_mast_coor, elin_mast_nbnode(i_elin_mast), elin_mast_code,&
                                poin_inte     , inte_weight                  , nb_poin_inte  )
!
! ----------------- Non-void intersection  
!
                    if (inte_weight .gt. pair_tole) then
!
                        total_weight = total_weight+inte_weight
!
! --------------------- Projection from para. space of element into sub-element para. space
!
                        call aprtpe(elem_slav_dime, poin_inte  , nb_poin_inte,&
                                    elem_slav_code, i_elin_slav)
!
! --------------------- Compute mean square gap and weight of intersection
!
                        call gapint(pair_tole     , elem_slav_dime  ,&
                                    elem_slav_code, elem_slav_nbnode, elem_slav_coor,&
                                    elem_mast_code, elem_mast_nbnode, elem_mast_coor,&
                                    nb_poin_inte  , poin_inte       ,&
                                    gap_moy       , inte_weight     )
!   
! --------------------- Save values
!
                        v_sdappa_gapi(patch_indx) = v_sdappa_gapi(patch_indx)+gap_moy
                        patch_weight_c(patch_nume)  = patch_weight_c(patch_nume)+inte_weight
                    end if
                end do
            end do
!
! --------- Add element paired
!
            if (total_weight .gt. pair_tole) then
                nb_pair            = nb_pair+1
                list_pair(nb_pair) = elem_mast_nume
            end if       
        end do
!
! ----- Save pairing informations (contact pair)
!
        if (nb_pair .ne. 0) then
            call apsave_pair(i_zone      , elem_slav_nume,&
                             nb_pair     , list_pair     ,&
                             nb_pair_zone, list_pair_zone)
        end if
    end do
!
! - Save values for patch
!
    call apsave_patch(mesh          , sdappa        , i_zone, pair_tole,&
                      patch_weight_c, patch_weight_t)
!
end subroutine       
