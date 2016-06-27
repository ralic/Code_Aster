subroutine apsave_patch(mesh          , sdappa        , i_zone, pair_tole,&
                        patch_weight_c, patch_weight_t)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
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
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: sdappa
    integer, intent(in) :: i_zone
    real(kind=8), intent(in) :: pair_tole
    real(kind=8), intent(in) :: patch_weight_c(*)
    real(kind=8), intent(in) :: patch_weight_t(*)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Save values for patch
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdappa           : name of pairing datastructure
! In  i_zone           : index of contact zone
! In  pair_tole        : tolerance for pairing
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_patch, patch_indx, patch_jdec, nb_patch
    integer, pointer :: v_mesh_comapa(:) => null()
    integer, pointer :: v_mesh_lpatch(:) => null()
    character(len=24) :: sdappa_gapi, sdappa_coef, sdappa_poid
    real(kind=8), pointer :: v_sdappa_gapi(:) => null()
    real(kind=8), pointer :: v_sdappa_coef(:) => null()
    real(kind=8), pointer :: v_sdappa_poid(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Access to patch
!
    call jeveuo(jexnum(mesh//'.PATCH',1), 'L', vi = v_mesh_lpatch)
    call jeveuo(mesh//'.COMAPA','L', vi = v_mesh_comapa)
    nb_patch   = v_mesh_lpatch(2*(i_zone-1)+2) 
    patch_jdec = v_mesh_lpatch(2*(i_zone-1)+1)-1
!
! - Access to pairing datastructures
!
    sdappa_gapi = sdappa(1:19)//'.GAPI'
    sdappa_coef = sdappa(1:19)//'.COEF'
    sdappa_poid = sdappa(1:19)//'.POID' 
    call jeveuo(sdappa_gapi, 'E', vr = v_sdappa_gapi)
    call jeveuo(sdappa_coef, 'E', vr = v_sdappa_coef)
    call jeveuo(sdappa_poid, 'E', vr = v_sdappa_poid)
!
! - Init
!
    do i_patch = 1, nb_patch
        patch_indx = i_patch-1+patch_jdec
        v_sdappa_coef(patch_indx) = 0.d0
        v_sdappa_poid(patch_indx) = 0.d0
    end do
!
! - For non-paired patchs => NAN for gap
!
    do i_patch = 1, nb_patch
        if (patch_weight_c(i_patch) .le. pair_tole) then
            patch_indx = i_patch-1+patch_jdec
            v_sdappa_gapi(patch_indx) = r8nnem()
            v_sdappa_coef(patch_indx) = 0.d0   
        end if
    end do
!
! - Compute gap
!
    do i_patch = 1, nb_patch
        patch_indx = i_patch-1+patch_jdec
        if (.not.isnan(v_sdappa_gapi(patch_indx))) then
            v_sdappa_gapi(patch_indx) = v_sdappa_gapi(patch_indx)/patch_weight_c(i_patch)
            v_sdappa_coef(patch_indx) = patch_weight_c(i_patch)/patch_weight_t(i_patch)
            v_sdappa_poid(patch_indx) = patch_weight_c(i_patch)
        end if          
    end do
!
end subroutine
