subroutine apsave_patch(mesh          , sdappa        , i_zone, pair_tole,&
                         patch_weight_c, patch_weight_t, nb_proc, list_pair_zmpi,&
                         nb_pair_zmpi, list_pair_zone, nb_pair_zone, i_proc)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/sdmpic.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/jedetr.h"
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
    integer, intent(inout) :: nb_pair_zone
    integer, pointer, intent(inout) :: list_pair_zone(:)
    integer, pointer, intent(in) :: nb_pair_zmpi(:)
    integer, pointer, intent(in) :: list_pair_zmpi(:)  
    integer, intent(in) :: nb_proc
    integer, intent(in) :: i_proc
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
    integer :: i_patch, patch_indx, patch_jdec, nb_patch, deca, idx_start, idx_end
    integer :: i_proc2, nb_pair_init,i_pair
    integer, pointer :: v_mesh_lpatch(:) => null()
    character(len=24) :: sdappa_gapi, sdappa_coef, sdappa_poid,njv_aux
    real(kind=8), pointer :: v_sdappa_gapi(:) => null()
    real(kind=8), pointer :: v_sdappa_coef(:) => null()
    real(kind=8), pointer :: v_sdappa_poid(:) => null()
    integer, pointer :: v_sdappa_dcl(:) => null()
    integer, pointer :: list_tmp(:) => null()
    integer, pointer :: list_aux(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Access to patch
!
    call jeveuo(jexnum(mesh//'.PATCH',1), 'L', vi = v_mesh_lpatch)
    call jeveuo(sdappa(1:19)//'.DCL', 'L', vi = v_sdappa_dcl)
    nb_patch   = v_mesh_lpatch(2*(v_sdappa_dcl(i_zone)-1)+2) 
    patch_jdec = v_mesh_lpatch(2*(v_sdappa_dcl(i_zone)-1)+1)-1
    
!
! - MPI control 
!
    njv_aux = sdappa(1:19)//'.AUX '
    nb_pair_init=0
    call sdmpic('SD_APPA_LAC1', sdappa)
!
! - Not first contact element
!
    if (nb_pair_zone .ne. 0) then
!
! ----- Get old contact elements and copy them in temporary one
!
        nb_pair_init = nb_pair_zone
        AS_ALLOCATE(vi = list_tmp, size = 3*nb_pair_init)
        do i_pair = 1, 3*nb_pair_init
            list_tmp(i_pair) = list_pair_zone(i_pair)    
        end do
    end if
    
    do i_proc2=1, nb_proc
        nb_pair_zone=nb_pair_zone+nb_pair_zmpi(i_proc2)
    end do
!
! - Re-allocate list of contact elements
!    
    AS_DEALLOCATE(vi = list_pair_zone)
    AS_ALLOCATE(vi = list_pair_zone, size = 3*nb_pair_zone)
    call wkvect(njv_aux,"V V I", 3*nb_pair_zone, vi=list_aux)

!
! ----- Add new pairs
!  
    deca = 0
    do i_proc2 = 1, i_proc
        deca = deca + nb_pair_zmpi(i_proc2)
    end do
    idx_start = 1 + 3*(deca+nb_pair_init) 
    idx_end   = 3*(nb_pair_zmpi(i_proc+1) + deca + nb_pair_init)
    if (nb_pair_zmpi(i_proc+1).ne.0) then
       ASSERT(idx_end-idx_start+1 .eq. 3*nb_pair_zmpi(i_proc+1))
       list_aux(idx_start:idx_end)=list_pair_zmpi(1:3*nb_pair_zmpi(i_proc+1))
    endif 
    call sdmpic('SD_APPA_LAC2', sdappa)

    list_pair_zone(:)=list_aux(:)
!
! ----- Copy old pairs
!
    do i_pair = 1, 3*nb_pair_init
        list_pair_zone(i_pair) = list_tmp(i_pair)    
    end do
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
        patch_indx = i_patch-1+patch_jdec
        if (patch_weight_c(patch_indx) .le. pair_tole) then
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
            v_sdappa_gapi(patch_indx) = v_sdappa_gapi(patch_indx)/patch_weight_c(patch_indx)
            v_sdappa_coef(patch_indx) = patch_weight_c(patch_indx)/patch_weight_t(patch_indx)
            v_sdappa_poid(patch_indx) = patch_weight_c(patch_indx)
        end if          
    end do
    call jedetr(njv_aux)
    AS_DEALLOCATE(vi = list_tmp)
!
end subroutine
