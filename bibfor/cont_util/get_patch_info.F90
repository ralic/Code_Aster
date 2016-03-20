subroutine get_patch_info(sdappa, patch_indx, nb_elem_patch, list_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
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
    character(len=19), intent(in) :: sdappa
    integer, intent(in) :: patch_indx
    integer, intent(out) :: nb_elem_patch
    integer, intent(out) :: list_elem(5)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Segment to segment - Information from patch
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  patch_indx       : index of patch
! Out nb_elem_patch    : number of elements in patch
! Out list_elem        : list of elements in patch
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_elem_patch
    character(len=24) :: sdappa_info
    integer, pointer :: v_sdappa_info(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_elem_patch  = 0
    list_elem(1:5) = 0
!
! - Access to datastructure
!
    sdappa_info = sdappa(1:19)//'.INFO'
    call jeveuo(sdappa_info, 'L', vi = v_sdappa_info)
!
! - Get parameters
!
    nb_elem_patch = v_sdappa_info(6*(patch_indx-1)+1)
    do i_elem_patch = 1, nb_elem_patch
        list_elem(i_elem_patch) = v_sdappa_info(6*(patch_indx-1)+1+i_elem_patch)
    end do
!
end subroutine
