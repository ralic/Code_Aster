subroutine gtlima(sdappa, sdcont_defi, i_zone)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/wkvect.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cfzone.h"
#include "asterfort/codent.h"
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
!
    character(len=19), intent(in) :: sdappa
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: i_zone  
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Create list of elements for current contact zone
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  i_zone           : index of contact zone
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: knuzo
    integer :: jcmmal, jcmesl
    integer :: i_elem, i_surf_mast, i_surf_slav
    integer :: nb_elem_mast, nb_elem_slav
    character(len=24) :: sdappa_mast, sdappa_slav 
    character(len=24) :: sdcont_mailco
    integer, pointer  :: v_sdcont_mailco(:) => null()
    integer, pointer  :: v_list_mast(:) => null()
    integer, pointer  :: v_list_slav(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Generate name of objects
!
    ASSERT(i_zone .le. 9)
    call codent(i_zone, 'G', knuzo)
    sdappa_mast = sdappa(1:19)//'.MAS'//knuzo(1:1)
    sdappa_slav = sdappa(1:19)//'.ESC'//knuzo(1:1)
!
! - Access to contact datastructures
!
    sdcont_mailco = sdcont_defi(1:16)//'.MAILCO'
    call jeveuo(sdcont_mailco, 'L', vi = v_sdcont_mailco)
!
! - Access to current contact zone
!
    call cfzone(sdcont_defi, i_zone, 'MAIT', i_surf_mast)
    call cfzone(sdcont_defi, i_zone, 'ESCL', i_surf_slav)
    call cfnbsf(sdcont_defi, i_surf_mast, 'MAIL', nb_elem_mast, jcmmal)
    call cfnbsf(sdcont_defi, i_surf_slav, 'MAIL', nb_elem_slav, jcmesl)
!
! - Create objects
!
    call wkvect(sdappa_mast, 'V V I', nb_elem_mast, vi = v_list_mast)
    call wkvect(sdappa_slav, 'V V I', nb_elem_slav, vi = v_list_slav)
!
! - Fill objects
!
    do i_elem = 1, nb_elem_mast
        v_list_mast(i_elem) = v_sdcont_mailco(jcmmal+i_elem)
    end do
    do i_elem = 1, nb_elem_slav    
        v_list_slav(i_elem) = v_sdcont_mailco(jcmesl+i_elem)
    end do
!
end subroutine
