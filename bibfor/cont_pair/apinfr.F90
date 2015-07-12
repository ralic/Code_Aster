subroutine apinfr(sdappa, questi_, i_poin, valr)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
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
    character(len=19), intent(in) :: sdappa
    character(len=*), intent(in) :: questi_
    integer, intent(in) :: i_poin
    real(kind=8), intent(out) :: valr
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Ask datastructure - By point - Integer
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  questi           : question
!                       'APPARI_PROJ_KSI1'  first para. coor. of projection of point
!                       'APPARI_PROJ_KSI2'  second para. coor. of projection of point
!                       'APPARI_DIST'       distance point-projection
! In  i_poin           : index of point (contact or non-contact)
! Out valr             : answer
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdappa_dist
    real(kind=8), pointer :: v_sdappa_dist(:) => null()
    character(len=24) :: sdappa_proj
    real(kind=8), pointer :: v_sdappa_proj(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    valr = 0.d0
    sdappa_proj = sdappa(1:19)//'.PROJ'
    sdappa_dist = sdappa(1:19)//'.DIST'
    call jeveuo(sdappa_proj, 'L', vr = v_sdappa_proj)
    call jeveuo(sdappa_dist, 'L', vr = v_sdappa_dist)
!
    if (questi_ .eq. 'APPARI_PROJ_KSI1') then
        valr = v_sdappa_proj(2*(i_poin-1)+1)
    else if (questi_.eq.'APPARI_PROJ_KSI2') then
        valr = v_sdappa_proj(2*(i_poin-1)+2)
    else if (questi_.eq.'APPARI_DIST') then
        valr = v_sdappa_dist(4*(i_poin-1)+1)
    else
        ASSERT(.false.)
    endif
!
end subroutine
