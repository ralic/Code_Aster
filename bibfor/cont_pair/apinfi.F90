subroutine apinfi(sdappa, questi_, i_poin, vali)
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
    integer, intent(out) :: vali
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
!                       'APPARI_TYPE'     type of pairing
!                       'APPARI_ENTITE'   index of entity paired
!                       'APPARI_ZONE'     index of zone
!                       'APPARI_MAILLE'   element of point
! In  i_poin           : index of point (contact or non-contact)
! Out vali             : answer
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdappa_appa
    integer, pointer :: v_sdappa_appa(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    vali = 0
    sdappa_appa = sdappa(1:19)//'.APPA'
    call jeveuo(sdappa_appa, 'L', vi = v_sdappa_appa)
!
    if (questi_ .eq. 'APPARI_TYPE') then
        vali = v_sdappa_appa(4*(i_poin-1)+1)
    else if (questi_.eq.'APPARI_ENTITE') then
        vali = v_sdappa_appa(4*(i_poin-1)+2)
    else if (questi_.eq.'APPARI_ZONE') then
        vali = v_sdappa_appa(4*(i_poin-1)+3)
    else if (questi_.eq.'APPARI_MAILLE') then
        vali = v_sdappa_appa(4*(i_poin-1)+4)
    else
        ASSERT(.false.)
    endif
!
end subroutine
