subroutine apzonv(sdappa, i_zone, questi_, valr)
!
implicit none
!
#include "asterfort/apmmvd.h"
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
    integer, intent(in) :: i_zone
    real(kind=8), intent(out) :: valr(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Ask datastructure - By zone - Vector real
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  questi           : question
! In  i_zone           : index of contact zone
! Out valr             : answer
!
! --------------------------------------------------------------------------------------------------
!
    integer :: zinzr
    character(len=24) :: sdappa_inzr
    real(kind=8), pointer :: v_sdappa_inzr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    valr(1:3)   = 0.d0
    zinzr       = apmmvd('ZINZR')
    sdappa_inzr = sdappa(1:19)//'.INZR'
    call jeveuo(sdappa_inzr, 'L', vr = v_sdappa_inzr)
!
    if (questi_ .eq. 'DIRE_APPA_VECT') then
        valr(1) = v_sdappa_inzr(zinzr*(i_zone-1)+1 )
        valr(2) = v_sdappa_inzr(zinzr*(i_zone-1)+2 )
        valr(3) = v_sdappa_inzr(zinzr*(i_zone-1)+3 )
    else if (questi_.eq.'VECT_MAIT') then
        valr(1) = v_sdappa_inzr(zinzr*(i_zone-1)+6 )
        valr(2) = v_sdappa_inzr(zinzr*(i_zone-1)+7 )
        valr(3) = v_sdappa_inzr(zinzr*(i_zone-1)+8 )
    else if (questi_.eq.'VECT_ESCL') then
        valr(1) = v_sdappa_inzr(zinzr*(i_zone-1)+9 )
        valr(2) = v_sdappa_inzr(zinzr*(i_zone-1)+10)
        valr(3) = v_sdappa_inzr(zinzr*(i_zone-1)+11)
    else
        ASSERT(.false.)
    endif
!
end subroutine
