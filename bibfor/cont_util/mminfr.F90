function mminfr(sdcont_defi_, question_, i_zone_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/mminfp.h"
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
    real(kind=8) :: mminfr
    character(len=*), intent(in) :: sdcont_defi_
    character(len=*), intent(in) :: question_
    integer, optional, intent(in) :: i_zone_
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Utility
!
! Get parameter (real) - By zone contact
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  question         : question to select parameter
! In  i_zone           : index of contact zone
! Out mminfr           : value for selected parameter
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_zone
    real(kind=8) :: vale_r
    character(len=24) :: sdcont_defi
!
! --------------------------------------------------------------------------------------------------
!
    if (present(i_zone_)) then
        i_zone = i_zone_
    else
        i_zone = 1
    endif
    sdcont_defi = sdcont_defi_
    call mminfp(i_zone, sdcont_defi, question_, vale_r_ = vale_r)
    mminfr = vale_r
end function
