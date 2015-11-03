subroutine apcopt(sdappa, i_poin, poin_coor)
!
implicit none
!
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
    integer, intent(in) :: i_poin
    real(kind=8), intent(out) :: poin_coor(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Get coordinate of point
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  i_poin           : current index of point
! Out poin_coor        : coordinates of point
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdappa_poin
    real(kind=8), pointer :: v_sdappa_poin(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdappa_poin = sdappa(1:19)//'.POIN'
    call jeveuo(sdappa_poin, 'L', vr = v_sdappa_poin)
!
    poin_coor(1) = v_sdappa_poin(3*(i_poin-1)+1)
    poin_coor(2) = v_sdappa_poin(3*(i_poin-1)+2)
    poin_coor(3) = v_sdappa_poin(3*(i_poin-1)+3)
!
end subroutine
