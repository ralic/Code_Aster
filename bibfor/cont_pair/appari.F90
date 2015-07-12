subroutine appari(sdappa, questi_, vali)
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
    integer, intent(out) :: vali
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Ask datastructure - Global - Integer
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  questi           : question
!                       APPARI_NBZONE      NBRE DE ZONES
!                       APPARI_NDIMG       DIMENSION DE L'ESPACE
!                       APPARI_NTPT        NBRE DE POINTS
!                       PROJ_NEWT_ITER     NBRE ITERATION POUR NEWTON PROJECTION
! Out vali             : answer
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdappa_infi
    integer, pointer :: v_sdappa_infi(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    vali = 0
    sdappa_infi = sdappa(1:19)//'.INFI'
    call jeveuo(sdappa_infi, 'L', vi = v_sdappa_infi)
!
    if (questi_ .eq. 'APPARI_NBZONE') then
        vali = v_sdappa_infi(1)
    else if (questi_.eq.'APPARI_NTPT') then
        vali = v_sdappa_infi(2)
    else if (questi_.eq.'APPARI_NTMA') then
        vali = v_sdappa_infi(3)
    else if (questi_.eq.'PROJ_NEWT_ITER') then
        vali = v_sdappa_infi(4)
    else if (questi_.eq.'APPARI_NDIMG') then
        vali = v_sdappa_infi(5)
    else if (questi_.eq.'APPARI_NTNO') then
        vali = v_sdappa_infi(6)
    else
        ASSERT(.false.)
    endif
!
end subroutine
