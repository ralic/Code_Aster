subroutine apzonl(sdappa, i_zone, questi_, vall)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/apzoni.h"
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
    aster_logical, intent(out) :: vall
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Ask datastructure - By zone - Boolean
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  questi           : question
!               DIRE_APPA_FIXE DIRECTION FIXE D'APPARIEMENT ?
!               APPA_MAIT_ESCL APPARIEMENT MAITRE-ESCLAVE ?
!               CALC_NORM_ESCL CALCUL DE LA NORMALE SUR NOEUD ESCLAVE ?
!               CALC_NORM_MAIT CALCUL DE LA NORMALE SUR NOEUD MAITRE ?
! In  i_zone           : index of contact zone
! Out vall             : answer
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iappa
!
! --------------------------------------------------------------------------------------------------
!
    vall   = .false.
!
    if (questi_ .eq. 'DIRE_APPA_FIXE') then
        call apzoni(sdappa, i_zone, 'DIRE_APPA', iappa)
        vall = iappa.eq.1
    else if (questi_.eq.'APPA_MAIT_ESCL') then
        call apzoni(sdappa, i_zone, 'TYPE_APPA', iappa)
        vall = iappa.eq.1
    else if (questi_.eq.'CALC_NORM_ESCL') then
        call apzoni(sdappa, i_zone, 'CALC_NORM_ESCL', iappa)
        vall = iappa.eq.1
    else if (questi_.eq.'CALC_NORM_MAIT') then
        call apzoni(sdappa, i_zone, 'CALC_NORM_MAIT', iappa)
        vall = iappa.eq.1
    else
        ASSERT(.false.)
    endif
!
end subroutine
