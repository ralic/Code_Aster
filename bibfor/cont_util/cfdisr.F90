function cfdisr(sdcont_defi_, question_)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfr.h"
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
    real(kind=8) :: cfdisr
    character(len=*), intent(in) :: sdcont_defi_
    character(len=*), intent(in) :: question_
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Utility
!
! Get parameter (real)
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  question         : question to select parameter
! Out cfdisr           : value for selected parameter
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_defi, question
    character(len=24) :: sdcont_paracr
    real(kind=8), pointer :: v_sdcont_paracr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_defi = sdcont_defi_
    question    = question_
    cfdisr      = 0.d0
!
! - Access to datastructure
!
    sdcont_paracr = sdcont_defi(1:16)//'.PARACR'
    call jeveuo(sdcont_paracr, 'L', vr = v_sdcont_paracr)
!
! - Get parameter
!
    if (question .eq. 'RESI_GEOM') then
        cfdisr = v_sdcont_paracr(1)
    else if (question.eq.'RESI_FROT') then
        cfdisr = v_sdcont_paracr(2)
    else if (question.eq.'RESI_ABSO') then
        cfdisr = v_sdcont_paracr(4)
    else if (question.eq.'COEF_RESI') then
        cfdisr = v_sdcont_paracr(5)
    else if (question.eq.'ALARME_JEU') then
        cfdisr = mminfr(sdcont_defi, 'ALARME_JEU')
    else if (question.eq.'PROJ_NEWT_RESI') then
        cfdisr = 1d-4
    else
        write(6,*) 'QUESTION: ',question
        ASSERT(.false.)
    endif
!
end function
