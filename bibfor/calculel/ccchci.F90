subroutine ccchci(questz, type_comp, crit, norm, nb_form, &
                  repi)
!
    implicit none
!
#include "asterfort/assert.h"
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
! person_in_charge: mathieu.courtois at edf.fr
!
    character(len=*), intent(in) :: questz
    character(len=16), intent(in) :: type_comp
    character(len=16), intent(in) :: crit
    character(len=16), intent(in) :: norm
    integer, intent(in) :: nb_form
    integer, intent(out) :: repi
!
! --------------------------------------------------------------------------------------------------
!
! Command CALC_CHAMP
!
! Get info for CHAM_UTIL
!
! --------------------------------------------------------------------------------------------------
!
! In  question       : question
! In  type_comp      : type of computation (CRITERE, NORME or FORMULE)
! In  crit           : type of criterion
! In  norm           : type of norm
! In  nb_form        : number of formulas
! Out repi           : answer
!
! --------------------------------------------------------------------------------------------------
!
    character(len=5) :: question
!
! --------------------------------------------------------------------------------------------------
!
    question = questz
    repi     = 0
!
    if (question .eq. 'NBCMP') then
        if (type_comp .eq. 'CRITERE') then
            if (crit .eq. 'VMIS' .or. crit .eq. 'INVA_2' .or. crit .eq. 'TRACE') then
                repi = 1
            else
                ASSERT(.false.)
            endif
        elseif (type_comp .eq. 'NORME') then
            if (norm .eq. 'L2') then
                repi = 1
            elseif (norm .eq. 'FROBENIUS') then
                repi = 1
            else
                ASSERT(.false.)
            endif
        elseif (type_comp .eq. 'FORMULE') then
            repi = nb_form
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
