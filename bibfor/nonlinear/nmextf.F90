subroutine nmextf(keyw_fact, i_keyw_fact, type_extr_cmp)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
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
    character(len=16), intent(in) :: keyw_fact
    integer, intent(in) :: i_keyw_fact
    character(len=8), intent(out) :: type_extr_cmp
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Extraction (OBSERVATION/SUIVI_DDL) utilities 
!
! Get type of extraction for components
!
! --------------------------------------------------------------------------------------------------
!
! In  keyw_fact        : factor keyword to read extraction parameters
! In  i_keyw_fact      : index of keyword to read extraction parameters
! Out type_extr_cmp    : type of extraction for components
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: answer
!
! --------------------------------------------------------------------------------------------------
!
    type_extr_cmp = ' '
    call getvtx(keyw_fact, 'EVAL_CMP', iocc=i_keyw_fact, scal=answer)
    if (answer .eq. 'VALE') then
        type_extr_cmp = ' '
    else if (answer.eq.'FORMULE') then
        call getvid(keyw_fact, 'FORMULE', iocc=i_keyw_fact, scal=type_extr_cmp)
    else
        ASSERT(.false.)
    endif
!
end subroutine
