subroutine nmdocv(keywordfact, iocc, algo_inte, keyword, value)
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/utmess.h"
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
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=16), intent(in) :: algo_inte
    character(len=14), intent(in) :: keyword
    real(kind=8), intent(out) :: value
!
! --------------------------------------------------------------------------------------------------
!
! <CARTE> CARCRI
!
! Get and check special keywords for convergence criterion
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact     : factor keyword to read (COMPORTEMENT)
! In  iocc            : factor keyword index in COMPORTEMENT
! In  algo_inte       : integration algorithm
! In  keyword         : keyword
! Out value           : real value of keyword
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iarg, iret, vali, iter_cplan
    real(kind=8) :: inte_rela_anal
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(keyword(1:9).eq.'RESI_INTE'.or.keyword.eq.'ITER_INTE_MAXI')
!
! - Get Values
!
    if (keyword .eq. 'RESI_INTE_RELA') then
        call getvr8(keywordfact, keyword, iocc=iocc, scal=value, nbret=iret,&
                    isdefault=iarg)
    else if (keyword .eq. 'RESI_INTE_MAXI') then
        call getvr8(keywordfact, keyword, iocc=iocc, scal=value, nbret=iret,&
                    isdefault=iarg)
    else if (keyword.eq.'ITER_INTE_MAXI') then
        call getvis(keywordfact, keyword, iocc=iocc, scal=vali, nbret=iret,&
                    isdefault=iarg)
        value      = vali
        iter_cplan = vali
    endif
!
    ASSERT(iret.ne.0)
!
! - Number of iterations for plane stress
!
    inte_rela_anal = -iter_cplan
!
! - Checking
!
    if (iarg .eq. 0) then
        if (algo_inte .eq. 'ANALYTIQUE') then
            call utmess('A', 'COMPOR4_70', sk=keyword)
            value = inte_rela_anal
        endif
    else
        if (algo_inte .eq. 'ANALYTIQUE') then
            if (keyword .eq. 'ITER_INTE_MAXI') then
                value = inte_rela_anal
            endif
        endif
    endif
!
    if (keyword .eq. 'RESI_INTE_RELA') then
        if (value .gt. 1.0001d-6) then
            call utmess('A', 'COMPOR4_62')
        endif
    endif
!
end subroutine
