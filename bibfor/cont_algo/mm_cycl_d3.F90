subroutine mm_cycl_d3(ds_contact    , i_cont_poin   ,&
                      indi_frot_prev, dist_frot_prev,&
                      indi_cont_eval, indi_frot_eval,&
                      dist_frot_curr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterc/r8prem.h"
#include "asterc/r8rddg.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/normev.h"
#include "asterfort/mm_cycl_erase.h"
#include "asterfort/mm_cycl_init.h"
#include "blas/ddot.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: i_cont_poin
    integer, intent(in) :: indi_frot_prev
    real(kind=8), intent(in) :: dist_frot_prev(3)
    integer, intent(in) :: indi_cont_eval
    integer, intent(in) :: indi_frot_eval
    real(kind=8), intent(in) :: dist_frot_curr(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Detection: sliding forward/backward
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  i_cont_poin      : contact point index
! In  indi_frot_prev   : previous friction indicator
! In  dist_frot_prev   : previous friction distance
! In  indi_cont_eval   : evaluation of new contact status
! In  indi_frot_eval   : evaluation of new friction status
! In  dist_frot_curr   : current friction distance
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_cyclis
    integer, pointer :: p_sdcont_cyclis(:) => null()
    character(len=24) :: sdcont_cycnbr
    integer, pointer :: p_sdcont_cycnbr(:) => null()
    character(len=24) :: sdcont_cyceta
    integer, pointer :: p_sdcont_cyceta(:) => null()
    real(kind=8) :: module_prev, module_curr
    real(kind=8) :: angle, prosca, val, tole_angl
    integer :: cycl_type, cycl_ecod, cycl_long, cycl_stat
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    cycl_type = 3
    cycl_stat = 0
    cycl_ecod = 0
    cycl_long = 0
    tole_angl = 2.d0
!
! - Access to cycling objects
!
    sdcont_cyclis = ds_contact%sdcont_solv(1:14)//'.CYCLIS'
    sdcont_cycnbr = ds_contact%sdcont_solv(1:14)//'.CYCNBR'
    sdcont_cyceta = ds_contact%sdcont_solv(1:14)//'.CYCETA'
    call jeveuo(sdcont_cyclis, 'E', vi = p_sdcont_cyclis)
    call jeveuo(sdcont_cycnbr, 'E', vi = p_sdcont_cycnbr)
    call jeveuo(sdcont_cyceta, 'E', vi = p_sdcont_cyceta)
!
! - Cycling break if: no contact, sticking or previous state was sticking
!
    if ((indi_cont_eval .eq. 0).or.&
        (indi_frot_eval .eq. 1).or.&
        (indi_frot_prev .eq. 1)) then
        call mm_cycl_erase(ds_contact, cycl_type, i_cont_poin)
        goto 99
    endif
!
! - Cycling detection
!
    prosca = ddot(3,dist_frot_prev,1,dist_frot_curr,1)
    module_curr = sqrt(dist_frot_curr(1)*dist_frot_curr(1)+&
                       dist_frot_curr(2)*dist_frot_curr(2)+&
                       dist_frot_curr(3)*dist_frot_curr(3))
    module_prev = sqrt(dist_frot_prev(1)*dist_frot_prev(1)+&
                       dist_frot_prev(2)*dist_frot_prev(2)+&
                       dist_frot_prev(3)*dist_frot_prev(3))
    angle  = 0.d0
    if ((module_prev*module_curr) .gt. r8prem()) then
        val = prosca/(module_prev*module_curr)
        if (val .gt. 1.d0) then
            val = 1.d0
        endif
        if (val .lt. -1.d0) then
            val = -1.d0
        endif
        angle = acos(val)
        angle = angle*r8rddg()
    endif
!
! - Detection
!
    cycl_stat = 0
    if (abs(angle-180.d0) .le. tole_angl) then
        cycl_stat = 10
    endif
!
! - Cycling save
!
    p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_type) = cycl_stat
    p_sdcont_cyclis(4*(i_cont_poin-1)+cycl_type) = cycl_ecod
    p_sdcont_cycnbr(4*(i_cont_poin-1)+cycl_type) = cycl_long
!
99  continue
!
    call jedema()
end subroutine
