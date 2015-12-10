subroutine mm_cycl_d1(ds_contact    , i_cont_poin   ,&
                      coef_cont     , pres_cont_prev, dist_cont_prev,&
                      indi_cont_eval, dist_cont     , pres_cont)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/iscode.h"
#include "asterfort/iscycl.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mm_cycl_shift.h"
#include "asterfort/mm_cycl_d1_ss.h"
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
    real(kind=8), intent(in) :: coef_cont
    real(kind=8), intent(in) :: pres_cont_prev
    real(kind=8), intent(in) :: dist_cont_prev
    integer, intent(in) :: indi_cont_eval
    real(kind=8), intent(in) :: dist_cont
    real(kind=8), intent(in) :: pres_cont
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Detection: contact/no-contact
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  i_cont_poin      : contact point index
! In  coef_cont        : augmented ratio for contact
! In  pres_cont_prev   : previous pressure contact in cycle
! In  dist_cont_prev   : previous pressure distance in cycle
! In  indi_cont_eval   : evaluation of new contact status
! In  dist_cont        : contact gap
! In  pres_cont        : contact pressure
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_cyclis
    integer, pointer :: p_sdcont_cyclis(:) => null()
    character(len=24) :: sdcont_cycnbr
    integer, pointer :: p_sdcont_cycnbr(:) => null()
    character(len=24) :: sdcont_cyceta
    integer, pointer :: p_sdcont_cyceta(:) => null()
    integer :: statut(30)
    integer :: cycl_type, cycl_long_acti
    integer :: cycl_ecod(1), cycl_long, cycl_sub_type, cycl_stat
    aster_logical :: detect
    real(kind=8) :: laug_cont_prev, laug_cont_curr
    real(kind=8) :: pres_near
    integer :: zone_cont_prev, zone_cont_curr
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    cycl_long_acti = 3
    cycl_type = 1
    pres_near = 1.d+2
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
! - Previous augmented lagrangian
!
    laug_cont_prev = pres_cont_prev - coef_cont * dist_cont_prev
!
! - Current augmented lagrangian
!
    laug_cont_curr = pres_cont - coef_cont * dist_cont
!
! - Cycle state
!
    cycl_ecod(1) = p_sdcont_cyclis(4*(i_cont_poin-1)+cycl_type)
    cycl_long = p_sdcont_cycnbr(4*(i_cont_poin-1)+cycl_type)
    call isdeco(cycl_ecod(1), statut, 30)
!
! - New iteration in cycle
!
    cycl_long = cycl_long + 1
    statut(cycl_long) = indi_cont_eval
    call iscode(statut, cycl_ecod(1), 30)
!
! - Cycling detection
!
    cycl_stat = 0
    if (cycl_long .eq. cycl_long_acti) then
        detect = iscycl(cycl_ecod(1), cycl_long_acti)
        if (detect) then
            cycl_stat = 10
            call mm_cycl_d1_ss(pres_near, laug_cont_prev, laug_cont_curr, zone_cont_prev,&
                               zone_cont_curr, cycl_sub_type)
            cycl_stat = cycl_stat + cycl_sub_type
        endif
    endif
!
! - End of cycling detection zone: shifting
!
    if (cycl_long .eq. cycl_long_acti) call mm_cycl_shift(cycl_long_acti, cycl_ecod(1),&
                                                          cycl_long)
!
! - Cycling save
!
    p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_type) = cycl_stat
    p_sdcont_cyclis(4*(i_cont_poin-1)+cycl_type) = cycl_ecod(1)
    p_sdcont_cycnbr(4*(i_cont_poin-1)+cycl_type) = cycl_long
!
    call jedema()
end subroutine
