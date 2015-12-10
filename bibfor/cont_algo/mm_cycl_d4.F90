subroutine mm_cycl_d4(ds_contact, i_cont_poin, indi_cont_eval)
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
    integer, intent(in) :: indi_cont_eval
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Detection: old flip/flop
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  i_cont_poin      : contact point index
! In  indi_cont_eval   : evaluation of new contact status
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
    integer :: cycl_ecod(1), cycl_long, cycl_stat
    aster_logical :: detect
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    cycl_type = 4
    cycl_long_acti = 20
    detect = .false.
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
        endif
    endif
!
! - End of cycling detection zone: shifting
!
    if (cycl_long .eq. cycl_long_acti) then
        call mm_cycl_shift(cycl_long_acti, cycl_ecod(1), cycl_long)
    endif
!
! - Cycling save
!
    p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_type) = cycl_stat
    p_sdcont_cyclis(4*(i_cont_poin-1)+cycl_type) = cycl_ecod(1)
    p_sdcont_cycnbr(4*(i_cont_poin-1)+cycl_type) = cycl_long
!
    call jedema()
end subroutine
