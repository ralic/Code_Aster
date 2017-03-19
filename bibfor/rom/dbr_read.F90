subroutine dbr_read(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterc/gcucon.h"
#include "asterfort/assert.h"
#include "asterfort/dbr_read_pod.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_ParaDBR), intent(inout) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Read parameters
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=16) :: k16bid, operation = ' '
    character(len=8) :: result_out = ' '
    integer :: ireuse, nb_mode_maxi, nocc
    aster_logical :: l_reuse
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_10')
    endif
!
! - Output datastructure
!
    call getres(result_out, k16bid, k16bid)
!
! - Is REUSE?
!
    call gcucon(result_out, 'MODE_EMPI', ireuse)
    l_reuse = ireuse .ne. 0
!
! - Maximum number of modes
!
    call getvis(' ', 'NB_MODE' , scal = nb_mode_maxi, nbret = nocc)
    if (nocc .eq. 0) then
        nb_mode_maxi = 0
    endif
!
! - Type of ROM methods
!
    call getvtx(' ', 'OPERATION', scal = operation)
    if (operation(1:3) .eq. 'POD') then
        call dbr_read_pod(operation, ds_para%para_pod)
    elseif (operation .eq. 'RB_GREEDY') then
        ASSERT(.false.)
    else
        ASSERT(.false.)
    endif
!
! - Save parameters in datastructure
!
    ds_para%operation    = operation
    ds_para%result_out   = result_out
    ds_para%nb_mode_maxi = nb_mode_maxi
    ds_para%l_reuse      = l_reuse
!
end subroutine
