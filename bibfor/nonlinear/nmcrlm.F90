subroutine nmcrlm(listr8_sdaster, sddisc, list_inst_work)
!
implicit none
!
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/dfllvd.h"
#include "asterfort/jedup1.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
    character(len=19), intent(in) :: list_inst_work
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: listr8_sdaster
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Time discretization datastructure
!
! Create list of times and information vector from LISTR8_SDASTER
!
! --------------------------------------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! In  listr8_sdaster   : list of reals (listr8_sdaster)
! In  list_inst_work   : name of working list of time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: llinr, nb_inst, i_inst
    real(kind=8) :: dtmin, deltat
    character(len=8) :: list_method
    character(len=24) :: sddisc_linf
    real(kind=8), pointer :: v_sddisc_linf(:) => null()
    real(kind=8), pointer :: v_vale(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    llinr       = dfllvd('LLINR')
    sddisc_linf = sddisc(1:19)//'.LINF'
    dtmin       = r8maem()
!
! - Access to list of times
!
    call jeveuo(listr8_sdaster//'.VALE', 'L', vr=v_vale)
    call jelira(listr8_sdaster//'.VALE', 'LONMAX', nb_inst)
!
! - At least one step
!
    if (nb_inst .lt. 2) then
        call utmess('F', 'DISCRETISATION_95')
    endif
!
! - Minimum time between two steps
!
    do i_inst = 1, nb_inst-1
        deltat = v_vale(1+i_inst) - v_vale(i_inst)
        dtmin  = min(deltat,dtmin)
    end do
!
! - List must increase
!
    if (dtmin .le. r8prem()) then
        call utmess('F', 'DISCRETISATION_87')
    endif
!
! - Copy listr8sdaster in list of times
!
    call jedup1(listr8_sdaster(1:19)//'.VALE', 'V', list_inst_work)
!
! - Create information vector
!
    call wkvect(sddisc_linf, 'V V R', llinr, vr = v_sddisc_linf)
!
! - Update information vector
!
    list_method = 'MANUEL'
    call utdidt('E', sddisc, 'LIST', 'METHODE',&
                valk_ = list_method)
    call utdidt('E', sddisc, 'LIST', 'DTMIN',&
                valr_ = dtmin)
    call utdidt('E', sddisc, 'LIST', 'NBINST',&
                vali_ = nb_inst)
!
end subroutine
