subroutine InitAlgoPara(ds_algopara)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
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
    type(NL_DS_AlgoPara), intent(inout) :: ds_algopara
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algorithm parameters management
!
! Initializations for algorithm parameters management
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_algopara      : datastructure for algorithm parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    real(kind=8) :: reli_rho_mini, reli_rho_maxi, reli_rho_excl, swap
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... Initializations for algorithm parameters management'
    endif
!
! - Update bounds for line search
!
    if (ds_algopara%l_line_search) then
        reli_rho_mini = ds_algopara%line_search%rho_mini
        reli_rho_maxi = ds_algopara%line_search%rho_maxi
        reli_rho_excl = ds_algopara%line_search%rho_excl
        if (reli_rho_mini .ge. -reli_rho_excl .and. reli_rho_mini .le. reli_rho_excl) then
            call utmess('A', 'MECANONLINE5_46')
            reli_rho_mini = +reli_rho_excl
        endif
        if (reli_rho_maxi .ge. -reli_rho_excl .and. reli_rho_maxi .le. reli_rho_excl) then
            call utmess('A', 'MECANONLINE5_47')
            reli_rho_maxi = -reli_rho_excl
        endif
        if (reli_rho_maxi .lt. reli_rho_mini) then
            call utmess('A', 'MECANONLINE5_44')
            swap = reli_rho_maxi
            reli_rho_maxi = reli_rho_mini
            reli_rho_mini = swap
        endif
        if (abs(reli_rho_maxi-reli_rho_mini) .le. r8prem()) then
            call utmess('F', 'MECANONLINE5_43')
        endif
        ds_algopara%line_search%rho_mini  = reli_rho_mini
        ds_algopara%line_search%rho_maxi  = reli_rho_maxi
    endif
!
end subroutine
