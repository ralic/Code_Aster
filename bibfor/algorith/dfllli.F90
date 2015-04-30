subroutine dfllli(listr8_sdaster, dtmin, nb_inst)
!
implicit none
!
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
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
    character(len=19), intent(in) :: listr8_sdaster
    real(kind=8), intent(out) :: dtmin
    integer, intent(out) :: nb_inst
!
! --------------------------------------------------------------------------------------------------
!
! Utility - List of times
!
! Some checks
!
! --------------------------------------------------------------------------------------------------
!
! In  listr8_sdaster   : list of reals (listr8_sdaster)
! Out dtmin            : minimum time between two steps
! Out nb_inst          : number of time steps in list
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_inst
    real(kind=8) :: deltat
    real(kind=8), pointer :: v_vale(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    dtmin = r8maem()
!
! - Access to list of times
!
    call jeveuo(listr8_sdaster//'.VALE', 'L', vr=v_vale)
    call jelira(listr8_sdaster//'.VALE', 'LONMAX', nb_inst)
!
! - At least one step
!
    if (nb_inst .lt. 2) then
        call utmess('F', 'DISCRETISATION_86')
    endif
!
! - Minimum time between two steps
!
    do i_inst = 1, nb_inst-1
        deltat = v_vale(1+i_inst) - v_vale(i_inst)
        dtmin = min(deltat,dtmin)
    end do
!
! - List must increase
!
    if (dtmin .le. r8prem()) then
        call utmess('F', 'DISCRETISATION_87')
    endif
!
end subroutine
