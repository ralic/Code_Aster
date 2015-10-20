subroutine ndxcfl(mate, cara_elem, sddyna, sddisc)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/ndynlo.h"
#include "asterfort/pascom.h"
#include "asterfort/pascou.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: mate
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Datastructures
!
! CFL condition for explicit dynamic
!
! --------------------------------------------------------------------------------------------------
!
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  sddyna           : name of dynamic parameters
! In  sddisc           : datastructure for time discretization
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_proj_modal
    character(len=8) :: mode_meca
!
! --------------------------------------------------------------------------------------------------
!
!
! - Active functionnalities
!
    l_proj_modal = ndynlo(sddyna,'PROJ_MODAL')
!
! - Compute CFL condition
!
    if (l_proj_modal) then
        call getvid('PROJ_MODAL', 'MODE_MECA', iocc=1, scal=mode_meca)
        call pascom(mode_meca, sddyna, sddisc)
    else
        call pascou(mate, cara_elem, sddyna, sddisc)
    endif
!
end subroutine
