subroutine romAlgoNLRead(ds_algorom)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterfort/romBaseRead.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_AlgoPara), intent(inout) :: ds_algorom
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Solving non-linear problem
!
! Read parameters for algorithm management
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_algorom       : datastructure for ROM parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=8) :: ds_empi_name
    character(len=16) :: keywf, answer
    character(len=24) :: grnode_int
    aster_logical :: l_hrom
    type(ROM_DS_Empi) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_41')
    endif
!
! - Initializations
!
    keywf      = 'MODELE_REDUIT'
    l_hrom     = .false._1
    grnode_int = ' '
!
! - Read
!
    call getvid(keywf, 'BASE_PRIMAL'   , iocc=1, scal = ds_empi_name)
    call getvtx(keywf, 'DOMAINE_REDUIT', iocc=1, scal = answer)
    l_hrom = answer .eq. 'OUI'
    if (l_hrom) then
        call getvtx(keywf,'GROUP_NO_INTERF', iocc=1, scal = grnode_int)
    endif
    call romBaseRead(ds_empi_name, ds_empi)
    ds_algorom%l_rom      = .true.
    ds_algorom%ds_empi    = ds_empi
    ds_algorom%l_hrom     = l_hrom
    ds_algorom%grnode_int = grnode_int
!
end subroutine
