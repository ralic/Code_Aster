subroutine romAlgoNLDSCreate(ds_algorom)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infniv.h"
#include "asterfort/romBaseInit.h"
#include "asterfort/romLineicBaseInit.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(ROM_DS_AlgoPara), intent(out) :: ds_algorom
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Solving non-linear problem
!
! Create ROM parameters datastructure
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_algorom       : datastructure for ROM parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    type(ROM_DS_LineicNumb) :: ds_lineicnumb
    type(ROM_DS_Empi) :: ds_empi
    type(ROM_DS_Empi) :: ds_empi_rid
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_36')
    endif
!
! - Initialization of datastructure for lineic base numbering
!
    call romLineicBaseInit(ds_lineicnumb)
!
! - Initialization of datastructure for empiric modes
!
    call romBaseInit(ds_lineicnumb, ds_empi)
!
! - Initialization of datastructure for truncated empiric modes
!
    call romBaseInit(ds_lineicnumb, ds_empi_rid)    
!
! - General parameters
!
    ds_algorom%l_rom       = .false._1
    ds_algorom%ds_empi     = ds_empi
    ds_algorom%ds_empi_rid = ds_empi_rid
    ds_algorom%l_hrom      = .false._1
    ds_algorom%grnode_int  = ' '
    ds_algorom%tabl_name   = ' '
    ds_algorom%gamma       = ' '
    ds_algorom%v_equa_int  => null()
!
end subroutine
