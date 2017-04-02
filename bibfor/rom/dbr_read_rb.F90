subroutine dbr_read_rb(ds_para_rb)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cresol.h"
#include "asterfort/infniv.h"
#include "asterfort/romMultiParaRead.h"
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
    type(ROM_DS_ParaDBR_RB), intent(inout) :: ds_para_rb
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Read parameters - For RB methods
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_para_rb       : datastructure for parameters (RB)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_27')
    endif
!
! - Read data for multiparametric problems
!
    call romMultiParaRead(ds_para_rb%ds_multipara)
!
! - Get solver parameters
!
    call cresol(ds_para_rb%solver)
!
end subroutine
