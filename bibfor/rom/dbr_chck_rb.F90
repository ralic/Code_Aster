subroutine dbr_chck_rb(operation, ds_para_rb, l_reuse)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/romMultiParaChck.h"
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
    character(len=16), intent(in) :: operation
    type(ROM_DS_ParaDBR_RB), intent(in) :: ds_para_rb
    aster_logical, intent(in) :: l_reuse
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Some checks - For RB methods
!
! --------------------------------------------------------------------------------------------------
!
! In  operation        : type of method
! In  ds_para_rb       : datastructure for parameters (RB)
! In  l_reuse          : .true. if reuse
!
! --------------------------------------------------------------------------------------------------
!
    if (l_reuse) then
        call utmess('F','ROM2_13', sk = operation)
    endif

    call romMultiParaChck(ds_para_rb%ds_multipara)
!
end subroutine
