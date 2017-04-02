subroutine dbr_clean_rb(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/romBaseClean.h"
#include "asterfort/assert.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/romMultiParaClean.h"
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
! DEFI_BASE_REDUITE
!
! Clean datastructures for GREEDY
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_para           : datastructure for parameters 
!
! --------------------------------------------------------------------------------------------------
!
    call romBaseClean(ds_para%ds_empi)
    call romMultiParaClean(ds_para%para_rb%multipara)
    AS_DEALLOCATE(vr = ds_para%para_rb%resi_norm)
!
end subroutine
