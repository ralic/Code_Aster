subroutine dbr_chck(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/dbr_chck_pod.h"
#include "asterfort/dbr_chck_rb.h"
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
    type(ROM_DS_ParaDBR), intent(in) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Some checks
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM7_7')
    endif
!
    if (ds_para%operation(1:3) .eq. 'POD') then
        call dbr_chck_pod(ds_para%operation, ds_para%para_pod, ds_para%ds_empi, ds_para%l_reuse)
    elseif (ds_para%operation .eq. 'GLOUTON') then
        call dbr_chck_rb(ds_para%operation, ds_para%para_rb, ds_para%l_reuse)
    else
        ASSERT(.false.)
    endif
!
end subroutine
