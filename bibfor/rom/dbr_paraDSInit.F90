subroutine dbr_paraDSInit(ds_empi, ds_para_pod, ds_para_rb, ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/infniv.h"
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
    type(ROM_DS_Empi), intent(in) :: ds_empi
    type(ROM_DS_ParaDBR_POD), intent(in) :: ds_para_pod
    type(ROM_DS_ParaDBR_RB), intent(in) :: ds_para_rb
    type(ROM_DS_ParaDBR), intent(out) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Initialization of datastructures for parameters
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
! In  ds_para_pod      : datastructure for POD parameters
! In  ds_para_rb       : datastructure for RB parameters
! Out ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_14')
    endif
!
! - General initialisations of datastructure
!
    ds_para%operation    = ' '
    ds_para%result_out   = ' '
    ds_para%nb_mode_maxi = 0
    ds_para%para_pod     = ds_para_pod
    ds_para%para_rb      = ds_para_rb
    ds_para%ds_empi      = ds_empi
    ds_para%l_reuse      = .false._1
!
end subroutine
