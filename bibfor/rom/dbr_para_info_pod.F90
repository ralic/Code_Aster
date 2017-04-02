subroutine dbr_para_info_pod(ds_para_pod)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/romSnapInfo.h"
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
    type(ROM_DS_ParaDBR_POD), intent(in) :: ds_para_pod
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Informations about DEFI_BASE_REDUITE parameters
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para_pod      : datastructure for parameters (POD)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=16) :: operation = ' '
    character(len=24) :: field_name = ' ', surf_num = ' '
    character(len=8)  :: result_in = ' ', axe_line = ' '
    real(kind=8) :: tole_svd, tole_incr
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM7_20')
    endif
!
! - Get parameters in datastructure - General for POD
!
    tole_svd     = ds_para_pod%tole_svd
    tole_incr    = ds_para_pod%tole_incr
    result_in    = ds_para_pod%result_in
    field_name   = ds_para_pod%field_name
    axe_line     = ds_para_pod%axe_line
    surf_num     = ds_para_pod%surf_num
!
! - Print - General for POD
!
    if (niv .ge. 2) then
        call utmess('I', 'ROM7_3' , sr = tole_svd)
        if (operation .eq. 'POD_INCR') then
            call utmess('I', 'ROM7_13' , sr = tole_incr)
        endif
        call utmess('I', 'ROM7_1' , sk = result_in)
        call utmess('I', 'ROM7_2' , sk = field_name)
    endif
!
! - Print about snapshots selection
!
    if (niv .ge. 2) then
        call romSnapInfo(ds_para_pod%ds_snap)
    endif
!
end subroutine
