subroutine dbr_para_info(ds_para)
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
#include "asterfort/romBaseInfo.h"
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
! Informations about DEFI_BASE_REDUITE parameters
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=16) :: operation = ' '
    character(len=24) :: field_type = ' ', surf_num = ' '
    character(len=8)  :: result_out = ' ', result_in = ' '
    character(len=8)  :: axe_line = ' ', base_type = ' '
    integer :: nb_mode_maxi
    real(kind=8) :: tole_svd, tole_incr
    aster_logical :: l_reuse
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
! - Get parameters in datastructure - General for DBR
!
    operation    = ds_para%operation
    result_out   = ds_para%result_out
    nb_mode_maxi = ds_para%nb_mode_maxi
    l_reuse      = ds_para%l_reuse
!
! - Print - General for DBR
!
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_24')
        call utmess('I', 'ROM5_16', sk = operation)
        call utmess('I', 'ROM5_17', si = nb_mode_maxi)
        if (l_reuse) then
            call utmess('I', 'ROM7_15', sk = result_out)
        endif
    endif
!
! - Get parameters in datastructure - General for POD
!
    tole_svd     = ds_para%para_pod%tole_svd
    tole_incr    = ds_para%para_pod%tole_incr
    result_in    = ds_para%para_pod%result_in
    field_type   = ds_para%para_pod%field_type
    base_type    = ds_para%para_pod%base_type
    axe_line     = ds_para%para_pod%axe_line
    surf_num     = ds_para%para_pod%surf_num
!
! - Print - General for POD
!
    if (niv .ge. 2) then
        call utmess('I', 'ROM7_3' , sr = tole_svd)
        if (operation .eq. 'POD_INCR') then
            call utmess('I', 'ROM7_13' , sr = tole_incr)
        endif
        call utmess('I', 'ROM7_1' , sk = result_in)
        call utmess('I', 'ROM7_2' , sk = field_type)
        if (base_type .eq. '3D') then
            call utmess('I', 'ROM7_4')
        elseif (base_type .eq. 'LINEIQUE') then
            call utmess('I', 'ROM7_5', nk = 2, valk = [axe_line, surf_num])
        endif
    endif
!
! - Print about snapshots selection
!
    call romSnapInfo(ds_para%para_pod%ds_snap)
!
! - Print about empiric base
!
    call romBaseInfo(ds_para%ds_empi)
!
end subroutine
