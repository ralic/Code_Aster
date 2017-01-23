subroutine dbr_read_pod(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/romSnapRead.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
    type(ROM_DS_ParaDBR), intent(inout) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Read parameters - For POD methods
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_para_pod      : datastructure for parameters (POD)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc, ifm, niv
    real(kind=8) :: tole_svd = 0.d0
    integer :: nb_mode_maxi = 0
    character(len=24) :: field_name = ' '
    character(len=8)  :: axe_line = ' ', surf_num = ' ', base_type = ' ', result_in = ' '
    type(ROM_DS_Snap) :: ds_snap
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_18')
    endif
!
! - Get parameters - Results to process
!
    call getvid(' ', 'RESULTAT', scal = result_in)
    call getvtx(' ', 'NOM_CHAM', scal = field_name, nbret = nocc)
    ASSERT(nocc .eq. 1)
!
! - Get parameters - Base type to numeration
!
    call getvtx(' ', 'TYPE_BASE', scal = base_type)
    if (base_type .eq. 'LINEIQUE') then
        call getvtx(' ', 'AXE', scal = axe_line, nbret = nocc)
        ASSERT(nocc .eq. 1)
        call getvtx(' ', 'SECTION', scal = surf_num, nbret = nocc)
        ASSERT(nocc .eq.1 )
    endif
!
! - Get parameters - For SVD selection
!
    call getvr8(' ', 'TOLE_SVD', scal = tole_svd)
    call getvis(' ', 'NB_MODE' , scal = nb_mode_maxi, nbret = nocc)
    if (nocc .eq. 0) then
        nb_mode_maxi = 0
    endif
!
! - Read parameters for snapshot selection
!
    ds_snap = ds_para%ds_snap
    call romSnapRead(result_in, ds_snap)
!
! - Save parameters in datastructure
!
    ds_para%nb_mode_maxi = nb_mode_maxi
    ds_para%ds_snap      = ds_snap
    ds_para%tole_svd     = tole_svd
    ds_para%field_name   = field_name
    ds_para%result_in    = result_in
    ds_para%operation    = 'POD'
    ds_para%ds_empi%base_type    = base_type
    ds_para%ds_empi%axe_line     = axe_line
    ds_para%ds_empi%surf_num     = surf_num
!
end subroutine
