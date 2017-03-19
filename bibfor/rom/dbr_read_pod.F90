subroutine dbr_read_pod(operation, ds_para_pod)
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
    character(len=16), intent(in) :: operation
    type(ROM_DS_ParaDBR_POD), intent(inout) :: ds_para_pod
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Read parameters - For POD methods
!
! --------------------------------------------------------------------------------------------------
!
! In  operation        : type of POD method
! IO  ds_para_pod      : datastructure for parameters (POD)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc, ifm, niv
    real(kind=8) :: tole_svd = 0.d0, tole_incr = 0.d0
    character(len=16) :: field_type = ' '
    character(len=8)  :: axe_line = ' '
    character(len=8)  :: surf_num = ' '
    character(len=8)  :: base_type = ' '
    character(len=8)  :: result_in = ' '
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
    call getvtx(' ', 'NOM_CHAM', scal = field_type, nbret = nocc)
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
    if (operation .eq. 'POD_INCR') then
        call getvr8(' ', 'TOLE', scal = tole_incr)
    endif
!
! - Read parameters for snapshot selection
!
    ds_snap = ds_para_pod%ds_snap
    call romSnapRead(result_in, ds_snap)
!
! - Save parameters in datastructure
!
    ds_para_pod%result_in    = result_in
    ds_para_pod%field_type   = field_type
    ds_para_pod%base_type    = base_type
    ds_para_pod%axe_line     = axe_line
    ds_para_pod%surf_num     = surf_num
    ds_para_pod%tole_svd     = tole_svd
    ds_para_pod%tole_incr    = tole_incr
    ds_para_pod%ds_snap      = ds_snap
!
end subroutine
