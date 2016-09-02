subroutine romBaseTruncation(ds_empi, nume_dof_rid, base_jv, ds_empi_rid)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/infniv.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcreb.h"
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
    type(ROM_DS_Empi), intent(in) :: ds_empi
    character(len=24), intent(in) :: nume_dof_rid
    character(len=1), intent(in) :: base_jv
    type(ROM_DS_Empi), intent(inout) :: ds_empi_rid
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Truncation of empirical modes on RID
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : input datastructure for empiric modes
! In  nume_dof_rid     : name of numbering (NUME_DDL) on RID
! In  base_jv          : JEVEUX base where created
! IO  ds_empi_rid      : output datastructure for empiric modes on RID
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_mode, nb_mode
    integer :: iret, nb_equa_rid
    character(len=8) :: base, base_rid
    character(len=19) :: mode, mode_rid, mode_tmp
    character(len=24) :: field_type = ' '
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_7')
    endif
!
! - Initializations
!
    mode_tmp = '&&CROM_INIT.RED'
!
! - Get parameters
!
    nb_mode    = ds_empi%nb_mode
    field_type = ds_empi%field_type
    base       = ds_empi%base
    base_rid   = ds_empi_rid%base
!
! - Create new datastructure
!
    call rscrsd(base_jv, base_rid, 'MODE_EMPI', nb_mode)
!
! - Copy and truncation of modes
!
    do i_mode = 1, nb_mode
        call rsexch(' ', base, field_type    , i_mode, mode, iret)
        call rsexch(' ', base_rid, field_type, i_mode, mode_rid, iret)
        call detrsd('CHAMP_GD', mode_tmp)
        call vtcreb(mode_tmp, 'V', 'R', nume_ddlz = nume_dof_rid, nb_equa_outz = nb_equa_rid)
        call vtcopy(mode, mode_tmp, 'F', iret)
        call copisd('CHAMP_GD', 'V', mode_tmp, mode_rid)
        call rsnoch(base_rid, field_type, i_mode)
    end do
!
! - Save values
!
    ds_empi_rid%nb_equa = nb_equa_rid
!
end subroutine
