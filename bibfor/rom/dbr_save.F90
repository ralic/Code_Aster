subroutine dbr_save(ds_empi, nb_mode, s, v, v_nume_slice)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/copisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelibe.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
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
    integer, intent(in) :: nb_mode
    real(kind=8), intent(in), pointer :: s(:)
    real(kind=8), intent(in), pointer :: v(:)
    integer, intent(in), pointer      :: v_nume_slice(:)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Save empiric modes
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
! In  nb_mode          : number of empiric modes
! In  s                : singular values
! In  v                : singular vectors
! In  v_nume_slice     : index of slices (for lineic bases)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: jv_para, iret
    integer :: nb_equa
    integer :: i_mode, i_equa
    character(len=8) :: base, model
    character(len=24) :: field_type, field_save, field_refe
    real(kind=8), pointer :: v_field_save(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_2', si = nb_mode, sk = ds_empi%base)
    endif
!
! - Get parameters
!
    nb_equa      = ds_empi%nb_equa
    field_type   = ds_empi%field_type
    field_refe   = ds_empi%field_refe
    base         = ds_empi%base
    model        = ds_empi%model
!
! - Create output datastructure
!
    call rscrsd('G', base, 'MODE_EMPI', nb_mode)
!
! - Save modes
!
    do i_mode = 1, nb_mode
        call rsexch(' ', base, field_type, i_mode, field_save, iret)
        ASSERT(iret.eq.100 .or. iret.eq.0)
        if (iret .eq. 100) then
            call copisd('CHAMP_GD', 'G', field_refe, field_save)
        endif
        call jeveuo(field_save(1:19)//'.VALE', 'E', vr = v_field_save)
        do i_equa = 1, nb_equa
            v_field_save(i_equa) = v(nb_equa*(i_mode-1)+i_equa)
        end do
        call rsnoch(base, field_type, i_mode)
        call rsadpa(base, 'E', 1, 'FREQ', i_mode, 0, sjv=jv_para)
        zr(jv_para)  = s(i_mode)
        call rsadpa(base, 'E', 1, 'MODELE', i_mode, 0, sjv=jv_para)
        zk8(jv_para) = model
        call rsadpa(base, 'E', 1, 'NOM_CHAM', i_mode, 0, sjv=jv_para)
        zk24(jv_para) = field_type
        call rsadpa(base, 'E', 1, 'NUME_PLAN', i_mode, 0, sjv=jv_para)
        zi(jv_para) = v_nume_slice(i_mode)
        call rsadpa(base, 'E', 1, 'NUME_MODE', i_mode, 0, sjv=jv_para)
        zi(jv_para) = i_mode
    end do
!
end subroutine
