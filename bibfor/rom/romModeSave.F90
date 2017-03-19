subroutine romModeSave(base        , i_mode     , model  ,&
                       field_type  , field_refe , nb_equa,&
                       mode_vectr_ ,&
                       mode_vectc_ ,&
                       mode_freq_  ,&
                       nume_slice_ ,&
                       nb_snap_)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
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
    character(len=8), intent(in) :: base
    integer, intent(in) :: i_mode
    character(len=8), intent(in) :: model
    character(len=24), intent(in) :: field_type
    character(len=24), intent(in) :: field_refe
    integer, intent(in) :: nb_equa
    real(kind=8), optional, intent(in) :: mode_vectr_(nb_equa)
    complex(kind=8), optional, intent(in) :: mode_vectc_(nb_equa)
    integer, optional, intent(in)     :: nume_slice_
    real(kind=8), optional, intent(in) :: mode_freq_
    integer, optional, intent(in)     :: nb_snap_
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Save empiric mode
!
! --------------------------------------------------------------------------------------------------
!
! In  base             : name of empiric base
! In  i_mode           : index of empiric modes
! In  model            : name of model
! In  field_type       : type of field (name in results datastructure)
! In  field_refe       : name of a reference field if necessary
! In  nb_equa          : length of empiric mode
! In  mode_vectr       : singular vector for empiric mode (real)
! In  mode_vectc       : singular vector for empiric mode (complex)
! In  nume_slice       : index of slices (for lineic bases)
! In  mode_freq        : singular value for empiric mode
! In  nb_snap          : number of snapshots used to construct empiric base
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, jv_para, nume_slice, nb_snap
    character(len=24) :: field
    real(kind=8) :: mode_freq
    real(kind=8), pointer :: v_field_r(:) => null()
    complex(kind=8), pointer :: v_field_c(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nume_slice = 0
    if (present(nume_slice_)) then
        nume_slice = nume_slice_
    endif
    mode_freq = 0.d0
    if (present(mode_freq_)) then
        mode_freq = mode_freq_
    endif
    nb_snap = 0
    if (present(nb_snap_)) then
        nb_snap = nb_snap_
    endif
!
! - Get current mode
!
    call rsexch(' ', base, field_type, i_mode, field, iret)
    ASSERT(iret .eq. 100 .or. iret.eq.0 .or. iret .eq. 110)
    if (iret .eq. 110) then
        call rsagsd(base, 0)
        call rsexch(' ', base, field_type, i_mode, field, iret)
        ASSERT(iret .eq. 100 .or. iret.eq.0)
    endif
    if (iret .eq. 100) then
        call copisd('CHAMP_GD', 'G', field_refe, field)
    endif
!
! - Access to current mode
!
    if (present(mode_vectc_)) then
        call jeveuo(field(1:19)//'.VALE', 'E', vc = v_field_c)
    else
        call jeveuo(field(1:19)//'.VALE', 'E', vr = v_field_r)
    endif
!
! - Save mode
!
    if (present(mode_vectc_)) then
        v_field_c(:) = mode_vectc_(1:nb_equa)
    else
        v_field_r(:) = mode_vectr_(1:nb_equa)
    endif
    call rsnoch(base, field_type, i_mode)
!
! - Save parameters
!
    call rsadpa(base, 'E', 1, 'NUME_MODE', i_mode, 0, sjv=jv_para)
    zi(jv_para)   = i_mode
    call rsadpa(base, 'E', 1, 'FREQ', i_mode, 0, sjv=jv_para)
    zr(jv_para)   = mode_freq
    call rsadpa(base, 'E', 1, 'MODELE', i_mode, 0, sjv=jv_para)
    zk8(jv_para)  = model
    call rsadpa(base, 'E', 1, 'NOM_CHAM', i_mode, 0, sjv=jv_para)
    zk24(jv_para) = field_type
    call rsadpa(base, 'E', 1, 'NUME_PLAN', i_mode, 0, sjv=jv_para)
    zi(jv_para)   = nume_slice
    call rsadpa(base, 'E', 1, 'NB_SNAP', i_mode, 0, sjv=jv_para)
    zi(jv_para)   = nb_snap
!
end subroutine
