subroutine romBaseSave(ds_empi      , nb_mode, nb_snap, mode_type,&
                       mode_vectr_  ,&
                       mode_vectc_  ,&
                       v_mode_freq_ ,&
                       v_nume_slice_)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/romModeSave.h"
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
    integer, intent(in) :: nb_mode
    integer, intent(in) :: nb_snap
    character(len=1), intent(in) :: mode_type
    real(kind=8), optional, intent(in), pointer :: mode_vectr_(:)
    complex(kind=8), optional, intent(in), pointer :: mode_vectc_(:)
    real(kind=8), optional, intent(in), pointer :: v_mode_freq_(:)
    integer, optional, intent(in), pointer      :: v_nume_slice_(:)
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Save empiric base
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
! In  nb_mode          : number of empiric modes
! In  nb_snap          : number of snapshots used to construct empiric base
! In  mode_type        : type of mode (real or complex, 'R' ou 'C')
! In  mode_freq        : singular values
! In  mode_vect        : singular vectors
! In  v_nume_slice     : index of slices (for lineic bases)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_mode
    integer :: nb_equa = 0, nume_slice
    real(kind=8) :: mode_freq
    character(len=8)  :: base = ' ', model = ' '
    character(len=24) :: field_refe = ' ', field_type = ' '
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_2', si = nb_mode, sk = ds_empi%base)
    endif
!
! - Initializations
!
    nume_slice = 0
    mode_freq  = 0.d0
!
! - Get parameters
!
    nb_equa      = ds_empi%nb_equa
    field_type   = ds_empi%field_type
    field_refe   = ds_empi%field_refe
    base         = ds_empi%base
    model        = ds_empi%model
!
! - Save modes
!
    do i_mode = 1, nb_mode
        if (present(v_nume_slice_)) then
            nume_slice = v_nume_slice_(i_mode)
        endif
        if (present(v_mode_freq_)) then
            mode_freq  = v_mode_freq_(i_mode)
        endif
        if (mode_type.eq.'R') then
            call romModeSave(base                , i_mode    , model  ,&
                             field_type          , field_refe, nb_equa,&
                             mode_vectr_   = mode_vectr_(nb_equa*(i_mode-1)+1:),&
                             mode_freq_    = mode_freq   ,&
                             nume_slice_   = nume_slice  ,&
                             nb_snap_      = nb_snap)
        elseif (mode_type.eq.'C') then
            call romModeSave(base                , i_mode    , model  ,&
                             field_type          , field_refe, nb_equa,&
                             mode_vectc_ = mode_vectc_(nb_equa*(i_mode-1)+1:),&
                             mode_freq_    = mode_freq   ,&
                             nume_slice_   = nume_slice  ,&
                             nb_snap_      = nb_snap)
        else
            ASSERT(.false.)
        endif
    end do
!
end subroutine
