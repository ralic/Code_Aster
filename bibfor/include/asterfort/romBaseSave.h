!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine romBaseSave(ds_empi      , nb_mode, nb_snap, mode_type,&
                           mode_vectr_  ,&
                           mode_vectc_  ,&
                           v_mode_freq_ ,&
                           v_nume_slice_)
        use Rom_Datastructure_type
        type(ROM_DS_Empi), intent(in) :: ds_empi
        integer, intent(in) :: nb_mode
        integer, intent(in) :: nb_snap
        character(len=1), intent(in) :: mode_type
        real(kind=8), optional, intent(in), pointer :: mode_vectr_(:)
        complex(kind=8), optional, intent(in), pointer :: mode_vectc_(:)
        real(kind=8), optional, intent(in), pointer :: v_mode_freq_(:)
        integer, optional, intent(in), pointer      :: v_nume_slice_(:)
    end subroutine romBaseSave
end interface
