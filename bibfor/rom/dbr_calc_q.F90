subroutine dbr_calc_q(ds_empi, ds_snap, q)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/as_allocate.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/rsexch.h"
#include "asterfort/jelibe.h"
#include "asterfort/jeveuo.h"
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
    type(ROM_DS_Snap), intent(in) :: ds_snap
    real(kind=8), pointer, intent(out) :: q(:)
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Compute
!
! Create snapshots matrix [Q]
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
! In  ds_snap          : datastructure for snapshot selection
! Out q                : pointer to [Q] matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_snap, i_equa, i_node, i_cmp, i_pl, i_2d
    integer :: nb_snap, nb_slice, nb_cmp, n_2d, nb_equa
    integer :: nume_inst, iret
    character(len=8)  :: base_type, result
    character(len=24) :: field_type, list_snap
    integer, pointer :: v_list_snap(:) => null()
    type(ROM_DS_LineicNumb) :: ds_line
    real(kind=8), pointer :: v_field_resu(:) => null()
    character(len=24) :: field_resu = '&&ROM_FIELDRESU'
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_1')
    endif
!
! - Get parameters
!
    result       = ds_snap%result
    nb_snap      = ds_snap%nb_snap
    list_snap    = ds_snap%list_snap
    ds_line      = ds_empi%ds_lineic
    nb_equa      = ds_empi%nb_equa
    nb_cmp       = ds_empi%nb_cmp
    base_type    = ds_empi%base_type
    field_type   = ds_empi%field_type
    ASSERT(nb_snap .gt. 0)
    ASSERT(nb_equa .gt. 0)
!
! - Get list of snapshots
!
    call jeveuo(list_snap, 'L', vi = v_list_snap)
!
! - Prepare snapshots matrix
!
    AS_ALLOCATE(vr = q, size = nb_equa * nb_snap)
!
! - Save the [Q] matrix depend on which type of reduced base
!    
    if (base_type .eq. 'LINEIQUE') then
        nb_slice  =  ds_line%nb_slice
        do i_snap = 1, nb_snap
            nume_inst = v_list_snap(i_snap)
            call rsexch(' '  , result, field_type, nume_inst, field_resu, iret)
            ASSERT(iret .eq. 0)
            call jeveuo(field_resu(1:19)//'.VALE', 'L', vr = v_field_resu)
            do i_equa = 1, nb_equa
                i_node = (i_equa - 1)/nb_cmp + 1
                i_cmp  = i_equa - (i_node - 1)*nb_cmp
                i_pl   = ds_line%v_nume_pl(i_node)
                n_2d   = ds_line%v_nume_sf(i_node)
                i_2d   = (n_2d - 1)*nb_cmp + i_cmp
                q(i_2d + nb_equa*(i_pl - 1)/nb_slice + nb_equa*(i_snap - 1))= v_field_resu(i_equa)
            enddo
            call jelibe(field_resu(1:19)//'.VALE')
        enddo
    else
        do i_snap = 1, nb_snap
            nume_inst = v_list_snap(i_snap)
            call rsexch(' '  , result, field_type, nume_inst, field_resu, iret)
            ASSERT(iret .eq. 0)
            call jeveuo(field_resu(1:19)//'.VALE', 'L', vr = v_field_resu)
            do i_equa = 1, nb_equa
                q(i_equa + nb_equa*(i_snap - 1)) = v_field_resu(i_equa)
            end do
            call jelibe(field_resu(1:19)//'.VALE')
        enddo
    endif
!
end subroutine
