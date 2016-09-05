subroutine dbr_calc_save(ds_empi, nb_mode, s, v)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/dbr_save.h"
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
    real(kind=8), intent(in), pointer :: v(:)
    real(kind=8), intent(in), pointer :: s(:)
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
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: base_type
    integer :: i_slice, i_mode, i_node, i_cmp, i_2d, i_equa
    integer :: nb_cmp, nb_equa, nb_slice, n_2d
    real(kind=8), pointer :: v_lin(:) => null()
    real(kind=8), pointer :: s_lin(:) => null()
    integer, pointer :: v_nume_slice(:) => null()
    integer, pointer :: v_nume_pl(:) => null()
    integer, pointer :: v_nume_sf(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_cmp       = ds_empi%nb_cmp
    nb_equa      = ds_empi%nb_equa
    base_type    = ds_empi%base_type
!
    if (base_type .eq. 'LINEIQUE') then
        nb_slice  = ds_empi%ds_lineic%nb_slice
        v_nume_pl = ds_empi%ds_lineic%v_nume_pl
        v_nume_sf = ds_empi%ds_lineic%v_nume_sf 
        AS_ALLOCATE(vr=v_lin, size = nb_equa*nb_mode*nb_slice)
        AS_ALLOCATE(vr=s_lin, size = nb_mode*nb_slice)
        AS_ALLOCATE(vi=v_nume_slice, size = nb_mode*nb_slice)
        
        do i_slice = 1, nb_slice
            do i_mode = 1, nb_mode
                s_lin(i_mode + nb_mode*(i_slice - 1)) = s(i_mode)
                v_nume_slice(i_mode + nb_mode*(i_slice - 1)) = i_slice
            enddo    
        enddo

        do i_equa = 1, nb_equa
            i_node  = (i_equa - 1)/nb_cmp + 1
            i_cmp   = i_equa - (i_node - 1)*nb_cmp
            i_slice = v_nume_pl(i_node)
            n_2d    = v_nume_sf(i_node)
            i_2d    = (n_2d - 1)*nb_cmp + i_cmp
            do i_mode = 1, nb_mode
                v_lin(i_equa + nb_equa*(i_mode - 1 + nb_mode*(i_slice - 1))) =&
                    v(i_2d + nb_equa/nb_slice*(i_mode - 1))
            enddo
        enddo

        call dbr_save(ds_empi, nb_mode*nb_slice, s_lin, v_lin, v_nume_slice)
        AS_DEALLOCATE(vr = v_lin)
        AS_DEALLOCATE(vr = s_lin)
        AS_DEALLOCATE(vi = v_nume_slice)
    else
        AS_ALLOCATE(vi=v_nume_slice, size = nb_mode)
        call dbr_save(ds_empi, nb_mode, s, v, v_nume_slice)
        AS_DEALLOCATE(vi = v_nume_slice)
    endif
!
end subroutine
