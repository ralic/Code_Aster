subroutine rrc_comp_dual(ds_para)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/rsexch.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/select_dof_3.h"
#include "blas/dgemm.h"
#include "blas/dgesv.h"
#include "asterfort/rsnoch.h"
#include "asterfort/copisd.h"
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
    type(ROM_DS_ParaRRC), intent(in) :: ds_para
!
! --------------------------------------------------------------------------------------------------
!
! REST_REDUIT_COMPLET - Compute
!
! Compute dual field
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_mode, nb_equa, nb_equa_rom, nb_cmp, nb_store
    integer :: iret, i_mode, i_equa, i_store, nume_store
    integer(kind=4) :: info
    character(len=8) :: result_rom, result_dom
    character(len=24) :: sigm_rom, mode
    integer, pointer :: v_noeu_rid(:) => null()
    real(kind=8), pointer :: v_mode(:) => null()
    real(kind=8), pointer :: v_dual(:) => null()
    real(kind=8), pointer :: v_dual_rom(:) => null()
    real(kind=8), pointer :: v_sigm_rom(:) => null()
    real(kind=8), pointer :: v_sigm_dom(:) => null()
    real(kind=8), pointer :: v_cohr(:) => null()
    real(kind=8), pointer :: v_matr(:) => null()
    real(kind=8), pointer :: v_vect(:) => null()
    integer(kind=4), pointer :: IPIV(:) => null()
    character(len=24) :: field_save
    real(kind=8), pointer :: v_field_save(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM6_22')
    endif
!
! - Get parameters
!
    nb_store     = ds_para%nb_store
    nb_mode      = ds_para%ds_empi_dual%nb_mode
    nb_equa      = ds_para%ds_empi_dual%nb_equa
    nb_cmp       = ds_para%ds_empi_dual%nb_cmp
    result_rom   = ds_para%result_rom
    result_dom   = ds_para%result_dom
!
! - List of equations in RID
!
    AS_ALLOCATE(vi = v_noeu_rid, size = nb_equa)
    call rsexch(' ', result_rom, ds_para%ds_empi_dual%field_name,&
                1, sigm_rom, iret)
    call jelira(sigm_rom(1:19)//'.VALE', 'LONMAX', nb_equa_rom)
    call select_dof_3(sigm_rom, nb_cmp, v_noeu_rid)
!
! - Get dual base
!
    AS_ALLOCATE(vr = v_dual, size = nb_equa*nb_mode)
    AS_ALLOCATE(vr = v_dual_rom, size = nb_equa_rom*nb_mode)
    do i_mode = 1, nb_mode
        call rsexch(' ', ds_para%ds_empi_dual%base, ds_para%ds_empi_dual%field_name,&
                    i_mode, mode, iret)
        ASSERT(iret .eq. 0)
        call jeveuo(mode(1:19)//'.VALE', 'L', vr = v_mode)
        do i_equa = 1, nb_equa
            v_dual(i_equa+nb_equa*(i_mode-1)) = v_mode(i_equa)
            if (v_noeu_rid(i_equa) .ne. 0) then
                v_dual_rom(v_noeu_rid(i_equa)+nb_equa_rom*(i_mode-1)) = v_mode(i_equa)
            endif 
        end do
    end do
!
! - Gappy POD 
!
    AS_ALLOCATE(vr  = v_cohr, size = nb_mode*(nb_store-1))
    AS_ALLOCATE(vr  = v_matr, size = nb_mode*nb_mode)
    AS_ALLOCATE(vr  = v_vect, size = nb_mode)
    AS_ALLOCATE(vi4 = IPIV  , size = nb_mode)
    do i_store = 1, nb_store-1
        nume_store = i_store
        call rsexch(' ', result_rom, ds_para%ds_empi_dual%field_name,&
                    nume_store, sigm_rom, iret)
        ASSERT(iret .eq. 0)
        call jeveuo(sigm_rom(1:19)//'.VALE', 'L', vr = v_sigm_rom)
        call dgemm('T', 'N', nb_mode, 1, nb_equa_rom, 1.d0, v_dual_rom,&
                    nb_equa_rom, v_sigm_rom, nb_equa_rom, 0.d0, v_vect, nb_mode)
        call dgemm('T', 'N', nb_mode, nb_mode, nb_equa_rom, 1.d0, v_dual_rom,&
                    nb_equa_rom, v_dual_rom, nb_equa_rom, 0.d0, v_matr, nb_mode)
        call dgesv(nb_mode, 1, v_matr, nb_mode, IPIV, v_vect, nb_mode, info)
        do i_mode = 1, nb_mode
            v_cohr(i_mode+nb_mode*(i_store-1)) = v_vect(i_mode)
        end do
    end do
!
! - Initial state
!
    nume_store = 0
    call rsexch(' ', result_dom, ds_para%ds_empi_dual%field_name,&
                nume_store, field_save, iret)
    ASSERT(iret .eq. 100)
    call copisd('CHAMP_GD', 'G', ds_para%ds_empi_dual%field_refe, field_save)
    call jeveuo(field_save(1:19)//'.VALE', 'E', vr = v_field_save)
    v_field_save(1:nb_equa) = 0.d0
    call rsnoch(result_dom, ds_para%ds_empi_dual%field_name,&
                nume_store)
!
! - Compute new field
!
    AS_ALLOCATE(vr = v_sigm_dom, size = nb_equa*(nb_store-1))
    call dgemm('N', 'N', nb_equa, nb_store-1, nb_mode, 1.d0, &
               v_dual, nb_equa, v_cohr, nb_mode, 0.d0, v_sigm_dom, nb_equa)
    do i_store = 1, nb_store-1
        nume_store = i_store
        call rsexch(' ', result_dom, ds_para%ds_empi_dual%field_name,&
                    nume_store, field_save, iret)
        ASSERT(iret .eq. 100)
        call copisd('CHAMP_GD', 'G', ds_para%ds_empi_dual%field_refe, field_save)
        call jeveuo(field_save(1:19)//'.VALE', 'E', vr = v_field_save)
        do i_equa = 1, nb_equa
            v_field_save(i_equa) = v_sigm_dom(i_equa+nb_equa*(nume_store-1))
        enddo
        call rsnoch(result_dom, ds_para%ds_empi_dual%field_name,&
                    nume_store)
    enddo
!
! - Clean
!
    AS_DEALLOCATE(vi  = v_noeu_rid)
    AS_DEALLOCATE(vr  = v_dual)
    AS_DEALLOCATE(vr  = v_dual_rom)
    AS_DEALLOCATE(vr  = v_cohr)
    AS_DEALLOCATE(vr  = v_matr)
    AS_DEALLOCATE(vr  = v_vect)
    AS_DEALLOCATE(vi4 = IPIV)
    AS_DEALLOCATE(vr  = v_sigm_dom)
!
end subroutine
