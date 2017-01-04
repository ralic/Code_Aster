subroutine romBaseChck(ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/dismoi.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterc/indik8.h"
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
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Check empiric modes base
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: mesh, model, gran_name, node_name, cmp_name
    character(len=16) :: modeli
    character(len=19) :: pfchno
    integer :: nb_dime, nb_cmp, nb_cmp_maxi, nb_equa
    integer :: i_equa, i_cmp
    integer :: nb_cmp_chck
    character(len=8) :: name_cmp_chck(6)
    integer :: indx_cmp_chck(6)
    integer :: nume_cmp, nume_node
    aster_logical :: l_find
    character(len=24) :: field_name = ' ', field_refe = ' '
    character(len=8), pointer :: v_list_cmp(:) => null()
    integer, pointer :: v_deeq(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    mesh       = ds_empi%mesh
    model      = ds_empi%model
    nb_cmp     = ds_empi%nb_cmp
    nb_equa    = ds_empi%nb_equa
    field_name = ds_empi%field_type
    field_refe = ds_empi%field_refe
!
! - Check mesh
!
    call dismoi('DIM_GEOM', mesh, 'MAILLAGE', repi = nb_dime)
    if (nb_dime .ne. 3) then
        call utmess('F','ROM5_20')
    endif
!
! - Check model
!
    call dismoi('MODELISATION', model, 'MODELE', repk=modeli)
    if (modeli .ne. '3D') then
        call utmess('F','ROM5_20')
    endif
!
! - Check fields
!
    call dismoi('NOM_GD'   , field_refe, 'CHAM_NO', repk = gran_name)
    call dismoi('PROF_CHNO', field_refe, 'CHAM_NO', repk = pfchno)
    call jeveuo(pfchno//'.DEEQ', 'L', vi = v_deeq)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', gran_name), 'L', vk8 = v_list_cmp)
    call jelira(jexnom('&CATA.GD.NOMCMP', gran_name), 'LONMAX', nb_cmp_maxi)
    if (field_name .eq. 'TEMP') then
        if (nb_cmp .ne. 1) then
            call utmess('F','ROM5_21', sk = field_name, si = 1)
        endif
        nb_cmp_chck      = 1
        name_cmp_chck(1) = 'TEMP'
        indx_cmp_chck(1) = indik8(v_list_cmp, 'TEMP', 1, nb_cmp_maxi)
    elseif (field_name .eq. 'DEPL') then
        if (nb_cmp .ne. 3) then
            call utmess('F','ROM5_21', sk = field_name, si = 3)
        endif
        nb_cmp_chck      = 3
        name_cmp_chck(1) = 'DX'
        indx_cmp_chck(1) = indik8(v_list_cmp, name_cmp_chck(1), 1, nb_cmp_maxi)
        name_cmp_chck(2) = 'DY'
        indx_cmp_chck(2) = indik8(v_list_cmp, name_cmp_chck(2), 1, nb_cmp_maxi)
        name_cmp_chck(3) = 'DZ'
        indx_cmp_chck(3) = indik8(v_list_cmp, name_cmp_chck(3), 1, nb_cmp_maxi)
    elseif (field_name .eq. 'FLUX_NOEU') then
        if (nb_cmp .ne. 3) then
            call utmess('F','ROM5_21', sk = field_name, si = 3)
        endif
        nb_cmp_chck      = 3
        name_cmp_chck(1) = 'FLUX'
        indx_cmp_chck(1) = indik8(v_list_cmp, name_cmp_chck(1), 1, nb_cmp_maxi)
        name_cmp_chck(2) = 'FLUY'
        indx_cmp_chck(2) = indik8(v_list_cmp, name_cmp_chck(2), 1, nb_cmp_maxi)
        name_cmp_chck(3) = 'FLUZ'
        indx_cmp_chck(3) = indik8(v_list_cmp, name_cmp_chck(3), 1, nb_cmp_maxi)
    elseif (field_name .eq. 'SIEF_NOEU') then
        if (nb_cmp .ne. 6) then
            call utmess('F','ROM5_21', sk = field_name, si = 6)
        endif
        nb_cmp_chck      = 6
        name_cmp_chck(1) = 'SIXX'
        indx_cmp_chck(1) = indik8(v_list_cmp, name_cmp_chck(1), 1, nb_cmp_maxi)
        name_cmp_chck(2) = 'SIYY'
        indx_cmp_chck(2) = indik8(v_list_cmp, name_cmp_chck(2), 1, nb_cmp_maxi)
        name_cmp_chck(3) = 'SIZZ'
        indx_cmp_chck(3) = indik8(v_list_cmp, name_cmp_chck(3), 1, nb_cmp_maxi)
        name_cmp_chck(4) = 'SIXZ'
        indx_cmp_chck(4) = indik8(v_list_cmp, name_cmp_chck(4), 1, nb_cmp_maxi)
        name_cmp_chck(5) = 'SIYZ'
        indx_cmp_chck(5) = indik8(v_list_cmp, name_cmp_chck(5), 1, nb_cmp_maxi)
        name_cmp_chck(6) = 'SIXY'
        indx_cmp_chck(6) = indik8(v_list_cmp, name_cmp_chck(6), 1, nb_cmp_maxi)
    else
        ASSERT(.false.)
    endif
!
    do i_equa = 1, nb_equa
        nume_node = v_deeq(2*(i_equa-1)+1)
        nume_cmp  = v_deeq(2*(i_equa-1)+2)
        l_find    = .false._1
        do i_cmp = 1, nb_cmp_chck
            if (nume_cmp .ne. indx_cmp_chck(i_cmp)) then
                cmp_name = v_list_cmp(nume_cmp)
                if (cmp_name .eq. 'LAGR') then
                    call utmess('F', 'ROM5_22')
                endif
            else
                l_find = .true._1
            endif
        end do
        if (.not.l_find) then
            call jenuno(jexnum(mesh//'.NOMNOE', nume_node), node_name)
            call utmess('F', 'ROM5_23', sk = node_name)
        endif
    end do
!
end subroutine
