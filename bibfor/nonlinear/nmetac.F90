subroutine nmetac(list_func_acti, sddyna, sdcont_defi, nb_field_maxi, list_field_acti)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/isfonc.h"
#include "asterfort/ndynlo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: nb_field_maxi
    aster_logical, intent(inout) :: list_field_acti(nb_field_maxi)
    character(len=19), intent(in) :: sddyna
    integer, intent(in) :: list_func_acti(*)
    character(len=24), intent(in) :: sdcont_defi
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Select fields depending on active functionnalities
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  list_func_acti   : list of active functionnalities
! In  sddyna           : name of dynamic parameters datastructure
! In  nb_field_maxi    : number of fields to active
! IO  list_field_acti  : list of fields to active
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_xfem, l_frot_xfem, l_xfem_czm, l_cont
    aster_logical :: l_dyna, l_inte_node, l_muap, l_strx
    aster_logical :: l_vibr_mode, l_crit_stab, l_dof_stab, l_ener
    integer :: i_field
    integer, pointer :: work_flag(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Active functionnalities
!
    l_dyna      = ndynlo(sddyna,'DYNAMIQUE' )
    l_muap      = ndynlo(sddyna,'MULTI_APPUI')
    l_crit_stab = isfonc(list_func_acti,'CRIT_STAB' )
    l_dof_stab  = isfonc(list_func_acti,'DDL_STAB' )
    l_vibr_mode = isfonc(list_func_acti,'MODE_VIBR' )
    l_strx      = isfonc(list_func_acti,'EXI_STRX')
    l_ener      = isfonc(list_func_acti,'ENERGIE' )
    l_cont_xfem = isfonc(list_func_acti,'CONT_XFEM' )
    l_cont      = isfonc(list_func_acti,'CONTACT' )
    if (l_cont_xfem) then
        l_frot_xfem = isfonc(list_func_acti,'FROT_XFEM')
        l_xfem_czm  = cfdisl(sdcont_defi,'EXIS_XFEM_CZM')
    endif
!
! - Working vector
!
    AS_ALLOCATE(vi = work_flag, size = nb_field_maxi)
!
! - Standard: DEPL/SIEF_ELGA/VARI_ELGA/FORC_NODA
!
    list_field_acti(1)  = .true.
    list_field_acti(2)  = .true.
    list_field_acti(3)  = .true.
    list_field_acti(16) = .true.
    work_flag(1)  = 1
    work_flag(2)  = 1
    work_flag(3)  = 1
    work_flag(16) = 1
!
! - Standard: COMPOR
!
    list_field_acti(4) = .true.
    work_flag(4) = 1
!
! - Dynamic: VITE/ACCE
!
    if (l_dyna) then
        list_field_acti(5) = .true.
        list_field_acti(6) = .true.
    endif
    work_flag(5) = 1
    work_flag(6) = 1
!
! - XFEM
!
    if (l_cont_xfem) then
        list_field_acti(7) = .true.
        if (l_frot_xfem) then
            list_field_acti(8) = .true.
        endif
        if (l_xfem_czm) then
            list_field_acti(9) = .true.
        endif
    endif
    work_flag(7) = 1
    work_flag(8) = 1
    work_flag(9) = 1
!
! - Contact
!
    if (l_cont) then
        l_inte_node = cfdisl(sdcont_defi,'ALL_INTEG_NOEUD')
        if (l_inte_node) then
            list_field_acti(10) = .true.
        endif
    endif
    work_flag(10) = 1
!
! - Stability criterion (buckling)
!
    if (l_crit_stab) then
        list_field_acti(11) = .true.
    endif
    work_flag(11) = 1
!
! - Stability criterion (with dof selection)
!
    if (l_dof_stab) then
        list_field_acti(18) = .true.
    endif
    work_flag(18) = 1
!
! - Vibration modes
!
    if (l_vibr_mode) then
        list_field_acti(12) = .true.
    endif
    work_flag(12) = 1
!
! - "MULTI-APPUIS": DEPL/VITE/ACCE d'entrainement
!
    if (l_muap) then
        list_field_acti(13) = .true.
        list_field_acti(14) = .true.
        list_field_acti(15) = .true.
    endif
    work_flag(13) = 1
    work_flag(14) = 1
    work_flag(15) = 1
!
! - Special elements: multifibers beams
!
    if (l_strx) then
        list_field_acti(17) = .true.
    endif
    work_flag(17) = 1
!
! - Energy
!
    if (l_ener) then
        list_field_acti(19) = .true.
        list_field_acti(20) = .true.
    endif
    work_flag(19) = 1
    work_flag(20) = 1
!
! - Check: if ASSERT -> you've forgottent to say what Aster do with the field
!
    do i_field = 1, nb_field_maxi
        ASSERT(work_flag(i_field).eq.1)
    end do
!
    AS_DEALLOCATE(vi = work_flag)
end subroutine
