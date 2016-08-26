subroutine comp_meca_pvar(model_      , compor_cart_, compor_list_, compor_info,&
                          l_list_elem_, l_info_full_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/comp_meca_name.h"
#include "asterfort/comp_meca_nbvari.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexatr.h"
#include "asterfort/wkvect.h"
#include "asterc/lcdiscard.h"
#include "asterfort/comp_meca_code.h"
#include "asterfort/comp_meca_exc2.h"
#include "asterfort/comp_meca_l.h"
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
    character(len=8), optional, intent(in) :: model_
    character(len=19), optional, intent(in) :: compor_cart_
    character(len=16), optional, intent(in) :: compor_list_(20)
    character(len=19), intent(in) :: compor_info
    aster_logical, optional, intent(in) :: l_list_elem_
    aster_logical, optional, intent(in) :: l_info_full_
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Prepare informations about internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  compor_info      : name of object for information about internal variables and comportement
! In  compor_cart      : name of <CARTE> COMPOR
! In  compor_list      : name of list of COMPOR (for SIMU_POINT_MAT)
! In  l_list_elem      : produce INFO.ZONE
! In  l_info_full      : produce INFO.RELA
!
!    Objects:
!       INFO.INFO = global parameters
!         v_info(1) = nb_elem_mesh
!          => total number of elements in mesh
!         v_info(2) = nb_zone
!          => total number of zone in CARTE
!         v_info(3) = nb_vari_maxi
!          => maximum number of internal variables
!         v_info(4) = nt_vari    
!          => total number of internal variables
!       INFO.VARI = Collection of nb_zone (from CARTE) x Vecteur_Info
!       For each zone   : Vector_Info is list of nb_vari name of internal variables (K16)
!       INFO.ZONE = list on nb_zone (from CARTE)
!       For each zone   : number of elements with this comportement
!       INFO.RELA = list on nb_zone (from CARTE) * 8
!       For each zone   : some information from comprotement (name of RELATION, DEFORMATION, ...)
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_excl, l_kit_meta, l_mult_comp, l_exte_comp, l_kit_thm
    aster_logical :: l_list_elem, l_info_full, l_zone_read
    character(len=8) :: mesh
    character(len=19) :: ligrmo
    integer, pointer :: v_info(:) => null()
    integer, pointer :: v_zone(:) => null()
    integer, pointer :: v_zone_read(:) => null()
    integer, pointer :: v_model_elem(:) => null()
    character(len=16), pointer :: v_vari(:) => null()
    character(len=16), pointer :: v_rela(:) => null()
    character(len=16), pointer :: v_compor_vale(:) => null()
    integer, pointer :: v_compor_desc(:) => null()
    integer, pointer :: v_compor_lima(:) => null()
    integer, pointer :: v_compor_lima_lc(:) => null()
    integer, pointer :: v_compor_ptma(:) => null()
    integer :: nb_vale, nb_cmp_max, nb_zone, nb_vari, nt_vari, nb_vari_maxi, nb_zone_acti
    integer :: i_zone, i_elem, nb_elem_mesh, iret
    character(len=16) :: type_matg, post_iter, vari_excl
    character(len=16) :: rela_comp, defo_comp, kit_comp(4), type_cpla, type_comp, mult_comp
    character(len=16) :: comp_code_py, rela_code_py, meta_code_py
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nb_zone_acti = 0
    l_list_elem  = .false._1
    l_info_full  = .false._1
    if (present(l_list_elem_)) then
        l_list_elem = l_list_elem_
    endif
    if (present(l_info_full_)) then
        l_info_full = l_info_full_
    endif
!
! - Access to COMPOR
!
    if (present(compor_cart_)) then
        call jeveuo(compor_cart_//'.DESC', 'L', vi   = v_compor_desc)
        call jeveuo(compor_cart_//'.VALE', 'L', vk16 = v_compor_vale)
        call jelira(compor_cart_//'.VALE', 'LONMAX', nb_vale)
        call jeveuo(jexnum(compor_cart_//'.LIMA', 1), 'L', vi = v_compor_lima)
        call jeveuo(jexatr(compor_cart_//'.LIMA', 'LONCUM'), 'L', vi = v_compor_lima_lc)
        nb_zone    = v_compor_desc(3)
        nb_cmp_max = nb_vale/v_compor_desc(2)
        call dismoi('NOM_MAILLA'  , compor_cart_, 'CARTE', repk=mesh)
        call dismoi('NB_MA_MAILLA', mesh        , 'MAILLAGE', repi=nb_elem_mesh)
        ligrmo = model_(1:8)//'.MODELE'
        call jeveuo(model_//'.MAILLE', 'L', vi = v_model_elem)
        call etenca(compor_cart_, ligrmo, iret)
        call jeveuo(compor_cart_//'.PTMA', 'L', vi = v_compor_ptma)
    else if (present(compor_list_)) then
        nb_zone      = 1
        nb_cmp_max   = 0
        nb_elem_mesh = 1
    else
        ASSERT(.false.)
    endif
!
! - Count internal variables
!
    if (present(compor_cart_)) then
        call comp_meca_nbvari(model_ = model_, compor_cart_ = compor_cart_,&
                              nt_vari = nt_vari, nb_vari_maxi = nb_vari_maxi)
    elseif (present(compor_list_)) then
        call comp_meca_nbvari(compor_list_ = compor_list_,&
                              nt_vari = nt_vari, nb_vari_maxi = nb_vari_maxi)
    else
        ASSERT(.false.)
    endif
    AS_ALLOCATE(vi = v_zone_read, size = nb_zone)
!
! - No internal variables names
!
    if (nt_vari .eq. 0) then
        goto 99
    endif
!
! - Create list of zones: for each zone (in CARTE), how many elements 
!
    if (l_list_elem) then
        call wkvect(compor_info(1:19)//'.ZONE', 'V V I', nb_zone, vi = v_zone)
    endif
!
! - Create list of comportment information (RELATION, DEFORMATION, etc.)
!
    if (l_info_full) then
        call wkvect(compor_info(1:19)//'.RELA', 'V V K16', 9*nb_zone, vk16 = v_rela)
    endif
!
! - Create list of internal variables names
!
    call jecrec(compor_info(1:19)//'.VARI', 'V V K16', 'NU', 'DISPERSE', 'VARIABLE',&
                nb_zone)
    do i_zone = 1, nb_zone
        call jecroc(jexnum(compor_info(1:19)//'.VARI', i_zone))
    end do
! 
    do i_elem = 1, nb_elem_mesh
!
! ----- Get current zone
!
        if (present(compor_cart_)) then
            i_zone = v_compor_ptma(i_elem)
            if (i_zone .eq. 0) then
                l_zone_read = .true.
            else
                ASSERT(i_zone .ne. 0)
                l_zone_read = v_zone_read(i_zone) .eq. 1
            endif
        else
            i_zone      = 1
            l_zone_read = .false._1
        endif
        if (.not. l_zone_read) then
!
! --------- Get parameters
!
            if (present(compor_cart_)) then
                rela_comp   = v_compor_vale(nb_cmp_max*(i_zone-1)+1)
                defo_comp   = v_compor_vale(nb_cmp_max*(i_zone-1)+3)
                type_comp   = v_compor_vale(nb_cmp_max*(i_zone-1)+4)
                type_cpla   = v_compor_vale(nb_cmp_max*(i_zone-1)+5)
                mult_comp   = v_compor_vale(nb_cmp_max*(i_zone-1)+7)
                kit_comp(1) = v_compor_vale(nb_cmp_max*(i_zone-1)+8)
                kit_comp(2) = v_compor_vale(nb_cmp_max*(i_zone-1)+9)
                kit_comp(3) = v_compor_vale(nb_cmp_max*(i_zone-1)+10)
                kit_comp(4) = v_compor_vale(nb_cmp_max*(i_zone-1)+11)
                type_matg   = v_compor_vale(nb_cmp_max*(i_zone-1)+13)
                post_iter   = v_compor_vale(nb_cmp_max*(i_zone-1)+14)
                read (v_compor_vale(nb_cmp_max*(i_zone-1)+2),'(I16)') nb_vari
            else
                rela_comp   = compor_list_(1)
                defo_comp   = compor_list_(3)
                type_comp   = compor_list_(4)
                type_cpla   = compor_list_(5)
                mult_comp   = compor_list_(7)
                kit_comp(1) = compor_list_(8)
                kit_comp(2) = compor_list_(9)
                kit_comp(3) = compor_list_(10)
                kit_comp(4) = compor_list_(11)
                type_matg   = compor_list_(13)
                post_iter   = compor_list_(14)
                read (compor_list_(2),'(I16)') nb_vari
            endif
!
! --------- Detection of specific cases
!
            call comp_meca_l(rela_comp, 'KIT_META' , l_kit_meta)
            call comp_meca_l(rela_comp, 'KIT_THM'  , l_kit_thm)
            call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp)
            if (l_kit_thm) then
                call comp_meca_l(kit_comp(4), 'EXTE_COMP', l_exte_comp)
            endif
            l_mult_comp = mult_comp .ne. ' ' .and. mult_comp .ne. 'VIDE'
!
! --------- Coding composite comportment
!
            call comp_meca_code(rela_comp, defo_comp   , type_cpla   , kit_comp    , type_matg,&
                                post_iter, comp_code_py, rela_code_py, meta_code_py)
!
! --------- Exception for name of internal variables
!
            call comp_meca_exc2(l_mult_comp, l_exte_comp,&
                                l_excl     , vari_excl)
!
! --------- Save names of relation
!
            if (l_info_full) then
                v_rela(9*(i_zone-1) + 1) = rela_comp
                v_rela(9*(i_zone-1) + 2) = defo_comp
                v_rela(9*(i_zone-1) + 3) = type_comp
                v_rela(9*(i_zone-1) + 4) = type_cpla
                v_rela(9*(i_zone-1) + 5) = kit_comp(1)
                v_rela(9*(i_zone-1) + 6) = kit_comp(2)
                v_rela(9*(i_zone-1) + 7) = kit_comp(3)
                v_rela(9*(i_zone-1) + 8) = kit_comp(4)
                v_rela(9*(i_zone-1) + 9) = mult_comp
            endif
!
! --------- Save name of internal variables
!
            call jeecra(jexnum(compor_info(1:19)//'.VARI', i_zone), 'LONMAX', nb_vari)
            call jeveuo(jexnum(compor_info(1:19)//'.VARI', i_zone), 'E', vk16 = v_vari)
            call comp_meca_name(nb_vari     , l_excl      , vari_excl   , l_kit_meta,&
                                defo_comp   ,&
                                comp_code_py, rela_code_py, meta_code_py,&
                                v_vari)
!
! --------- Save current zone
!
            v_zone_read(i_zone) = 1
            nb_zone_acti        = nb_zone_acti + 1
!
            call lcdiscard(comp_code_py)
            call lcdiscard(rela_code_py)
            call lcdiscard(meta_code_py)
        endif
    end do
!
! - Count number of elements by zone (in CARTE)
!
    if (l_list_elem) then
        if (present(compor_cart_)) then
            do i_elem = 1, nb_elem_mesh
                i_zone = v_compor_ptma(i_elem)
                if (i_zone .ne. 0 .and. v_model_elem(i_elem) .ne. 0) then
                    v_zone(i_zone) = v_zone(i_zone)+1
                endif
            end do
        else
            v_zone(1) = 1
        endif
    endif
!
 99 continue
!
! - Save general information
!
    call wkvect(compor_info(1:19)//'.INFO', 'V V I', 5, vi = v_info)
    v_info(1) = nb_elem_mesh
    v_info(2) = nb_zone
    v_info(3) = nb_vari_maxi
    v_info(4) = nt_vari
    v_info(5) = nb_zone_acti
!
    AS_DEALLOCATE(vi = v_zone_read)
!
    call jedema()
!
end subroutine
