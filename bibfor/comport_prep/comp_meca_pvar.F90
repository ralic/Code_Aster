subroutine comp_meca_pvar(list_vari_name, compor_cart, compor_list)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/lcinfo.h"
#include "asterc/lcvari.h"
#include "asterc/lcdiscard.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/comp_meca_code.h"
#include "asterfort/comp_meca_exc2.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_meca_name.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/detrsd.h"
#include "asterfort/cesexi.h"
#include "asterfort/wkvect.h"
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
    character(len=19), intent(in) :: list_vari_name
    character(len=19), optional, intent(in) :: compor_cart
    character(len=16), optional, intent(in) :: compor_list(20)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Prepare informations about internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  list_vari_name : object (collection) to save information about internal variables
! In  compor_cart    : name of <CARTE> COMPOR
! In  compor_list    : name of list of COMPOR (for SIMU_POINT_MAT)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_occ
    integer :: j_list_occ, j_vari_name, j_vari_link
    character(len=19) :: compor_s
    character(len=16) :: vari_excl, prev_rela_comp
    character(len=16) :: rela_comp, defo_comp, type_cpla, type_matg, post_iter
    character(len=16) :: kit_comp(4)
    character(len=16) :: comp_elem_py, rela_comp_py, rela_meta_py
    integer :: j_comp_d, j_comp_l, iadc
    aster_logical :: l_kit_meta, l_affe
    aster_logical :: l_cristal, l_exte_comp, l_pmf
    aster_logical :: l_excl, l_kit_thm, prev_exte_comp, prev_pmf
    integer :: nb_elem, nocc, nb_vari, nb_vari_all
    integer :: i_elem, iocc, i_kit
    integer :: idummy
    integer :: nume_elem, old_nume
    character(len=16), pointer :: cesv(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    compor_s    = '&&COMPOR.CARCES'
    list_occ    = '&&COMPMECA.LISTOCC'
    nocc        = 0
    nb_vari_all = 0
!
! - Transform COMPOR in CHAM_ELEM_S
!
    if (present(compor_cart)) then
        ASSERT(.not.present(compor_list))
        call carces(compor_cart, 'ELEM', ' ', 'V', compor_s,&
                    'A', idummy)
        call jeveuo(compor_s//'.CESD', 'L', j_comp_d)
        call jeveuo(compor_s//'.CESV', 'L', vk16=cesv)
        call jeveuo(compor_s//'.CESL', 'L', j_comp_l)
    endif
!
! - Number of elements
!
    if (present(compor_list)) then
        ASSERT(.not.present(compor_cart))
        nb_elem = 1
    else if (present(compor_cart)) then
        ASSERT(.not.present(compor_list))
        nb_elem = zi(j_comp_d)
    endif
!
! - Object to define reference element
!
    call wkvect(list_occ, 'V V I', nb_elem, j_list_occ)
!
! - Get element on which comportment has been defined
!
    prev_rela_comp = " "
    prev_exte_comp = .false.
    prev_pmf = .false.
    do i_elem = 1, nb_elem
        nume_elem = i_elem
        if (present(compor_cart)) then
            call cesexi('C', j_comp_d, j_comp_l, nume_elem, 1,&
                        1, 1, iadc)
            if (iadc .gt. 0) then
                rela_comp = cesv(1+iadc-2+1)
                if (rela_comp .ne. prev_rela_comp) then
                    call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp)
                    call comp_meca_l(rela_comp, 'PMF', l_pmf)
                    prev_rela_comp = rela_comp
                    prev_exte_comp = l_exte_comp
                    prev_pmf = l_pmf
                else
                    l_exte_comp = prev_exte_comp
                    l_pmf = prev_pmf
                endif
!
                if (.not.l_exte_comp .and. .not. l_pmf) then
                    read (cesv(1+iadc-2+12),'(I16)') iocc
                    l_affe = (iocc.ne.99999)
                    read (cesv(1+iadc-2+2 ),'(I16)') nb_vari
                    if (l_affe) then
                        old_nume = zi(j_list_occ-1+iocc)
                        if (old_nume .eq. 0) then
                            nocc = nocc + 1
                            zi(j_list_occ-1+iocc) = nume_elem
                            nb_vari_all = nb_vari_all + nb_vari
                        endif
                    endif
                else
                    nb_vari = 1
                    nb_vari_all = nb_vari_all + nb_vari
                endif
            endif
        else if (present(compor_list)) then
            rela_comp = compor_list(1)
            call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp)
            if (.not.l_exte_comp) then
                iocc = 1
                nocc = 1
                read (compor_list(2),'(I16)') nb_vari
                zi(j_list_occ-1+iocc) = nume_elem
                nb_vari_all = nb_vari_all + nb_vari
            endif
        endif
    enddo
!
! - No internal variables names
!
    if (nocc .eq. 0) goto 99
!
! - Create list of internal variables names
!
    call jecrec(list_vari_name(1:19)//'.NAME', 'V V K16', 'NU', 'CONTIG', 'VARIABLE',&
                nocc)
    call jeecra(list_vari_name(1:19)//'.NAME', 'LONT', nb_vari_all)
!
! - Create link between COMPOR and internal variable names
!
    call wkvect(list_vari_name(1:19)//'.LINK', 'V V I', nocc, j_vari_link)
!
    do iocc = 1, nocc
!
! ----- Get reference element
!
        nume_elem = zi(j_list_occ-1+iocc)
        zi(j_vari_link-1+iocc) = nume_elem
!
! ----- No reference element (external comportment) -> exit
!
        if (nume_elem .eq. 0) then
            call jecroc(jexnum(list_vari_name(1:19)//'.NAME', iocc))
            nb_vari = 1
            call jeecra(jexnum(list_vari_name(1:19)//'.NAME', iocc), 'LONMAX', nb_vari)
            goto 10
        endif
!
! ----- Get info
!
        if (present(compor_cart)) then
            call cesexi('C', j_comp_d, j_comp_l, nume_elem, 1,&
                        1, 1, iadc)
            if (iadc .gt. 0) then
                rela_comp = cesv(1+iadc-2+1)
                read (cesv(1+iadc-2+2 ),'(I16)') nb_vari
                defo_comp = cesv(1+iadc-2+3)
                type_cpla = cesv(1+iadc-2+5)
                type_matg = cesv(1+iadc-2+13)
                post_iter = cesv(1+iadc-2+14)
                call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp)
                do i_kit = 1, 4
                    kit_comp(i_kit) = cesv(1+iadc-2+7+i_kit)
                end do
                if (kit_comp(4).eq.'MFRONT') then
                    l_exte_comp = .true.
                endif
            endif
        else if (present(compor_list)) then
            rela_comp = compor_list(1)
            read (compor_list(2),'(I16)') nb_vari
            defo_comp = compor_list(3)
            type_cpla = compor_list(5)
            type_matg = compor_list(13)
            post_iter = compor_list(14)
            call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp)
            do i_kit = 1, 4
                kit_comp(i_kit) = compor_list(7+i_kit)
            end do
            if (kit_comp(4).eq.'MFRONT') then
                l_exte_comp = .true.
            endif
        endif
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'CRISTAL', l_cristal)
        call comp_meca_l(rela_comp, 'KIT_META', l_kit_meta)
        call comp_meca_l(rela_comp, 'PMF', l_pmf)
!
! ----- Coding composite comportment
!
        call comp_meca_code(rela_comp, defo_comp   , type_cpla   , kit_comp    , type_matg,&
                            post_iter, comp_elem_py, rela_comp_py, rela_meta_py)
!
! ----- Exception for name of internal variables
!
        call comp_meca_exc2(defo_comp, l_kit_meta, l_cristal, l_pmf, l_excl,&
                            vari_excl)
        call comp_meca_l(rela_comp, 'KIT_THM', l_kit_thm)
        if (l_kit_thm.and.l_exte_comp) then
            l_excl=.true.
        endif
!
! ----- Save name of internal variables
!
        call jecroc(jexnum(list_vari_name(1:19)//'.NAME', iocc))
        call jeecra(jexnum(list_vari_name(1:19)//'.NAME', iocc), 'LONMAX', nb_vari)
        call jeveuo(jexnum(list_vari_name(1:19)//'.NAME', iocc), 'E', j_vari_name)
        call comp_meca_name(nb_vari     , l_excl      , vari_excl, l_kit_meta, comp_elem_py,&
                            rela_comp_py, rela_meta_py, zk16(j_vari_name))
!
 10     continue
!
        call lcdiscard(comp_elem_py)
        call lcdiscard(rela_comp_py)
        call lcdiscard(rela_meta_py)
!
    enddo
!
 99 continue
    if (present(compor_cart)) then
        call detrsd('CHAMP', compor_s)
    endif
    call jedetr(list_occ)
    call jedema()
!
end subroutine
