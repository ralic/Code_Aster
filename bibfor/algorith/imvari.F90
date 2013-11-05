subroutine imvari(list_vari_name, compor_cart, compor_list)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/cesexi.h"
#include "asterfort/carces.h"
#include "asterfort/detrsd.h"
#include "asterfort/jeexin.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    character(len=19), intent(in) :: list_vari_name
    character(len=19), optional, intent(in) :: compor_cart
    character(len=16), optional, intent(in) :: compor_list(20)
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Print informations about internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  list_vari_name : object (collection) to save information about internal variables
! In  compor_cart    : if COMPOR is <CARTE>
! In  compor_list    : if COMPOR is <LIST> (SIMU_POINT_MAT operator)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: j_vari_name, j_vari_link
    character(len=19) :: compor_s
    integer :: j_comp_d, j_comp_v, j_comp_l, iadc
    integer :: nbocc, nume_elem, iocc, ivari, idummy, iret
    integer :: nb_vari, jdecal
    integer :: nb_vari_thmc, nb_vari_hydr, nb_vari_meca, nb_vari_ther
    integer :: nb_vari_flua, nb_vari_plas, nb_vari_cpla, nb_vari_coup
    integer :: nb_vari_comp(9), nb_vari_cg(2)
    character(len=16) :: vari_excl, vari_name
    character(len=16) :: rela_comp, defo_comp, type_comp, type_cpla, kit_comp(9)
    logical :: l_excl, l_kit_thm, l_kit_ddi, l_kit_cg
    character(len=16) :: rela_thmc, rela_hydr, rela_meca, rela_ther
    character(len=16) :: rela_flua, rela_plas, rela_cpla, rela_coup
    character(len=16) :: rela_cg(2)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    compor_s = '&&COMPOR.CARCES'
    call utmess('I', 'COMPOR4_1')
!
! - Transform COMPOR in CHAM_ELEM_S
!
    if (present(compor_cart)) then
        ASSERT(.not.present(compor_list))
        call carces(compor_cart, 'ELEM', ' ', 'V', compor_s, &
                    'A', idummy)
        call jeveuo(compor_s//'.CESD', 'L', j_comp_d)
        call jeveuo(compor_s//'.CESV', 'L', j_comp_v)
        call jeveuo(compor_s//'.CESL', 'L', j_comp_l)
    endif
!
! - Access to link between COMPOR and internal variable names
!
    call jeexin(list_vari_name(1:19)//'.LINK',iret)
    if (iret.eq.0) then
        nbocc = 0
    else
        call jeveuo(list_vari_name(1:19)//'.LINK', 'L', j_vari_link)
        call jelira(list_vari_name(1:19)//'.LINK', 'LONMAX', nbocc)
    endif
!
    do iocc = 1, nbocc
!
        call utmess('I', 'COMPOR4_2', si = iocc)
!
! ----- Get reference element
!
        nume_elem = zi(j_vari_link-1+iocc)
!
! ----- No reference element (no affectation or external comportment) -> exit
!
        if (nume_elem .eq. 0) then
            call utmess('I', 'COMPOR4_3')
            goto 10
        endif
!
! ----- Get info
!
        if (present(compor_cart)) then
            call cesexi('C', j_comp_d, j_comp_l, nume_elem, 1,&
                        1, 1, iadc)
            if (iadc .gt. 0) then
                rela_comp = zk16(j_comp_v+iadc-2+1)
                defo_comp = zk16(j_comp_v+iadc-2+3)
                type_comp = zk16(j_comp_v+iadc-2+4)
                type_cpla = zk16(j_comp_v+iadc-2+5)
                read (zk16(j_comp_v+iadc-2+17),'(I16)') nb_vari_comp(1)
                read (zk16(j_comp_v+iadc-2+18),'(I16)') nb_vari_comp(2)
                read (zk16(j_comp_v+iadc-2+19),'(I16)') nb_vari_comp(3)
                read (zk16(j_comp_v+iadc-2+20),'(I16)') nb_vari_comp(4)
                kit_comp(1)  = zk16(j_comp_v+iadc-2+8)
                kit_comp(2)  = zk16(j_comp_v+iadc-2+9)
                kit_comp(3)  = zk16(j_comp_v+iadc-2+10)
                kit_comp(4)  = zk16(j_comp_v+iadc-2+11)
            endif
        elseif (present(compor_list)) then
            rela_comp = compor_list(1)
            defo_comp = compor_list(3)
            type_comp = compor_list(4)
            type_cpla = compor_list(5)
            nb_vari_comp(1) = 0
            nb_vari_comp(2) = 0
            nb_vari_comp(3) = 0
            nb_vari_comp(4) = 0
            kit_comp(1)  = compor_list(8)
            kit_comp(2)  = compor_list(9)
            kit_comp(3)  = compor_list(10)
            kit_comp(4)  = compor_list(11)
        endif
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'KIT_DDI'  , l_kit_ddi)
        call comp_meca_l(rela_comp, 'KIT_CG'   , l_kit_cg)
        call comp_meca_l(rela_comp, 'KIT_THM'  , l_kit_thm)
!
! ----- Acces to list of name of internal variables
!
        call jeveuo(jexnum(list_vari_name(1:19)//'.NAME', iocc), 'L', j_vari_name)
        call jelira(jexnum(list_vari_name(1:19)//'.NAME', iocc), 'LONMAX', nb_vari)
!
! ----- Exceptions ?
!
        l_excl = .false.
        vari_excl = zk16(j_vari_name-1+1)
        if (vari_excl(1:2).eq.'&&') l_excl = .true.
!
! ----- Print
!
        if (type_comp.eq.'COMP_INCR') then
            call utmess('I', 'COMPOR4_4', sk = rela_comp)
        endif
        if (type_comp.eq.'COMP_ELAS') then
            call utmess('I', 'COMPOR4_5', sk = rela_comp)
        endif
        call utmess('I', 'COMPOR4_6', sk = defo_comp)
        if (type_cpla.eq.'DEBORST') then
            call utmess('I', 'COMPOR4_8')
        endif
        call utmess('I', 'COMPOR4_9', si = nb_vari)
        if (l_kit_thm) then
!
            call utmess('I', 'COMPOR4_30')
            rela_thmc = kit_comp(1)
            rela_ther = kit_comp(2)
            rela_hydr = kit_comp(3)
            rela_meca = kit_comp(4)
            nb_vari_thmc = nb_vari_comp(1)
            nb_vari_ther = nb_vari_comp(2)
            nb_vari_hydr = nb_vari_comp(3)
            nb_vari_meca = nb_vari_comp(4)
            jdecal = 0
!
!           THM behaviours: must be reordered and start by 'rela_meca'
            if (rela_meca .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_34', sk = rela_meca, si = nb_vari_meca)
            endif
            do ivari = 1, nb_vari_meca
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo
            jdecal = jdecal + nb_vari_meca
!
            if (rela_thmc .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_31', sk = rela_thmc, si = nb_vari_thmc)
            endif
            do ivari = 1, nb_vari_thmc
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo
            jdecal = jdecal + nb_vari_thmc
!
            if (rela_ther .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_32', sk = rela_ther, si = nb_vari_ther)
            endif
            do ivari = 1, nb_vari_ther
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo
            jdecal = jdecal + nb_vari_ther
!
            if (rela_hydr .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_33', sk = rela_hydr, si = nb_vari_hydr)
            endif
            do ivari = 1, nb_vari_hydr
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo
            jdecal = jdecal + nb_vari_hydr
!
        elseif (l_kit_ddi) then
!
            call utmess('I', 'COMPOR4_40')
            rela_flua = kit_comp(1)
            rela_plas = kit_comp(2)
            rela_coup = kit_comp(3)
            rela_cpla = kit_comp(4)
            nb_vari_flua = nb_vari_comp(1)
            nb_vari_plas = nb_vari_comp(2)
            nb_vari_coup = nb_vari_comp(3)
            nb_vari_cpla = nb_vari_comp(4)
!
            if (rela_flua .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_41', sk = rela_flua, si = nb_vari_flua)
            endif
            jdecal = 0
            do ivari = 1, nb_vari_flua
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo
!
            if (rela_plas .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_42', sk = rela_plas, si = nb_vari_plas)
            endif
            jdecal = jdecal + nb_vari_flua
            do ivari = 1, nb_vari_plas
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo
!
            if (rela_coup .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_43', sk = rela_coup, si = nb_vari_coup)
            endif
            jdecal = jdecal + nb_vari_plas
            do ivari = 1, nb_vari_coup
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo
!
            if (rela_cpla .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_44', sk = rela_cpla, si = nb_vari_cpla)
            endif
            jdecal = jdecal + nb_vari_coup
            do ivari = 1, nb_vari_cpla
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo

        elseif (l_kit_cg) then
!
            call utmess('I', 'COMPOR4_50')
            rela_cg(1) = kit_comp(1)
            rela_cg(2) = kit_comp(2)
            nb_vari_cg(1) = nb_vari_comp(1)
            nb_vari_cg(2) = nb_vari_comp(2)
!
            if (rela_cg(1) .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_51', sk = rela_cg(1), si = nb_vari_cg(1))
            endif
            jdecal = 0
            do ivari = 1, nb_vari_cg(1)
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo
!
            if (rela_cg(2) .ne. 'VIDE') then
                call utmess('I', 'COMPOR4_52', sk = rela_cg(2), si = nb_vari_cg(2))
            endif
            jdecal = jdecal + nb_vari_cg(1)
            do ivari = 1, nb_vari_cg(2)
                vari_name = zk16(j_vari_name-1+ivari+jdecal)
                call utmess('I', 'COMPOR4_20', sk = vari_name, si = ivari+jdecal)
            enddo
        else   
!
! --------- Name of internal variables
!
            if (l_excl) then
                if (vari_excl(1:6).eq.'&&POLY') then
                    call utmess('I', 'COMPOR4_10')
                    if (vari_excl.eq.'&&POLY_SIMO') then
                        call utmess('I', 'COMPOR4_11')
                    endif
                elseif (vari_excl.eq.'&&KMET') then
                    call utmess('I', 'COMPOR4_12')
                else
                    ASSERT(.false.)
                endif
            else
                do ivari = 1, nb_vari
                    call utmess('I', 'COMPOR4_20', sk = zk16(j_vari_name-1+ivari), si = ivari)
                enddo
            endif
        endif
!
10      continue
    enddo
!
    if (present(compor_cart)) call detrsd('CHAMP', compor_s)
    call jedema()
!
end subroutine
