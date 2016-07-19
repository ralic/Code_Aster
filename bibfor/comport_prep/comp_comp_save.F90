subroutine comp_comp_save(mesh, compor, nb_cmp, v_info_valk, v_info_vali)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/comp_read_mesh.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: compor
    integer, intent(in) :: nb_cmp
    character(len=16), intent(in) :: v_info_valk(:)
    integer          , intent(in) :: v_info_vali(:)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (AFFE_MATERIAU)
!
! Save informations in COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  compor           : name of <CARTE> COMPOR
! In  nb_cmp           : number of components in <CARTE> COMPOR
! In  v_info_valk      : comportment informations (character)
! In  v_info_vali      : comportment informations (integer)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_elem_affe
    aster_logical :: l_affe_all
    integer :: nb_elem_affe
    integer :: iocc, nocc
    character(len=16) :: rela_comp, defo_comp, type_comp, type_cpla, mult_comp, kit_comp(4)
    character(len=16) :: post_iter, type_matg
    integer :: nb_vari, nume_comp(4), nb_vari_exte, unit_comp, nb_vari_comp(4)
    character(len=16) :: keywordfact
    character(len=16), pointer :: v_compor_valv(:) => null()
    integer, pointer :: v_elem_affe(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    list_elem_affe = '&&COMPCOMPSAVE.LIST'
    keywordfact    = 'AFFE_COMPOR'
    call getfac(keywordfact, nocc)
!
! - Access to COMPOR <CARTE>
!
    call jeveuo(compor//'.VALV', 'E', vk16=v_compor_valv)
!
! - Read list
!
    do iocc = 1, nocc
!
! ----- Get options
!
        nume_comp(:)    = 0
        nb_vari_comp(:) = 0
        nb_vari_exte = v_info_vali(4*(iocc-1)+ 1)
        unit_comp    = v_info_vali(4*(iocc-1)+ 2)
        nb_vari      = v_info_vali(4*(iocc-1)+ 3)
        nume_comp(1) = v_info_vali(4*(iocc-1)+ 4)
        rela_comp    = v_info_valk(16*(iocc-1)+ 1)
        defo_comp    = v_info_valk(16*(iocc-1)+ 2)
        type_comp    = v_info_valk(16*(iocc-1)+ 3)
        type_cpla    = v_info_valk(16*(iocc-1)+ 4)
        kit_comp(1)  = v_info_valk(16*(iocc-1)+ 5)
        kit_comp(2)  = v_info_valk(16*(iocc-1)+ 6)
        kit_comp(3)  = v_info_valk(16*(iocc-1)+ 7)
        kit_comp(4)  = v_info_valk(16*(iocc-1)+ 8)
        mult_comp    = v_info_valk(16*(iocc-1)+ 14) 
        type_matg    = v_info_valk(16*(iocc-1)+ 15)
        post_iter    = v_info_valk(16*(iocc-1)+ 16)
!
! ----- Set options in COMPOR <CARTE>
!
        v_compor_valv(1) = rela_comp
        write (v_compor_valv(2),'(I16)') nb_vari
        v_compor_valv(3) = defo_comp
        v_compor_valv(4) = type_comp
        v_compor_valv(5) = type_cpla
        write (v_compor_valv(6),'(I16)') nume_comp(1)
        v_compor_valv(7) = mult_comp
        v_compor_valv(8) = kit_comp(1)
        v_compor_valv(9) = kit_comp(2)
        v_compor_valv(10) = kit_comp(3)
        v_compor_valv(11) = kit_comp(4)
        v_compor_valv(13) = type_matg
        v_compor_valv(14) = post_iter
        write (v_compor_valv(15),'(I16)') nume_comp(2)
        write (v_compor_valv(16),'(I16)') nume_comp(3)
        write (v_compor_valv(17),'(I16)') nb_vari_comp(1)
        write (v_compor_valv(18),'(I16)') nb_vari_comp(2)
        write (v_compor_valv(19),'(I16)') nb_vari_comp(3)
        write (v_compor_valv(20),'(I16)') nb_vari_comp(4)
!
! ----- Get list of elements where comportment is defined
!
        call comp_read_mesh(mesh          , keywordfact, iocc        ,&
                            list_elem_affe, l_affe_all , nb_elem_affe)
!
! ----- Affect in COMPOR <CARTE>
!
        if (l_affe_all) then
            call nocart(compor, 1, nb_cmp)
        else
            call jeveuo(list_elem_affe, 'L', vi = v_elem_affe)
            call nocart(compor, 3, nb_cmp, mode = 'NUM', nma = nb_elem_affe,&
                        limanu = v_elem_affe)
            call jedetr(list_elem_affe)
        endif
    end do
!
    call jedetr(compor//'.NCMP')
    call jedetr(compor//'.VALV')
!
end subroutine
