subroutine comp_comp_save(mesh, compor, nb_cmp, list_vale)
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
    character(len=19), intent(in) :: list_vale
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (AFFE_MATERIAU)
!
! Save informations in COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh        : name of mesh
! In  compor      : name of <CARTE> COMPOR
! In  nb_cmp      : number of components in <CARTE> COMPOR
! In  list_vale   : list of informations to save
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_elem_affe
    aster_logical :: l_affe_all
    integer :: nb_elem_affe
    integer :: iocc, nocc
    character(len=16) :: rela_comp, defo_comp, type_comp, type_cpla, mult_comp
    integer :: nb_vari, nume_comp, nb_vari_exte, unit_comp
    character(len=16) :: keywordfact
    character(len=16), pointer :: valv(:) => null()
    integer, pointer :: vali(:) => null()
    character(len=24), pointer :: valk(:) => null()
    integer, pointer :: v_elem_affe(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    list_elem_affe = '&&COMPCOMPSAVE.LIST'

    keywordfact = 'AFFE_COMPOR'
    call getfac(keywordfact, nocc)
!
! - Access to COMPOR <CARTE>
!
    call jeveuo(compor//'.VALV', 'E', vk16=valv)
!
! - Access to list
!
    call jeveuo(list_vale(1:19)//'.VALI', 'L', vi=vali)
    call jeveuo(list_vale(1:19)//'.VALK', 'L', vk24=valk)
!
! - Read list
!
    do iocc = 1, nocc
!
! ----- Get options
!
        nb_vari_exte = vali(1+4*(iocc-1) -1 + 1)
        unit_comp = vali(1+4*(iocc-1) -1 + 2)
        nb_vari = vali(1+4*(iocc-1) -1 + 3)
        nume_comp = vali(1+4*(iocc-1) -1 + 4)
        rela_comp = valk(1+16*(iocc-1) -1 + 1)(1:16)
        defo_comp = valk(1+16*(iocc-1) -1 + 2)(1:16)
        type_comp = valk(1+16*(iocc-1) -1 + 3)(1:16)
        type_cpla = valk(1+16*(iocc-1) -1 + 4)(1:16)
        mult_comp = valk(1+16*(iocc-1) -1 + 14)(1:16)
!
! ----- Set options in COMPOR <CARTE>
!
        valv(1) = rela_comp
        write (valv(2),'(I16)') nb_vari
        valv(3) = defo_comp
        valv(4) = type_comp
        valv(5) = type_cpla
        write (valv(6),'(I16)') nume_comp
        valv(7) = mult_comp
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
