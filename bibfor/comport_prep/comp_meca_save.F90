subroutine comp_meca_save(model         , mesh            , chmate          , compor, nb_cmp,&
                          info_comp_valk, info_comp_nvar)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_read_mesh.h"
#include "asterfort/dismoi.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmdpmf.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: chmate
    character(len=19), intent(in) :: compor
    integer, intent(in) :: nb_cmp
    character(len=16), intent(in) :: info_comp_valk(:)
    integer, intent(in) :: info_comp_nvar(:)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Save informations in COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  mesh             : name of mesh
! In  chmate           : name of material field
! In  compor           : name of <CARTE> COMPOR
! In  nb_cmp           : number of components in <CARTE> COMPOR
! In  info_comp_valk   : comportment informations (character)
! In  info_comp_nvar   : comportment informations (int. vari. count)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_elem_affe
    aster_logical :: l_affe_all
    integer :: nb_elem_affe, nb_model_affe
    integer, pointer :: v_elem_affe(:) => null()
    integer, pointer :: v_model_elem(:) => null()
    integer :: i_elem_affe
    character(len=19) :: ligrmo
    character(len=16) :: keywordfact
    integer :: iocc, nbocc
    character(len=16), pointer :: v_compor_valv(:) => null()
    character(len=16) :: defo_comp, rela_comp, type_comp, type_cpla, mult_comp
    character(len=16) :: kit_comp(4), type_matg, post_iter
    aster_logical :: l_cristal, l_umat, l_mfront
    aster_logical :: l_pmf, l_is_pmf
    integer :: nb_vari, nb_vari_comp(4), elem_nume, nume_comp(4)
!
! --------------------------------------------------------------------------------------------------
!
    nbocc = 0
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
    list_elem_affe = '&&COMPMECASAVE.LIST'
    l_is_pmf  = .false.
    ligrmo    =  model//'.MODELE'
!
! - Access to MODEL
!
    call jeveuo(model//'.MAILLE', 'L', vi=v_model_elem)
!
! - Access to COMPOR <CARTE>
!
    call jeveuo(compor//'.VALV', 'E', vk16 = v_compor_valv)
!
! - Loop on occurrences of COMPORTEMENT
!
    do iocc = 1, nbocc
!
! ----- Get infos
!
        nb_vari         = info_comp_nvar(10*(iocc-1) + 1)
        nb_vari_comp(1) = info_comp_nvar(10*(iocc-1) + 2)
        nb_vari_comp(2) = info_comp_nvar(10*(iocc-1) + 3)
        nb_vari_comp(3) = info_comp_nvar(10*(iocc-1) + 4)
        nb_vari_comp(4) = info_comp_nvar(10*(iocc-1) + 5)
        nume_comp(1)    = info_comp_nvar(10*(iocc-1) + 6)
        nume_comp(2)    = info_comp_nvar(10*(iocc-1) + 7)
        nume_comp(3)    = info_comp_nvar(10*(iocc-1) + 8)
        nume_comp(4)    = info_comp_nvar(10*(iocc-1) + 9)
        rela_comp       = info_comp_valk(16*(iocc-1) + 1)
        defo_comp       = info_comp_valk(16*(iocc-1) + 2)
        type_comp       = info_comp_valk(16*(iocc-1) + 3)
        type_cpla       = info_comp_valk(16*(iocc-1) + 4)
        kit_comp(1)     = info_comp_valk(16*(iocc-1) + 5)
        kit_comp(2)     = info_comp_valk(16*(iocc-1) + 6)
        kit_comp(3)     = info_comp_valk(16*(iocc-1) + 7)
        kit_comp(4)     = info_comp_valk(16*(iocc-1) + 8)
        mult_comp       = info_comp_valk(16*(iocc-1) + 14)
        type_matg       = info_comp_valk(16*(iocc-1) + 15)
        post_iter       = info_comp_valk(16*(iocc-1) + 16)
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'CRISTAL', l_cristal)
        call comp_meca_l(rela_comp, 'UMAT'   , l_umat)
        call comp_meca_l(rela_comp, 'MFRONT' , l_mfront)
!
! ----- Multifiber beams
!
        call comp_meca_l(rela_comp, 'PMF', l_pmf)
        if (l_pmf) then
            l_is_pmf = .true.
        endif
!
! ----- Get elements
!
        call comp_read_mesh(mesh          , keywordfact, iocc        ,&
                            list_elem_affe, l_affe_all , nb_elem_affe)
!
! ----- Check if elements belong to model
!
        nb_model_affe = 0
        if (nb_elem_affe .ne. 0) then
            call jeveuo(list_elem_affe, 'L', vi = v_elem_affe)
            do i_elem_affe = 1, nb_elem_affe
                elem_nume = v_elem_affe(i_elem_affe)
                if (v_model_elem(elem_nume) .ne. 0) then
                    nb_model_affe = nb_model_affe + 1
                endif
            end do
        endif
        if (.not.l_affe_all) then
            if (nb_model_affe.eq.0) then
                call utmess('A', 'COMPOR4_72', si = iocc)
            endif
        endif
!
! ----- Set in <CARTE>
!
        v_compor_valv(1) = rela_comp
        write (v_compor_valv(2),'(I16)') nb_vari
        v_compor_valv(3) = defo_comp
        v_compor_valv(4) = type_comp
        v_compor_valv(5) = type_cpla
        if (.not.l_pmf) then
            write (v_compor_valv(6),'(I16)') nume_comp(1)
        endif
        v_compor_valv(7) = mult_comp
        v_compor_valv(8) = kit_comp(1)
        v_compor_valv(9) = kit_comp(2)
        v_compor_valv(10) = kit_comp(3)
        v_compor_valv(11) = kit_comp(4)
        write (v_compor_valv(12),'(I16)') iocc
        v_compor_valv(13) = type_matg
        v_compor_valv(14) = post_iter
        write (v_compor_valv(15),'(I16)') nume_comp(2)
        write (v_compor_valv(16),'(I16)') nume_comp(3)
        write (v_compor_valv(17),'(I16)') nb_vari_comp(1)
        write (v_compor_valv(18),'(I16)') nb_vari_comp(2)
        write (v_compor_valv(19),'(I16)') nb_vari_comp(3)
        write (v_compor_valv(20),'(I16)') nb_vari_comp(4)
!
! ----- Affect in <CARTE>
!
        if (l_affe_all) then
            call nocart(compor, 1, nb_cmp)
        else
            call jeveuo(list_elem_affe, 'L', vi = v_elem_affe)
            call nocart(compor, 3, nb_cmp, mode = 'NUM', nma = nb_elem_affe,&
                        limanu = v_elem_affe)
            call jedetr(list_elem_affe)
        endif
    enddo
!
! - Compor <CARTE> fusing for multifiber beams
!
    if (l_is_pmf) then
        call nmdpmf(compor, chmate)
    endif
!
    call jedetr(compor//'.NCMP')
    call jedetr(compor//'.VALV')
!
end subroutine
