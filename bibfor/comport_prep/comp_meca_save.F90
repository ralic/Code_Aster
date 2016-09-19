subroutine comp_meca_save(model         , mesh, chmate, compor, nb_cmp,&
                          ds_compor_prep)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getexm.h"
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
    type(NL_DS_ComporPrep), intent(in) :: ds_compor_prep
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
! In  ds_compor_prep   : datastructure to prepare comportement
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
    integer :: i_comp, nb_comp
    character(len=16), pointer :: v_compor_valv(:) => null()
    character(len=16) :: defo_comp, rela_comp, type_comp, type_cpla, mult_comp
    character(len=16) :: kit_comp(4), type_matg, post_iter
    aster_logical :: l_cristal, l_pmf, l_is_pmf
    integer :: nb_vari, nb_vari_comp(4), elem_nume, nume_comp(4)
!
! --------------------------------------------------------------------------------------------------
!
    keywordfact    = 'COMPORTEMENT'
    list_elem_affe = '&&COMPMECASAVE.LIST'
    nb_comp        = ds_compor_prep%nb_comp
    l_is_pmf       = .false.
    ligrmo         =  model//'.MODELE'
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
    do i_comp = 1, nb_comp
!
! ----- Get infos
!
        nb_vari         = ds_compor_prep%v_comp(i_comp)%nb_vari
        nb_vari_comp(:) = ds_compor_prep%v_comp(i_comp)%nb_vari_comp(:)
        nume_comp(:)    = ds_compor_prep%v_comp(i_comp)%nume_comp(:)
        rela_comp       = ds_compor_prep%v_comp(i_comp)%rela_comp
        defo_comp       = ds_compor_prep%v_comp(i_comp)%defo_comp
        type_comp       = ds_compor_prep%v_comp(i_comp)%type_comp
        type_cpla       = ds_compor_prep%v_comp(i_comp)%type_cpla
        kit_comp(:)     = ds_compor_prep%v_comp(i_comp)%kit_comp(:)
        mult_comp       = ds_compor_prep%v_comp(i_comp)%mult_comp
        type_matg       = ds_compor_prep%v_comp(i_comp)%type_matg
        post_iter       = ds_compor_prep%v_comp(i_comp)%post_iter
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'CRISTAL', l_cristal)
        call comp_meca_l(rela_comp, 'PMF'    , l_pmf)
!
! ----- Multifiber beams
!
        if (l_pmf) then
            l_is_pmf = .true.
        endif
!
! ----- Get elements
!
        call comp_read_mesh(mesh          , keywordfact, i_comp        ,&
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
                call utmess('A', 'COMPOR4_72', si = i_comp)
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
