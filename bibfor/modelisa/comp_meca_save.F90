subroutine comp_meca_save(mesh            , chmate          , compor          , nb_cmp,&
                          p_info_comp_valk, p_info_comp_vali, p_info_comp_nvar)
!
    implicit none
!
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterc/zaswri.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdpmf.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: chmate
    character(len=19), intent(in) :: compor
    integer, intent(in) :: nb_cmp
    character(len=16), pointer, intent(in) :: p_info_comp_valk(:)
    integer          , pointer, intent(in) :: p_info_comp_vali(:)
    integer          , pointer, intent(in) :: p_info_comp_nvar(:)
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Save informations in COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  chmate           : name of material field
! In  compor           : name of <CARTE> COMPOR
! In  nb_cmp           : number of components in <CARTE> COMPOR
! In  p_info_comp_valk : pointer to comportment informations (character)
! In  p_info_comp_vali : pointer to comportment informations (integer)
! In  p_info_comp_nvar : pointer to comportment informations (int. vari. count)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_elem_affe
    logical :: l_affe_all
    integer :: nb_elem_affe
    integer, pointer :: p_elem_affe(:) => null()
    character(len=16) :: keywordfact
    integer :: iocc, nbocc
    character(len=8) :: typmcl(2)
    character(len=16) :: motcle(2)
    integer :: nt
    character(len=16), pointer :: p_compor_valv(:) => null()
    character(len=16) :: defo_comp, rela_comp, type_comp, type_cpla, mult_comp
    character(len=16) :: kit_comp(9), type_matg, post_iter
    logical :: l_cristal, l_zmat, l_umat, l_mfront, l_exte_comp
    logical :: l_matr_tgsc, l_crit_rupt
    logical :: l_pmf, l_is_pmf, l_zmat_open
    integer :: nume_comp, nb_vari, nb_vari_comp(9)
    integer :: nb_vari_exte, unit_comp
!
! --------------------------------------------------------------------------------------------------
!
    nbocc       = 0
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
    list_elem_affe = '&&COMPMECASAVE.LIST'
    motcle(1)   = 'GROUP_MA'
    motcle(2)   = 'MAILLE'
    typmcl(1)   = 'GROUP_MA'
    typmcl(2)   = 'MAILLE'
    l_is_pmf    = .false.
    l_zmat_open = .false.
!
! - Access to COMPOR <CARTE>
!
    call jeveuo(compor//'.VALV', 'E', vk16 = p_compor_valv)
!
! - Loop on occurrences of COMPORTEMENT
!
    do iocc = 1, nbocc
!
! ----- Get infos
!
        nb_vari_exte = p_info_comp_vali(2*(iocc-1) + 1)
        unit_comp    = p_info_comp_vali(2*(iocc-1) + 2)
        nume_comp    = p_info_comp_nvar(10*(iocc-1) + 1)
        nb_vari      = p_info_comp_nvar(10*(iocc-1) + 2)
        nb_vari_comp(1) = p_info_comp_nvar(10*(iocc-1) + 3)
        nb_vari_comp(2) = p_info_comp_nvar(10*(iocc-1) + 4)
        nb_vari_comp(3) = p_info_comp_nvar(10*(iocc-1) + 5)
        nb_vari_comp(4) = p_info_comp_nvar(10*(iocc-1) + 6)
        rela_comp    = p_info_comp_valk(16*(iocc-1) + 1)
        defo_comp    = p_info_comp_valk(16*(iocc-1) + 2)
        type_comp    = p_info_comp_valk(16*(iocc-1) + 3)
        type_cpla    = p_info_comp_valk(16*(iocc-1) + 4)
        kit_comp(1)  = p_info_comp_valk(16*(iocc-1) + 5)
        kit_comp(2)  = p_info_comp_valk(16*(iocc-1) + 6)
        kit_comp(3)  = p_info_comp_valk(16*(iocc-1) + 7)
        kit_comp(4)  = p_info_comp_valk(16*(iocc-1) + 8)
        kit_comp(5)  = p_info_comp_valk(16*(iocc-1) + 9)
        kit_comp(6)  = p_info_comp_valk(16*(iocc-1) + 10)
        kit_comp(7)  = p_info_comp_valk(16*(iocc-1) + 11)
        kit_comp(8)  = p_info_comp_valk(16*(iocc-1) + 12)
        kit_comp(9)  = p_info_comp_valk(16*(iocc-1) + 13)
        mult_comp    = p_info_comp_valk(16*(iocc-1) + 14)
        type_matg    = p_info_comp_valk(16*(iocc-1) + 15)
        post_iter    = p_info_comp_valk(16*(iocc-1) + 16)
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'MATR_TGSC', l_matr_tgsc, type_matg = type_matg)
        call comp_meca_l(rela_comp, 'CRIT_RUPT', l_crit_rupt, post_iter = post_iter)
        call comp_meca_l(rela_comp, 'CRISTAL'  , l_cristal)
        call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp)
        call comp_meca_l(rela_comp, 'UMAT'     , l_umat)
        call comp_meca_l(rela_comp, 'MFRONT'   , l_mfront)
!
! ----- Multifiber beams
!
        call comp_meca_l(rela_comp, 'PMF', l_pmf)
        if (l_pmf) l_is_pmf = .true.
!
! ----- Z-MAT
!
        call comp_meca_l(rela_comp, 'ZMAT', l_zmat)
        if (l_zmat) l_zmat_open = .true.
!
! ----- Get mesh
!
        call getvtx(keywordfact, 'TOUT', iocc = iocc, nbret = nt)
        if (nt .ne. 0) then
            l_affe_all = .true.
        else
            l_affe_all = .false.
            call reliem(' ', mesh, 'NU_MAILLE', keywordfact, iocc, &
                        2, motcle, typmcl, list_elem_affe, nb_elem_affe)
            if (nb_elem_affe .eq. 0) l_affe_all = .true.
        endif
!
! ----- Set in <CARTE>
!
        p_compor_valv(1)  = rela_comp
        write (p_compor_valv(2),'(I16)') nb_vari
        p_compor_valv(3)  = defo_comp
        p_compor_valv(4)  = type_comp
        p_compor_valv(5)  = type_cpla
        if (.not.l_pmf) then
            write (p_compor_valv(6),'(I16)') nume_comp
        endif
        if (l_cristal) then
            p_compor_valv(7)  = mult_comp
        else
            write (p_compor_valv(7),'(I16)') unit_comp
        endif
        p_compor_valv(8)  = kit_comp(1)
        p_compor_valv(9)  = kit_comp(2)
        p_compor_valv(10) = kit_comp(3)
        p_compor_valv(11) = kit_comp(4)
        if (l_exte_comp) then
            p_compor_valv(12) = kit_comp(5)
        else
            write (p_compor_valv(12),'(I16)') iocc
        endif 
        if (l_exte_comp) then
            if (l_matr_tgsc) call utmess('F','COMPOR4_59')
            if (l_crit_rupt) call utmess('F','COMPOR4_60')
            p_compor_valv(13) = kit_comp(6)
            p_compor_valv(14) = kit_comp(7)
        else
            p_compor_valv(13) = type_matg
            p_compor_valv(14) = post_iter
        endif
        p_compor_valv(15) = kit_comp(8)
        p_compor_valv(16) = kit_comp(9)
        write (p_compor_valv(17),'(I16)') nb_vari_comp(1)
        write (p_compor_valv(18),'(I16)') nb_vari_comp(2)
        write (p_compor_valv(19),'(I16)') nb_vari_comp(3)
        write (p_compor_valv(20),'(I16)') nb_vari_comp(4)
!
! ----- Affect in <CARTE>
!
        if (l_affe_all) then
            call nocart(compor, 1, nb_cmp)
        else
            call jeveuo(list_elem_affe, 'L', vi = p_elem_affe)
            call nocart(compor, 3, nb_cmp, mode = 'NUM', nma = nb_elem_affe,&
                        limanu = p_elem_affe)
            call jedetr(list_elem_affe)
        endif
    enddo
!
! - Compor <CARTE> fusing
!
    if (l_is_pmf) call nmdpmf(compor, chmate)
!
! - Init ZASTER_HANDLER
!
    if (l_zmat_open) call zaswri()
!
    call jedetr(compor//'.NCMP')
    call jedetr(compor//'.VALV')
!
end subroutine

