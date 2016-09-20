subroutine comp_ntvari(model_ , compor_cart_, compor_list_, compor_info,&
                       nt_vari, nb_vari_maxi, nb_zone     , v_exte)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexatr.h"
#include "asterfort/comp_nbvari.h"
#include "asterfort/comp_mfront_modelem.h"
#include "asterfort/comp_read_exte.h"
#include "asterfort/teattr.h"
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
! aslint: disable=W1003
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), optional, intent(in) :: model_
    character(len=19), optional, intent(in) :: compor_cart_
    character(len=16), optional, intent(in) :: compor_list_(20)
    character(len=19), intent(in) :: compor_info
    integer, intent(out) :: nt_vari
    integer, intent(out) :: nb_vari_maxi
    integer, intent(out) :: nb_zone
    type(NL_DS_ComporExte), pointer, intent(out) :: v_exte(:)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Count total of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  compor_cart      : name of <CARTE> COMPOR
! In  compor_list      : name of list of COMPOR (for SIMU_POINT_MAT)
! Out nt_vari          : total number of internal variables (on all <CARTE> COMPOR)
! Out nb_vari_maxi     : maximum number of internal variables on all comportments"
! Out nb_zone          : number of affected zones
! Out v_exte           : pointer to external constitutive laws parameters
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_mfront_cp, l_mfront_proto, l_umat, l_mfront_offi
    integer, pointer :: v_model_elem(:) => null()
    character(len=16), pointer :: v_compor_vale(:) => null()
    integer, pointer :: v_zone(:) => null()
    integer, pointer :: v_compor_desc(:) => null()
    integer, pointer :: v_compor_lima(:) => null()
    integer, pointer :: v_compor_lima_lc(:) => null()
    integer :: nb_vale, nb_cmp_max, nb_vari, nb_elem, nb_elem_mesh
    integer :: i_zone, codret, iret, i_elem, posit
    integer :: type_affe, indx_affe, elem_type_nume, elem_nume, model_dim
    character(len=16) :: elem_type_name
    character(len=16) :: type_matg, post_iter
    character(len=16) :: rela_comp, defo_comp, mult_comp, kit_comp(4), type_cpla
    character(len=16) :: model_mfront
    character(len=255) :: libr_name, subr_name
    character(len=16) :: principal
    character(len=8) :: mesh
!
! --------------------------------------------------------------------------------------------------
!
    nt_vari      = 0
    nb_vari_maxi = 0
    nb_zone      = 0
    v_exte       => null()
    if (present(model_)) then
        call jeveuo(model_//'.MAILLE', 'L', vi = v_model_elem)
    endif
!
! - Access to <CARTE> COMPOR
!
    if (present(compor_cart_)) then
        call jeveuo(compor_cart_//'.DESC', 'L', vi   = v_compor_desc)
        call jeveuo(compor_cart_//'.VALE', 'L', vk16 = v_compor_vale)
        call jelira(compor_cart_//'.VALE', 'LONMAX', nb_vale)
        call jeveuo(jexnum(compor_cart_//'.LIMA', 1), 'L', vi = v_compor_lima)
        call jeveuo(jexatr(compor_cart_//'.LIMA', 'LONCUM'), 'L', vi = v_compor_lima_lc)
        nb_zone    = v_compor_desc(3)
        nb_cmp_max = nb_vale/v_compor_desc(2)
        call dismoi('NOM_MAILLA'  , compor_cart_, 'CARTE'   , repk=mesh)
        call dismoi('NB_MA_MAILLA', mesh        , 'MAILLAGE', repi=nb_elem_mesh)
    else if (present(compor_list_)) then
        nb_zone      = 1
        nb_cmp_max   = 0
        nb_elem_mesh = 1
    endif
!
! - Create list of zones: for each zone (in CARTE), how many elements 
!
    call jeveuo(compor_info(1:19)//'.ZONE', 'L', vi = v_zone)
!
! - Prepare objects for external constitutive laws
!
    allocate(v_exte(nb_zone))
!
! - Count internal variables by comportment
!
    do i_zone = 1, nb_zone
!
        subr_name    = ' '
        libr_name    = ' '
        model_mfront = ' '
        model_dim    = 0
!
! ----- Get parameters
!
        if (present(compor_cart_)) then
            rela_comp   = v_compor_vale(nb_cmp_max*(i_zone-1)+1)
            defo_comp   = v_compor_vale(nb_cmp_max*(i_zone-1)+3)
            type_cpla   = v_compor_vale(nb_cmp_max*(i_zone-1)+5)
            mult_comp   = v_compor_vale(nb_cmp_max*(i_zone-1)+7)
            kit_comp(1) = v_compor_vale(nb_cmp_max*(i_zone-1)+8)
            kit_comp(2) = v_compor_vale(nb_cmp_max*(i_zone-1)+9)
            kit_comp(3) = v_compor_vale(nb_cmp_max*(i_zone-1)+10)
            kit_comp(4) = v_compor_vale(nb_cmp_max*(i_zone-1)+11)
            type_matg   = v_compor_vale(nb_cmp_max*(i_zone-1)+13)
            post_iter   = v_compor_vale(nb_cmp_max*(i_zone-1)+14)
        else
            rela_comp   = compor_list_(1)
            defo_comp   = compor_list_(3)
            type_cpla   = compor_list_(5)
            mult_comp   = compor_list_(7)
            kit_comp(1) = compor_list_(8)
            kit_comp(2) = compor_list_(9)
            kit_comp(3) = compor_list_(10)
            kit_comp(4) = compor_list_(11)
            type_matg   = compor_list_(13)
            post_iter   = compor_list_(14)
        endif
!
! ----- Find right TYPELEM
!
        if (present(compor_cart_)) then
            type_affe = v_compor_desc(1+3+(i_zone-1)*2)
            indx_affe = v_compor_desc(1+4+(i_zone-1)*2)
            if (type_affe .eq. 3) then
                nb_elem   = v_compor_lima_lc(1+indx_affe)-v_compor_lima_lc(indx_affe)
                posit     = v_compor_lima_lc(indx_affe)
            elseif (type_affe .eq. 1) then
                nb_elem   = nb_elem_mesh
                posit     = 0
            else
                ASSERT(.false.)
            endif
        else
            type_affe = 0
            nb_elem   = 1
            ASSERT(i_zone .eq. 1)
        endif 
        do i_elem = 1, nb_elem
            if (type_affe .eq. 3) then
                elem_nume = v_compor_lima(posit+i_elem-1)
            elseif (type_affe .eq. 1) then
                elem_nume = i_elem
            elseif (type_affe .eq. 0) then
                elem_nume = 1
            else
                ASSERT(.false.)
            endif
            if (elem_nume .ne. 0 .and. type_affe .gt. 0) then
                elem_type_nume = v_model_elem(elem_nume)
                if (elem_type_nume .ne. 0) then
                    call jenuno(jexnum('&CATA.TE.NOMTE', elem_type_nume), elem_type_name)
                    call teattr('C', 'PRINCIPAL'      , principal  , iret, typel = elem_type_name)
                    if (principal .eq. 'OUI') then
                        goto 20
                    endif
                endif
            endif
        end do
!
    20  continue
!
! ----- Get parameters for external programs (MFRONT/UMAT)
!
        call comp_read_exte(rela_comp, kit_comp ,&
                            l_umat   , l_mfront_proto , l_mfront_offi,&
                            libr_name, subr_name)
        l_mfront_cp = type_cpla .eq. 'ANALYTIQUE' .and. (l_mfront_proto .or. l_mfront_offi)
!
! ----- Get number of internal variables for MFRONT
!
        if (l_mfront_offi) then
            if (present(model_)) then
                elem_type_nume = v_model_elem(elem_nume)
                if (elem_type_nume .ne. 0 .and. libr_name .ne. ' ') then
                    call jenuno(jexnum('&CATA.TE.NOMTE', elem_type_nume), elem_type_name)
                    call comp_mfront_modelem(elem_type_name, l_mfront_cp ,&
                                             model_dim     , model_mfront,&
                                             codret)
                    ASSERT(codret .eq. 0)
                endif
            else
                model_dim    = 3
                model_mfront = '_Tridimensional'
            endif
        endif
!
! ----- Save parameters for external constitutive laws
!
        v_exte(i_zone)%l_umat         = l_umat
        v_exte(i_zone)%l_mfront_proto = l_mfront_proto
        v_exte(i_zone)%l_mfront_offi  = l_mfront_offi
        v_exte(i_zone)%subr_name      = subr_name
        v_exte(i_zone)%libr_name      = libr_name
        v_exte(i_zone)%model_mfront   = model_mfront
        v_exte(i_zone)%model_dim      = model_dim
        v_exte(i_zone)%nb_vari_umat   = 0
!
! ----- Get number of internal variables
!
        call comp_nbvari(rela_comp, defo_comp, type_cpla   , kit_comp ,&
                         type_matg, post_iter, mult_comp   , libr_name,&
                         subr_name, model_dim, model_mfront, nb_vari)
        nt_vari      = nt_vari+nb_vari
        nb_vari_maxi = max(nb_vari_maxi,nb_vari)
    end do
!
end subroutine
