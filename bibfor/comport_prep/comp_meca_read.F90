subroutine comp_meca_read(l_etat_init, ds_compor_prep, model)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getexm.h"
#include "asterc/mfront_get_nbvari.h"
#include "asterfort/deprecated_behavior.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/comp_meca_incr.h"
#include "asterfort/comp_read_typmod.h"
#include "asterfort/comp_meca_rkit.h"
#include "asterfort/comp_read_exte.h"
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
    aster_logical, intent(in) :: l_etat_init
    type(NL_DS_ComporPrep), intent(inout) :: ds_compor_prep
    character(len=8), intent(in), optional :: model
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Read informations from command file
!
! --------------------------------------------------------------------------------------------------
!
! In  l_etat_init      : .true. if initial state is defined
! IO  ds_compor_prep   : datastructure to prepare comportement
! In  model            : name of model
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: mesh = ' '
    character(len=16) :: keywordfact
    integer :: i_comp, nb_comp, model_dim, iret
    character(len=16) :: defo_comp, rela_comp, type_cpla, mult_comp, type_comp
    character(len=16) :: type_matg, post_iter, model_mfront
    character(len=16) :: kit_comp(4)
    character(len=255) :: libr_name, subr_name
    integer :: unit_comp, nb_vari_umat, nb_vari_mfront
    aster_logical :: l_cristal, l_umat, l_mfront_proto, l_mfront_offi, l_kit_thm, l_kit
    integer, pointer :: v_model_elem(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    keywordfact = 'COMPORTEMENT'
    nb_comp     = ds_compor_prep%nb_comp
    mesh        = ' '
!
! - Pointer to list of elements in model
!
    if ( present(model) ) then
        call jeveuo(model//'.MAILLE', 'L', vi = v_model_elem)
        call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
    endif
!
! - Read informations
!
    do i_comp = 1, nb_comp
!
        libr_name      = ' '
        subr_name      = ' '
        model_mfront   = ' '
        model_dim      = 0
        nb_vari_umat   = 0
        nb_vari_mfront = 0
        unit_comp      = 0
        rela_comp      = 'VIDE'
        defo_comp      = 'VIDE'
        mult_comp      = ' '
        type_cpla      = 'VIDE'
        libr_name      = ' '
        type_matg      = ' '
        post_iter      = ' '
        kit_comp(1:4)  = 'VIDE'
!
! ----- Get RELATION from command file
!
        call getvtx(keywordfact, 'RELATION', iocc = i_comp, scal = rela_comp)
        call deprecated_behavior(rela_comp)
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'KIT'    , l_kit)
        call comp_meca_l(rela_comp, 'KIT_THM', l_kit_thm)
        call comp_meca_l(rela_comp, 'CRISTAL', l_cristal)
!
! ----- Get DEFORMATION from command file
!
        call getvtx(keywordfact, 'DEFORMATION', iocc = i_comp, scal = defo_comp)
!
! ----- Modified matrix
!
        if (getexm(keywordfact,'TYPE_MATR_TANG') .eq. 1) then
            call getvtx(keywordfact, 'TYPE_MATR_TANG', iocc = i_comp, scal=type_matg, nbret=iret)
            if (iret .eq. 0) then
                type_matg = ' '
            endif
        endif
!
! ----- Damage post-treatment
!
        if (getexm(keywordfact,'POST_ITER') .eq. 1) then
            call getvtx(keywordfact, 'POST_ITER', iocc = i_comp, scal=post_iter, nbret=iret)
            if (iret .eq. 0) then
                post_iter = ' '
            endif
        endif
!
! ----- For KIT
!
        if (l_kit) then
            call comp_meca_rkit(keywordfact, i_comp, rela_comp, kit_comp)
        endif
!
! ----- Get parameters for external programs (MFRONT/UMAT)
!
        call comp_read_exte(rela_comp  , kit_comp      ,&
                            l_umat     , l_mfront_proto, l_mfront_offi,&
                            libr_name  , subr_name     ,&
                            keywordfact, i_comp        , nb_vari_umat)
!
! ----- Get multi-comportment *CRISTAL
!
        if (l_cristal) then
            call getvid(keywordfact, 'COMPOR', iocc = i_comp, scal = mult_comp)
        endif
!
! ----- Get model for MFRONT
!
        if (l_mfront_proto .or. l_mfront_offi) then
            call comp_read_typmod(mesh       , v_model_elem,&
                                  keywordfact, i_comp      , rela_comp,&
                                  model_dim  , model_mfront, type_cpla)
        endif
!
! ----- Get number of internal variables
!
        if (l_mfront_proto .or. l_mfront_offi) then
            call mfront_get_nbvari(libr_name, subr_name, model_mfront, model_dim, nb_vari_mfront)
            if ( nb_vari_mfront .eq. 0 ) then
                nb_vari_mfront = 1
            endif
        endif
!
! ----- Select type of comportment (incremental or total)
!
        call comp_meca_incr(rela_comp, defo_comp, type_comp, l_etat_init)
!
! ----- Save parameters in list
!
        ds_compor_prep%v_comp(i_comp)%rela_comp      = rela_comp
        ds_compor_prep%v_comp(i_comp)%defo_comp      = defo_comp
        ds_compor_prep%v_comp(i_comp)%type_comp      = type_comp
        ds_compor_prep%v_comp(i_comp)%type_cpla      = type_cpla
        ds_compor_prep%v_comp(i_comp)%kit_comp(:)    = kit_comp(:)
        ds_compor_prep%v_comp(i_comp)%mult_comp      = mult_comp
        ds_compor_prep%v_comp(i_comp)%type_matg      = type_matg
        ds_compor_prep%v_comp(i_comp)%post_iter      = post_iter
        ds_compor_prep%v_exte(i_comp)%libr_name      = libr_name 
        ds_compor_prep%v_exte(i_comp)%subr_name      = subr_name
        ds_compor_prep%v_exte(i_comp)%nb_vari_umat   = nb_vari_umat
        ds_compor_prep%v_exte(i_comp)%nb_vari_mfront = nb_vari_mfront
        ds_compor_prep%v_exte(i_comp)%model_mfront   = model_mfront
        ds_compor_prep%v_exte(i_comp)%model_dim      = model_dim
        ds_compor_prep%v_comp(i_comp)%nb_vari_exte   = max(nb_vari_mfront, nb_vari_umat)
    end do
!
end subroutine
