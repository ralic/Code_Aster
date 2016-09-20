subroutine comp_nbvari(rela_comp    , defo_comp    , type_cpla    , kit_comp_ ,&
                       type_matg_   , post_iter_   , mult_comp_   , libr_name_,&
                       subr_name_   , model_dim_   , model_mfront_, nb_vari_  ,&
                       nb_vari_umat_, nb_vari_comp_, nume_comp_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_nbvari_std.h"
#include "asterfort/comp_nbvari_kit.h"
#include "asterfort/comp_nbvari_ext.h"
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
    character(len=16), intent(in) :: rela_comp
    character(len=16), intent(in) :: defo_comp
    character(len=16), intent(in) :: type_cpla
    character(len=16), optional, intent(in) :: kit_comp_(4)
    character(len=16), optional, intent(in) :: type_matg_
    character(len=16), optional, intent(in) :: post_iter_
    character(len=16), optional, intent(in) :: mult_comp_
    character(len=255), optional, intent(in) :: libr_name_
    character(len=255), optional, intent(in) :: subr_name_
    integer, optional, intent(in) :: model_dim_
    character(len=16), optional, intent(in) :: model_mfront_
    integer, optional, intent(out) :: nb_vari_
    integer, optional, intent(in) :: nb_vari_umat_
    integer, optional, intent(out) :: nb_vari_comp_(4)
    integer, optional, intent(out) :: nume_comp_(4)
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Count the number of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_comp        : RELATION comportment
! In  defo_comp        : DEFORMATION comportment
! In  type_cpla        : plane stress method
! Out nb_vari          : number of internal variables
! In  kit_comp         : KIT comportment
! In  type_matg        : type of tangent matrix
! In  post_iter        : type of post_treatment
! In  mult_comp        : multi-comportment
! In  nb_vari_umat     : number of internal variables for UMAT
! In  libr_name        : name of library if UMAT or MFront
! In  subr_name        : name of comportement in library if UMAT or MFront
! In  model_dim        : dimension of modelisation (2D or 3D)
! In  model_mfront     : type of modelisation MFront
! Out nb_vari_comp     : number of internal variables kit comportment
! Out nume_comp        : number LCxxxx subroutine
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_vari_rela, nb_vari
    aster_logical :: l_cristal, l_kit_meta, l_kit_thm, l_kit_ddi, l_kit_cg, l_exte_comp
    aster_logical :: l_kit, l_meca_mfront
    aster_logical :: l_mfront_proto, l_mfront_offi, l_umat
    character(len=16) :: kit_comp(4), type_matg, post_iter, mult_comp
    integer :: nb_vari_exte, nume_comp(4)=0, nb_vari_comp(4)=0
    integer :: nb_vari_umat, model_dim
    character(len=255) :: libr_name, subr_name
    character(len=16) :: model_mfront
!
! --------------------------------------------------------------------------------------------------
!
    kit_comp(1:4) = 'VIDE'
    type_matg     = 'VIDE'
    post_iter     = 'VIDE'
    mult_comp     = 'VIDE'
    nb_vari_umat  = 0
    nb_vari_exte  = 0
    nb_vari       = 0
    if (present(kit_comp_)) then
        kit_comp(1:4) = kit_comp_(1:4)
    endif
    if (present(type_matg_)) then
        type_matg = type_matg_
    endif
    if (present(post_iter_)) then
        post_iter = post_iter_
    endif
    if (present(mult_comp_)) then
        mult_comp = mult_comp_
    endif
    if (present(nb_vari_umat_)) then
        nb_vari_umat = nb_vari_umat_
    endif
    if (present(libr_name_)) then
        libr_name = libr_name_
    endif
    if (present(subr_name_)) then
        subr_name = subr_name_
    endif
    if (present(model_dim_)) then
        model_dim = model_dim_
    endif
    if (present(model_mfront_)) then
        model_mfront = model_mfront_
    endif
!
! - Detection of specific cases
!
    call comp_meca_l(rela_comp, 'KIT'         , l_kit)
    call comp_meca_l(rela_comp, 'CRISTAL'     , l_cristal)
    call comp_meca_l(rela_comp, 'KIT_META'    , l_kit_meta)
    call comp_meca_l(rela_comp, 'KIT_THM'     , l_kit_thm)
    call comp_meca_l(rela_comp, 'KIT_DDI'     , l_kit_ddi)
    call comp_meca_l(rela_comp, 'KIT_CG'      , l_kit_cg)
    call comp_meca_l(rela_comp, 'EXTE_COMP'   , l_exte_comp)
    call comp_meca_l(rela_comp, 'MFRONT_PROTO', l_mfront_proto)
    call comp_meca_l(rela_comp, 'MFRONT_OFFI' , l_mfront_offi)
    call comp_meca_l(rela_comp, 'UMAT'        , l_umat)
!
! - Get number of internal variables for standard laws
!
    call comp_nbvari_std(rela_comp, defo_comp, type_cpla   , nb_vari  ,&
                         kit_comp , type_matg, post_iter   , mult_comp,&
                         l_cristal, nume_comp, nb_vari_rela)
!
! - Get number of internal variables for KIT
!
    if (l_kit) then
        call comp_nbvari_kit(kit_comp  , defo_comp   , nb_vari_rela, &
                             l_kit_meta, l_kit_thm   , l_kit_ddi   , l_kit_cg,&
                             nb_vari   , nb_vari_comp, nume_comp   , l_meca_mfront)
        if (l_meca_mfront) then
            l_mfront_proto = .true._1
            l_exte_comp    = .true._1
        endif
    endif
!
! - Get number of internal variables for external comportments
!
    if (l_exte_comp) then
        call comp_nbvari_ext(l_umat        , nb_vari_umat ,&
                             l_mfront_proto, l_mfront_offi,&
                             libr_name     , subr_name    ,&
                             model_dim     , model_mfront ,&
                             nb_vari_exte)
        if (l_meca_mfront) then
            nb_vari_comp(4) = nb_vari_exte
        endif
    endif
!
! - Total number of internal variables
!
    nb_vari = nb_vari_exte + nb_vari
!
! - Output
!
    if (present(nb_vari_)) then
        nb_vari_ = nb_vari
    endif
    if (present(nume_comp_)) then
        nume_comp_(1:4) = nume_comp(1:4)
    endif
    if (present(nb_vari_comp_)) then
        nb_vari_comp_(1:4) = nb_vari_comp(1:4)
    endif
!
end subroutine
