subroutine carc_save(model, mesh, carcri, nb_cmp, ds_compor_para)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getexm.h"
#include "asterfort/getvtx.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_meca_mod.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exicp.h"
#include "asterfort/mfront_get_libname.h"
#include "asterfort/mfront_get_function.h"
#include "asterfort/jedetr.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdocv.h"
#include "asterfort/nocart.h"
#include "asterfort/comp_meca_rkit.h"
#include "asterfort/comp_read_exte.h"
#include "asterfort/comp_read_mesh.h"
#include "asterfort/utlcal.h"
#include "asterc/mfront_set_double_parameter.h"
#include "asterc/mfront_set_integer_parameter.h"
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
    character(len=19), intent(in) :: carcri
    integer, intent(in) :: nb_cmp
    type(NL_DS_ComporParaPrep), intent(in) :: ds_compor_para
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Save informations in <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  carcri           : name of <CARTE> CARCRI
! In  nb_cmp           : number of components in <CARTE> CARCRI
! In  ds_compor_para   : datastructure to prepare parameters for constitutive laws
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_elem_affe
    aster_logical :: l_affe_all
    integer :: nb_elem_affe, model_dim
    integer, pointer :: v_elem_affe(:) => null()
    character(len=16) :: keywordfact
    integer :: i_comp, nb_comp
    real(kind=8), pointer :: p_carc_valv(:) => null()
    character(len=16) :: algo_inte, rela_comp, model_mfront
    character(len=255) :: libr_name, subr_name
    real(kind=8) :: iter_inte_maxi, resi_inte_rela, parm_theta, vale_pert_rela, algo_inte_r
    real(kind=8) :: resi_deborst_max, seuil, amplitude, taux_retour, parm_alpha
    real(kind=8) :: post_iter, post_incr
    character(len=16) :: kit_comp(9) = (/' ',' ',' ',' ',' ',' ',' ',' ',' '/)
    integer :: type_matr_t, iter_inte_pas, iter_deborst_max
    aster_logical :: plane_stress, l_mfront, l_mfront_offi, l_umat, l_kit_thm, l_kit
!
! --------------------------------------------------------------------------------------------------
!

!
! - Initializations
!
    keywordfact    = 'COMPORTEMENT'
    nb_comp        = ds_compor_para%nb_comp
    list_elem_affe = '&&CARCSAVE.LIST'
!
! - Access to <CARTE>
!
    call jeveuo(carcri//'.VALV', 'E', vr = p_carc_valv)
!
! - Loop on occurrences of COMPORTEMENT
!
    do i_comp = 1, nb_comp
!
! ----- Get infos
!
        type_matr_t      = ds_compor_para%v_para(i_comp)%type_matr_t
        parm_theta       = ds_compor_para%v_para(i_comp)%parm_theta
        iter_inte_pas    = ds_compor_para%v_para(i_comp)%iter_inte_pas
        vale_pert_rela   = ds_compor_para%v_para(i_comp)%vale_pert_rela
        resi_deborst_max = ds_compor_para%v_para(i_comp)%resi_deborst_max
        iter_deborst_max = ds_compor_para%v_para(i_comp)%iter_deborst_max
        seuil            = ds_compor_para%v_para(i_comp)%seuil
        amplitude        = ds_compor_para%v_para(i_comp)%amplitude
        taux_retour      = ds_compor_para%v_para(i_comp)%taux_retour
        post_iter        = ds_compor_para%v_para(i_comp)%post_iter
        parm_alpha       = ds_compor_para%v_para(i_comp)%parm_alpha
        post_incr        = ds_compor_para%v_para(i_comp)%post_incr
        rela_comp        = ds_compor_para%v_para(i_comp)%rela_comp
        algo_inte        = ds_compor_para%v_para(i_comp)%algo_inte
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'KIT'    , l_kit)
        call comp_meca_l(rela_comp, 'KIT_THM', l_kit_thm)
!
! ----- Get list of elements where comportment is defined
!
        call comp_read_mesh(mesh          , keywordfact, i_comp        ,&
                            list_elem_affe, l_affe_all , nb_elem_affe)
!
! ----- Get ALGO_INTE - Plane stress
!
        plane_stress = exicp(model, l_affe_all, list_elem_affe, nb_elem_affe)
        if (plane_stress) then
            if (rela_comp .eq. 'VMIS_ECMI_LINE' .or. rela_comp .eq. 'VMIS_ECMI_TRAC' .or.&
                rela_comp .eq. 'VMIS_ISOT_LINE' .or. rela_comp .eq. 'VMIS_ISOT_TRAC') then
                algo_inte = 'SECANTE'
            endif
        endif
        call utlcal('NOM_VALE', algo_inte, algo_inte_r)
!
! ----- For KIT
!
        if (l_kit) then
            call comp_meca_rkit(keywordfact, i_comp, rela_comp, kit_comp)
        endif
!
! ----- Get parameters for external programs (MFRONT/UMAT)
!
        call comp_read_exte(rela_comp  , kit_comp ,&
                            l_umat     , l_mfront , l_mfront_offi,&
                            libr_name  , subr_name,&
                            keywordfact, i_comp   )
!
! ----- Get RESI_INTE_RELA/ITER_INTE_MAXI
!
        call nmdocv(keywordfact, i_comp, algo_inte, 'ITER_INTE_MAXI', iter_inte_maxi)
        if (l_mfront) then
            if (l_mfront_offi .or. l_kit_thm) then
                call nmdocv(keywordfact, i_comp, algo_inte, 'RESI_INTE_RELA', resi_inte_rela)
            else
                call nmdocv(keywordfact, i_comp, algo_inte, 'RESI_INTE_MAXI', resi_inte_rela)
            endif
            call comp_meca_mod(mesh       , model       ,&
                               keywordfact, i_comp        , rela_comp,&
                               model_dim  , model_mfront)
            call mfront_set_double_parameter(libr_name, subr_name, model_mfront,&
                                             "epsilon", resi_inte_rela)
            call mfront_set_integer_parameter(libr_name, subr_name, model_mfront,&
                                              "iterMax", int(iter_inte_maxi))
        else
            call nmdocv(keywordfact, i_comp, algo_inte, 'RESI_INTE_RELA', resi_inte_rela)
        endif
!
! ----- Set in <CARTE>
!
        p_carc_valv(1)  = iter_inte_maxi
        p_carc_valv(2)  = type_matr_t
        p_carc_valv(3)  = resi_inte_rela
        p_carc_valv(4)  = parm_theta
        p_carc_valv(5)  = iter_inte_pas
        p_carc_valv(6)  = algo_inte_r
        p_carc_valv(7)  = vale_pert_rela
        p_carc_valv(8)  = resi_deborst_max
        p_carc_valv(9)  = iter_deborst_max
        p_carc_valv(10) = seuil
        p_carc_valv(11) = amplitude
        p_carc_valv(12) = taux_retour
        p_carc_valv(13) = post_iter
        p_carc_valv(21) = post_incr
!       exte_comp UMAT / MFRONT
        p_carc_valv(14) = ds_compor_para%v_para(i_comp)%c_pointer%nbvarext
        p_carc_valv(15) = ds_compor_para%v_para(i_comp)%c_pointer%namevarext
        p_carc_valv(16) = ds_compor_para%v_para(i_comp)%c_pointer%fct_ldc
!       cf. CALC_POINT_MAT / PMDORC
        p_carc_valv(17) = 0
        p_carc_valv(18) = parm_alpha
!       exte_comp UMAT / MFRONT
        p_carc_valv(19) = ds_compor_para%v_para(i_comp)%c_pointer%matprop
        p_carc_valv(20) = ds_compor_para%v_para(i_comp)%c_pointer%nbprop
!
! ----- Affect in <CARTE>
!
        if (l_affe_all) then
            call nocart(carcri, 1, nb_cmp)
        else
            call jeveuo(list_elem_affe, 'L', vi = v_elem_affe)
            call nocart(carcri, 3, nb_cmp, mode = 'NUM', nma = nb_elem_affe,&
                        limanu = v_elem_affe)
            call jedetr(list_elem_affe)
        endif
    enddo
!
    call jedetr(carcri//'.NCMP')
!
end subroutine
