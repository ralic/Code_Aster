subroutine comp_meca_cvar(ds_compor_prep)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/comp_nbvari.h"
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
    type(NL_DS_ComporPrep), intent(inout) :: ds_compor_prep
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Count all internal variables
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_compor_prep   : datastructure to prepare comportement
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_comp, nb_comp
    character(len=16) :: keywordfact
    character(len=16) :: type_matg, post_iter
    character(len=16) :: rela_comp, defo_comp, mult_comp, kit_comp(4), type_cpla
    integer :: nume_comp(4), nb_vari, nb_vari_comp(4), nb_vari_umat, model_dim
    character(len=255) :: libr_name, subr_name
    character(len=16) :: model_mfront
!
! --------------------------------------------------------------------------------------------------
!
    keywordfact    = 'COMPORTEMENT'
    nb_comp        = ds_compor_prep%nb_comp
!
! - Loop on occurrences of COMPORTEMENT
!
    do i_comp = 1, nb_comp
!
! ----- Init
!
        nb_vari           = 0
        nume_comp(1:4)    = 0
        nb_vari_comp(1:4) = 0
!
! ----- Options
!
        rela_comp    = ds_compor_prep%v_comp(i_comp)%rela_comp
        defo_comp    = ds_compor_prep%v_comp(i_comp)%defo_comp
        type_cpla    = ds_compor_prep%v_comp(i_comp)%type_cpla
        kit_comp(:)  = ds_compor_prep%v_comp(i_comp)%kit_comp(:)
        mult_comp    = ds_compor_prep%v_comp(i_comp)%mult_comp
        type_matg    = ds_compor_prep%v_comp(i_comp)%type_matg
        post_iter    = ds_compor_prep%v_comp(i_comp)%post_iter
        libr_name    = ds_compor_prep%v_exte(i_comp)%libr_name
        subr_name    = ds_compor_prep%v_exte(i_comp)%subr_name
        nb_vari_umat = ds_compor_prep%v_exte(i_comp)%nb_vari_umat
        model_mfront = ds_compor_prep%v_exte(i_comp)%model_mfront
        model_dim    = ds_compor_prep%v_exte(i_comp)%model_dim
!
! ----- Count internal variables
!
        call comp_nbvari(rela_comp   , defo_comp   , type_cpla   , kit_comp ,&
                         type_matg   , post_iter   , mult_comp   , libr_name,&
                         subr_name   , model_dim   , model_mfront, nb_vari  ,&
                         nb_vari_umat, nb_vari_comp, nume_comp)
!
! ----- Save informations
!
        ds_compor_prep%v_comp(i_comp)%nb_vari         = nb_vari
        ds_compor_prep%v_comp(i_comp)%nb_vari_comp(:) = nb_vari_comp(:)
        ds_compor_prep%v_comp(i_comp)%nume_comp(:)    = nume_comp(:)
    end do
!
end subroutine
