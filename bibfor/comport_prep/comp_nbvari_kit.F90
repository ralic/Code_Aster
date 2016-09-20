subroutine comp_nbvari_kit(kit_comp  , defo_comp   , nb_vari_rela, &
                           l_kit_meta, l_kit_thm   , l_kit_ddi   , l_kit_cg,&
                           nb_vari   , nb_vari_comp, nume_comp   , l_meca_mfront)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cg_kit_nvar.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/ddi_kit_nvar.h"
#include "asterfort/thm_kit_nvar.h"
#include "asterfort/meta_kit_nvar.h"
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
    character(len=16), intent(in) :: kit_comp(4)
    character(len=16), intent(in) :: defo_comp
    integer, intent(in) :: nb_vari_rela
    aster_logical, intent(in) :: l_kit_meta
    aster_logical, intent(in) :: l_kit_thm
    aster_logical, intent(in) :: l_kit_ddi
    aster_logical, intent(in) :: l_kit_cg
    integer, intent(inout) :: nb_vari
    integer, intent(inout) :: nume_comp(4)
    integer, intent(out) :: nb_vari_comp(4)
    aster_logical, intent(out) :: l_meca_mfront
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Count number of internal variables for KIT
!
! --------------------------------------------------------------------------------------------------
!
! In  kit_comp         : KIT comportment
! In  defo_comp        : DEFORMATION comportment
! In  nb_vari_rela     : number of internal variables for RELATION
! In  l_kit_meta       : .true. if kit metallurgy
! In  l_kit_thm        : .true. if kit THM
! In  l_kit_ddi        : .true. if kit DDI
! In  l_kit_cg         : .true. if kit CG
! IO  nb_vari          : number of internal variables
! IO  nume_comp        : number LCxxxx subroutine
! Out nb_vari_comp     : number of internal variables kit comportment
! Out l_meca_front     : .true. if using MFRONT in KIT_THM => need to update nb_vari_comp(4)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: rela_thmc, rela_ther, rela_hydr, rela_meca, rela_meta
    integer :: nb_vari_thmc, nb_vari_ther, nb_vari_hydr, nb_vari_meca, nb_vari_meta
    character(len=16) :: rela_flua, rela_plas, rela_cpla, rela_coup
    integer :: nb_vari_flua, nb_vari_plas, nb_vari_cpla, nb_vari_coup
    character(len=16) :: rela_comp_cg(2)
    integer :: nb_vari_cg(2)
    integer :: nume_comp_plas
!
! --------------------------------------------------------------------------------------------------
!
    nb_vari_comp(:) = 0
    l_meca_mfront   = .false.
    rela_meta       = kit_comp(1)
!
! - Number of internal variables for KIT THM
!
    if (l_kit_thm) then
        rela_thmc = kit_comp(1)
        rela_ther = kit_comp(2)
        rela_hydr = kit_comp(3)
        rela_meca = kit_comp(4)
        call thm_kit_nvar(rela_thmc   , rela_hydr   , rela_meca   , rela_ther, nb_vari_thmc,&
                          nb_vari_hydr, nb_vari_meca, nb_vari_ther)
        nb_vari_comp(1) = nb_vari_thmc
        nb_vari_comp(2) = nb_vari_ther
        nb_vari_comp(3) = nb_vari_hydr
        if (rela_meca .eq. 'MFRONT') then
            nb_vari_comp(4) = 0
            l_meca_mfront   = .true.
        else
            nb_vari_comp(4) = nb_vari_meca
        endif
    endif
!
! - Number of internal variables for KIT META
!
    if (l_kit_meta) then
        call meta_kit_nvar(rela_meta, nb_vari_meta)
        nb_vari_comp(1) = nb_vari_rela
        nb_vari_comp(2) = nb_vari_meta
        nb_vari = nb_vari_rela*(nb_vari_meta+1) + 1
        if (defo_comp .eq. 'SIMO_MIEHE') then
            nb_vari = nb_vari + 1
        endif
        if (defo_comp .eq. 'GDEF_LOG') then
            nb_vari = nb_vari + 6
        endif
    endif
!
! - Number of internal variables for KIT DDI
!
    if (l_kit_ddi) then
        rela_flua = kit_comp(1)
        rela_plas = kit_comp(2)
        rela_cpla = kit_comp(3)
        rela_coup = kit_comp(4)
        call ddi_kit_nvar(rela_flua   , rela_plas   , rela_cpla   , rela_coup     , nb_vari_flua,&
                          nb_vari_plas, nb_vari_cpla, nb_vari_coup, nume_comp_plas)
        nb_vari_comp(1) = nb_vari_flua
        nb_vari_comp(2) = nb_vari_plas
        nb_vari_comp(3) = nb_vari_cpla
        nb_vari_comp(4) = nb_vari_coup
        nume_comp(2)    = nume_comp_plas
        nume_comp(3)    = nb_vari_plas
    endif
!
! - Number of internal variables for KIT CG
!
    if (l_kit_cg) then
        rela_comp_cg(1) = kit_comp(1)
        rela_comp_cg(2) = kit_comp(2)
        call cg_kit_nvar(rela_comp_cg, nb_vari_cg)
        nb_vari_comp(1) = nb_vari_cg(1)
        nb_vari_comp(2) = nb_vari_cg(2)
    endif
!
end subroutine
