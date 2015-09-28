subroutine comp_meca_read(l_etat_init, info_comp_valk, info_comp_vali, &
                          model)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterc/mfront_get_nbvari.h"
#include "asterfort/deprecated_algom.h"
#include "asterfort/deprecated_behavior.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_incr.h"
#include "asterfort/comp_meca_mod.h"
#include "asterfort/comp_meca_rkit.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/mfront_get_libname.h"
#include "asterfort/mfront_get_function.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 2091 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=16), intent(out) :: info_comp_valk(:)
    integer          , intent(out) :: info_comp_vali(:)
    character(len=8), intent(in), optional :: model
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Read informations from command file
!
! --------------------------------------------------------------------------------------------------
!
! In  l_etat_init      : .true. if initial state is defined
! IO  info_comp_valk   : comportment informations (character)
! IO  info_comp_vali   : comportment informations (integer)
! In  model            : name of model
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    integer :: iocc, nbocc, ndim
    integer :: nb_vari_all
    character(len=16) :: defo_comp, rela_comp, type_cpla, mult_comp, type_comp
    character(len=16) :: type_matg, post_iter, nom_mod_mfront
    character(len=16) :: kit_comp(4)
    character(len=255) :: libr_name, subr_name
    integer :: unit_comp, nb_vari_exte
    aster_logical :: l_cristal, l_umat, l_mfront, l_mfront_offi, l_kit
!
! --------------------------------------------------------------------------------------------------
!
    nbocc       = 0
    nb_vari_all = 0
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
!
! - Read informations
!
    do iocc = 1, nbocc
        nb_vari_exte  = 0
        unit_comp     = 0
        rela_comp     = 'VIDE'
        defo_comp     = 'VIDE'
        mult_comp     = ' '
        type_cpla     = 'VIDE'
        libr_name     = ' '
        type_matg     = ' '
        post_iter     = ' '
        kit_comp(1:4) = 'VIDE'
!
! ----- Get RELATION from command file
!
        call getvtx(keywordfact, 'RELATION', iocc = iocc, scal = rela_comp)
        call deprecated_behavior(rela_comp)
!
! ----- Get DEFORMATION from command file
!
        call getvtx(keywordfact, 'DEFORMATION', iocc = iocc, scal = defo_comp)
        call deprecated_algom(defo_comp)
!
! ----- Modified matrix
!
        if (getexm(keywordfact,'TYPE_MATR_TANG') .eq. 1) then
            call getvtx(keywordfact, 'TYPE_MATR_TANG', iocc = iocc, scal = type_matg)
        endif
!
! ----- Damage post-treatment
!
        if (getexm(keywordfact,'POST_ITER') .eq. 1) then
            call getvtx(keywordfact, 'POST_ITER', iocc = iocc, scal = post_iter)
        endif
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'CRISTAL'    , l_cristal)
        call comp_meca_l(rela_comp, 'KIT'        , l_kit)
        call comp_meca_l(rela_comp, 'UMAT'       , l_umat)
        call comp_meca_l(rela_comp, 'MFRONT_OFFI', l_mfront_offi)
        l_mfront = l_mfront_offi
        if (.not. l_mfront) then
            call comp_meca_l(rela_comp, 'MFRONT'     , l_mfront)
        endif
!
! ----- Get multi-comportment *CRISTAL
!
        if (l_cristal) then
            call getvid(keywordfact, 'COMPOR', iocc = iocc, scal = mult_comp)
        endif
!
! ----- Get KIT
!
        if (l_kit) then
            call comp_meca_rkit(keywordfact, iocc, rela_comp, kit_comp)
            if (kit_comp(4).eq.'MFRONT') then
                l_mfront = .true.
            endif
        endif
!
! ----- Get external program - UMAT
!
        if (l_umat) then
            call getvis(keywordfact, 'NB_VARI', iocc = iocc, scal = nb_vari_exte)
            call getvtx(keywordfact, 'LIBRAIRIE', iocc = iocc, scal = libr_name)
            call getvtx(keywordfact, 'NOM_ROUTINE', iocc = iocc, scal = subr_name)
        endif
!
! ----- Get external program - MFRONT
!
        if (l_mfront) then
            if (l_mfront_offi) then
                call mfront_get_libname(libr_name)
                call mfront_get_function(rela_comp, subr_name)
            else
                call getvtx(keywordfact, 'LIBRAIRIE', iocc = iocc, scal = libr_name)
                call getvtx(keywordfact, 'NOM_ROUTINE', iocc = iocc, scal = subr_name)
            endif
            if ( .not. present(model) ) then
! ------------- CALC_POINT_MAT case
                ndim = 3
                nom_mod_mfront = '_Tridimensional'
            else
! ------------- STAT_NON_LINE case
                call comp_meca_mod(keywordfact, iocc, model, ndim, nom_mod_mfront)
            endif
            call mfront_get_nbvari(libr_name, subr_name, nom_mod_mfront, ndim, nb_vari_exte)
            if ( nb_vari_exte.eq.0 ) then
                nb_vari_exte = 1
            endif
        endif
!
! ----- Select type of comportment (incremental or total)
!
        call comp_meca_incr(rela_comp, defo_comp, type_comp, l_etat_init)
!
! ----- Save options in list
!
        info_comp_valk(16*(iocc-1) + 1)  = rela_comp
        info_comp_valk(16*(iocc-1) + 2)  = defo_comp
        info_comp_valk(16*(iocc-1) + 3)  = type_comp
        info_comp_valk(16*(iocc-1) + 4)  = type_cpla
        info_comp_valk(16*(iocc-1) + 5)  = kit_comp(1)
        info_comp_valk(16*(iocc-1) + 6)  = kit_comp(2)
        info_comp_valk(16*(iocc-1) + 7)  = kit_comp(3)
        info_comp_valk(16*(iocc-1) + 8)  = kit_comp(4)
        info_comp_valk(16*(iocc-1) + 14) = mult_comp
        info_comp_valk(16*(iocc-1) + 15) = type_matg
        info_comp_valk(16*(iocc-1) + 16) = post_iter
        info_comp_vali(1*(iocc-1)  + 1)  = nb_vari_exte
    end do
!
end subroutine
