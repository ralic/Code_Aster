subroutine comp_meca_read(list_vale, l_etat_init, nbocc)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_incr.h"
#include "asterfort/comp_meca_rkit.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 2091 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=19), intent(in) :: list_vale
    logical, intent(in) :: l_etat_init
    integer, intent(out) :: nbocc
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Read informations from command file
!
! --------------------------------------------------------------------------------------------------
!
! In  list_vale   : list of informations to save
! In  l_etat_init : .true. if initial state is defined
! Out nbocc       : number of occurrences of COMPORTEMENT
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    integer :: iocc
    integer :: ikit
    integer :: j_lvalk, j_lvali
    integer :: nb_vari_all
    character(len=16) :: defo_comp, rela_comp, type_cpla, mult_comp, subr_name, type_comp
    character(len=16) :: type_matg, post_iter
    character(len=16) :: kit_comp(9)
    character(len=128) :: libr_name
    integer :: unit_comp, nb_vari_exte
    logical :: l_cristal, l_zmat, l_umat, l_mfront, l_exte_comp
    logical :: l_kit
    logical :: l_matr_tgsc, l_crit_rupt
!
! --------------------------------------------------------------------------------------------------
!
    nbocc       = 0
    nb_vari_all = 0
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
!
! - List contruction
!
    if (nbocc.ne.0) then
        call wkvect(list_vale(1:19)//'.VALK', 'V V K24', 16*nbocc, j_lvalk)
        call wkvect(list_vale(1:19)//'.VALI', 'V V I'  , 2*nbocc, j_lvali)
    endif
!
! - Read informations
!
    do iocc = 1, nbocc
        nb_vari_exte = 0
        unit_comp    = 0
        rela_comp = 'VIDE'
        defo_comp = 'VIDE'
        mult_comp = ' '
        type_cpla = 'VIDE'
        libr_name = ' '
        type_matg = ' '
        post_iter = ' '
        do ikit = 1, 9
            kit_comp(ikit) = 'VIDE'
        enddo
!
! ----- Get RELATION from command file
!
        call getvtx(keywordfact, 'RELATION', iocc = iocc, scal = rela_comp)
!
! ----- Get DEFORMATION from command file
!
        call getvtx(keywordfact, 'DEFORMATION', iocc = iocc, scal = defo_comp)
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
        call comp_meca_l(rela_comp, 'MATR_TGSC', l_matr_tgsc, type_matg = type_matg)
        call comp_meca_l(rela_comp, 'CRIT_RUPT', l_crit_rupt, post_iter = post_iter)
        call comp_meca_l(rela_comp, 'CRISTAL'  , l_cristal)
        call comp_meca_l(rela_comp, 'KIT'      , l_kit)
        call comp_meca_l(rela_comp, 'ZMAT'     , l_zmat)
        call comp_meca_l(rela_comp, 'UMAT'     , l_umat)
        call comp_meca_l(rela_comp, 'MFRONT'   , l_mfront)
        call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp)
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
        endif
!
! ----- Get external program
!
        if (l_zmat) then
            call getvis(keywordfact, 'NB_VARI', iocc = iocc, scal = nb_vari_exte)
            call getvis(keywordfact, 'UNITE', iocc = iocc, scal = unit_comp)
        endif
        if (l_umat) then
            call getvis(keywordfact, 'NB_VARI', iocc = iocc, scal = nb_vari_exte)
            call getvtx(keywordfact, 'LIBRAIRIE', iocc = iocc, scal = libr_name)
            call getvtx(keywordfact, 'NOM_ROUTINE', iocc = iocc, scal = subr_name)
        endif
        if (l_mfront) then
            call getvis(keywordfact, 'NB_VARI', iocc = iocc, scal = nb_vari_exte)
            call getvtx(keywordfact, 'LIBRAIRIE', iocc = iocc, scal = libr_name)
            call getvtx(keywordfact, 'NOM_ROUTINE', iocc = iocc, scal = subr_name)
        endif
        if (l_umat .or. l_mfront) then
            ASSERT(.not.l_kit)
            if (l_kit) then
                call utmess('F','COMPOR4_61')
            endif
            do ikit = 1, 8
                kit_comp(ikit) = libr_name(16*(ikit-1)+1:16*ikit)
            end do
            kit_comp(9) = subr_name
        endif
!
! ----- Select type of comportment (incremental or total)
!
        call comp_meca_incr(rela_comp, defo_comp, type_comp, l_etat_init)
!
! ----- Save options in list
!
        zk24(j_lvalk+16*(iocc-1) -1 + 1)  = rela_comp
        zk24(j_lvalk+16*(iocc-1) -1 + 2)  = defo_comp
        zk24(j_lvalk+16*(iocc-1) -1 + 3)  = type_comp
        zk24(j_lvalk+16*(iocc-1) -1 + 4)  = type_cpla
        zk24(j_lvalk+16*(iocc-1) -1 + 5)  = kit_comp(1)
        zk24(j_lvalk+16*(iocc-1) -1 + 6)  = kit_comp(2)
        zk24(j_lvalk+16*(iocc-1) -1 + 7)  = kit_comp(3)
        zk24(j_lvalk+16*(iocc-1) -1 + 8)  = kit_comp(4)
        zk24(j_lvalk+16*(iocc-1) -1 + 9)  = kit_comp(5)
        zk24(j_lvalk+16*(iocc-1) -1 + 10) = kit_comp(6)
        zk24(j_lvalk+16*(iocc-1) -1 + 11) = kit_comp(7)
        zk24(j_lvalk+16*(iocc-1) -1 + 12) = kit_comp(8)
        zk24(j_lvalk+16*(iocc-1) -1 + 13) = kit_comp(9)
        zk24(j_lvalk+16*(iocc-1) -1 + 14) = mult_comp
        zk24(j_lvalk+16*(iocc-1) -1 + 15) = type_matg
        zk24(j_lvalk+16*(iocc-1) -1 + 16) = post_iter
        zi(j_lvali+2*(iocc-1) -1 + 1) = nb_vari_exte
        zi(j_lvali+2*(iocc-1) -1 + 2) = unit_comp
    end do
!

end subroutine
