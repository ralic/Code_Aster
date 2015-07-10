subroutine cfdist(sdcont_defi, i_zone         , elem_slav_indx, poin_coor, time_curr,&
                  gap_user   , node_slav_indx_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdism.h"
#include "asterfort/fointe.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfl.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: i_zone
    integer, intent(in) :: elem_slav_indx
    real(kind=8), intent(in) :: poin_coor(3)
    real(kind=8), intent(in) :: time_curr
    real(kind=8), intent(out) :: gap_user
    integer, optional, intent(in) :: node_slav_indx_
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue/Discrete method - Compute user gap
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  i_zone           : index of contact zone
! In  elem_slav_indx   : index of slave element (in contact datastructure)
! In  time_curr        : current time
! In  poin_coor        : coordinates of (contact) point
! In  node_slav_indx   : index of slave node (in contact datastructure)
! Out gap_user         : user gap (from DIST_* keywords)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ier
    character(len=8) :: para_name(4)
    real(kind=8) :: para_vale(4)
    real(kind=8) :: gap_user_mast, gap_user_slav, gap_structural
    character(len=8) :: gap_mast_func, gap_slav_func
    aster_logical :: l_dist_beam, l_dist_shell, l_dist_slav, l_dist_mast
    character(len=24) :: sdcont_jeucoq
    real(kind=8), pointer :: v_sdcont_jeucoq(:) => null()
    character(len=24) :: sdcont_jeupou
    real(kind=8), pointer :: v_sdcont_jeupou(:) => null()
    character(len=24) :: sdcont_jeufo1
    character(len=8), pointer :: v_sdcont_jeufo1(:) => null()
    character(len=24) :: sdcont_jeufo2
    character(len=8), pointer :: v_sdcont_jeufo2(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    gap_user       = 0.d0
    gap_user_mast  = 0.d0
    gap_user_slav  = 0.d0
    gap_structural = 0.d0
!
! - Acces to contact objects
!
    sdcont_jeucoq = sdcont_defi(1:16)//'.JEUCOQ'
    sdcont_jeupou = sdcont_defi(1:16)//'.JEUPOU'
    sdcont_jeufo1 = sdcont_defi(1:16)//'.JFO1CO'
    sdcont_jeufo2 = sdcont_defi(1:16)//'.JFO2CO'
    call jeveuo(sdcont_jeucoq, 'L', vr  = v_sdcont_jeucoq)
    call jeveuo(sdcont_jeupou, 'L', vr  = v_sdcont_jeupou)
    call jeveuo(sdcont_jeufo1, 'L', vk8 = v_sdcont_jeufo1)
    call jeveuo(sdcont_jeufo2, 'L', vk8 = v_sdcont_jeufo2)
!
! - Set parameters for evaluate functions
!
    para_name(1) = 'X'
    para_name(2) = 'Y'
    para_name(3) = 'Z'
    para_name(4) = 'INST'
    para_vale(1) = poin_coor(1)
    para_vale(2) = poin_coor(2)
    para_vale(3) = poin_coor(3)
    para_vale(4) = time_curr
!
! - Supplementary gaps
!
    l_dist_beam  = mminfl(sdcont_defi, 'DIST_POUTRE', i_zone)
    l_dist_shell = mminfl(sdcont_defi, 'DIST_COQUE' , i_zone)
    l_dist_mast  = mminfl(sdcont_defi, 'DIST_MAIT'  , i_zone)
    l_dist_slav  = mminfl(sdcont_defi, 'DIST_ESCL'  , i_zone)
!
! - Evaluate DIST_MAIT
!
    if (l_dist_mast) then
        gap_mast_func = v_sdcont_jeufo1(i_zone)
        call fointe('F', gap_mast_func, 4, para_name, para_vale,&
                    gap_user_mast, ier)
    endif
!
! - Evaluate DIST_ESCL
!
    if (l_dist_slav) then
        gap_slav_func = v_sdcont_jeufo2(i_zone)
        call fointe('F', gap_slav_func, 4, para_name, para_vale,&
                    gap_user_slav, ier)
    endif
!
! - Evaluate DIST_POUTRE/DIST_COQUE
!
    if (l_dist_shell .or. l_dist_beam) then
        if (present(node_slav_indx_)) then
            call cfdism(sdcont_defi, l_dist_beam, l_dist_shell, node_slav_indx_, gap_structural)
        else
            if (l_dist_beam) then
                gap_structural = gap_structural+v_sdcont_jeupou(elem_slav_indx)
            endif
            if (l_dist_shell) then
                gap_structural = gap_structural+v_sdcont_jeucoq(elem_slav_indx)
            endif
        endif
    endif
!
! - Total user gap
!
    gap_user = gap_user_mast + gap_user_slav + gap_structural
!
end subroutine
