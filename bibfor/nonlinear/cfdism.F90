subroutine cfdism(ds_contact, l_dist_beam, l_dist_shell, node_slav_indx, gap_structural)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfinvm.h"
#include "asterfort/cfnben.h"
#include "asterfort/jeveuo.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    aster_logical, intent(in) :: l_dist_beam
    aster_logical, intent(in) :: l_dist_shell
    integer, intent(in) :: node_slav_indx
    real(kind=8), intent(out) :: gap_structural
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue/Discrete method - Compute structural gap
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  l_dist_beam      : .true. if gap for beams
! In  l_dist_chell     : .true. if gap for shells
! In  node_slav_indx   : index of slave node (in contact datastructure)
! Out gap_structural   : gap from structural elements
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_jeupou, sdcont_jeucoq
    integer :: nt_elem_slav, jdeciv, i_elem_slav, elem_slav_indx
    real(kind=8) :: gap_elem
    real(kind=8), pointer :: v_sdcont_jeupou(:) => null()
    real(kind=8), pointer :: v_sdcont_jeucoq(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    gap_structural = 0.d0
    gap_elem       = 0.d0
!
! - Access to contact datastructures
!
    sdcont_jeucoq = ds_contact%sdcont_defi(1:16)//'.JEUCOQ'
    sdcont_jeupou = ds_contact%sdcont_defi(1:16)//'.JEUPOU'
    call jeveuo(sdcont_jeupou, 'L', vr = v_sdcont_jeupou)
    call jeveuo(sdcont_jeucoq, 'L', vr = v_sdcont_jeucoq)
!
    call cfnben(ds_contact%sdcont_defi, node_slav_indx, 'CONINV', nt_elem_slav, jdeciv)
!
! - Loop on slave elements
!
    do i_elem_slav = 1, nt_elem_slav
        call cfinvm(ds_contact%sdcont_defi, jdeciv, i_elem_slav, elem_slav_indx)
        if (l_dist_beam) then
            gap_elem = gap_elem+v_sdcont_jeupou(elem_slav_indx)
        endif
        if (l_dist_shell) then
            gap_elem = gap_elem+v_sdcont_jeucoq(elem_slav_indx)
        endif
    end do
!
! - Mean value
!
    gap_structural = gap_elem/nt_elem_slav
!
end subroutine
