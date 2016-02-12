subroutine apsvnl(sdcont_defi, sdappa, model_ndim, nt_node)
!
implicit none
!
#include "asterfort/jeveuo.h"
#include "asterfort/cfnumn.h"
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
! aslint: disable=W1306
!
    character(len=24), intent(in) :: sdcont_defi
    character(len=19), intent(in) :: sdappa
    integer, intent(in) :: model_ndim 
    integer, intent(in) :: nt_node
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Smooth normals at nodes
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdappa           : name of pairing datastructure
! In  model_ndim       : dimension of model
! In  nt_node          : total number of nodes
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdappa_psno, sdappa_norl
    real(kind=8), pointer :: v_sdappa_psno(:) => null()
    real(kind=8), pointer :: v_sdappa_norl(:) => null()
    integer :: i_node, i_dime, node_curr
    integer :: node_indx(nt_node), node_nume(nt_node)
!
! --------------------------------------------------------------------------------------------------
!
    sdappa_psno = sdappa(1:14)//'.PSNO'
    sdappa_norl = sdappa(1:19)//'.NORL'
    call jeveuo(sdappa_psno//'.VALE', 'E', vr = v_sdappa_psno)
    call jeveuo(sdappa_norl         , 'L', vr = v_sdappa_norl)
!
    do i_node = 1,nt_node
        node_indx(i_node) = i_node
    end do
!
    call cfnumn(sdcont_defi, nt_node, node_indx, node_nume)
!
    do i_node = 1, nt_node
        node_curr = node_nume(i_node)
        do i_dime=1,model_ndim
            v_sdappa_psno(3*(node_curr-1)+i_dime) = &
                v_sdappa_norl(3*(i_node-1)+i_dime)
        end do
    end do
!
end subroutine
