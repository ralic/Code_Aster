subroutine aptnol(sdappa, model_ndim, nt_node)
!
implicit none
!
#include "asterfort/jeveuo.h"
#include "asterfort/mmnorm.h"
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
!
    character(len=19), intent(in) :: sdappa
    integer, intent(in) :: model_ndim 
    integer, intent(in) :: nt_node
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Compute normals at nodes
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  model_ndim       : dimension of model
! In  nt_node          : total number of nodes
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdappa_tgno, sdappa_norl
    real(kind=8), pointer :: v_sdappa_tgno(:) => null()
    real(kind=8), pointer :: v_sdappa_norl(:) => null()
    integer :: i_node
    real(kind=8) :: tau1(3), tau2(3), norm(3), noor
!
! --------------------------------------------------------------------------------------------------
!
    sdappa_tgno = sdappa(1:19)//'.TGNO'
    sdappa_norl = sdappa(1:19)//'.NORL'
    call jeveuo(sdappa_tgno, 'L', vr = v_sdappa_tgno)
    call jeveuo(sdappa_norl, 'E', vr = v_sdappa_norl)
!
! - Loop on nodes
!
    do i_node=1, nt_node
        noor      = 0.d0
        norm(1:3) = 0.d0
        tau1(1) = v_sdappa_tgno(6*(i_node-1)+1)
        tau1(2) = v_sdappa_tgno(6*(i_node-1)+2)
        tau1(3) = v_sdappa_tgno(6*(i_node-1)+3)
        tau2(1) = v_sdappa_tgno(6*(i_node-1)+4)
        tau2(2) = v_sdappa_tgno(6*(i_node-1)+5)
        tau2(3) = v_sdappa_tgno(6*(i_node-1)+6)
        call mmnorm(model_ndim, tau1, tau2, norm, noor)
        v_sdappa_norl(3*(i_node-1)+1) = norm(1)
        v_sdappa_norl(3*(i_node-1)+2) = norm(2)
        v_sdappa_norl(3*(i_node-1)+3) = norm(3)
    end do
!
end subroutine
