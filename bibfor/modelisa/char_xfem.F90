subroutine char_xfem(mesh, model, l_xfem, connex_inv, ch_xfem_stat,&
                     ch_xfem_node, ch_xfem_lnno, ch_xfem_ltno)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/celces.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnocns.h"
#include "asterfort/jeexin.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    aster_logical, intent(out) :: l_xfem
    character(len=19), intent(out) :: connex_inv
    character(len=19), intent(out) :: ch_xfem_node
    character(len=19), intent(out) :: ch_xfem_stat
    character(len=19), intent(out) :: ch_xfem_lnno
    character(len=19), intent(out) :: ch_xfem_ltno
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Get fields for XFEM method
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh         : name of mesh
! In  model        : name of model
! Out l_xfem       : .true. if xfem
! Out connex_inv   : inverse connectivity (blank if not xfem)
! Out ch_xfem_node : xfem node-field (blank if not xfem)
! Out ch_xfem_stat : status of nodes field (blank if not xfem)
! Out ch_xfem_lnno : normal level-set field (blank if not xfem)
! Out ch_xfem_ltno : tangent level-set field (blank if not xfem)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ier
!
! --------------------------------------------------------------------------------------------------
!
!
!
! - Initializations
!
    l_xfem = .false.
    ch_xfem_node = ' '
    ch_xfem_stat = ' '
    ch_xfem_lnno = ' '
    ch_xfem_ltno = ' '
    connex_inv = ' '
!
    call jeexin(model//'.XFEM_CONT', ier)
    if (ier .ne. 0) then
        l_xfem = .true.
        connex_inv = '&&CHXFEM.CNXINV'
        call cncinv(mesh, [0], 0, 'V', connex_inv)
        ch_xfem_node = '&&CHXFEM.NOXFEM'
        call cnocns(model//'.NOXFEM', 'V', ch_xfem_node)
        ch_xfem_stat = '&&CHXFEM.STAT'
        ch_xfem_lnno = '&&CHXFEM.LNNO'
        ch_xfem_ltno = '&&CHXFEM.LTNO'
        call celces(model//'.STNO', 'V', ch_xfem_stat)
        call celces(model//'.LNNO', 'V', ch_xfem_lnno)
        call celces(model//'.LTNO', 'V', ch_xfem_ltno)
    endif
!
end subroutine
