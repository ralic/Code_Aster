subroutine nmsuiy(sd_prnt, vale_r, i_dof_monitor)
!
implicit none
!
#include "asterfort/impfoi.h"
#include "asterfort/nmimcr.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sd_prnt
    real(kind=8), intent(in) :: vale_r
    integer, intent(inout) :: i_dof_monitor
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear operators - DOF monitor
!
! Write value
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_prnt          : datastructure for print informations
! In  vale_r           : value to print
! IO  i_dof_monitor    : index of current monitoring
!
! --------------------------------------------------------------------------------------------------
!
    character(len=9) :: typcol
    character(len=1) :: indsui
!
! --------------------------------------------------------------------------------------------------
!
    call impfoi(0, 1, i_dof_monitor, indsui)
    typcol = 'SUIVDDL'//indsui
    call nmimcr(sd_prnt, typcol, vale_r, .true._1)
    i_dof_monitor = i_dof_monitor + 1
!
end subroutine
