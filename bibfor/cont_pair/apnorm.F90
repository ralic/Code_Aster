subroutine apnorm(elem_nbnode, elem_code, elem_dime, elem_coor,&
                  ksi1       , ksi2     , elem_norm)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/mmtang.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmdonf.h"
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
    integer, intent(in) :: elem_nbnode
    character(len=8), intent(in) :: elem_code
    integer, intent(in) :: elem_dime
    real(kind=8), intent(in) :: elem_coor(3,9)
    real(kind=8), intent(in) :: ksi1
    real(kind=8), intent(in) :: ksi2
    real(kind=8), intent(out) :: elem_norm(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Compute _exterior_ normal at fiven parametric cooordinates
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_nbnode      : number of node for current element
! In  elem_code        : code of current element
! In  elem_dime        : dimension of current element
! In  elem_coor        : coordinates of nodes for current element
! In  ksi1             : first parametric coordinate of the point
! In  ksi2             : second parametric coordinate of the point
! Out elem_norm        : normal direction of element
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: dff(2,9), noor
!
! --------------------------------------------------------------------------------------------------
!
    elem_norm(1:3) = 0.d0
    tau1(1:3)      = 0.d0
    tau2(1:3)      = 0.d0
!
    call mmdonf(elem_dime, elem_nbnode, elem_code, ksi1, ksi2,&
                dff)
    call mmtang(elem_dime, elem_nbnode, elem_coor, dff , tau1,&
                tau2)
    call mmnorm(elem_dime, tau1, tau2, elem_norm, noor)
!
    elem_norm(1:3) = -elem_norm(1:3)
!
end subroutine
