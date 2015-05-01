subroutine rsvnmi(t, vm)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    real(kind=8) :: t(*), vm
!
    real(kind=8) :: inv
    integer :: i
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    inv = 0.0d0
    vm = 0.0d0
!
    do 10, i = 1, 3, 1
!
    inv = inv + t(i)*t(i)
!
    10 end do
!
    do 20, i = 4, 6, 1
!
    inv = inv + 2.0d0*t(i)*t(i)
!
    20 end do
!
    vm = sqrt(3.0d0*inv*0.5d0)
!
end subroutine
