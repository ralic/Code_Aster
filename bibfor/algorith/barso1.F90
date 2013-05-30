subroutine barso1(n1, n2, n3, coor, poin)
    implicit   none
    integer :: n1, n2, n3, poin(*)
    real(kind=8) :: coor(*)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BARSOUM :
!-----------------------------------------------------------------------
!
    real(kind=8) :: vx, vy, vz
!     ------------------------------------------------------------------
!
    vx = 0.25d0 * ( coor(3*(poin(n2)-1)+1) - coor(3*(poin(n1)-1)+1) )
    vy = 0.25d0 * ( coor(3*(poin(n2)-1)+2) - coor(3*(poin(n1)-1)+2) )
    vz = 0.25d0 * ( coor(3*(poin(n2)-1)+3) - coor(3*(poin(n1)-1)+3) )
!
    coor(3*(poin(n3)-1)+1) = coor(3*(poin(n1)-1)+1) + vx
    coor(3*(poin(n3)-1)+2) = coor(3*(poin(n1)-1)+2) + vy
    coor(3*(poin(n3)-1)+3) = coor(3*(poin(n1)-1)+3) + vz
!
end subroutine
