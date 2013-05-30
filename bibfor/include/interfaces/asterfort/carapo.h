!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine carapo(sect, geom, orien, xl, pgl,&
                      itype, a, xiy, xiz, xjx,&
                      alfay, alfaz, ey, ez, a2,&
                      xiy2, xiz2, xjx2, alfay2, alfaz2)
        real(kind=8) :: sect(*)
        real(kind=8) :: geom(6)
        real(kind=8) :: orien(3)
        real(kind=8) :: xl
        real(kind=8) :: pgl(3, 3)
        integer :: itype
        real(kind=8) :: a
        real(kind=8) :: xiy
        real(kind=8) :: xiz
        real(kind=8) :: xjx
        real(kind=8) :: alfay
        real(kind=8) :: alfaz
        real(kind=8) :: ey
        real(kind=8) :: ez
        real(kind=8) :: a2
        real(kind=8) :: xiy2
        real(kind=8) :: xiz2
        real(kind=8) :: xjx2
        real(kind=8) :: alfay2
        real(kind=8) :: alfaz2
    end subroutine carapo
end interface
