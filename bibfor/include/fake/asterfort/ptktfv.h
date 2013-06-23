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
    subroutine ptktfv(itype, sk, e, rof, ce,&
                      a1, ai1, a2, ai2, xl,&
                      xiy1, xiy2, xiz1, xiz2, xjx1,&
                      xjx2, g, alfay1, alfay2, alfaz1,&
                      alfaz2, ey, ez)
        integer :: itype
        real(kind=8) :: sk(*)
        real(kind=8) :: e
        real(kind=8) :: rof
        real(kind=8) :: ce
        real(kind=8) :: a1
        real(kind=8) :: ai1
        real(kind=8) :: a2
        real(kind=8) :: ai2
        real(kind=8) :: xl
        real(kind=8) :: xiy1
        real(kind=8) :: xiy2
        real(kind=8) :: xiz1
        real(kind=8) :: xiz2
        real(kind=8) :: xjx1
        real(kind=8) :: xjx2
        real(kind=8) :: g
        real(kind=8) :: alfay1
        real(kind=8) :: alfay2
        real(kind=8) :: alfaz1
        real(kind=8) :: alfaz2
        real(kind=8) :: ey
        real(kind=8) :: ez
    end subroutine ptktfv
end interface
