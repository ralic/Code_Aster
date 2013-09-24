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
    subroutine nmtacr(mode, ndimsi, mat, sigel, vim,&
                      epm, dp, sp, xi, f,&
                      g, fds, gds, fdp, gdp,&
                      fdx, gdx, dpmax, sig, tang)
        integer :: ndimsi
        integer :: mode
        real(kind=8) :: mat(14)
        real(kind=8) :: sigel(ndimsi)
        real(kind=8) :: vim(9)
        real(kind=8) :: epm(6)
        real(kind=8) :: dp
        real(kind=8) :: sp
        real(kind=8) :: xi
        real(kind=8) :: f
        real(kind=8) :: g
        real(kind=8) :: fds
        real(kind=8) :: gds
        real(kind=8) :: fdp
        real(kind=8) :: gdp
        real(kind=8) :: fdx
        real(kind=8) :: gdx
        real(kind=8) :: dpmax
        real(kind=8) :: sig(ndimsi)
        real(kind=8) :: tang(6, 6)
    end subroutine nmtacr
end interface
