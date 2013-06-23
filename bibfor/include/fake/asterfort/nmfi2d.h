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
    subroutine nmfi2d(npg, lgpg, mate, option, geom,&
                      deplm, ddepl, sigmo, sigma, fint,&
                      ktan, vim, vip, tm, tp,&
                      crit, compor, typmod, codret)
        integer :: lgpg
        integer :: npg
        integer :: mate
        character(len=16) :: option
        real(kind=8) :: geom(2, 4)
        real(kind=8) :: deplm(8)
        real(kind=8) :: ddepl(8)
        real(kind=8) :: sigmo(6, npg)
        real(kind=8) :: sigma(6, npg)
        real(kind=8) :: fint(8)
        real(kind=8) :: ktan(8, 8)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: vip(lgpg, npg)
        real(kind=8) :: tm
        real(kind=8) :: tp
        real(kind=8) :: crit(*)
        character(len=16) :: compor(*)
        character(len=8) :: typmod(*)
        integer :: codret
    end subroutine nmfi2d
end interface
