!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine viporo(nbvari, vintm, vintp, advico, vicphi,&
                      phi0, deps, depsv, alphfi, dt,&
                      dp1, dp2, signe, sat, cs,&
                      tbiot, phi, phim, retcom, cbiot,&
                      unsks, alpha0, aniso, phenom)
        integer :: nbvari
        real(kind=8) :: vintm(nbvari)
        real(kind=8) :: vintp(nbvari)
        integer :: advico
        integer :: vicphi
        real(kind=8) :: phi0
        real(kind=8) :: deps(6)
        real(kind=8) :: depsv
        real(kind=8) :: alphfi
        real(kind=8) :: dt
        real(kind=8) :: dp1
        real(kind=8) :: dp2
        real(kind=8) :: signe
        real(kind=8) :: sat
        real(kind=8) :: cs
        real(kind=8) :: tbiot(6)
        real(kind=8) :: phi
        real(kind=8) :: phim
        integer :: retcom
        real(kind=8) :: cbiot
        real(kind=8) :: unsks
        real(kind=8) :: alpha0
        integer :: aniso
        character(len=16) :: phenom
    end subroutine viporo
end interface 
