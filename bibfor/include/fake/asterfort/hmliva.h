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
    subroutine hmliva(yachai, option, meca, ther, hydr,&
                      imate, ndim, dimdef, dimcon, nbvari,&
                      yamec, yate, addeme, adcome, advihy,&
                      advico, vihrho, vicphi, vicpvp, vicsat,&
                      addep1, adcp11, adcp12, addete, adcote,&
                      congem, congep, vintm, vintp, dsde,&
                      epsv, depsv, p1, dp1, t,&
                      dt, phi, pvp, h11, h12,&
                      rho11, phi0, pvp0, sat, retcom,&
                      thmc, biot, rinstp)
        integer :: nbvari
        integer :: dimcon
        integer :: dimdef
        logical :: yachai
        character(len=16) :: option
        character(len=16) :: meca
        character(len=16) :: ther
        character(len=16) :: hydr
        integer :: imate
        integer :: ndim
        integer :: yamec
        integer :: yate
        integer :: addeme
        integer :: adcome
        integer :: advihy
        integer :: advico
        integer :: vihrho
        integer :: vicphi
        integer :: vicpvp
        integer :: vicsat
        integer :: addep1
        integer :: adcp11
        integer :: adcp12
        integer :: addete
        integer :: adcote
        real(kind=8) :: congem(dimcon)
        real(kind=8) :: congep(dimcon)
        real(kind=8) :: vintm(nbvari)
        real(kind=8) :: vintp(nbvari)
        real(kind=8) :: dsde(dimcon, dimdef)
        real(kind=8) :: epsv
        real(kind=8) :: depsv
        real(kind=8) :: p1
        real(kind=8) :: dp1
        real(kind=8) :: t
        real(kind=8) :: dt
        real(kind=8) :: phi
        real(kind=8) :: pvp
        real(kind=8) :: h11
        real(kind=8) :: h12
        real(kind=8) :: rho11
        real(kind=8) :: phi0
        real(kind=8) :: pvp0
        real(kind=8) :: sat
        integer :: retcom
        character(len=16) :: thmc
        real(kind=8) :: biot
        real(kind=8) :: rinstp
    end subroutine hmliva
end interface
