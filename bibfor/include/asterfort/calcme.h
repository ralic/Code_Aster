!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine calcme(option, compor, thmc, meca, imate,&
                      typmod, carcri, instam, instap, tref,&
                      ndim, dimdef, dimcon, nvimec, yate,&
                      addeme, adcome, addete, defgem, congem,&
                      congep, vintm, vintp, addep1, addep2,&
                      dsde, deps, p1, p2,&
                      t, dt, retcom, dp1, dp2,&
                      sat, tbiot, ang2, aniso, phenom)
        integer :: nvimec
        integer :: dimcon
        integer :: dimdef
        character(len=16) :: option
        character(len=16) :: compor(*)
        character(len=16) :: thmc
        character(len=16) :: meca
        integer :: imate
        character(len=8) :: typmod(2)
        real(kind=8) :: carcri(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: tref
        integer :: ndim
        integer :: yate
        integer :: addeme
        integer :: adcome
        integer :: addete
        real(kind=8) :: defgem(dimdef)
        real(kind=8) :: congem(dimcon)
        real(kind=8) :: congep(dimcon)
        real(kind=8) :: vintm(nvimec)
        real(kind=8) :: vintp(nvimec)
        integer :: addep1
        integer :: addep2
        real(kind=8) :: dsde(dimcon, dimdef)
        real(kind=8) :: deps(6)
        real(kind=8) :: p1
        real(kind=8) :: p2
        real(kind=8) :: t
        real(kind=8) :: dt
        integer :: retcom
        real(kind=8) :: dp1
        real(kind=8) :: dp2
        real(kind=8) :: sat
        real(kind=8) :: tbiot(6)
        real(kind=8) :: ang2(3)
        integer :: aniso
        character(len=16) :: phenom
    end subroutine calcme
end interface 
