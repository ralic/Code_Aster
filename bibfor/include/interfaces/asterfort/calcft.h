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
    subroutine calcft(option, thmc, imate, ndim, dimdef,&
                      dimcon, yamec, yap1, yap2, addete,&
                      addeme, addep1, addep2, adcote, congep,&
                      dsde, t, grat, phi, pvp,&
                      rgaz, biot, sat, dsatp1, lambp,&
                      dlambp, lambs, dlambs, lambt, dlambt,&
                      mamolv, lambct, rho11, h11, h12)
        integer :: dimcon
        integer :: dimdef
        character(len=16) :: option
        character(len=16) :: thmc
        integer :: imate
        integer :: ndim
        integer :: yamec
        integer :: yap1
        integer :: yap2
        integer :: addete
        integer :: addeme
        integer :: addep1
        integer :: addep2
        integer :: adcote
        real(kind=8) :: congep(1:dimcon)
        real(kind=8) :: dsde(1:dimcon, 1:dimdef)
        real(kind=8) :: t
        real(kind=8) :: grat(3)
        real(kind=8) :: phi
        real(kind=8) :: pvp
        real(kind=8) :: rgaz
        real(kind=8) :: biot
        real(kind=8) :: sat
        real(kind=8) :: dsatp1
        real(kind=8) :: lambp
        real(kind=8) :: dlambp
        real(kind=8) :: lambs
        real(kind=8) :: dlambs
        real(kind=8) :: lambt
        real(kind=8) :: dlambt
        real(kind=8) :: mamolv
        real(kind=8) :: lambct
        real(kind=8) :: rho11
        real(kind=8) :: h11
        real(kind=8) :: h12
    end subroutine calcft
end interface
