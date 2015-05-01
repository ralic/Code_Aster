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
    subroutine nmrepl(modele, numedd, mate, carele, comref,&
                      compor, lischa, parmet, carcri, fonact,&
                      iterat, sdstat, sdpilo, sdnume, sddyna,&
                      method, defico, resoco, deltat, valinc,&
                      solalg, veelem, veasse, sdtime, sddisc,&
                      etan, conv, eta, rho, offset,&
                      ldccvg, pilcvg, matass)
        character(len=24) :: modele
        character(len=24) :: numedd
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: comref
        character(len=24) :: compor
        character(len=19) :: lischa
        real(kind=8) :: parmet(*)
        character(len=24) :: carcri
        integer :: fonact(*)
        integer :: iterat
        character(len=24) :: sdstat
        character(len=19) :: sdpilo
        character(len=19) :: sdnume
        character(len=19) :: sddyna
        character(len=16) :: method(*)
        character(len=24) :: defico
        character(len=24) :: resoco
        real(kind=8) :: deltat
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: veelem(*)
        character(len=19) :: veasse(*)
        character(len=24) :: sdtime
        character(len=19) :: sddisc
        real(kind=8) :: etan
        real(kind=8) :: conv(*)
        real(kind=8) :: eta
        real(kind=8) :: rho
        real(kind=8) :: offset
        integer :: ldccvg
        integer :: pilcvg
        character(len=19) :: matass
    end subroutine nmrepl
end interface
