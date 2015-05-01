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
    subroutine nmnewt(noma, modele, numins, numedd, numfix,&
                      mate, carele, comref, compor, lischa,&
                      method, fonact, carcri, parcon, conv,&
                      parmet, parcri, sdstat, sdtime,&
                      sderro, sdimpr, sdnume, sddyna, sddisc,&
                      sdcrit, sdsuiv, sdpilo, sdconv, solveu,&
                      maprec, matass, valinc, solalg, meelem,&
                      measse, veelem, veasse, defico, resoco,&
                      deficu, resocu, eta, nbiter)
        character(len=8) :: noma
        character(len=24) :: modele
        integer :: numins
        character(len=24) :: numedd
        character(len=24) :: numfix
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: comref
        character(len=24) :: compor
        character(len=19) :: lischa
        character(len=16) :: method(*)
        integer :: fonact(*)
        character(len=24) :: carcri
        real(kind=8) :: parcon(*)
        real(kind=8) :: conv(*)
        real(kind=8) :: parmet(*)
        real(kind=8) :: parcri(*)
        character(len=24) :: sdstat
        character(len=24) :: sdtime
        character(len=24) :: sderro
        character(len=24) :: sdimpr
        character(len=19) :: sdnume
        character(len=19) :: sddyna
        character(len=19) :: sddisc
        character(len=19) :: sdcrit
        character(len=24) :: sdsuiv
        character(len=19) :: sdpilo
        character(len=24) :: sdconv
        character(len=19) :: solveu
        character(len=19) :: maprec
        character(len=19) :: matass
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: meelem(*)
        character(len=19) :: measse(*)
        character(len=19) :: veelem(*)
        character(len=19) :: veasse(*)
        character(len=24) :: defico
        character(len=24) :: resoco
        character(len=24) :: deficu
        character(len=24) :: resocu
        real(kind=8) :: eta
        integer :: nbiter
    end subroutine nmnewt
end interface
