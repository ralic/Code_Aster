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
    subroutine nminit(result, model, numedd, numfix, mate,&
                      compor, carele, parmet, lischa, maprec,&
                      solveu, carcri, numins, sdstat, sddisc,&
                      sdnume, defico, sdcrit, comref, fonact,&
                      parcon, parcri, method, lisch2, mesh,&
                      sdpilo, sddyna, sdimpr, sdsuiv, sdobse,&
                      sdtime, sderro, sdpost, sd_inout, sdener,&
                      sdconv, sdcriq, deficu, resocu, resoco,&
                      valinc, solalg, measse, veelem, meelem,&
                      veasse, codere)
        character(len=8) :: result
        character(len=24) :: model
        character(len=24) :: numedd
        character(len=24) :: numfix
        character(len=24) :: mate
        character(len=24) :: compor
        character(len=24) :: carele
        real(kind=8) :: parmet(*)
        character(len=19) :: lischa
        character(len=19) :: maprec
        character(len=19) :: solveu
        character(len=24) :: carcri
        integer :: numins
        character(len=24) :: sdstat
        character(len=19) :: sddisc
        character(len=19) :: sdnume
        character(len=24) :: defico
        character(len=19) :: sdcrit
        character(len=24) :: comref
        integer :: fonact(*)
        real(kind=8) :: parcon(*)
        real(kind=8) :: parcri(*)
        character(len=16) :: method(*)
        character(len=19) :: lisch2
        character(len=8) :: mesh
        character(len=19) :: sdpilo
        character(len=19) :: sddyna
        character(len=24) :: sdimpr
        character(len=24) :: sdsuiv
        character(len=19) :: sdobse
        character(len=24) :: sdtime
        character(len=24) :: sderro
        character(len=19) :: sdpost
        character(len=24), intent(out) :: sd_inout
        character(len=19) :: sdener
        character(len=24) :: sdconv
        character(len=24) :: sdcriq
        character(len=24) :: deficu
        character(len=24) :: resocu
        character(len=24) :: resoco
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: measse(*)
        character(len=19) :: veelem(*)
        character(len=19) :: meelem(*)
        character(len=19) :: veasse(*)
        character(len=24) :: codere
    end subroutine nminit
end interface
