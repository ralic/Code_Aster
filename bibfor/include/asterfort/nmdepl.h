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
#include "asterf_types.h"
!
interface
    subroutine nmdepl(modele, numedd, mate, carele, comref,&
                      compor, lischa, fonact, sdstat, parmet,&
                      carcri, noma, method, numins, iterat,&
                      solveu, matass, sddisc, sddyna, sdnume,&
                      sdpilo, sdtime, sderro, defico, resoco,&
                      deficu, resocu, valinc, solalg, veelem,&
                      veasse, eta, conv, lerrit)
        character(len=24) :: modele
        character(len=24) :: numedd
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: comref
        character(len=24) :: compor
        character(len=19) :: lischa
        integer :: fonact(*)
        character(len=24) :: sdstat
        real(kind=8) :: parmet(*)
        character(len=24) :: carcri
        character(len=8) :: noma
        character(len=16) :: method(*)
        integer :: numins
        integer :: iterat
        character(len=19) :: solveu
        character(len=19) :: matass
        character(len=19) :: sddisc
        character(len=19) :: sddyna
        character(len=19) :: sdnume
        character(len=19) :: sdpilo
        character(len=24) :: sdtime
        character(len=24) :: sderro
        character(len=24) :: defico
        character(len=24) :: resoco
        character(len=24) :: deficu
        character(len=24) :: resocu
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: veelem(*)
        character(len=19) :: veasse(*)
        real(kind=8) :: eta
        real(kind=8) :: conv(*)
        aster_logical :: lerrit
    end subroutine nmdepl
end interface
