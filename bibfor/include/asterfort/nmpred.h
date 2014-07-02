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
#include "asterf_types.h"
!
interface
    subroutine nmpred(modele, numedd, numfix, mate, carele,&
                      comref, compor, lischa, method, solveu,&
                      fonact, parmet, carcri, sdimpr, sdstat,&
                      sdtime, sddisc, sdnume, sderro, numins,&
                      valinc, solalg, matass, maprec, defico,&
                      resoco, resocu, sddyna, meelem, measse,&
                      veelem, veasse, lerrit)
        character(len=24) :: modele
        character(len=24) :: numedd
        character(len=24) :: numfix
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: comref
        character(len=24) :: compor
        character(len=19) :: lischa
        character(len=16) :: method(*)
        character(len=19) :: solveu
        integer :: fonact(*)
        real(kind=8) :: parmet(*)
        character(len=24) :: carcri
        character(len=24) :: sdimpr
        character(len=24) :: sdstat
        character(len=24) :: sdtime
        character(len=19) :: sddisc
        character(len=19) :: sdnume
        character(len=24) :: sderro
        integer :: numins
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: matass
        character(len=19) :: maprec
        character(len=24) :: defico
        character(len=24) :: resoco
        character(len=24) :: resocu
        character(len=19) :: sddyna
        character(len=19) :: meelem(*)
        character(len=19) :: measse(*)
        character(len=19) :: veelem(*)
        character(len=19) :: veasse(*)
        aster_logical :: lerrit
    end subroutine nmpred
end interface
