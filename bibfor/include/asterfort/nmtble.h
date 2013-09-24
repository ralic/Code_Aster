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
    subroutine nmtble(modele, noma, mate, defico, resoco,&
                      niveau, fonact, sdimpr, sdstat, sdtime,&
                      sddyna, sderro, sdconv, sddisc, numins,&
                      valinc, solalg)
        character(len=24) :: modele
        character(len=8) :: noma
        character(len=24) :: mate
        character(len=24) :: defico
        character(len=24) :: resoco
        integer :: niveau
        integer :: fonact(*)
        character(len=24) :: sdimpr
        character(len=24) :: sdstat
        character(len=24) :: sdtime
        character(len=19) :: sddyna
        character(len=24) :: sderro
        character(len=24) :: sdconv
        character(len=19) :: sddisc
        character(len=19) :: valinc(*)
        integer :: numins
        character(len=19) :: solalg(*)
    end subroutine nmtble
end interface
