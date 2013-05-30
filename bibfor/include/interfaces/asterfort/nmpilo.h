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
    subroutine nmpilo(sdpilo, deltat, rho, solalg, veasse,&
                      modele, mate, compor, resoco, valinc,&
                      nbatte, numedd, nbeffe, eta, pilcvg,&
                      carele)
        integer :: nbatte
        character(len=19) :: sdpilo
        real(kind=8) :: deltat
        real(kind=8) :: rho
        character(len=19) :: solalg(*)
        character(len=19) :: veasse(*)
        character(len=24) :: modele
        character(len=24) :: mate
        character(len=24) :: compor
        character(len=24) :: resoco
        character(len=19) :: valinc(*)
        character(len=24) :: numedd
        integer :: nbeffe
        real(kind=8) :: eta(nbatte)
        integer :: pilcvg
        character(len=24) :: carele
    end subroutine nmpilo
end interface
