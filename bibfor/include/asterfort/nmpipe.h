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
    subroutine nmpipe(modele, ligrpi, cartyp, careta, mate,&
                      compor, resoco, valinc, depdel, ddepl0,&
                      ddepl1, tau, nbeffe, eta, pilcvg,&
                      typpil, carele)
        character(len=24) :: modele
        character(len=19) :: ligrpi
        character(len=19) :: cartyp
        character(len=19) :: careta
        character(len=24) :: mate
        character(len=24) :: compor
        character(len=24) :: resoco
        character(len=19) :: valinc(*)
        character(len=19) :: depdel
        character(len=19) :: ddepl0
        character(len=19) :: ddepl1
        real(kind=8) :: tau
        integer :: nbeffe
        real(kind=8) :: eta(2)
        integer :: pilcvg
        character(len=24) :: typpil
        character(len=24) :: carele
    end subroutine nmpipe
end interface
