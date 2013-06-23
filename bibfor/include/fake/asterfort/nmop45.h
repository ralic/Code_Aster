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
    subroutine nmop45(matrig, matgeo, defo, option, nfreq,&
                      cdsp, bande, mod45, ddlexc, nddle,&
                      modes, modes2, ddlsta, nsta)
        character(len=19) :: matrig
        character(len=19) :: matgeo
        integer :: defo
        character(len=16) :: option
        integer :: nfreq
        integer :: cdsp
        real(kind=8) :: bande(2)
        character(len=4) :: mod45
        character(len=24) :: ddlexc
        integer :: nddle
        character(len=8) :: modes
        character(len=8) :: modes2
        character(len=24) :: ddlsta
        integer :: nsta
    end subroutine nmop45
end interface
