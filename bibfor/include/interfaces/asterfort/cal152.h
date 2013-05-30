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
    subroutine cal152(option, max, may, maz, model,&
                      phib24, iphi1, iphi2, imade, modmec,&
                      chamno, num, vrai, i, j,&
                      mij, cij, kij)
        character(len=9) :: option
        character(len=19) :: max
        character(len=19) :: may
        character(len=19) :: maz
        character(len=2) :: model
        character(len=24) :: phib24
        integer :: iphi1
        integer :: iphi2
        integer :: imade
        character(len=8) :: modmec
        character(len=19) :: chamno
        character(len=14) :: num
        logical :: vrai
        integer :: i
        integer :: j
        real(kind=8) :: mij
        real(kind=8) :: cij
        real(kind=8) :: kij
    end subroutine cal152
end interface
