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
    subroutine vpfopr(option, typres, lmasse, lraide, ldynam,&
                      omemin, omemax, omeshi, nbfreq, npivot,&
                      omecor, precsh, nbrssa, nblagr, solveu,&
                      det, idet)
        character(len=*) :: option
        character(len=16) :: typres
        integer :: lmasse
        integer :: lraide
        integer :: ldynam
        real(kind=8) :: omemin
        real(kind=8) :: omemax
        real(kind=8) :: omeshi
        integer :: nbfreq
        integer :: npivot(2)
        real(kind=8) :: omecor
        real(kind=8) :: precsh
        integer :: nbrssa
        integer :: nblagr
        character(len=19) :: solveu
        real(kind=8) :: det(2)
        integer :: idet(2)
    end subroutine vpfopr
end interface
