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
    subroutine mmform(ndim, nommae, nommam, nne, nnm,&
                      xpc, ypc, xpr, ypr, ffe,&
                      dffe, ddffe, ffm, dffm, ddffm,&
                      ffl, dffl, ddffl)
        integer :: ndim
        character(len=8) :: nommae
        character(len=8) :: nommam
        integer :: nne
        integer :: nnm
        real(kind=8) :: xpc
        real(kind=8) :: ypc
        real(kind=8) :: xpr
        real(kind=8) :: ypr
        real(kind=8) :: ffe(9)
        real(kind=8) :: dffe(2, 9)
        real(kind=8) :: ddffe(3, 9)
        real(kind=8) :: ffm(9)
        real(kind=8) :: dffm(2, 9)
        real(kind=8) :: ddffm(3, 9)
        real(kind=8) :: ffl(9)
        real(kind=8) :: dffl(2, 9)
        real(kind=8) :: ddffl(3, 9)
    end subroutine mmform
end interface
