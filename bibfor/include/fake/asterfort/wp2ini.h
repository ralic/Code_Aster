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
    subroutine wp2ini(appr, lmasse, lamor, lraide, lmatra,&
                      lmtpsc, sigma, xh, xb, optiof,&
                      prorto, nborto, nbvect, neq, lbloq,&
                      lddl, alpha, beta, signe, yh,&
                      yb, solveu)
        integer :: neq
        character(len=1) :: appr
        integer :: lmasse
        integer :: lamor
        integer :: lraide
        integer :: lmatra
        integer :: lmtpsc
        complex(kind=8) :: sigma
        real(kind=8) :: xh(*)
        real(kind=8) :: xb(*)
        character(*) :: optiof
        real(kind=8) :: prorto
        integer :: nborto
        integer :: nbvect
        integer :: lbloq(*)
        integer :: lddl(*)
        real(kind=8) :: alpha(*)
        real(kind=8) :: beta(*)
        real(kind=8) :: signe(*)
        real(kind=8) :: yh(neq, *)
        real(kind=8) :: yb(neq, *)
        character(len=19) :: solveu
    end subroutine wp2ini
end interface
