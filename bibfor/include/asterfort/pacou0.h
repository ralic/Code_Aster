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
    subroutine pacou0(x, fvec, qt, r, c,&
                      d, fvcold, g, p, s,&
                      t, w, xold, work, check,&
                      vecr1, vecr2, typflu, vecr3, amor,&
                      masg, vecr4, vecr5, veci1, vg,&
                      indic, nbm, nmode, nt)
        integer :: nt
        real(kind=8) :: x(*)
        real(kind=8) :: fvec(*)
        real(kind=8) :: qt(nt, *)
        real(kind=8) :: r(nt, *)
        real(kind=8) :: c(*)
        real(kind=8) :: d(*)
        real(kind=8) :: fvcold(*)
        real(kind=8) :: g(*)
        real(kind=8) :: p(*)
        real(kind=8) :: s(*)
        real(kind=8) :: t(*)
        real(kind=8) :: w(*)
        real(kind=8) :: xold(*)
        real(kind=8) :: work(*)
        logical(kind=1) :: check
        real(kind=8) :: vecr1(*)
        real(kind=8) :: vecr2(*)
        character(len=8) :: typflu
        real(kind=8) :: vecr3(*)
        real(kind=8) :: amor(*)
        real(kind=8) :: masg(*)
        real(kind=8) :: vecr4(*)
        real(kind=8) :: vecr5(*)
        integer :: veci1(*)
        real(kind=8) :: vg
        integer :: indic
        integer :: nbm
        integer :: nmode
    end subroutine pacou0
end interface
