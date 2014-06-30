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
    subroutine calcmd(np1, a, a0, n, m,&
                      typj, vg, vgt, vgt0, vd,&
                      vd0, rr, rr0, ri, n2,&
                      ier, ichoc, premac, prerel, mtmp1,&
                      mtmp2, ttr, u, w, d,&
                      intge1, intge2, indx, indxf, loc)
        integer :: n2
        integer :: np1
        real(kind=8) :: a(np1, *)
        real(kind=8) :: a0(np1, *)
        integer :: n
        integer :: m
        integer :: typj
        real(kind=8) :: vg(np1, *)
        real(kind=8) :: vgt(np1, *)
        real(kind=8) :: vgt0(np1, *)
        real(kind=8) :: vd(np1, *)
        real(kind=8) :: vd0(np1, *)
        real(kind=8) :: rr(*)
        real(kind=8) :: rr0(*)
        real(kind=8) :: ri(*)
        integer :: ier
        integer :: ichoc
        real(kind=8) :: premac
        real(kind=8) :: prerel
        real(kind=8) :: mtmp1(np1, *)
        real(kind=8) :: mtmp2(np1, *)
        real(kind=8) :: ttr(n2, *)
        real(kind=8) :: u(*)
        real(kind=8) :: w(*)
        real(kind=8) :: d(*)
        integer :: intge1(*)
        integer :: intge2(*)
        integer :: indx(*)
        integer :: indxf(*)
        logical(kind=1) :: loc(*)
    end subroutine calcmd
end interface
