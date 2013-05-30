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
    subroutine reere3(elrefp, nnop, igeom, xg, depl,&
                      grand, ndim, he, fisno, nfiss,&
                      nfh, nfe, ddlt, fe, dgdgl,&
                      cinem, xe, ff, dfdi, f,&
                      eps, grad)
        integer :: ddlt
        integer :: nfiss
        integer :: ndim
        integer :: nnop
        character(len=8) :: elrefp
        integer :: igeom
        real(kind=8) :: xg(ndim)
        real(kind=8) :: depl(ddlt, nnop)
        logical :: grand
        real(kind=8) :: he(nfiss)
        integer :: fisno(nnop, nfiss)
        integer :: nfh
        integer :: nfe
        real(kind=8) :: fe(4)
        real(kind=8) :: dgdgl(4, ndim)
        character(len=3) :: cinem
        real(kind=8) :: xe(ndim)
        real(kind=8) :: ff(nnop)
        real(kind=8) :: dfdi(nnop, ndim)
        real(kind=8) :: f(3, 3)
        real(kind=8) :: eps(6)
        real(kind=8) :: grad(ndim, ndim)
    end subroutine reere3
end interface
