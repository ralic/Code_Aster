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
    subroutine reeref(elrefp, axi, nnop, nnos, geom,&
                      xg, idepl, grand, ndim, he,&
                      r, ur, fisno, nfiss, nfh,&
                      nfe, ddls, ddlm, fe, dgdgl,&
                      cinem, xe, ff, dfdi, f,&
                      eps, grad)
        integer :: nfiss
        integer :: ndim
        integer :: nnop
        character(len=8) :: elrefp
        logical :: axi
        integer :: nnos
        real(kind=8) :: geom(*)
        real(kind=8) :: xg(ndim)
        integer :: idepl
        logical :: grand
        real(kind=8) :: he(nfiss)
        real(kind=8) :: r
        real(kind=8) :: ur
        integer :: fisno(nnop, nfiss)
        integer :: nfh
        integer :: nfe
        integer :: ddls
        integer :: ddlm
        real(kind=8) :: fe(4)
        real(kind=8) :: dgdgl(4, ndim)
        character(len=3) :: cinem
        real(kind=8) :: xe(ndim)
        real(kind=8) :: ff(nnop)
        real(kind=8) :: dfdi(nnop, ndim)
        real(kind=8) :: f(3, 3)
        real(kind=8) :: eps(6)
        real(kind=8) :: grad(ndim, ndim)
    end subroutine reeref
end interface
