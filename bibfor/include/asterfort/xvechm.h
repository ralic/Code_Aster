!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xvechm(nnops, ddls, ddlm, ndim, pla,&
                      saut, sautm, nd, ffc, w11, w11m, jac,&
                      q1, dt, ta, q1m, ta1, q2, q2m, dffc,&
                      rho11, gradpf, rho11m, gradpfm, ffp2,&
                      jmate, thmc, meca, hydr, t, vect, ffp,&
                      nnop, delta, lamb, am, r, p, psup,&
                      pinf, pf, ncompn, jheavn, ifiss, nfiss,&
                      nfh, ifa, jheafa, ncomph)
                           
        integer :: nnops
        integer :: nnop
        integer :: ddls
        integer :: ddlm
        integer :: ndim
        integer :: pla(27)
        real(kind=8) :: saut(3)
        real(kind=8) :: sautm(3)
        real(kind=8) :: nd(3)
        real(kind=8) :: ffc(16)
        real(kind=8) :: w11
        real(kind=8) :: w11m
        real(kind=8) :: jac
        real(kind=8) :: q1
        real(kind=8) :: dt
        real(kind=8) :: ta
        real(kind=8) :: q1m
        real(kind=8) :: ta1
        real(kind=8) :: q2
        real(kind=8) :: q2m
        real(kind=8) :: dffc(16,3)
        real(kind=8) :: rho11
        real(kind=8) :: gradpf(3)
        real(kind=8) :: rho11m
        real(kind=8) :: gradpfm(3)
        real(kind=8) :: ffp2(27)
        integer :: jmate
        character(len=16) :: thmc
        character(len=16) :: meca
        character(len=16) :: hydr
        real(kind=8) :: t
        real(kind=8) :: vect(560)
        real(kind=8) :: ffp(27)
        real(kind=8) :: delta(6)
        real(kind=8) :: lamb(3)
        real(kind=8) :: am(3)
        real(kind=8) :: r
        real(kind=8) :: p(3,3)
        real(kind=8) :: psup
        real(kind=8) :: pinf
        real(kind=8) :: pf
        integer :: ncompn
        integer :: jheavn
        integer :: nfiss
        integer :: ifiss
        integer :: nfh
        integer :: ifa
        integer :: jheafa
        integer :: ncomph
    end subroutine xvechm
end interface
