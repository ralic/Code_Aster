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
    subroutine xmathm(jmate, thmc, meca, hydr, t, ndim,&
                      nnops, nnop, nddls, nddlm, ffc,&
                      pla, nd, jac, ffp, ffp2, dt, ta, saut,&
                      dffc, rho11, gradpf, mmat,&
                      dsidep, p, r, jheavn, ncompn, ifiss,&
                      nfiss, nfh, ifa, jheafa, ncomph)
                           
        integer :: jmate
        character(len=16) :: thmc
        character(len=16) :: meca
        character(len=16) :: hydr
        real(kind=8) :: t
        integer :: ndim
        integer :: nnops
        integer :: nnop
        integer :: nddls
        integer :: nddlm
        real(kind=8) :: ffc(16)
        integer :: pla(27)
        real(kind=8) :: nd(3)
        real(kind=8) :: jac
        real(kind=8) :: ffp(27)
        real(kind=8) :: ffp2(27)
        real(kind=8) :: dt
        real(kind=8) :: ta
        real(kind=8) :: saut(3)
        real(kind=8) :: dffc(16,3)
        real(kind=8) :: rho11
        real(kind=8) :: gradpf(3)
        real(kind=8) :: mmat(560,560)
        real(kind=8) :: dsidep(6,6)
        real(kind=8) :: p(3,3)
        real(kind=8) :: r
        integer :: jheavn
        integer :: ncompn
        integer :: ifiss
        integer :: nfiss
        integer :: nfh
        integer :: ifa
        integer :: jheafa
        integer :: ncomph
    end subroutine xmathm
end interface
