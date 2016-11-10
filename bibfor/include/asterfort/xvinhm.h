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
    subroutine xvinhm(jmate, thmc, meca, hydr, ndim,&
                      cohes, dpf, saut, sautm, nd, lamb,&
                      w11m, rho11m, alpha, job, t, pf,&
                      rho11, w11, ipgf, rela, dsidep,&
                      delta, r, am)
                           
        integer :: jmate
        character(len=16) :: thmc
        character(len=16) :: meca
        character(len=16) :: hydr
        integer :: ndim
        real(kind=8) :: cohes(5)
        real(kind=8) :: dpf
        real(kind=8) :: saut(3)
        real(kind=8) :: sautm(3)
        real(kind=8) :: nd(3)
        real(kind=8) :: lamb(3)
        real(kind=8) :: w11m
        real(kind=8) :: rho11m
        real(kind=8) :: alpha(5)
        character(len=8) :: job
        real(kind=8) :: t
        real(kind=8) :: pf
        real(kind=8) :: rho11
        real(kind=8) :: w11
        integer :: ipgf
        real(kind=8) :: rela
        real(kind=8) :: dsidep(6,6)
        real(kind=8) :: delta(6)
        real(kind=8) :: r
        real(kind=8) :: am(3)
    end subroutine xvinhm
end interface
