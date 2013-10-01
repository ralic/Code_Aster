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
    subroutine xmmsa5(ndim, ipgf, imate, saut, lamb,&
                      nd, tau1, tau2, cohes, job,&
                      rela, alpha, dsidep, delta, p,&
                      am, r)
        integer :: ndim
        integer :: ipgf
        integer :: imate
        real(kind=8) :: saut(3)
        real(kind=8) :: lamb(3)
        real(kind=8) :: nd(3)
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: cohes(3)
        character(len=8) :: job
        real(kind=8) :: rela
        real(kind=8) :: alpha(3)
        real(kind=8) :: dsidep(6, 6)
        real(kind=8) :: delta(6)
        real(kind=8) :: p(3, 3)
        real(kind=8) :: am(3)
        real(kind=8) :: r
    end subroutine xmmsa5
end interface
