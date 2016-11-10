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
    subroutine xhvco5(ndim, nnop, nnops, pla, nd,&
                      tau1, tau2, mu, nddls, jac,&
                      ffc, ffp, nddlm, wsaut,&
                      saut, vect, ifiss, nfiss, nfh,&
                      ifa, jheafa, ncomph, jheavn, ncompn, pf)
        integer :: ndim
        integer :: nnop
        integer :: nnops
        integer :: pla(27)
        real(kind=8) :: nd(3)
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: mu(3)
        integer :: nddls
        real(kind=8) :: jac
        real(kind=8) :: ffc(16)
        real(kind=8) :: ffp(27)
        integer :: nddlm
        real(kind=8) :: wsaut(3)
        real(kind=8) :: saut(3)
        real(kind=8) :: vect(560)
        integer :: ifiss
        integer :: nfiss
        integer :: nfh
        integer :: ifa
        integer :: jheafa
        integer :: ncomph
        integer :: jheavn
        integer :: ncompn
        real(kind=8) :: pf
    end subroutine xhvco5
end interface
