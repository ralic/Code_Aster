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
    subroutine xsifl1(angl, basloc, coeff, coeff3, ddlm,&
                      ddls, dfdi, ff, he, idepl,&
                      igthet, ipref, ipres, ithet, jac,&
                      jlst, ka, mu, mult, nd,&
                      ndim, nfh, nnop, nnops, itemps,&
                      nompar, option, presn, singu, xg)
        integer :: nnop
        integer :: ndim
        real(kind=8) :: angl(2)
        real(kind=8) :: basloc(9*nnop)
        real(kind=8) :: coeff
        real(kind=8) :: coeff3
        integer :: ddlm
        integer :: ddls
        real(kind=8) :: dfdi(nnop, ndim)
        real(kind=8) :: ff(27)
        real(kind=8) :: he(2)
        integer :: idepl
        integer :: igthet
        integer :: ipref
        integer :: ipres
        integer :: ithet
        real(kind=8) :: jac
        integer :: jlst
        real(kind=8) :: ka
        real(kind=8) :: mu
        real(kind=8) :: mult
        real(kind=8) :: nd(3)
        integer :: nfh
        integer :: nnops
        integer :: itemps
        character(len=8) :: nompar(4)
        character(len=16) :: option
        real(kind=8) :: presn(27)
        integer :: singu
        real(kind=8) :: xg(4)
    end subroutine xsifl1
end interface 
