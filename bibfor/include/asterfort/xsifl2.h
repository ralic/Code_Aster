!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
interface 
    subroutine xsifl2(basloc, coeff, coeff3, ddld, ddlm,&
                      ddls, dfdi, ff, idepl, igthet,&
                      ithet, jac, ndim, nnop,&
                      nnos, tau1, tau2, nd, xg)
        integer :: nnop
        integer :: ndim
        real(kind=8) :: basloc(9*nnop)
        real(kind=8) :: coeff
        real(kind=8) :: coeff3
        integer :: ddld
        integer :: ddlm
        integer :: ddls
        real(kind=8) :: dfdi(nnop, ndim)
        real(kind=8) :: ff(27)
        integer :: idepl
        integer :: igthet
        integer :: ithet
        real(kind=8) :: jac
        integer :: nnos
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: nd(3)
        real(kind=8) :: xg(3)
    end subroutine xsifl2
end interface 
