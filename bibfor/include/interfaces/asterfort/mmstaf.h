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
    subroutine mmstaf(noma, ndim, chdepd, coefaf, lpenaf,&
                      nummae, aliase, nne, nummam, ksipc1,&
                      ksipc2, ksipr1, ksipr2, mlagf1, mlagf2,&
                      tau1, tau2, norm, indco, indfr,&
                      rese)
        character(len=8) :: noma
        integer :: ndim
        character(len=19) :: chdepd
        real(kind=8) :: coefaf
        logical :: lpenaf
        integer :: nummae
        character(len=8) :: aliase
        integer :: nne
        integer :: nummam
        real(kind=8) :: ksipc1
        real(kind=8) :: ksipc2
        real(kind=8) :: ksipr1
        real(kind=8) :: ksipr2
        real(kind=8) :: mlagf1(9)
        real(kind=8) :: mlagf2(9)
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: norm(3)
        integer :: indco
        integer :: indfr
        real(kind=8) :: rese(3)
    end subroutine mmstaf
end interface
