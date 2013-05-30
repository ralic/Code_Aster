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
    subroutine lcgrad(resi, rigi, ndim, ndimsi, neps,&
                      sigma, apg, lag, grad, aldc,&
                      r, c, ktg, sig, dsidep)
        integer :: neps
        integer :: ndim
        logical :: resi
        logical :: rigi
        integer :: ndimsi
        real(kind=8) :: sigma(6)
        real(kind=8) :: apg
        real(kind=8) :: lag
        real(kind=8) :: grad(ndim)
        real(kind=8) :: aldc
        real(kind=8) :: r
        real(kind=8) :: c
        real(kind=8) :: ktg(6, 6, 4)
        real(kind=8) :: sig(neps)
        real(kind=8) :: dsidep(neps, neps)
    end subroutine lcgrad
end interface
