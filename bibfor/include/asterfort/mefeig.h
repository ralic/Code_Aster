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
    subroutine mefeig(ndim, nbmod, matm, matr, mata,&
                      fre, ksi, mavr, alfr, alfi,&
                      mat1, mavi, w, z, ind)
        integer :: nbmod
        integer :: ndim(14)
        real(kind=8) :: matm(nbmod, nbmod)
        real(kind=8) :: matr(nbmod, nbmod)
        real(kind=8) :: mata(nbmod, nbmod)
        real(kind=8) :: fre(nbmod)
        real(kind=8) :: ksi(nbmod)
        real(kind=8) :: mavr(2*nbmod, 2*nbmod)
        real(kind=8) :: alfr(2*nbmod)
        real(kind=8) :: alfi(2*nbmod)
        real(kind=8) :: mat1(2*nbmod, 2*nbmod)
        real(kind=8) :: mavi(2*nbmod, 2*nbmod)
        real(kind=8) :: w(4*nbmod)
        real(kind=8) :: z(4*nbmod, 2*nbmod)
        integer :: ind(2*nbmod)
    end subroutine mefeig
end interface
