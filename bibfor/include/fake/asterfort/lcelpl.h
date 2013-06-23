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
    subroutine lcelpl(mod, loi, nmat, materd, materf,&
                      timed, timef, deps, nvi, vind,&
                      vinf, nr, yd, yf, sigd,&
                      sigf, drdy)
        integer :: nr
        integer :: nvi
        integer :: nmat
        character(len=8) :: mod
        character(len=16) :: loi
        real(kind=8) :: materd(nmat, 2)
        real(kind=8) :: materf(nmat, 2)
        real(kind=8) :: timed
        real(kind=8) :: timef
        real(kind=8) :: deps(6)
        real(kind=8) :: vind(nvi)
        real(kind=8) :: vinf(nvi)
        real(kind=8) :: yd(nr)
        real(kind=8) :: yf(nr)
        real(kind=8) :: sigd(6)
        real(kind=8) :: sigf(6)
        real(kind=8) :: drdy(nr, nr)
    end subroutine lcelpl
end interface
