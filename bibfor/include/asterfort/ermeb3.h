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
    subroutine ermeb3(noe, ifa, tymvol, nnof, iref1,&
                      iref2, ivois, igeom, isig, nbcmp,&
                      inst, nx, ny, nz, sig11,&
                      sig22, sig33, sig12, sig13, sig23,&
                      chx, chy, chz)
        integer :: noe(9, 6, 4)
        integer :: ifa
        integer :: tymvol
        integer :: nnof
        integer :: iref1
        integer :: iref2
        integer :: ivois
        integer :: igeom
        integer :: isig
        integer :: nbcmp
        real(kind=8) :: inst
        real(kind=8) :: nx(9)
        real(kind=8) :: ny(9)
        real(kind=8) :: nz(9)
        real(kind=8) :: sig11(9)
        real(kind=8) :: sig22(9)
        real(kind=8) :: sig33(9)
        real(kind=8) :: sig12(9)
        real(kind=8) :: sig13(9)
        real(kind=8) :: sig23(9)
        real(kind=8) :: chx(9)
        real(kind=8) :: chy(9)
        real(kind=8) :: chz(9)
    end subroutine ermeb3
end interface
