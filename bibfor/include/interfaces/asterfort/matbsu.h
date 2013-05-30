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
    subroutine matbsu(nb1, xr, npgsr, intsn, b1mnc,&
                      b2mnc, b1mni, b2mni, b1mri, b2mri,&
                      b1src, b2src, b1su, b2su)
        integer :: nb1
        real(kind=8) :: xr(*)
        integer :: npgsr
        integer :: intsn
        real(kind=8) :: b1mnc(3, 51)
        real(kind=8) :: b2mnc(3, 51)
        real(kind=8) :: b1mni(3, 51)
        real(kind=8) :: b2mni(3, 51)
        real(kind=8) :: b1mri(3, 51, 4)
        real(kind=8) :: b2mri(3, 51, 4)
        real(kind=8) :: b1src(2, 51, 4)
        real(kind=8) :: b2src(2, 51, 4)
        real(kind=8) :: b1su(5, 51)
        real(kind=8) :: b2su(5, 51)
    end subroutine matbsu
end interface
