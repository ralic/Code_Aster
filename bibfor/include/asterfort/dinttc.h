!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine dinttc(coord1, coord2, xo1o2, yo1o2, zo1o2,&
                      do1o2, r, norm, nint, nhop,&
                      npir, coord, nbi)
        real(kind=8) :: coord1(3)
        real(kind=8) :: coord2(3)
        real(kind=8) :: xo1o2
        real(kind=8) :: yo1o2
        real(kind=8) :: zo1o2
        real(kind=8) :: do1o2
        real(kind=8) :: r
        integer :: norm(2, 4)
        integer :: nint
        integer :: nhop
        integer :: npir
        real(kind=8) :: coord(3, 12)
        integer :: nbi
    end subroutine dinttc
end interface
