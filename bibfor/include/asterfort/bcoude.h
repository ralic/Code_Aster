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
    subroutine bcoude(igau, icou, isect, l, h,&
                      a, m, nno, ncou, nsect,&
                      ff, df1, df2, mmt, b)
        integer :: igau
        integer :: icou
        integer :: isect
        real(kind=8) :: l
        real(kind=8) :: h
        real(kind=8) :: a
        integer :: m
        integer :: nno
        integer :: ncou
        integer :: nsect
        real(kind=8) :: ff(*)
        real(kind=8) :: df1(*)
        real(kind=8) :: df2(*)
        integer :: mmt
        real(kind=8) :: b(4, *)
    end subroutine bcoude
end interface
