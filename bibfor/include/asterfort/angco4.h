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
    subroutine angco4(coor, zk1, izk, icoude, zk2,&
                      rayon, theta, angl1, angl2, angl3,&
                      angl4, pgl1, pgl2, pgl3, pgl4,&
                      omega, dn1n2, epsi, crit)
        real(kind=8) :: coor(*)
        real(kind=8) :: zk1(3)
        integer :: izk
        integer :: icoude
        real(kind=8) :: zk2(3)
        real(kind=8) :: rayon
        real(kind=8) :: theta
        real(kind=8) :: angl1(3)
        real(kind=8) :: angl2(3)
        real(kind=8) :: angl3(3)
        real(kind=8) :: angl4(3)
        real(kind=8) :: pgl1(3, 3)
        real(kind=8) :: pgl2(3, 3)
        real(kind=8) :: pgl3(3, 3)
        real(kind=8) :: pgl4(3, 3)
        real(kind=8) :: omega
        real(kind=8) :: dn1n2
        real(kind=8) :: epsi
        character(len=8) :: crit
    end subroutine angco4
end interface
