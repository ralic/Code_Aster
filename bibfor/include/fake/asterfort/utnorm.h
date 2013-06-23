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
    subroutine utnorm(igeom, nsomm, naret, ino, poinc1,&
                      poinc2, jno, mno, zrino2, zrino1,&
                      zrjno2, zrjno1, x3, y3, hf,&
                      xn, yn, jac, laxi, jacob,&
                      ifm, niv)
        integer :: igeom
        integer :: nsomm
        integer :: naret
        integer :: ino
        real(kind=8) :: poinc1
        real(kind=8) :: poinc2
        integer :: jno
        integer :: mno
        real(kind=8) :: zrino2
        real(kind=8) :: zrino1
        real(kind=8) :: zrjno2
        real(kind=8) :: zrjno1
        real(kind=8) :: x3
        real(kind=8) :: y3
        real(kind=8) :: hf
        real(kind=8) :: xn(9)
        real(kind=8) :: yn(9)
        real(kind=8) :: jac(9)
        logical :: laxi
        real(kind=8) :: jacob
        integer :: ifm
        integer :: niv
    end subroutine utnorm
end interface
