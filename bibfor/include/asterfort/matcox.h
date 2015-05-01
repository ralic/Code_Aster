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
    subroutine matcox(ndim, pp, ddt1, ddt2, ddt3,&
                      ddt4, p, nno, ddlh, ddls,&
                      jac, ffp, singu, rr, mmat)
        integer :: ndim
        real(kind=8) :: pp(3, 3)
        real(kind=8) :: ddt1(3, 3)
        real(kind=8) :: ddt2(3, 3)
        real(kind=8) :: ddt3(3, 3)
        real(kind=8) :: ddt4(3, 3)
        real(kind=8) :: p(3, 3)
        integer :: nno
        integer :: ddlh
        integer :: ddls
        real(kind=8) :: jac
        real(kind=8) :: ffp(27)
        integer :: singu
        real(kind=8) :: rr
        real(kind=8) :: mmat(216, 216)
    end subroutine matcox
end interface
