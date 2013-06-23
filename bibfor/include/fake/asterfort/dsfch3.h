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
    subroutine dsfch3(nno, nnf, poids, dpdef, dpdnf,&
                      dpdkf, dsdeef, dsdnnf, dsdkkf, dsdenf,&
                      dsdekf, dsdnkf, coor, dpdeg, dpdng,&
                      dpdkg, dsdeeg, dsdnng, dsdkkg, dsdeng,&
                      dsdekg, dsdnkg, dsdxxf, dsdyyf, dsdzzf,&
                      dsdxyf, dsdyzf, dsdxzf, jac)
        integer :: nno
        integer :: nnf
        real(kind=8) :: poids
        real(kind=8) :: dpdef(1)
        real(kind=8) :: dpdnf(1)
        real(kind=8) :: dpdkf(1)
        real(kind=8) :: dsdeef(1)
        real(kind=8) :: dsdnnf(1)
        real(kind=8) :: dsdkkf(1)
        real(kind=8) :: dsdenf(1)
        real(kind=8) :: dsdekf(1)
        real(kind=8) :: dsdnkf(1)
        real(kind=8) :: coor(1)
        real(kind=8) :: dpdeg(1)
        real(kind=8) :: dpdng(1)
        real(kind=8) :: dpdkg(1)
        real(kind=8) :: dsdeeg(1)
        real(kind=8) :: dsdnng(1)
        real(kind=8) :: dsdkkg(1)
        real(kind=8) :: dsdeng(1)
        real(kind=8) :: dsdekg(1)
        real(kind=8) :: dsdnkg(1)
        real(kind=8) :: dsdxxf(1)
        real(kind=8) :: dsdyyf(1)
        real(kind=8) :: dsdzzf(1)
        real(kind=8) :: dsdxyf(1)
        real(kind=8) :: dsdyzf(1)
        real(kind=8) :: dsdxzf(1)
        real(kind=8) :: jac
    end subroutine dsfch3
end interface
