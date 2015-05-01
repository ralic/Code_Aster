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
    subroutine dfda2d(kpg, nno, poids, sdfrde, sdfrdk,&
                      sdedx, sdedy, sdkdx, sdkdy, sdfdx,&
                      sdfdy, geom, jac)
        integer :: kpg
        integer :: nno
        real(kind=8) :: poids
        real(kind=8) :: sdfrde(4, 4)
        real(kind=8) :: sdfrdk(4, 4)
        real(kind=8) :: sdedx(4)
        real(kind=8) :: sdedy(4)
        real(kind=8) :: sdkdx(4)
        real(kind=8) :: sdkdy(4)
        real(kind=8) :: sdfdx(4, 4)
        real(kind=8) :: sdfdy(4, 4)
        real(kind=8) :: geom(2, 4)
        real(kind=8) :: jac
    end subroutine dfda2d
end interface
