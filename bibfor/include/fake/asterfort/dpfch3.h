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
    subroutine dpfch3(nno, nnf, poids, dfrdef, dfrdnf,&
                      dfrdkf, coor, dfrdeg, dfrdng, dfrdkg,&
                      dfdx, dfdy, dfdz, jac)
        integer :: nno
        integer :: nnf
        real(kind=8) :: poids
        real(kind=8) :: dfrdef(1)
        real(kind=8) :: dfrdnf(1)
        real(kind=8) :: dfrdkf(1)
        real(kind=8) :: coor(1)
        real(kind=8) :: dfrdeg(1)
        real(kind=8) :: dfrdng(1)
        real(kind=8) :: dfrdkg(1)
        real(kind=8) :: dfdx(1)
        real(kind=8) :: dfdy(1)
        real(kind=8) :: dfdz(1)
        real(kind=8) :: jac
    end subroutine dpfch3
end interface
