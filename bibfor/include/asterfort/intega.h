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
    subroutine intega(npgf, jac, poidsf, vectx, vecty,&
                      vectz, mat11, mat22, mat33, mat12,&
                      mat13, mat23, nx, ny, nz,&
                      inte)
        integer :: npgf
        real(kind=8) :: jac(9)
        real(kind=8) :: poidsf(9)
        real(kind=8) :: vectx(9)
        real(kind=8) :: vecty(9)
        real(kind=8) :: vectz(9)
        real(kind=8) :: mat11(9)
        real(kind=8) :: mat22(9)
        real(kind=8) :: mat33(9)
        real(kind=8) :: mat12(9)
        real(kind=8) :: mat13(9)
        real(kind=8) :: mat23(9)
        real(kind=8) :: nx(9)
        real(kind=8) :: ny(9)
        real(kind=8) :: nz(9)
        real(kind=8) :: inte
    end subroutine intega
end interface
