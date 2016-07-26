!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xmmab3(ndim, nno, nnos, nnol, pla,&
                      ffc, ffp, jac, knp, nfh,&
                      seuil, tau1, tau2, mu, singu,&
                      fk, lact, ddls, ddlm, mmat)
        integer :: ndim
        integer :: nno
        integer :: nnos
        integer :: nnol
        integer :: pla(27)
        real(kind=8) :: ffc(8)
        real(kind=8) :: ffp(27)
        real(kind=8) :: jac
        real(kind=8) :: knp(3, 3)
        integer :: nfh
        real(kind=8) :: seuil
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: mu
        integer :: singu
        integer :: lact(8)
        integer :: ddls
        integer :: ddlm
        real(kind=8) :: fk(27,3,3)
        real(kind=8) :: mmat(216, 216)
    end subroutine xmmab3
end interface
