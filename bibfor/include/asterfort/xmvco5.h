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
    subroutine xmvco5(ndim, nno, nnol, pla, nd,&
                      tau1, tau2, mu, ddls, jac,&
                      ffc, ffp, nnos, ddlm, wsaut,&
                      saut, vtmp)
        integer :: ndim
        integer :: nno
        integer :: nnol
        integer :: pla(27)
        real(kind=8) :: nd(3)
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: mu(3)
        integer :: ddls
        real(kind=8) :: jac
        real(kind=8) :: ffc(8)
        real(kind=8) :: ffp(27)
        integer :: nnos
        integer :: ddlm
        real(kind=8) :: wsaut(3)
        real(kind=8) :: saut(3)
        real(kind=8) :: vtmp(400)
    end subroutine xmvco5
end interface
