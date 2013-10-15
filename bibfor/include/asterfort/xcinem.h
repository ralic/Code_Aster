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
    subroutine xcinem(axi, nnop, nnos, idepl, grand, ndim, he,&
                      r, ur, fisno, nfiss, nfh, nfe, ddls, ddlm,&
                      fe, dgdgl, ff, dfdi, f, eps, grad)
        logical, intent(in) :: axi
        integer, intent(in) :: nnop
        integer, intent(in) :: nnos
        integer, intent(in) :: idepl
        logical, intent(in) :: grand
        integer, intent(in) :: ndim
        real(kind=8), intent(in) :: he(nfiss)
        real(kind=8), intent(in) :: r
        real(kind=8), intent(in) :: ur
        integer, intent(in) :: fisno(nnop, nfiss)
        integer, intent(in) :: nfiss
        integer, intent(in) :: nfh
        integer, intent(in) :: nfe
        integer, intent(in) :: ddls
        integer, intent(in) :: ddlm
        real(kind=8), intent(in) :: fe(4)
        real(kind=8), intent(in) :: dgdgl(4, ndim)
        real(kind=8), intent(in) :: ff(nnop)
        real(kind=8), intent(in) :: dfdi(nnop, ndim)
        real(kind=8), intent(out) :: f(3, 3)
        real(kind=8), intent(out) :: eps(6)
        real(kind=8), intent(out) :: grad(ndim, ndim)
    end subroutine xcinem
end interface
