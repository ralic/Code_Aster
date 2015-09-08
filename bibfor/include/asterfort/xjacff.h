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
    subroutine xjacff(elrefp, elrefc, elc, ndim, fpg,&
                      jinter, ifa, cface, ipg, nnop,&
                      igeom, jbasec, xg, jac, ffp,&
                      ffpc, dfdi, nd, tau1, tau2,&
                      ifiss, ncompp, ncompb)
        integer :: nno
        integer :: ndim
        character(len=8) :: elrefp
        character(len=8) :: elrefc
        character(len=8) :: elc
        character(len=8) :: fpg
        integer :: jinter
        integer :: ifa
        integer :: cface(30, 6)
        integer :: ipg
        integer :: nnop
        integer :: igeom
        integer :: jbasec
        real(kind=8) :: xg(3)
        real(kind=8) :: jac
        real(kind=8) :: ffp(27)
        real(kind=8) :: ffpc(27)
        real(kind=8) :: dfdi(nnop, 3)
        real(kind=8) :: nd(3)
        real(kind=8) :: tau1(ndim)
        real(kind=8) :: tau2(ndim)
        integer, intent(in), optional :: ifiss
        integer, intent(in), optional :: ncompp
        integer, intent(in), optional :: ncompb
    end subroutine xjacff
end interface
