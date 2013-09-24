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
    subroutine xmvco2(ndim, nno, nnol, nnos, lamb,&
                      am, delta, pla, lact, nfh,&
                      ddls, ddlm, nfiss, ifiss, jheafa,&
                      ifa, ncomph, jfisno, jac, ffc,&
                      ffp, singu, r, rr, vtmp,&
                      p)
        integer :: ndim
        integer :: nno
        integer :: nnol
        integer :: nnos
        real(kind=8) :: lamb(3)
        real(kind=8) :: am(3)
        real(kind=8) :: delta(6)
        integer :: pla(27)
        integer :: lact(8)
        integer :: nfh
        integer :: ddls
        integer :: ddlm
        integer :: nfiss
        integer :: ifiss
        integer :: jheafa
        integer :: ifa
        integer :: ncomph
        integer :: jfisno
        real(kind=8) :: jac
        real(kind=8) :: ffc(8)
        real(kind=8) :: ffp(27)
        integer :: singu
        real(kind=8) :: r
        real(kind=8) :: rr
        real(kind=8) :: vtmp(400)
        real(kind=8) :: p(3, 3)
    end subroutine xmvco2
end interface
