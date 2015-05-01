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
    subroutine xmvco1(ndim, nno, nnol, sigma, pla,&
                      lact, dtang, nfh, ddls, jac,&
                      ffc, ffp, singu, rr, cstaco,&
                      nd, tau1, tau2, jheavn, ncompn,&
                      nfiss, ifiss, jheafa, ncomph, ifa,&
                      vtmp)
        integer :: ndim
        integer :: nno
        integer :: nnol
        real(kind=8) :: sigma(6)
        integer :: pla(27)
        integer :: lact(8)
        real(kind=8) :: dtang(3)
        integer :: nfh
        integer :: ddls
        integer :: ifiss
        integer :: nfiss
        integer :: jheavn
        integer :: jheafa
        integer :: ncompn
        integer :: ncomph
        integer :: ifa
        real(kind=8) :: jac
        real(kind=8) :: ffc(8)
        real(kind=8) :: ffp(27)
        integer :: singu
        real(kind=8) :: rr
        real(kind=8) :: cstaco
        real(kind=8) :: nd(3)
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: vtmp(400)
    end subroutine xmvco1
end interface
