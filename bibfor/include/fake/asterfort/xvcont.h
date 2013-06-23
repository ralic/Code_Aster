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
    subroutine xvcont(algocr, cohes, coefcp, coefcr, ddlm,&
                      ddls, ffc, ffp, idepl, idepm,&
                      ifa, ifiss, imate, indco, ipgf,&
                      jac, jfisno, jheafa, lact, ncomph,&
                      nd, nddl, ndim, nfh, nfiss,&
                      nno, nnol, nnos, nvit, pla,&
                      rela, reac, rr, singu, tau1,&
                      tau2, vtmp)
        integer :: algocr
        real(kind=8) :: cohes(3)
        real(kind=8) :: coefcp
        real(kind=8) :: coefcr
        integer :: ddlm
        integer :: ddls
        real(kind=8) :: ffc(8)
        real(kind=8) :: ffp(27)
        integer :: idepl
        integer :: idepm
        integer :: ifa
        integer :: ifiss
        integer :: imate
        integer :: indco
        integer :: ipgf
        real(kind=8) :: jac
        integer :: jfisno
        integer :: jheafa
        integer :: lact(8)
        integer :: ncomph
        real(kind=8) :: nd(3)
        integer :: nddl
        integer :: ndim
        integer :: nfh
        integer :: nfiss
        integer :: nno
        integer :: nnol
        integer :: nnos
        integer :: nvit
        integer :: pla(27)
        real(kind=8) :: rela
        real(kind=8) :: reac
        real(kind=8) :: rr
        integer :: singu
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: vtmp(400)
    end subroutine xvcont
end interface
