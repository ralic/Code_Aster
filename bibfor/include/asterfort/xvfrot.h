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
    subroutine xvfrot(algofr, coeffp, coeffr, ddlm, ddls,&
                      ffc, ffp, idepl, idepm, ifa,&
                      ifiss, indco, jac, jheavn, ncompn, jheafa,&
                      lact, mu, ncomph, nd, nddl,&
                      ndim, nfh, nfiss, nno, nnol,&
                      nnos, nvit, pla, reac12, rr,&
                      seuil, singu, tau1, tau2, vtmp)
        integer :: algofr
        real(kind=8) :: coeffp
        real(kind=8) :: coeffr
        integer :: ddlm
        integer :: ddls
        real(kind=8) :: ffc(8)
        real(kind=8) :: ffp(27)
        integer :: idepl
        integer :: idepm
        integer :: ifa
        integer :: ifiss
        integer :: indco
        real(kind=8) :: jac
        integer :: jheafa
        integer :: lact(8)
        real(kind=8) :: mu
        integer :: ncomph
        integer :: jheavn
        integer :: ncompn
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
        real(kind=8) :: reac12(3)
        real(kind=8) :: rr
        real(kind=8) :: seuil
        integer :: singu
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: vtmp(400)
    end subroutine xvfrot
end interface
