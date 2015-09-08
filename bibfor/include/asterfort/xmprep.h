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
    subroutine xmprep(cface, contac, elref, elrefc, elc,&
                      ffc, ffp, fpg, iaint, ibasec,&
                      iptint, ifa, igeom, ipgf, jac,&
                      jlst, lact, nd, ndim, ninter,&
                      nlact, nno, nnos, nptf, nvit,&
                      rr, singu, tau1, tau2)
        integer :: cface(30, 6)
        integer :: contac
        character(len=8) :: elref
        character(len=8) :: elrefc
        character(len=8) :: elc
        real(kind=8) :: ffc(8)
        real(kind=8) :: ffp(27)
        character(len=8) :: fpg
        integer :: iaint
        integer :: ibasec
        integer :: iptint
        integer :: ifa
        integer :: igeom
        integer :: ipgf
        real(kind=8) :: jac
        integer :: jlst
        integer :: lact(8)
        real(kind=8) :: nd(3)
        integer :: ndim
        integer :: ninter
        integer :: nlact
        integer :: nno
        integer :: nnos
        integer :: nptf
        integer :: nvit
        real(kind=8) :: rr
        integer :: singu
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
    end subroutine xmprep
end interface
