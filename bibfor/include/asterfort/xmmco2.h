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
    subroutine xmmco2(ndim, nno, nnos, nnol, ddls,&
                      ddlm, dsidep, p, r, nfh,&
                      jac, ffp, ffc, pla, singu,&
                      nfiss, jheafa, jheavn, ncompn, ifa, ncomph,&
                      ifiss, rr, mmat)
        integer :: ndim
        integer :: nno
        integer :: nnos
        integer :: nnol
        integer :: ddls
        integer :: ddlm
        real(kind=8) :: dsidep(6, 6)
        real(kind=8) :: p(3, 3)
        real(kind=8) :: r
        integer :: nfh
        real(kind=8) :: jac
        real(kind=8) :: ffp(27)
        real(kind=8) :: ffc(8)
        integer :: pla(27)
        integer :: singu
        integer :: nfiss
        integer :: jheafa
        integer :: jheavn
        integer :: ncompn
        integer :: ifa
        integer :: ncomph
        integer :: ifiss
        real(kind=8) :: rr
        real(kind=8) :: mmat(216, 216)
    end subroutine xmmco2
end interface
