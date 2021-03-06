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
    subroutine xmmsa3(ndim, nno, nnos, ffp, nddl,&
                      nvec, v1, v2, v3, nfh,&
                      singu, fk, ddls, ddlm, jheavn, ncompn,&
                      nfiss, ifiss, jheafa, ncomph, ifa,&
                      saut)
        integer :: nddl
        integer :: ndim
        integer :: nno
        integer :: nnos
        real(kind=8) :: ffp(27)
        integer :: nvec
        real(kind=8) :: v1(nddl)
        real(kind=8) :: v2(*)
        real(kind=8) :: v3(*)
        integer :: nfh
        integer :: singu
        real(kind=8) :: fk(27,3,3)
        integer :: ddls
        integer :: ddlm
        integer :: nfiss
        integer :: ifiss
        integer :: jheafa
        integer :: ncomph
        integer :: jheavn
        integer :: ncompn
        integer :: ifa
        real(kind=8) :: saut(3)
    end subroutine xmmsa3
end interface
