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
    subroutine nmfihm(ndim, nddl, nno1, nno2, npg,&
                      lgpg, ipg, wref, vff1, vff2,&
                      idf2, dffr2, mate, option, geom,&
                      ddlm, ddld, iu, ip, sigm,&
                      sigp, vect, matr, vim, vip,&
                      tm, tp, crit, compor, typmod)
        integer :: lgpg
        integer :: npg
        integer :: nno2
        integer :: nno1
        integer :: nddl
        integer :: ndim
        integer :: ipg
        real(kind=8) :: wref(npg)
        real(kind=8) :: vff1(nno1, npg)
        real(kind=8) :: vff2(nno2, npg)
        integer :: idf2
        real(kind=8) :: dffr2(ndim-1, nno2, npg)
        integer :: mate
        character(len=16) :: option
        real(kind=8) :: geom(ndim, nno2)
        real(kind=8) :: ddlm(nddl)
        real(kind=8) :: ddld(nddl)
        integer :: iu(3, 16)
        integer :: ip(4)
        real(kind=8) :: sigm(2*ndim-1, npg)
        real(kind=8) :: sigp(2*ndim-1, npg)
        real(kind=8) :: vect(nddl)
        real(kind=8) :: matr(nddl*nddl)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: vip(lgpg, npg)
        real(kind=8) :: tm
        real(kind=8) :: tp
        real(kind=8) :: crit
        character(len=16) :: compor(*)
        character(len=8) :: typmod(*)
    end subroutine nmfihm
end interface
