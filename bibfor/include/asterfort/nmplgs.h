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
    subroutine nmplgs(ndim, nno1, vff1, idfde1, nno2,&
                      vff2, idfde2, npg, iw, geom,&
                      typmod, option, mate, compor, crit,&
                      instam, instap, angmas, ddlm, ddld,&
                      sigm, lgpg, vim, sigp, vip,&
                      matr, vect, codret, dfdi2, livois,&
                      nbvois, numa, lisoco, nbsoco)
        integer :: lgpg
        integer :: npg
        integer :: nno2
        integer :: nno1
        integer :: ndim
        real(kind=8) :: vff1(nno1, npg)
        integer :: idfde1
        real(kind=8) :: vff2(nno2, npg)
        integer :: idfde2
        integer :: iw
        real(kind=8) :: geom(ndim, nno1)
        character(len=8) :: typmod(*)
        character(len=16) :: option
        integer :: mate
        character(len=16) :: compor(*)
        real(kind=8) :: crit(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: angmas(3)
        real(kind=8) :: ddlm(*)
        real(kind=8) :: ddld(*)
        real(kind=8) :: sigm(2*ndim, npg)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: sigp(2*ndim, npg)
        real(kind=8) :: vip(lgpg, npg)
        real(kind=8) :: matr(*)
        real(kind=8) :: vect(*)
        integer :: codret
        real(kind=8) :: dfdi2(nno2, ndim)
        integer, parameter :: nvoima=12, nscoma=4
        integer :: livois(1:nvoima)
        integer :: nbvois
        integer :: numa
        integer :: lisoco(1:nvoima, 1:nscoma, 1:2)
        integer :: nbsoco(1:nvoima)
    end subroutine nmplgs
end interface
