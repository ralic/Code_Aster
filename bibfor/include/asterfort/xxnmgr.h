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
    subroutine xxnmgr(elrefp, elrese, ndim, coorse, igeom,&
                      he, nfh, ddlc, ddlm, nfe,&
                      instam, instap, ideplp, sigm, vip,&
                      basloc, nnop, npg, typmod, option,&
                      imate, compor, lgpg, idecpg, crit,&
                      idepl, lsn, lst, nfiss, heavn,&
                      sigp, vi, matuu, ivectu, codret, jstno)
        integer :: nfiss
        integer :: lgpg
        integer :: npg
        integer :: nnop
        integer :: nfe
        integer :: nfh
        integer :: ndim
        character(len=8) :: elrefp
        character(len=8) :: elrese
        real(kind=8) :: coorse(*)
        integer :: igeom
        real(kind=8) :: he(nfiss)
        integer :: ddlc
        integer :: ddlm
        real(kind=8) :: instam
        real(kind=8) :: instap
        integer :: ideplp
        real(kind=8) :: sigm(2*ndim, npg)
        real(kind=8) :: vip(lgpg, npg)
        real(kind=8) :: basloc(3*ndim*nnop)
        character(len=8) :: typmod(*)
        character(len=16) :: option
        integer :: imate
        character(len=16) :: compor(4)
        integer :: idecpg
        real(kind=8) :: crit(3)
        integer :: idepl
        real(kind=8) :: lsn(nnop)
        real(kind=8) :: lst(nnop)
        integer :: heavn(nnop, 5)
        real(kind=8) :: sigp(2*ndim, npg)
        real(kind=8) :: vi(lgpg, npg)
        real(kind=8) :: matuu(*)
        integer :: ivectu
        integer :: codret
        integer :: jstno
    end subroutine xxnmgr
end interface
