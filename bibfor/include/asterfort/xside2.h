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
    subroutine xside2(elrefp, ndim, coorse, elrese, igeom,&
                      he, nfh, ddlc, ddlm, nfe,&
                      basloc, nnop, npg, idecpg, typmod,&
                      imate, compor, idepl, lsn, lst,&
                      nfiss, heavn, jstno, sig)
        integer :: nfiss
        integer :: npg
        integer :: nnop
        integer :: ndim
        character(len=8) :: elrefp
        real(kind=8) :: coorse(*)
        character(len=8) :: elrese
        integer :: igeom
        real(kind=8) :: he(nfiss)
        integer :: nfh
        integer :: ddlc
        integer :: ddlm
        integer :: nfe
        real(kind=8) :: basloc(6*nnop)
        integer :: idecpg
        character(len=8) :: typmod(*)
        integer :: imate
        character(len=16) :: compor(4)
        integer :: idepl
        real(kind=8) :: lsn(nnop)
        real(kind=8) :: lst(nnop)
        integer :: heavn(nnop, 5)
        integer :: jstno
        real(kind=8) :: sig(4, npg)
    end subroutine xside2
end interface
