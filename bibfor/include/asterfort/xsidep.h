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
    subroutine xsidep(nnop, nfh, nfe, ddlc, ddlm,&
                      igeom, typmod, imate, compor, jpintt,&
                      cnset, heavt, lonch, basloc, idepl,&
                      lsn, lst, sig, jpmilt, nfiss,&
                      jheavn)
        integer :: nfiss
        integer :: nnop
        integer :: nfh
        integer :: nfe
        integer :: ddlc
        integer :: ddlm
        integer :: igeom
        character(len=8) :: typmod(*)
        integer :: imate
        character(len=16) :: compor(4)
        integer :: jpintt
        integer :: cnset(128)
        integer :: heavt(*)
        integer :: lonch(10)
        real(kind=8) :: basloc(*)
        integer :: idepl
        real(kind=8) :: lsn(nnop)
        real(kind=8) :: lst(nnop)
        real(kind=8) :: sig(*)
        integer :: jpmilt
        integer :: jheavn
    end subroutine xsidep
end interface
