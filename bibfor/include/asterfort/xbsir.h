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
    subroutine xbsir(ndim, nnop, nfh, nfe, ddlc,&
                     ddlm, igeom, compor, jpintt, cnset,&
                     heavt, lonch, basloc, sigref, nbsig,&
                     idepl, lsn, lst, ivectu, jpmilt,&
                     nfiss, jfisno)
        integer :: nfiss
        integer :: nnop
        integer :: ndim
        integer :: nfh
        integer :: nfe
        integer :: ddlc
        integer :: ddlm
        integer :: igeom
        character(len=16) :: compor(*)
        integer :: jpintt
        integer :: cnset(128)
        integer :: heavt(*)
        integer :: lonch(10)
        real(kind=8) :: basloc(*)
        real(kind=8) :: sigref
        integer :: nbsig
        integer :: idepl
        real(kind=8) :: lsn(nnop)
        real(kind=8) :: lst(nnop)
        integer :: ivectu
        integer :: jpmilt
        integer :: jfisno
    end subroutine xbsir
end interface
