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
    subroutine xsifle(ndim, ifa, jptint, cface,&
                      igeom, nfh, singu, nfe, ddlc,&
                      ddlm, jlst, ipres, ipref, itemps,&
                      idepl, nnop, valres, basloc, ithet,&
                      nompar, presn, option, igthet, jbasec,&
                      contac)
        integer :: nnop
        integer :: ndim
        integer :: ifa
        integer :: jptint
        integer :: cface(18, 6)
        integer :: igeom
        integer :: nfh
        integer :: singu
        integer :: nfe
        integer :: ddlc
        integer :: ddlm
        integer :: jlst
        integer :: ipres
        integer :: ipref
        integer :: itemps
        integer :: idepl
        real(kind=8) :: valres(3)
        real(kind=8) :: basloc(9*nnop)
        integer :: ithet
        character(len=8) :: nompar(4)
        real(kind=8) :: presn(27)
        character(len=16) :: option
        integer :: igthet
        integer :: jbasec
        integer :: contac
    end subroutine xsifle
end interface
