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
    subroutine xrige3(elrefp, ndim, coorse, igeom, he,&
                      heavn, ddlh, ddlc, nfe, basloc,&
                      nnop, npg, lsn, lst, sig,&
                      matuu)
        integer :: nnop
        integer :: ndim
        character(len=8) :: elrefp
        real(kind=8) :: coorse(*)
        integer :: igeom
        real(kind=8) :: he
        integer :: heavn(27,5)
        integer :: ddlh
        integer :: ddlc
        integer :: nfe
        real(kind=8) :: basloc(9*nnop)
        integer :: npg
        real(kind=8) :: lsn(nnop)
        real(kind=8) :: lst(nnop)
        real(kind=8) :: sig(90)
        real(kind=8) :: matuu(*)
    end subroutine xrige3
end interface
