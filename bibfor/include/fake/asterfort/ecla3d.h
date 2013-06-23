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
    subroutine ecla3d(nomte, elrefa, fapg, npg, npoini,&
                      nterm1, nsomm1, csomm1, tyma, nbno2,&
                      connx, mxnbn2, mxnbpi, mxnbte, mxnbse,&
                      nbsel, corsel)
        integer :: mxnbse
        integer :: mxnbte
        integer :: mxnbpi
        integer :: mxnbn2
        character(len=16) :: nomte
        character(len=8) :: elrefa
        character(len=8) :: fapg
        integer :: npg
        integer :: npoini
        integer :: nterm1(mxnbpi)
        integer :: nsomm1(mxnbpi, mxnbte)
        real(kind=8) :: csomm1(mxnbpi, mxnbte)
        integer :: tyma(mxnbse)
        integer :: nbno2(mxnbse)
        integer :: connx(mxnbn2, mxnbse)
        integer :: nbsel
        integer :: corsel(mxnbse)
    end subroutine ecla3d
end interface
