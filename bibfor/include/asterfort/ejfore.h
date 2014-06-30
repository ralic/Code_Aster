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
    subroutine ejfore(ndim, nddl, axi, nno1, nno2,&
                      npg, ipg, wref, vff1, vff2,&
                      idf2, dffr2, geom, iu, ip,&
                      sigref, fhyref, vect)
        integer :: npg
        integer :: nno2
        integer :: nno1
        integer :: nddl
        integer :: ndim
        logical(kind=1) :: axi
        integer :: ipg
        real(kind=8) :: wref(npg)
        real(kind=8) :: vff1(nno1, npg)
        real(kind=8) :: vff2(nno2, npg)
        integer :: idf2
        real(kind=8) :: dffr2(ndim-1, nno2, npg)
        real(kind=8) :: geom(ndim, nno2)
        integer :: iu(3, 16)
        integer :: ip(4)
        real(kind=8) :: sigref
        real(kind=8) :: fhyref
        real(kind=8) :: vect(nddl)
    end subroutine ejfore
end interface
