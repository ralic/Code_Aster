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
    subroutine norfpd(ndim, nno1, nno2, nno3, npg, iw,&
                      vff1, vff2, vff3, idff1, vu, vp, vpi,&
                      typmod, nomte, geomi, sigref, epsref, piref, vect)
        integer :: ndim
        integer :: npg
        integer :: nno1
        integer :: nno2
        integer :: nno3
        integer :: iw
        real(kind=8) :: vff1(nno1, npg)
        real(kind=8) :: vff2(nno2, npg)
        real(kind=8) :: vff3(nno3, npg)
        integer :: idff1
        integer :: vu(3, 27)
        integer :: vp(27)
        integer :: vpi(3, 27)
        character(len=8) :: typmod(*)
        real(kind=8) :: geomi(ndim, nno1)
        real(kind=8) :: sigref
        real(kind=8) :: epsref
        real(kind=8) :: piref
        real(kind=8) :: vect(*)
        character(len=16) :: nomte
    end subroutine norfpd
end interface
