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
    subroutine fneihm(fnoevo, deltat, perman, nno1, nno2,&
                      npi, npg, wref, iu, ip,&
                      ipf, iq, vff1, vff2, dffr2,&
                      geom, ang, congem, r, vectu,&
                      mecani, press1, press2, tempe, dimdef,&
                      dimcon, dimuel, ndim, axi)
        integer :: ndim
        integer :: dimuel
        integer :: dimcon
        integer :: dimdef
        integer :: npg
        integer :: npi
        integer :: nno2
        integer :: nno1
        logical :: fnoevo
        real(kind=8) :: deltat
        logical :: perman
        real(kind=8) :: wref(npg)
        integer :: iu(3, 18)
        integer :: ip(2, 9)
        integer :: ipf(2, 2, 9)
        integer :: iq(2, 2, 9)
        real(kind=8) :: vff1(nno1, npi)
        real(kind=8) :: vff2(nno2, npi)
        real(kind=8) :: dffr2(ndim-1, nno2, npi)
        real(kind=8) :: geom(ndim, nno2)
        real(kind=8) :: ang(24)
        real(kind=8) :: congem(dimcon, npi)
        real(kind=8) :: r(dimdef)
        real(kind=8) :: vectu(dimuel)
        integer :: mecani(8)
        integer :: press1(9)
        integer :: press2(9)
        integer :: tempe(5)
        logical :: axi
    end subroutine fneihm
end interface
