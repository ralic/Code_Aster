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
#include "asterf_types.h"
!
interface
    subroutine aseihm(option, axi, ndim, nno1, nno2,&
                      npi, npg, dimuel, dimdef, dimcon,&
                      nbvari, imate, iu, ip, ipf,&
                      iq, mecani, press1, press2, tempe,&
                      vff1, vff2, dffr2, instam, instap,&
                      deplm, deplp, sigm, sigp, varim,&
                      varip, nomail, wref, geom, ang,&
                      compor, perman, crit, vectu, matuu,&
                      retcom)
        integer :: nbvari
        integer :: dimcon
        integer :: dimdef
        integer :: dimuel
        integer :: npi
        integer :: nno2
        integer :: nno1
        integer :: ndim
        character(len=16) :: option
        aster_logical :: axi
        integer :: npg
        integer :: imate
        integer :: iu(3, 18)
        integer :: ip(2, 9)
        integer :: ipf(2, 2, 9)
        integer :: iq(2, 2, 9)
        integer :: mecani(8)
        integer :: press1(9)
        integer :: press2(9)
        integer :: tempe(5)
        real(kind=8) :: vff1(nno1, npi)
        real(kind=8) :: vff2(nno2, npi)
        real(kind=8) :: dffr2(ndim-1, nno2, npi)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: deplm(dimuel)
        real(kind=8) :: deplp(dimuel)
        real(kind=8) :: sigm(dimcon, npi)
        real(kind=8) :: sigp(dimcon, npi)
        real(kind=8) :: varim(nbvari, npi)
        real(kind=8) :: varip(nbvari, npi)
        character(len=8) :: nomail
        real(kind=8) :: wref(npi)
        real(kind=8) :: geom(ndim, nno2)
        real(kind=8) :: ang(24)
        character(len=16) :: compor(*)
        aster_logical :: perman
        real(kind=8) :: crit(*)
        real(kind=8) :: vectu(dimuel)
        real(kind=8) :: matuu(dimuel*dimuel)
        integer :: retcom
    end subroutine aseihm
end interface
