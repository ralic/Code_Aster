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
#include "asterf_types.h"
!
interface 
    subroutine assesu(nno, nnos, nface, geom, crit,&
                      deplm, deplp, congem, congep, vintm,&
                      vintp, defgem, defgep, dsde, matuu,&
                      vectu, rinstm, rinstp, option, imate,&
                      mecani, press1, press2, tempe, dimdef,&
                      dimcon, dimuel, nbvari, ndim, compor,&
                      typmod, typvf, axi, perman)
        integer, parameter :: maxfa=6
        integer :: ndim
        integer :: nbvari
        integer :: dimuel
        integer :: dimcon
        integer :: dimdef
        integer :: nno
        integer :: nnos
        integer :: nface
        real(kind=8) :: geom(ndim, nno)
        real(kind=8) :: crit(*)
        real(kind=8) :: deplm(dimuel)
        real(kind=8) :: deplp(dimuel)
        real(kind=8) :: congem(dimcon, maxfa+1)
        real(kind=8) :: congep(dimcon, maxfa+1)
        real(kind=8) :: vintm(nbvari, maxfa+1)
        real(kind=8) :: vintp(nbvari, maxfa+1)
        real(kind=8) :: defgem(dimdef)
        real(kind=8) :: defgep(dimdef)
        real(kind=8) :: dsde(dimcon, dimdef)
        real(kind=8) :: matuu(dimuel*dimuel)
        real(kind=8) :: vectu(dimuel)
        real(kind=8) :: rinstm
        real(kind=8) :: rinstp
        character(len=16) :: option
        integer :: imate
        integer :: mecani(5)
        integer :: press1(7)
        integer :: press2(7)
        integer :: tempe(5)
        character(len=16) :: compor(*)
        character(len=8) :: typmod(2)
        integer :: typvf
        aster_logical :: axi
        aster_logical :: perman
    end subroutine assesu
end interface 
