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
    subroutine matthm(ndim, axi, nno1, nno2, dimuel,&
                      dimdef, iu, ip, ipf, iq,&
                      yap1, yap2, yate, addep1, addep2,&
                      addlh1, vff1, vff2, dffr2, wref,&
                      geom, ang, wi, q)
        integer :: dimdef
        integer :: dimuel
        integer :: nno2
        integer :: nno1
        integer :: ndim
        aster_logical :: axi
        integer :: iu(3, 18)
        integer :: ip(2, 9)
        integer :: ipf(2, 2, 9)
        integer :: iq(2, 2, 9)
        integer :: yap1
        integer :: yap2
        integer :: yate
        integer :: addep1
        integer :: addep2
        integer :: addlh1
        real(kind=8) :: vff1(nno1)
        real(kind=8) :: vff2(nno2)
        real(kind=8) :: dffr2(ndim-1, nno2)
        real(kind=8) :: wref
        real(kind=8) :: geom(ndim, nno2)
        real(kind=8) :: ang(24)
        real(kind=8) :: wi
        real(kind=8) :: q(dimdef, dimuel)
    end subroutine matthm
end interface
