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
    subroutine calcva(kpi, yachai, yamec, yate, yap1,&
                      yap2, defgem, defgep, addeme, addep1,&
                      addep2, addete, ndim, t0, p10,&
                      p20, depsv, epsv, deps, t,&
                      p1, p2, grat, grap1, grap2,&
                      dp1, dp2, dt, retcom)
        integer :: ndim
        integer :: kpi
        aster_logical :: yachai
        integer :: yamec
        integer :: yate
        integer :: yap1
        integer :: yap2
        real(kind=8) :: defgem(*)
        real(kind=8) :: defgep(*)
        integer :: addeme
        integer :: addep1
        integer :: addep2
        integer :: addete
        real(kind=8) :: t0
        real(kind=8) :: p10
        real(kind=8) :: p20
        real(kind=8) :: depsv
        real(kind=8) :: epsv
        real(kind=8) :: deps(6)
        real(kind=8) :: t
        real(kind=8) :: p1
        real(kind=8) :: p2
        real(kind=8) :: grat(ndim)
        real(kind=8) :: grap1(ndim)
        real(kind=8) :: grap2(ndim)
        real(kind=8) :: dp1
        real(kind=8) :: dp2
        real(kind=8) :: dt
        integer :: retcom
    end subroutine calcva
end interface
