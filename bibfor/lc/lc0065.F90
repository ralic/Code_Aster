subroutine lc0065(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, dsidep, codret)
    implicit none
#include "asterfort/lcsflu.h"
#include "asterfort/lcsend.h"
!
!
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: Etienne grimal at edf.fr
! ======================================================================
!.......................................................................
!     BUT: LOI DE FLUAGE DE KIT_RGI
!
!          RELATION : 'FLUA_PORO_BETON'
    integer :: imate, ndim, kpg, ksp, codret, icomp, nvi, iret
    real(kind=8) :: crit(*), angmas(*)
    real(kind=8) :: instam, instap, tampon(*)
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigm(6), sigp(6)
    real(kind=8) :: vim(*), vip(*)
    real(kind=8) :: dsidep(6, 6)
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
!
    integer :: i, j
!
    if (compor(1) .eq. 'KIT_DDI') then
! - si module de fluage associe au module d'endommagement
        if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then

!
          call lcsflu(fami,kpg,ksp,ndim,imate,compor,crit,instam,&
              instap,epsm,deps,sigm,vim,option,angmas,sigp,vip,&
              tampon,typmod,icomp,nvi,dsidep,codret)

!
          call lcsend(fami,kpg,ksp,ndim,imate,compor,crit,instam,&
              instap,epsm,deps,sigm,vim,option,angmas,sigp,vip,&
              tampon,typmod,icomp,nvi,dsidep,codret)

!
        else if (option(1:9).eq.'RIGI_MECA') then
          call lcsflu(fami,kpg,ksp,ndim,imate,compor,crit,instam,&
              instap,epsm,deps,sigm,vim,option,angmas,sigp,vip,&
              tampon,typmod,icomp,nvi,dsidep,codret)
!
        endif
    else
! - si module de fluage seul
        call lcsflu(fami,kpg,ksp,ndim,imate,compor,crit,instam,&
                  instap,epsm,deps,sigm,vim,option,angmas,sigp,vip,&
                 tampon,typmod,icomp,nvi,dsidep,codret)
!
    endif
!
end subroutine
