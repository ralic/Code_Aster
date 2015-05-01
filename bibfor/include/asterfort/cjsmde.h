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
    subroutine cjsmde(mod, crit, mater, nvi, epsd,&
                      deps, sigd, sigf, vind, vinf,&
                      noconv, aredec, stopnc, niter, epscon,&
                      trac)
        character(len=8) :: mod
        real(kind=8) :: crit(*)
        real(kind=8) :: mater(14, 2)
        integer :: nvi
        real(kind=8) :: epsd(6)
        real(kind=8) :: deps(6)
        real(kind=8) :: sigd(6)
        real(kind=8) :: sigf(6)
        real(kind=8) :: vind(*)
        real(kind=8) :: vinf(*)
        aster_logical :: noconv
        aster_logical :: aredec
        aster_logical :: stopnc
        integer :: niter
        real(kind=8) :: epscon
        aster_logical :: trac
    end subroutine cjsmde
end interface
