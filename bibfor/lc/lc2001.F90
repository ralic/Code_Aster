subroutine lc2001(fami, kpg, ksp, ndim, imate,&
                  neps, deps, nsig, sigm, option,&
                  angmas, sigp, vip, typmod, ndsde,&
                  dsidep, codret)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/nmelas.h"
#include "asterfort/nmorth.h"
#include "asterfort/rccoma.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W0104
!
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: ndim
    integer, intent(in) :: imate
    integer, intent(in) :: neps
    real(kind=8), intent(in) :: deps(neps)
    integer, intent(in) :: nsig
    real(kind=8), intent(in) :: sigm(nsig)
    character(len=16), intent(in) :: option
    real(kind=8), intent(in) :: angmas(3)
    real(kind=8), intent(out) :: sigp(nsig)
    real(kind=8), intent(out) :: vip(1)
    character(len=8), intent(in) :: typmod(*)
    integer, intent(in) :: ndsde
    real(kind=8), intent(out) :: dsidep(ndsde)
    integer, intent(out) :: codret
!
! --------------------------------------------------------------------------------------------------
!
! Behaviour - Special IMPLEX
!
! 'ELAS'
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: mcmate
    integer :: icodre
!
! --------------------------------------------------------------------------------------------------
!
    call rccoma(imate, 'ELAS', 1, mcmate, icodre)
    ASSERT(icodre.eq.0)
    if (mcmate .eq. 'ELAS') then
        call nmelas(fami, kpg, ksp, ndim, typmod,&
                    imate, deps, sigm, option, sigp,&
                    vip, dsidep, codret)
    else 
        ASSERT(.false.)
    endif
!
end subroutine
