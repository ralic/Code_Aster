subroutine lc0037(fami, kpg, ksp, ndim, imate,&
                  compor, mult_comp, carcri, instam, instap,&
                  neps, epsm, deps, sigm, vim, option,&
                  angmas, sigp, vip, &
                  wkin, typmod, icomp,&
                  nvi, dsidep, codret)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/nmvprk.h"
#include "asterfort/plasti.h"
#include "asterfort/utlcal.h"
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
! aslint: disable=W1504,W0104
!
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: ndim
    integer, intent(in) :: imate
    character(len=16), intent(in) :: compor(*)
    character(len=16), intent(in) :: mult_comp
    real(kind=8), intent(in) :: carcri(*)
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
    integer, intent(in) :: neps
    real(kind=8), intent(in) :: epsm(neps)
    real(kind=8), intent(in) :: deps(neps)
    real(kind=8), intent(in) :: sigm(6)
    real(kind=8), intent(in) :: vim(*)
    character(len=16), intent(in) :: option
    real(kind=8), intent(in) :: angmas(3)
    real(kind=8), intent(out) :: sigp(6)
    real(kind=8), intent(out) :: vip(*)
    real(kind=8), intent(in) :: wkin(*)
    character(len=8), intent(in) :: typmod(*)
    integer, intent(in) :: icomp
    integer, intent(in) :: nvi
    real(kind=8), intent(out) :: dsidep(6, 6)
    integer, intent(out) :: codret
!
! --------------------------------------------------------------------------------------------------
!
! Behaviour
!
! polycristal, monocristal
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: algo_inte
    character(len=11) :: meting
    common /meti/   meting
!
! --------------------------------------------------------------------------------------------------
!
    if (compor(1).eq.'POLYCRISTAL') then
        call nmvprk(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, carcri, instam, instap,&
                    neps, epsm, deps, sigm, vim,&
                    option, angmas, sigp, vip, dsidep,&
                    codret, mult_comp)
    elseif (compor(1).eq.'MONOCRISTAL') then
        call utlcal('VALE_NOM', algo_inte, carcri(6))
        if (algo_inte(1:6) .eq. 'NEWTON') then
            meting = algo_inte(1:11)
            call plasti(fami, kpg, ksp, typmod, imate,&
                        compor, carcri, instam, instap,&
                        epsm, deps, sigm,&
                        vim, option, angmas, sigp, vip,&
                        dsidep, icomp, nvi, wkin, codret, mult_comp)
        else if (algo_inte.eq.'RUNGE_KUTTA') then
            meting = 'RUNGE_KUTTA'
            call nmvprk(fami, kpg, ksp, ndim, typmod,&
                        imate, compor, carcri, instam, instap,&
                        neps, epsm, deps, sigm, vim,&
                        option, angmas, sigp, vip, dsidep,&
                        codret, mult_comp)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
