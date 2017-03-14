subroutine lc0006(fami, kpg, ksp, ndim, imate,&
                  compor, carcri, instam, instap, neps,&
                  epsm, deps, nsig, sigm, vim,&
                  option, angmas, sigp, vip, nwkin,&
                  wkin, typmod, icomp, nvi, ndsde,&
                  dsidep, nwkout, wkout, codret)
!
implicit none
!
#include "asterfort/eibex.h"
#include "asterfort/lcdsbe.h"
#include "asterfort/lceigv.h"
#include "asterfort/lcldsb.h"
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
    real(kind=8), intent(in) :: carcri(*)
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
    real(kind=8), intent(in) :: epsm(*)
    real(kind=8), intent(in) :: deps(*)
    real(kind=8), intent(in) :: sigm(*)
    real(kind=8), intent(in) :: vim(*)
    character(len=16), intent(in) :: option
    real(kind=8), intent(in) :: angmas(*)
    real(kind=8), intent(out) :: sigp(*)
    real(kind=8), intent(out) :: vip(*)
    integer, intent(in) :: nwkin
    real(kind=8), intent(in) :: wkin(nwkin)
    character(len=8), intent(in) :: typmod(*)
    integer, intent(in) :: nwkout
    real(kind=8), intent(out) :: wkout(nwkout)
    integer, intent(in) :: icomp
    integer, intent(in) :: nvi
    real(kind=8), intent(out) :: dsidep(*)
    integer, intent(out) :: codret
    integer, intent(in) :: neps
    integer, intent(in) :: nsig
    integer, intent(in) :: ndsde
!
! --------------------------------------------------------------------------------------------------
!
! Behaviour
!
! ENDO_ISOT_BETON
!
! --------------------------------------------------------------------------------------------------
!
    if (typmod(2) .eq. 'GRADVARI') then
!
        call lceigv(fami, kpg, ksp, neps, imate,&
                    compor, epsm, deps, vim, option,&
                    sigp, vip, dsidep)
!
!     FORMULATION NON-LOCALE AVEC REGULARISATION DES DEFORMATIONS
    else if (typmod(2).eq.'GRADEPSI') then
!
        call lcdsbe(fami, ndim, typmod, imate, compor,&
                    epsm, deps, vim, option, sigp,&
                    vip, dsidep, wkout)
!     FORMULATION LOCALE
    else
        if (carcri(2) .ne. 9) then
            call lcldsb(fami, kpg, ksp, ndim,&
                        imate, compor, epsm, deps, vim,&
                        option, sigp,&
                        vip, dsidep)
        else
            call eibex(fami, kpg, ksp, ndim, imate,&
                       compor, instam, instap, epsm, deps,&
                       vim, option, sigp, vip, dsidep,&
                       codret)
        endif
!
    endif
!
end subroutine
