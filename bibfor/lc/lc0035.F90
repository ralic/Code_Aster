subroutine lc0035(fami, kpg, ksp, ndim, imate,&
                  compor, carcri, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, wkin, typmod, icomp,&
                  nvi, dsidep, codret)
!
implicit none
!
#include "asterfort/lkcomp.h"
#include "asterfort/lkpost.h"
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
    real(kind=8), intent(in) :: carcri(*)
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
    real(kind=8), intent(in) :: epsm(*)
    real(kind=8), intent(in) :: deps(*)
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
! letk
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: algo_inte
!
! --------------------------------------------------------------------------------------------------
!
    call utlcal('VALE_NOM', algo_inte, carcri(6))
!
    if ((algo_inte(1:10).eq.'SPECIFIQUE') .or. (option(1:14).eq.'RIGI_MECA_TANG')) then
        call lkcomp(fami, kpg, ksp, typmod, imate, instam, instap, &
                    deps, sigm, vim,&
                    option, sigp, vip, dsidep, codret,&
                    nvi)
    else
        call plasti(fami, kpg, ksp, typmod, imate,&
                    compor, carcri, instam, instap,&
                    epsm, deps, sigm,&
                    vim, option, angmas, sigp, vip,&
                    dsidep, icomp, nvi, wkin, codret)
    endif
!
! --- AJOUT DE CRITERES D'INTERPRETATION POUR AIDER AUX POST-TRAITEMENTS
    if (option(1:5) .ne. 'RIGI_') then
        call lkpost(imate, sigp, nvi, vip)
    endif
!
end subroutine
