! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine redece(fami, kpg, ksp, ndim, typmod,&
                      imate, compor, mult_comp, carcri, instam, instap,&
                      neps, epsdt, depst, nsig, sigd,&
                      vind, option, angmas, nwkin, wkin,&
                      cp, numlc, tempd, tempf, tref,&
                      sigf, vinf, ndsde, dsde, nwkout,&
                      wkout, codret)
        integer :: nwkout
        integer :: ndsde
        integer :: nwkin
        integer :: nsig
        integer :: neps
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: ndim
        character(len=8) :: typmod(*)
        integer :: imate
        character(len=16) :: compor(*)
        character(len=16), intent(in) :: mult_comp
        real(kind=8) :: carcri(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: epsdt(neps)
        real(kind=8) :: depst(neps)
        real(kind=8) :: sigd(nsig)
        real(kind=8) :: vind(*)
        character(len=16) :: option
        real(kind=8) :: angmas(*)
        real(kind=8) :: wkin(nwkin)
        aster_logical :: cp
        integer :: numlc
        real(kind=8) :: tempd
        real(kind=8) :: tempf
        real(kind=8) :: tref
        real(kind=8) :: sigf(nsig)
        real(kind=8) :: vinf(*)
        real(kind=8) :: dsde(ndsde)
        real(kind=8) :: wkout(nwkout)
        integer :: codret
    end subroutine redece
end interface
