subroutine nm1dci(fami, kpg, ksp, imate, em,&
                  ep, sigm, deps, vim, option,&
                  materi, sigp, vip, dsde)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!
    implicit none
! ----------------------------------------------------------------------
!          PLASTICITE VON MISES CINEMATIQUE BILINEAIRE MONODIM
!          ON PEUT AVOIR T0 DIFF TREF
!
! IN FAMI   : FAMILLE DU POINT DE GAUSS
! IN KPG    :  NUMERO DU POINT DE GAUSS
! IN KSP    :  NUMERO DU SOUS-POINT DE GAUSS
! IN IMATE  : POINTEUR MATERIAU
! IN  EM        : MODULE D YOUNG MOINS
! IN  EP        : MODULE D YOUNG PLUS
!
! IN  SIGM    : CONTRAINTE AU TEMPS MOINS
! IN  DEPS    : DEFORMATION  TOTALE PLUS - DEFORMATION MOINS
!                       - INCREMENT DEFORMATION THERMIQUE
! IN  VIM     : VARIABLE INTERNES MOINS
! IN  OPTION     : OPTION DE CALCUL
!
! OUT SIG     : CONTRAINTES PLUS
! OUT VIP     : VARIABLE INTERNES PLUS
! OUT DSDE    : DSIG/DEPS
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
#include "asterfort/rcvalb.h"
    real(kind=8) :: ep, em, sigy
    real(kind=8) :: sigm, deps, vim(2)
    real(kind=8) :: sigp, vip(2), dsde, sieleq
    character(len=16) :: option
    character(len=*) :: fami, materi
    integer :: kpg, ksp, imate
!     ------------------------------------------------------------------
!     VARIABLES LOCALES
!     ------------------------------------------------------------------
    real(kind=8) :: sige, dp, valres(2), etm, etp, xp, xm, hm, hp
!
    integer :: icodre(2)
    character(len=8) :: nomecl(2)
!
    data nomecl/'D_SIGM_E','SY'/
!     ------------------------------------------------------------------
    call rcvalb(fami, kpg, ksp, '-', imate,&
                materi, 'ECRO_LINE', 0, ' ', 0.d0,&
                1, nomecl, valres, icodre, 1)
    etm = valres(1)
    hm = em*etm/ (em-etm)
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                materi, 'ECRO_LINE', 0, ' ', 0.d0,&
                2, nomecl, valres, icodre, 1)
    etp = valres(1)
    hp = ep*etp/ (ep-etp)
    sigy = valres(2)
    xm = vim(1)
!     ------------------------------------------------------------------
    sige = ep* (sigm/em+deps) - hp/hm*xm
!
    sieleq = abs(sige)
!     ------------------------------------------------------------------
!     CALCUL EPSP, P , SIG
!     ------------------------------------------------------------------
    if (option(1:9) .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        if (sieleq .le. sigy) then
            vip(2) = 0.d0
            dsde = ep
            dp = 0.d0
            xp = hp/hm*xm
            sigp = ep* (sigm/em+deps)
            vip(1) = xp
        else
            vip(2) = 1.d0
            dp = (sieleq-sigy)/ (ep+hp)
            if (option .eq. 'FULL_MECA_ELAS') then
                dsde = ep
            else
                dsde = etp
            endif
            xp = hp/hm*xm + hp*dp*sige/sieleq
            sigp = xp + sigy*sige/sieleq
            vip(1) = xp
        endif
    endif
    if (option(1:10) .eq. 'RIGI_MECA_') then
        if ((vim(2).lt.0.5d0) .or. (option.eq.'RIGI_MECA_ELAS')) then
            dsde = ep
        else
            dsde = etp
        endif
    endif
end subroutine
