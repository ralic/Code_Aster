subroutine lcimpl(fami, kpg, ksp, imate, em,&
                  ep, sigm, tmoins, tplus, deps,&
                  vim, option, sigp, vip, dsde)
!
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
    implicit none
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
#include "asterfort/rcvalb.h"
#include "asterfort/verift.h"
    real(kind=8) :: em, ep, et, sigy, tmoins, tplus
    real(kind=8) :: sigm, deps, pm, vim(*), vip(*), dt, p
    real(kind=8) :: sigp, dsde
    character(len=16) :: option
    character(len=*) :: fami
    integer :: kpg, ksp, imate
!     ------------------------------------------------------------------
!     VARIABLES LOCALES
!     ------------------------------------------------------------------
    real(kind=8) :: rprim, rm, sige, valres(2), depsth
    real(kind=8) :: sieleq, rp, dp
    integer :: codres(2)
    character(len=8) :: nomecl(2)
    data nomecl/'D_SIGM_E','SY'/
!
!
    pm = vim(1)
    dt = tplus-tmoins
!
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ECRO_LINE', 0, ' ', [0.d0],&
                1, nomecl, valres, codres, 1)
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ECRO_LINE', 0, ' ', [0.d0],&
                1, nomecl(2), valres(2), codres(2), 0)
!
    if (codres(2) .ne. 0) valres(2) = 0.d0
    et = valres(1)
    sigy = valres(2)
    rprim = ep*et/ (ep-et)
    rm = rprim*vim(1) + sigy
!
!     ------------------------------------------------------------------
!     ESTIMATION ELASTIQUE
!     ------------------------------------------------------------------
    call verift(fami, kpg, ksp, 'T', imate,&
                epsth=depsth)
    sige = ep* (sigm/em+deps-depsth)
    sieleq = abs(sige)
!     ------------------------------------------------------------------
!     CALCUL EPSP, P , SIG
!     ------------------------------------------------------------------
    if (option .eq. 'RAPH_MECA') then
        if (sieleq .le. rm) then
            dp=0.d0
            sigp = sige
            dsde = ep
            vip(1) = vim(1)
        else
            dp = abs(sige) - rm
            dp = dp/ (rprim+ep)
            rp = sigy + rprim* (pm+dp)
            vip(1) = vim(1) + dp
            sigp = sige/ (1.d0+ep*dp/rp)
        endif
        vip(2) = dp/dt
    endif
    if (option(1:16) .eq. 'RIGI_MECA_IMPLEX') then
!    EXTRAPOLATION
        dp=max(vim(2)*dt,0.d0)
        p= vim(1) + dp
!    MISE A JOUR DE LA VARIABLE INTERNE
        rp=sigy+rprim*(p)
!    CONTRAINTES
        sigp=sige/(1.d0+(ep*dp/rp))
!    MATRICE
        dsde = ep
    endif
!
end subroutine
