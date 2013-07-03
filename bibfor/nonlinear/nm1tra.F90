subroutine nm1tra(imate, tp, defm, deps, epspm,&
                  pm, sig, epspp, pp, dsdep)
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
! ----------------------------------------------------------------------
!
    implicit none
! ----------------------------------------------------------------------
!      PLASTICITE VON MISES ISOTROPE A PARTIR D'UNE COURBE DE TRACTION
!
! IN  IMAT     : MATERIAU
! IN  TP       : TEMPERATURE PLUS
!
! IN  DEFM    : DEFORMATION MOINS
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  EPSPM   : DEFORMATION  PLASTIQUE MOINS
! IN  PM      : DEFORMATION  PLASTIQUE CUMULEE MOINS
!
! OUT SIG     : CONTRAINTES PLUS
! OUT EPSPP   : DEFORMATION  PLASTIQUE PLUS
! OUT PP      : DEFORMATION  PLASTIQUE CUMULEE PLUS
! OUT DSDEP   : DSIG/DEPS TEMPS PLUS
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
    real(kind=8) :: tp, defm
    real(kind=8) :: deps, epspm, pm
    real(kind=8) :: sig, epspp, pp, dsdep
    integer :: imate
!     ------------------------------------------------------------------
!     VARIABLES LOCALES
!     ------------------------------------------------------------------
    real(kind=8) :: ep, dum, rm, rp, sig1, zero
    real(kind=8) :: dpcum, depsip
    integer :: jprolp, jvalep, nbvalp
!     ------------------------------------------------------------------
!
    zero = 0.0d0
!
!     RECUPERATION DE YOUNG A TP
    call rctrac(imate, 1, 'SIGM', tp, jprolp,&
                jvalep, nbvalp, ep)
!
!     CALCUL DU SIGMA1
    sig1 = ep*( defm + deps - epspm )
!
!     CALCUL DE R(P-)
    call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                dum, dum, dum, pm, rm,&
                dum, dum, dum, dum)
!
    if ((abs(sig1) - rm) .lt. zero) then
        sig = sig1
        epspp = epspm
        pp = pm
        dsdep = ep
    else
        dpcum = ( abs(sig1) - rm ) / ep
        pp = pm + dpcum
!       CALCUL DE R(P+)
        call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                    dum, dum, dum, pp, rp,&
                    dum, dum, dum, dum)
        depsip = ( rp - rm )/ep
        if (deps .gt. zero) then
            epspp = epspm + ( dpcum - depsip )
            dsdep = abs((rp - ep*( defm - epspm ))/deps)
        else if (deps .lt. zero) then
            epspp = epspm - ( dpcum - depsip )
            dsdep = abs((rp - ep*( defm - epspm ))/deps)
        else
            epspp = epspm + ( dpcum - depsip )
            dsdep = ep
        endif
        sig = ep*( defm + deps - epspp )
!
    endif
!
end subroutine
