subroutine rsllin(mod, nmat, materd, materf, matcst,&
                  deps, sigd, vind, sigf, theta)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!       INTEGRATION ELASTIQUE LINEAIRE ISOTROPE SUR DT POUR
!         LE MODELE DE ROUSSELIER (UTILISATION DES CONTRAINTES
!         EFFECTIVES)
!       IN  MOD    :  MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           MATCST :  'OUI' SI MATERIAU CONSTANT SUR DT
!       VAR DEPS   :  INCREMENT DE DEFORMATION
!           SIGD   :  CONTRAINTE  A T
!           VIND   :  VARIABLES INTERNES A T
!       OUT SIGF   :  CONTRAINTE A T+DT
!           VINF   :  VARIABLES INTERNES A T+DT
!       ----------------------------------------------------------------
#include "asterfort/lcopil.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmm.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcsove.h"
    integer :: nmat
!
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: vind(*)
    real(kind=8) :: dkooh(6, 6), hookf(6, 6), ident(6, 6)
    real(kind=8) :: dsig(6), deps(6), thde(6)
    real(kind=8) :: rho, f, f0, un, theta
!
    parameter       ( un     = 1.d0   )
!
    character(len=8) :: mod
    character(len=3) :: matcst
!       ----------------------------------------------------------------
!
! --    CALCUL DE RHO
!
    f = vind(2)
    f0 = materf(3,2)
    rho = (un-f)/(un-f0)
!
    if (matcst(1:3) .eq. 'OUI') then
!
! --INTEGRATION ELASTIQUE : SIGF = SIGD+ RHO HOOKF DEPS
        call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
        call lcprsv(theta, deps, thde)
        call lcprmv(hookf, thde, sigf)
        call lcprsv(rho, sigf, sigf)
        call lcsove(sigd, sigf, sigf)
    else
!                                                  -1
! --DEFORMATION ELASTIQUE A T ET T+DT : EPSEF = HOOKD/RHO  SIGD + DEPS
! --INTEGRATION ELASTIQUE :              SIGF = RHO*HOOKF EPSEF
        call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
        call lcopil('ISOTROPE', mod, materd(1, 1), dkooh)
        call lcprmm(dkooh, hookf, ident)
        call lcprmv(ident, sigd, sigf)
        call lcprsv(theta, deps, thde)
        call lcprmv(hookf, thde, dsig)
        call lcprsv(rho, dsig, dsig)
        call lcsove(sigf, dsig, sigf)
    endif
!
end subroutine
