subroutine betinc(materf, nmat, sige, nseuil, dpc,&
                  dpt, sigf, verifc, verift)
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
!       BETON_DOUBLE_DP: CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,P1,P2)
!                   AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
!       INCREMENTATION DE LA CONTRAINTE APRES CONVERGENCE
!       IN  MATERF :  COEFFICIENTS MATERIAU A T+DT
!           NMAT   :  DIMENSION MATER
!           SIGE   :  CONTRAINTE A T+DT (PREDICTION ELASTIQUE)
!           NSEUIL :  SEUIL D'ELASTICITE ACTIVE
!           DPC    :  INCREMENT DE MULTIPLICATEUR PLASTIQUE APRES
!                     CONVERGENCE, EN COMPRESSION
!           DPT    :  INCREMENT DE MULTIPLICATEUR PLASTIQUE APRES
!                     CONVERGENCE, EN TRACTION
!       OUT SIGF   :  CONTRAINTE A T+DT
!           VERIFC :  TEST DE VALIDITE DE LA PROJECTION AU SOMMET DU
!                     CONE COMPRESSION
!           VERIFT :  TEST DE VALIDITE DE LA PROJECTION AU SOMMET DU
!                     CONE TRACTION
!       ----------------------------------------------------------------
#include "asterfort/lcdevi.h"
#include "asterfort/lchydr.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsv.h"
    integer :: nmat, nseuil, i
    real(kind=8) :: materf(nmat, 2), dpc, dpt
    real(kind=8) :: un, d23, rac2, deux, trois
    real(kind=8) :: sige(6), sigf(6)
    parameter       ( un   = 1.d0   )
    parameter       ( deux = 2.d0   )
    parameter       ( trois = 3.d0   )
    parameter       ( d23  =  .66666666666666d0 )
    real(kind=8) :: dev(6), sigeq, sigh, p, sighf
    real(kind=8) :: k, lambda, mu, e, nu, coef, beta
    real(kind=8) :: a, b, c, d
    real(kind=8) :: verifc, verift
!       ----------------------------------------------------------------
    integer :: ndt, ndi
    common /tdim/   ndt  , ndi
!       ----------------------------------------------------------------
!
! --- INITIALISATION
!
    rac2 = sqrt (deux)
    e = materf(1,1)
    nu = materf(2,1)
    beta = materf(3,2)
!
    a = rac2 * (beta - un) / (deux * beta - un)
    b = rac2 / trois * beta / (deux * beta - un)
    c = rac2
    d = deux * rac2 / trois
!
! --- CONTRAINTE EQUIVALENTE
!
    call lcdevi(sige, dev)
    call lcprsc(dev, dev, p)
    sigeq = sqrt (1.5d0 * p)
!
! --- CONTRAINTE HYDROSTATIQUE
!
    call lchydr(sige, sigh)
!
! --- COEFFICIENTS DE LAME
!
    lambda = (nu * e)/((un + nu)*(un - deux * nu))
    mu = e /(deux*(un + nu))
!
! --- MODULE DE COMPRESSION HYDROSTATIQUE
!
    k = lambda + d23 * mu
!
! --- MISE A JOUR DE LA CONTRAINTE HYDROSTATIQUE
!
    if (nseuil .lt. 4) then
        sighf = sigh - k * (dpc * a / b + dpt * c / d)
    else if (nseuil.eq.11) then
        sighf = sigh - k * dpc * a / b
    else if (nseuil.eq.22) then
        sighf = sigh - k * dpt * c / d
    else if (nseuil.eq.33) then
        sighf = sigh - k * (dpc * a / b + dpt * c / d)
    endif
!
! --- MISE A JOUR DU DEVIATEUR DES CONTRAINTES
!
    if (nseuil .lt. 4) then
        coef = un - rac2 * mu * (dpc /(b * sigeq) + dpt /(d * sigeq))
        call lcprsv(coef, dev, sigf)
    else
        coef = 0.d0
        call lcinve(coef, sigf)
    endif
!
! --- MISE A JOUR DES CONTRAINTES
!
    do 10 i = 1, ndi
        sigf(i) = sigf(i) + sighf
10  end do
!
! --  VERIFICATION
!
    verifc = sigh - sighf - sigeq * a * k / (mu * rac2)
    verift = sigh - sighf - sigeq * c * k / (mu * rac2)
!
end subroutine
