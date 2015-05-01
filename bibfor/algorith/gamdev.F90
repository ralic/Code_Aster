function gamdev(alpha)
    implicit none
    real(kind=8) :: gamdev
#include "asterc/getran.h"
#include "asterc/r8pi.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
    real(kind=8) :: alpha
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     GENERATION D'UNE REALISATION D'UNE VARIABLE ALEATOIRE X
!     A VALEUR RELLES, DE LOI GAMMA DE PARAMETRE ALPHA>1.
!
!     PROCEDURE DUE A : AHRENS J.H., DIETER U., "COMPUTER METHOD
!     FOR SAMPLING FROM GAMMA, BETA, POISSON AND BINOMIAL DISTRIBUTION",
!     COMPUTING, 12, 223-246, 1974;
!
!     IN: ALPHA    PARAMETRE ALPHA DE LA LOI GAMMA
! ----------------------------------------------------------------------
    real(kind=8) :: beta, beta2, f0, c1, c2, gamma2, un
    real(kind=8) :: v, u, y, unif, pi, gamm1, vref
    real(kind=8) :: valr
! DEB ------------------------------------------------------------------
!
    pi = r8pi()
    un = 1.d0
    ASSERT(alpha.gt.1.d0)
!
    gamma2 = alpha-1.d0
    gamm1 = 1d0/gamma2
    beta = sqrt(2d0*alpha-1.d0)
    beta2 = 1d0/beta**2
    f0 = 0.5d0+(1d0/pi)*atan2(-gamma2/beta,un)
    c1 = 1d0-f0
    c2 = f0-0.5d0
    vref = 0d0
    v = -1d0
!
 1  continue
    if (-v .gt. vref) then
        call getran(u)
        y = beta*tan(pi*(u*c1+c2))+gamma2
        call getran(unif)
        ASSERT(unif.gt.0.d0)
        v = -log(unif)
        vref = log(1+beta2*((y-gamma2)**2))+gamma2*log(y*gamm1)-y +gamma2
        goto 1
    endif
!
    gamdev = y
    if (v .lt. 0) then
        valr = gamdev
        call utmess('A', 'ALGORITH13_18', sr=valr)
    endif
!
end function
