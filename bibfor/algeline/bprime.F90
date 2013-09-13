function bprime(nbmat, mater, parame, invar1, s,&
                epssig)
!
    implicit none
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/cos3t.h"
#include "asterfort/lglord.h"
#include "asterfort/utmess.h"
#include "blas/ddot.h"
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), parame(5), invar1, s(6), epssig, bprime
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DU PARAMETRE BPRIME ---------------------------------
! ======================================================================
! IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
! --- : MATER  : PARAMETRES DU MODELE ----------------------------------
! --- : PARAME : VARIABLES D'ECROUISSAGE -------------------------------
! --- : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
! --- : INVAR1 : PREMIER INVARIANT DU TENSEUR DES CONTRAINTES ----------
! --- : S      : DEVIATEUR DES CONTRAINTES -----------------------------
! --- : EPSSIG : EPSILON -----------------------------------------------
! OUT : BPRIME : PARAMETRE CONTROLANT LE COMPORTEMENT VOLUMIQUE --------
! ------------ : DU MATERIAU -------------------------------------------
! ======================================================================
    integer :: ndt, ndi
    real(kind=8) :: mun, un, deux, trois, six, epstol
    real(kind=8) :: mult, sigc, gamma, ksi, pref
    real(kind=8) :: sgamp, agamp, mgamp, sii, fact1, fact2
    real(kind=8) :: rcos3t
    real(kind=8) :: phi0, c0, sigt0, sig1, sig2, sig3, alpha, sinpsi
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter  ( mun    = -1.0d0   )
    parameter  ( un     =  1.0d0   )
    parameter  ( deux   =  2.0d0   )
    parameter  ( trois  =  3.0d0   )
    parameter  ( six    =  6.0d0   )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    epstol = r8prem()
    sigt0 = 0.0d0
! ======================================================================
! --- INITIALISATION DES PARAMETRES MATERIAU ---------------------------
! ======================================================================
    mult = mater( 3,2)
    sigc = mater( 9,2)
    gamma = mater(10,2)
    ksi = mater(11,2)
    pref = mater(15,2)
! ======================================================================
! --- RECUPERATION DES VARIABLES D'ECROUISSAGE -------------------------
! ======================================================================
    sgamp = parame(1)
    agamp = parame(2)
    mgamp = parame(4)
! ======================================================================
! --- CALCULS INTERMEDIAIRE POUR LE CALCUL DE BPRIME -------------------
! ======================================================================
    sii=ddot(ndt,s,1,s,1)
    sii = sqrt (sii)
    rcos3t = cos3t(s, pref, epssig)
! ======================================================================
! --- CALCUL DE PHI0 = 2*ARCTAN(RAC(1+A*M*S**(A-1))) - PI/2 ------------
! ======================================================================
! --- SI SGAMP = 0 ON A ALPHA = SIG1/SIG3 ------------------------------
! ======================================================================
    if (sgamp .lt. epstol) goto 10
    fact1 = sgamp**(agamp-un)
    fact2 = un+agamp*mgamp*fact1
    if (fact2 .lt. epstol) then
        call utmess('F', 'ALGELINE_4')
    endif
    fact2 = sqrt(fact2)
    phi0 = deux*atan2(fact2,un) - r8pi()/deux
! ======================================================================
! --- CALCUL DE C0 = SIGC*S**A/(RAC(1+A*M*S**(A-1))) -------------------
! ======================================================================
    c0 = sigc * sgamp**agamp/fact2
! ======================================================================
! --- CALCUL DE SIGT0 = 2*C0*RAC((1-SIN(PHI0))/(1+SIN(PHI0)) -----------
! ======================================================================
    if ((un+sin(phi0)) .lt. epstol) then
        call utmess('F', 'ALGELINE_4')
    endif
    sigt0 = deux*c0*sqrt((un-sin(phi0))/(un+sin(phi0)))
10  continue
! ======================================================================
! --- CALCULS DE INTERMEDIAIRE -----------------------------------------
! ======================================================================
    sig1 = invar1/trois + sqrt(deux/trois)*sii*rcos3t
    sig2 = invar1/trois - sqrt(deux/trois)*sii* ( rcos3t/deux+sqrt(trois*(un-rcos3t*rcos3t))/deux&
           & )
    sig3 = invar1/trois + sqrt(deux/trois)*sii* (-rcos3t/deux+sqrt(trois*(un-rcos3t*rcos3t))/deux&
           & )
! ======================================================================
! --- RECUPERATION DE SIG1 (MAX) ET SIG3 (MIN) -------------------------
! ======================================================================
    call lglord(sig1, sig2, sig3)
! ======================================================================
! --- CALCUL DE ALPHA = (SIG1-SIGT0)/(SIG3-SIGT0) ----------------------
! ======================================================================
    if (abs(sig3-sigt0) .lt. epstol) then
        alpha = un
    else
        alpha = (sig1-sigt0)/(sig3-sigt0)
    endif
! ======================================================================
! --- CALCUL DE SIN(PSI) = GAMMA*(ALPHA-MULT-1) / (KSI*ALPHA+MULT+1) ---
! ======================================================================
    sinpsi = gamma*(alpha-mult-un)/(ksi*alpha+mult+un)
! ======================================================================
! --- AJUSTEMENT DE LA LOI DE DILATANCE --------------------------------
! ======================================================================
    if (sinpsi .lt. mun) sinpsi = mun
    if (sinpsi .gt. un) sinpsi = un
! ======================================================================
! --- CALCUL DE BPRIME = -2*RAC(6)*SIN(PSI)/(3-SIN(PSI)) ---------------
! ======================================================================
    bprime = mun*deux*sqrt(six)*sinpsi/(trois-sinpsi)
! ======================================================================
end function
