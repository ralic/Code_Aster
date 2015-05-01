subroutine hbderi(gamma, nbmat, materf, vg, eta,&
                  param2, parame)
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
! =====================================================================
    implicit      none
#include "asterc/r8pi.h"
    integer :: nbmat
    real(kind=8) :: gamma, materf(nbmat, 2), parame(5), vg, eta, param2(4)
! ======================================================================
! --- HOEK BROWN : CALCUL DES DERIVEES DES FONCTIONS DE LA VARIABLES ---
! --- D ECROUISSAGE PAR RAPPORT A LA VARIABLE D ECROUISSAGE GAMMA ------
! --- DERIVE : S*SIG_C**2, M*SIG_C, B, VH, VG --------------------------
! ======================================================================
! IN  GAMMA  VALEUR DE LA VARIABLE D ECROUISSAGE -----------------------
! IN  NBMAT  NOMBRE DE DONNEES MATERIAU --------------------------------
! IN  MATERF DONNEES MATERIAU ------------------------------------------
! IN  VG     VALEUR DE LA FONCTION G DE GAMMA --------------------------
! IN  PARAM2 VALEUR DES PARAMETRES D ECROUISSAGE S*SIGC2,M*SIGC,B,PHI---
! OUT PARAME DERIVEES DES PARAMETRES D ECROUISSAGE S*SIGC2,M*SIGC,B,H,G
! ======================================================================
    real(kind=8) :: aux2, aux3, aux4, aux5, aux6
    real(kind=8) :: grup, gres, send, srup, mend, mrup
    real(kind=8) :: ap, dp, pphi1, k, pphi2, pi, pphi0
! ======================================================================
! --- RECUPERATION DES DONNEES MATERIAU --------------------------------
! ======================================================================
    pi = r8pi()/180.0d0
    grup = materf(1,2)
    gres = materf(2,2)
    mend = materf(5,2)
    mrup = materf(6,2)
    send = materf(3,2)
    srup = materf(4,2)
    ap = materf(11,2)
    dp = materf(12,2)
    pphi1 = materf(9,2)
    k = materf(5,1)
    pphi2 = materf(15,2)
    pphi0 = materf(16,2)
! ======================================================================
    if (gamma .lt. grup) then
        aux2 = (srup-send)/grup
        aux3 = (mrup-mend)/grup
        aux4 = 0.d0
        aux5 =-2.0d0*(pphi1-pphi0)*pi*cos(param2(4)*pi)/ (grup*3.0d0*(&
        1.0d0+sin(param2(4)*pi))**2)
        aux6 = aux5*vg*(eta+1.0d0) +6.0d0*k*(pphi1-pphi0)*pi*cos( param2(4)*pi)/ (grup*(3.0d0+sin&
               &(param2(4)*pi))*(1.0d0+sin( param2(4)*pi)))
! ======================================================================
    else if (gamma.lt.gres) then
        aux2 = 0.d0
        aux3 = 0.d0
        aux4 = 2.d0*ap*gamma + dp
        aux5 =-2.0d0*(pphi2-pphi1)*pi*cos(param2(4)*pi)/ ((gres-grup)*&
        3.0d0*(1.0d0+sin(param2(4)*pi))**2)
        aux6 = aux5*vg*(eta+1.0d0) +6.0d0*k*(pphi2-pphi1)*pi*cos( param2(4)*pi)/ ((gres-grup)*(3.&
               &0d0+sin(param2(4)*pi)) *(1.0d0+ sin(param2(4)*pi)))
! ======================================================================
    else
        aux2 = 0.d0
        aux3 = 0.d0
        aux4 = 0.d0
        aux5 = 0.0d0
        aux6 = 0.0d0
    endif
! ======================================================================
! --- STOCKAGE ---------------------------------------------------------
! ======================================================================
    parame(1) = aux2
    parame(2) = aux3
    parame(3) = aux4
    parame(4) = aux5
    parame(5) = aux6
! ======================================================================
end subroutine
