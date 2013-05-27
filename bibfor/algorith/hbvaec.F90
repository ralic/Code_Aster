subroutine hbvaec(gamma, nbmat, materf, parame)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! =====================================================================
    implicit      none
    integer :: nbmat
    real(kind=8) :: gamma, materf(nbmat, 2), parame(4)
! ======================================================================
! --- HOEK BROWN : CALCUL DES FONCTIONS DE LA VARIABLE D ECROUISSAGE ---
! --- PARAME : S*SIG_C**2, M*SIG_C, B, PHI -----------------------------
! ======================================================================
! IN  GAMMA  VALEUR DE LA VARIABLE D ECROUISSAGE -----------------------
! IN  NBMAT  NOMBRE DE DONNEES MATERIAU --------------------------------
! IN  MATERF DONNEES MATERIAU ------------------------------------------
! OUT PARAME PARAMETRES D ECROUISSAGE S*SIGC2, M*SIGC, B, PPHI ---------
! ======================================================================
    real(kind=8) :: aux2, aux3, aux4, aux5
    real(kind=8) :: grup, gres, send, srup, mend, mrup, pphi1
    real(kind=8) :: bres, ap, dp, cp, pphi2, pphi0
! ======================================================================
! --- RECUPERATION DES DONNEES MATERIAU --------------------------------
! ======================================================================
    grup = materf(1,2)
    gres = materf(2,2)
    mend = materf(5,2)
    mrup = materf(6,2)
    send = materf(3,2)
    srup = materf(4,2)
    bres = materf(10,2)
    ap = materf(11,2)
    dp = materf(12,2)
    cp = materf(13,2)
    pphi1 = materf(9,2)
    pphi2 = materf(15,2)
    pphi0 = materf(16,2)
! ======================================================================
    if (gamma .lt. grup) then
        aux2 = gamma*(srup-send)/grup + send
        aux3 = gamma*(mrup-mend)/grup + mend
        aux4 = 0.d0
        aux5 = (pphi1-pphi0)*gamma/grup + pphi0
! ======================================================================
    else if (gamma.lt.gres) then
        aux2 = srup
        aux3 = mrup
        aux4 = ap*gamma**2 + dp*gamma + cp
        aux5 = gamma*(pphi2-pphi1)/(gres-grup)+ (pphi1*gres-pphi2* grup)/(gres-grup)
! ======================================================================
    else
        aux2 = srup
        aux3 = mrup
        aux4 = bres
        aux5 = pphi2
    endif
! ======================================================================
! --- STOCKAGE ---------------------------------------------------------
! ======================================================================
    parame(1) = aux2
    parame(2) = aux3
    parame(3) = aux4
    parame(4) = aux5
! ======================================================================
end subroutine
