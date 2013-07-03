subroutine varecr(gamp, nbmat, mater, parame)
!
    implicit      none
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbmat
    real(kind=8) :: gamp, parame(5), mater(nbmat, 2)
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
! --- BUT : CALCUL DES VARIABLES D'ECROUISSAGE -------------------------
! ======================================================================
! IN  : GAMP   : DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE --------------
! --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
! --- : MATER  : PARAMETRES DU MODELE ----------------------------------
! OUT : PARAME : VARIABLE D'ECROUISSAGE S(GAMP) ------------------------
! ------------ : SGAMP, AGAMP, KGAMP, MGAMP, OMEGA ---------------------
! ======================================================================
    real(kind=8) :: sgamp, agamp, kgamp, mgamp, omega, epsult
    real(kind=8) :: gamult, gammae, mult, me, ae, mpic, apic, eta, sigc, zero
    real(kind=8) :: sigp1, sigp2, fact1, fact2, fact3, puis1, un, deux, trois
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( zero   = 0.0d0   )
    parameter       ( un     = 1.0d0   )
    parameter       ( deux   = 2.0d0   )
    parameter       ( trois  = 3.0d0   )
    parameter       ( epsult = 1.0d-03 )
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE -----------------------------
! ======================================================================
    gamult = mater( 1,2)
    gammae = mater( 2,2)
    mult = mater( 3,2)
    me = mater( 4,2)
    ae = mater( 5,2)
    mpic = mater( 6,2)
    apic = mater( 7,2)
    eta = mater( 8,2)
    sigc = mater( 9,2)
    sigp1 = mater(13,2)
    sigp2 = mater(14,2)
! ======================================================================
! CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS GAMP > GAMULT(1-EPS) -
! ======================================================================
    if (gamp .gt. (gamult*(un-epsult))) then
        sgamp = zero
        omega = zero
        agamp = un
        mgamp = mult
        kgamp = sqrt(deux/trois)
! ======================================================================
! SINON ----------------------------------------------------------------
! ======================================================================
    else
! ======================================================================
! --- CALCUL DE OMEGA(GAMP) = ------------------------------------------
! --- (GAMP/GAMMAE)**ETA*  ---------------------------------------------
! ------------  *((AE-APIC)/(1-AE))*((GAMULT-GAMMAE)/(GAMULT-GAMP)) ----
! ======================================================================
        fact1 = (gamp/gammae)**eta
        fact2 = (ae-apic)/(un-ae)
        fact3 = (gamult-gammae)/(gamult-gamp)
        omega = fact1*fact2*fact3
! ======================================================================
! --- CALCUL DE A(GAMP) = (APIC+OMEGA(GAMP))/(1+OMEGA(GAMP)) -----------
! ======================================================================
        agamp = (apic+omega)/(un+omega)
! ======================================================================
! --- CALCUL DE K(GAMP) = (2/3)**(1/(2*A(GAMP))) -----------------------
! ======================================================================
        puis1 = un/(deux*agamp)
        kgamp = (deux/trois)**puis1
! ======================================================================
! --- CAS OU GAMP < GAMMAE ---------------------------------------------
! ======================================================================
        if (gamp .lt. gammae) then
! ======================================================================
! --- CALCUL DE S(GAMP) = (1-GAMP/GAMMAE) ------------------------------
! ======================================================================
            sgamp = un-gamp/gammae
! ======================================================================
! --- CALCUL DE M(GAMP) = ----------------------------------------------
! ----- SIGC/SIGP1*((MPIC*SIGP1/SIGC+1)**(APIC/A(GAMP))-S(GAMP)) -------
! ======================================================================
            fact1 = sigc/sigp1
            fact2 = mpic*sigp1/sigc+un
            puis1 = apic/agamp
            mgamp = fact1*(fact2**puis1-sgamp)
! ======================================================================
! --- CAS OU GAMP >= GAMMAE --------------------------------------------
! ======================================================================
        else
! ======================================================================
! --- CALCUL DE S(GAMP) = 0 --------------------------------------------
! ======================================================================
            sgamp = zero
! ======================================================================
! --- CALCUL DE M(GAMP) = ----------------------------------------------
! ----- SIGC/SIGP2*(ME*SIGP2/SIGC)**(AE/A(GAMP)) -----------------------
! ======================================================================
            fact1 = sigc/sigp2
            fact2 = me*sigp2/sigc
            puis1 = ae/agamp
            mgamp = fact1*(fact2**puis1)
        endif
    endif
! ======================================================================
! --- STOCKAGE ---------------------------------------------------------
! ======================================================================
    parame(1) = sgamp
    parame(2) = agamp
    parame(3) = kgamp
    parame(4) = mgamp
    parame(5) = omega
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
