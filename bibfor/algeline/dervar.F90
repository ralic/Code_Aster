subroutine dervar(gamp, nbmat, mater, parame, derpar)
!
    implicit      none
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), gamp, parame(5), derpar(4)
! ======================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DES DERIVEES DES VARIABLES D'ECROUISSAGE ------------
! ======================================================================
! IN  : GAMP   : DEFORMATION DEVIATOIRE PLASTIQUE CUMULEE --------------
! --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------------
! --- : MATER  : PARAMETRES DU MODELE ----------------------------------
! --- : PARAME : VARIABLES D'ECROUISSAGE (S,A,K,M,O) -------------------
! OUT : DERPAR : DERIVE DES VARIABLES D'ECROUISSAGE PAR RAPPORT A GAMP -
! ------------ : (DS/DGAMP,DA/DGAMP,DK/DGAMP,DM/DGAMP,DO/DGAMP) --------
! ======================================================================
    real(kind=8) :: mun, zero, un, deux, trois, epsult
    real(kind=8) :: gamult, gammae, me, ae, mpic, apic, eta, sigc, sigp1, sigp2
    real(kind=8) :: agamp, omega
    real(kind=8) :: ds, domega, dado, da, dk, dmds, dmda, dm
    real(kind=8) :: fact1, fact2, fact3, fact4, puis1
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    =  -1.0d0   )
    parameter       ( zero   =   0.0d0   )
    parameter       ( un     =   1.0d0   )
    parameter       ( deux   =   2.0d0   )
    parameter       ( trois  =   3.0d0   )
    parameter       ( epsult =   1.0d-03 )
! ======================================================================
    call jemarq()
! ======================================================================
! --- RECUPERATION DES PARAMETRES DU MODELE ----------------------------
! ======================================================================
    gamult = mater( 1,2)
    gammae = mater( 2,2)
    me = mater( 4,2)
    ae = mater( 5,2)
    mpic = mater( 6,2)
    apic = mater( 7,2)
    eta = mater( 8,2)
    sigc = mater( 9,2)
    sigp1 = mater(13,2)
    sigp2 = mater(14,2)
! ======================================================================
! --- RECUPERATION DES PARAMETRES DU MODELE ----------------------------
! ======================================================================
    agamp = parame(2)
    omega = parame(5)
! ======================================================================
! --- CALCUL DES DERIVEES DES VARIABLES D'ECROUISSAGES -----------------
! --- POUR LE CAS GAMP > GAMULT(1-EPS) ---------------------------------
! ======================================================================
    if (gamp .gt. (gamult*(un-epsult))) then
        ds = zero
        da = zero
        dk = zero
        dm = zero
! ======================================================================
! SINON ----------------------------------------------------------------
! ======================================================================
    else
! ======================================================================
! --- CALCUL DE DS/DGAMP = -1/GAMMAE -----------------------------------
! ======================================================================
        if (gamp .lt. gammae) then
            ds = mun/gammae
        else
            ds = zero
        endif
! ======================================================================
! --- CALCUL DE DOMEGA/DGAMP = -----------------------------------------
! ------- (GAMULT-GAMMAE)/(GAMMAE)**ETA*((AE-APIC)/(1-AE))* ------------
! ---- (ETA*GAMP**(ETA-1)/(GAMULT-GAMP) + GAMP**ETA/(GAMULT-GAMP)**2 ---
! ======================================================================
        fact1 = (gamult-gammae)/(gammae**eta)
        fact2 = (ae-apic)/(un-ae)
        fact3 = eta*(gamp**(eta-un))/(gamult-gamp)
        fact4 = (gamp**eta)/((gamult-gamp)*(gamult-gamp))
        domega = fact1*fact2*(fact3+fact4)
! ======================================================================
! --- CALCUL DE DA/DOMEGA = (1-APIC)/(1+OMEGA(GAMP))**2 ----------------
! ======================================================================
        dado = (un-apic)/((un+omega)*(un+omega))
! ======================================================================
! --- CALCUL DE DA/DGAMP = DA/DOMEGA * DOMEGA/DGAMP --------------------
! ======================================================================
        da = dado * domega
! ======================================================================
! --- CALCUL DE DK/DGAMP = -(2/3)**(1/(2*A(GAMP)))* --------------------
! ------------------------  LOG(2/3)/(2*A(GAMP)*A(GAMP))*DA/DGAMP ------
! ======================================================================
        puis1 = un/(deux*agamp)
        dk = mun*((deux/trois)**puis1)* log(deux/trois)*da/(deux* agamp*agamp)
! ======================================================================
! --- CALCUL DE DM/DGAMP = ---------------------------------------------
! --- SI GAMP  <  GAMMAE : DM/DA*DA/DGAMP+DM/DS*DS/DGAMP ---------------
! --- AVEC DM/DA = -SIGC/SIGP1*LOG(MPIC*SIGP1/SIGC+1)*APIC/A(GAMP)**2* -
! ---------------- (MPIC*SIGP1/SIGC+1)**(APIC/A(GAMP)) -----------------
! -------- DM/DS = -SIGC/SIGP1 -----------------------------------------
! --- SI GAMP  >= GAMMAE : DM/DA*DA/DGAMP ------------------------------
! --- AVEC DM/DA = -SIGC/SIGP2*LOG(ME*SIGP2/SIGC)*AE/A(GAMP)**2* -------
! ---------------- (ME*SIGP2/SIGC)**(AE/A(GAMP)) -----------------------
! ======================================================================
        if (gamp .lt. gammae) then
            dmds = mun*sigc/sigp1
            fact1 = mpic*sigp1/sigc+un
            fact2 = apic/agamp
            dmda = mun*sigc*log(fact1)*fact2* (fact1**fact2)/(sigp1* agamp)
            dm = dmda*da + dmds*ds
        else
            fact1 = me*sigp2/sigc
            fact2 = ae/agamp
            dmda = mun*sigc*log(fact1)*fact2* (fact1**fact2)/(sigp2* agamp)
            dm = dmda*da
        endif
    endif
! ======================================================================
! --- STOCKAGE ---------------------------------------------------------
! ======================================================================
    derpar(1) = ds
    derpar(2) = da
    derpar(3) = dk
    derpar(4) = dm
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
