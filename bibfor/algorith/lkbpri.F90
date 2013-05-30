function lkbpri(val, vin, nbmat, mater, para,&
                invar, s)
!
    implicit  none
    include 'asterc/r8pi.h'
    include 'asterfort/cos3t.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lkhtet.h'
    integer :: val, nbmat
    real(kind=8) :: vin(7), mater(nbmat, 2), para(3), invar, s(6), lkbpri
! =====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRP_6
! =================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
! =====================================================================
! --- BUT : CALCUL DU PARAMETRE BPRIME --------------------------------
! =====================================================================
! IN  : VAL    : INDICATEUR POUR LE CALCUL DE SIN(PSI) ----------------
! ----: VIN    : VARIABLE INTERNE (ICI XIP)
! ----: NBMAT  : NOMBRE DE PARAMETRES DU MODELE -----------------------
! --- : MATER  : PARAMETRES DU MODELE ---------------------------------
! --- : PARA   : VARIABLES D'ECROUISSAGE ------------------------------
! ------------ : PARA(1)=AXI ----------------------------------------
! ------------ : PARA(2)=SXI ----------------------------------------
! ------------ : PARA(3)=MXI ----------------------------------------
! --- : INVAR : PREMIER INVARIANT DU TENSEUR DES CONTRAINTES ----------
! --- : S     : DEVIATEUR DES CONTRAINTES -----------------------------
! OUT : LKBPRI : PARAMETRE CONTROLANT LE COMPORTEMENT VOLUMIQUE -------
! ------------ : DU MATERIAU ------------------------------------------
! =====================================================================
    integer :: ndt, ndi
    real(kind=8) :: zero, un, deux, trois, six, lgleps, pi
    real(kind=8) :: xip, pref, sigc, h0ext, s0, mult, xie, mvmax
    real(kind=8) :: mu0v, xi0v, mu1, xi1
    real(kind=8) :: sii, rcos3t, h0e
    real(kind=8) :: h0c, htheta
    real(kind=8) :: c, phi, alres, sigtil, sigmin, sigmax, siglim, alpha
    real(kind=8) :: sinpsi
    real(kind=8) :: troisd, tiers, fact1, fact2
! =====================================================================
! --- INITIALISATION DE PARAMETRES ------------------------------------
! =====================================================================
    parameter  ( zero   =  0.0d0   )
    parameter  ( un     =  1.0d0   )
    parameter  ( deux   =  2.0d0   )
    parameter  ( trois  =  3.0d0   )
    parameter  ( six    =  6.0d0   )
    parameter  ( lgleps =  1.0d-8 )
! =====================================================================
    common /tdim/   ndt , ndi
! =====================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ----------------------------
! =====================================================================
    pi = r8pi()
    pref = mater(1,2)
    sigc = mater(3,2)
    h0ext = mater(4,2)
    s0 = mater(11,2)
    mult = mater(15,2)
    xie = mater(17,2)
    mvmax = mater(19,2)
!
    mu0v = mater(24,2)
    xi0v = mater(25,2)
    mu1 = mater(26,2)
    xi1 = mater(27,2)
! =================================================================
! --- CALCUL DE ALPHA RES -----------------------------------------
! =================================================================
    alres = un + mult
! =================================================================
! --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
! =================================================================
    call lcprsc(s, s, sii)
    sii = sqrt (sii)
! =================================================================
! --- CALCUL DE h(THETA), H0E ET H0C -----------------------------
! =================================================================
    rcos3t = cos3t (s, pref, lgleps)
    call lkhtet(nbmat, mater, rcos3t, h0e, h0c,&
                htheta)
! =================================================================
! --- CALCUL DE C tilde -------------------------------------------
! =================================================================
    if (para(2) .eq. zero) then
        fact1 = un
    else
        fact1 = un + para(1)*para(3)*para(2)**(para(1)-un)
    endif
    c = sigc*(para(2))**para(1)/deux/sqrt(fact1)
! =================================================================
! --- CALCUL DE PHI tilde -----------------------------------------
! =================================================================
    phi = deux*atan(sqrt(fact1))-pi/deux
! =================================================================
! --- CALCUL DE SIGMA tilde ---------------------------------------
! =================================================================
    xip = vin(1)
!
    if (xip .le. xie) sigtil = c/tan(phi)
!
    if (xip .gt. xie) sigtil = zero
!
! =================================================================
! --- CALCUL DE SIGMIN ET SIGMAX ----------------------------------
! =================================================================
    troisd = trois/deux
    tiers = un/trois
!
    fact2 = (deux*htheta -(h0c + h0ext))/deux/(h0c-h0ext)
!
    sigmin = tiers * (invar - (troisd-fact2)*sqrt(troisd)*sii)
    sigmax = tiers * (invar + (troisd+fact2)*sqrt(troisd)*sii)
!
! =================================================================
! --- CALCUL DE SIGLIM  -------------------------------------------
! =================================================================
!
    siglim = sigmin + sigc * (mvmax*sigmin/sigc + s0)
!
! =================================================================
! --- CALCUL DE ALPHA  --------------------------------------------
! =================================================================
!
    alpha = (sigmax+sigtil)/(sigmin+sigtil)
!
! =================================================================
! --- CALCUL DE SIN(PSI) ------------------------------------------
! =================================================================
!
    if (val .eq. 0) then
!
        sinpsi = mu0v*((sigmax - siglim)/(xi0v*sigmax + siglim))
    else
        sinpsi = mu1*((alpha - alres)/(xi1*alpha + alres))
!
!
    endif
! =================================================================
! --- CALCUL DE LKBPRI=BPRIME -------------------------------------
! =================================================================
    lkbpri = -deux*sqrt(six)*sinpsi/(trois-sinpsi)
! =================================================================
end function
