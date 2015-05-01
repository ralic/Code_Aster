subroutine dpvpva(vin, nbmat, mater, fonecr)
!
    implicit      none
    integer :: nbmat
    real(kind=8) :: vin(4), fonecr(3), mater(nbmat, 2)
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
! ================================================================
! --- MODELE VISC_DRUC_PRAG : DRUCKER PRAGER VISCOPLASTIQUE-------
! ================================================================
! --- BUT : CALCUL DES VARIABLES D'ECROUISSAGE -------------------
! ================================================================
! --- : VIN    : TABLEAU DES VARIABLE INTERNES (ICI P) -----------
! --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE ------------------
! --- : MATER  : PARAMETRES DU MODELE ----------------------------
! OUT : FONECR : FONCTIONS D'ECROUISSAGE -------------------------
! ------------ : ALPHA,R ,BETA  ----------------------------------
! ================================================================
    real(kind=8) :: alpha, r, beta
    real(kind=8) :: alpha0, beta0, r0
    real(kind=8) :: alphap, betap, rpic
    real(kind=8) :: alphau, betau, rult
    real(kind=8) :: p, zero, ppic, pult
! ================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------
! ================================================================
    parameter       ( zero   =  0.0d0   )
! ================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE -----------------------
! ================================================================
    ppic = mater(4,2)
    pult = mater(5,2)
    alpha0 = mater(6,2)
    alphap = mater(7,2)
    alphau = mater(8,2)
    r0 = mater(9,2)
    rpic = mater(10,2)
    rult = mater(11,2)
    beta0 = mater(12,2)
    betap = mater(13,2)
    betau = mater(14,2)
!
! ================================================================
! CALCUL DES FONCTIONS D'ECROUISSAGES POUR LE CAS 0<P<PPIC-----
! ================================================================
    p = vin(1)
    if ((p.ge. zero) .and. (p.lt. ppic)) then
        alpha = alpha0 + (alphap-alpha0)/ppic * p
!
        r = r0 + (rpic-r0)/ppic * p
!
        beta = beta0 + (betap - beta0)/ppic * p
! ================================================================
! CALCUL DES FONCTIONS D'ECROUISSAGES POUR LE CAS PPIC< P < PULT
! ================================================================
    else if ((p.ge.ppic).and.(p.lt.pult)) then
        alpha = alphap + (alphau-alphap)/(pult-ppic) * (p - ppic)
!
        r = rpic + (rult - rpic)/(pult - ppic) * (p - ppic)
!
        beta = betap + (betau - betap)/(pult - ppic) * (p - ppic)
! ================================================================
! CALCUL DES FONCTIONS D'ECROUISSAGES POUR LE CAS P > PULT ----
! ================================================================
    else if (p.ge.pult) then
        alpha = alphau
!
        r = rult
!
        beta = betau
    endif
! ================================================================
! --- STOCKAGE ---------------------------------------------------
! ================================================================
    fonecr(1) = alpha
    fonecr(2) = r
    fonecr(3) = beta
! ================================================================
end subroutine
