subroutine lkcrip(invar, s, vin, nbmat, mater,&
                  ucrip, seuil)
!
    implicit    none
    include 'asterfort/cos3t.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lkhtet.h'
    include 'asterfort/lkvacp.h'
    include 'asterfort/lkvarp.h'
    integer :: nbmat
    real(kind=8) :: invar, s(6), mater(nbmat, 2), vin(7), seuil
! =================================================================
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
! =================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
! =================================================================
! --- BUT : VALEUR SEUIL POUR LE CONVEXE ELASTO-PLASTIQUE ---------
! =================================================================
! IN  : INVAR :  INVARIANT DES CONTRAINTES ------------------------
! --- : S     :  DEVIATEUR DES CONTRAINTES ------------------------
! --- : VIN   :  VARIABLES INTERNES -------------------------------
! --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! OUT : UCRIP
!       SEUIL :  VALEUR DE F(S) ELASTOPLASTIQUE -------------------
! =================================================================
    integer :: ndi, ndt
    real(kind=8) :: sii, sigc, pref, lgleps
    real(kind=8) :: rcos3t, h0e, h0c, htheta, ucrip
    real(kind=8) :: paraep(3), varpl(4), zero
! =================================================================
    common /tdim/   ndt , ndi
! =================================================================
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( lgleps  = 1.0d-8 )
    parameter       ( zero  = 0.d0 )
! =================================================================
! =================================================================
! --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
! =================================================================
    sigc = mater(3,2)
    pref = mater(1,2)
! =================================================================
! --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
! =================================================================
    call lcprsc(s, s, sii)
    sii = sqrt (sii)
! =================================================================
! --- APPEL A HOC ET  H(THETA) ------------------------------------
! =================================================================
!
    rcos3t = cos3t (s, pref, lgleps)
    call lkhtet(nbmat, mater, rcos3t, h0e, h0c,&
                htheta)
! =================================================================
! --- APPEL AUX FONCTIONS D ECROUISSAGE DU CRITERE ELASTOPLASTIQUE-
! =================================================================
!
    call lkvarp(vin, nbmat, mater, paraep)
!
    call lkvacp(nbmat, mater, paraep, varpl)
!
! =================================================================
! ---  CRITERE ELASTOPLASTIQUE ------------------------------------
! =================================================================
    ucrip = varpl(1)*sii*htheta + varpl(2)*invar+varpl(3)
    if (ucrip .lt. zero) goto 100
!
    seuil = sii*htheta - sigc*h0c*(ucrip)**paraep(1)
! =================================================================
100  continue
end subroutine
