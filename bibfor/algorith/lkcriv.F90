subroutine lkcriv(vintr, invar, s, vin, nbmat,&
                  mater, ucriv, seuil)
!
    implicit    none
    include 'asterfort/cos3t.h'
    include 'asterfort/lcprsc.h'
    include 'asterfort/lkhtet.h'
    include 'asterfort/lkvacv.h'
    include 'asterfort/lkvarv.h'
    integer :: nbmat
    real(kind=8) :: invar, s(6), mater(nbmat, 2), vin(7), seuil
! =================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --- BUT : CRITERE VISQUEUX --------------------------------------
! =================================================================
! IN  : VINTR  :  VIN(3) ou XIVMAX ---------------------------------
! --- : INVAR :  INVARIANT DES CONTRAINTES ------------------------
! --- : S     :  DEVIATEUR DU TENSEUR DES CONTRAINTES A T+DT ------
! --- : VIN   :  VARIABLES INTERNES -------------------------------
! --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! OUT : SEUIL :  VALEUR DE F(S) VISQUEUX  -------------------------
! =================================================================
    integer :: ndi, ndt
    real(kind=8) :: sii, sigc, pref, lgleps
    real(kind=8) :: h0e, h0c, htheta
    real(kind=8) :: rcos3t, ucriv
    real(kind=8) :: paravi(3), varvi(4), vintr
! =================================================================
    common /tdim/   ndt , ndi
! =================================================================
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( lgleps  = 1.0d-8 )
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
! --- APPEL AUX FONCTIONS D ECROUISSAGE DU CRITERE VISQUEUX -------
! =================================================================
!
    call lkvarv(vintr, nbmat, mater, paravi)
    call lkvacv(nbmat, mater, paravi, varvi)
! =================================================================
! ---  CRITERE ELASTOPLASTIQUE ------------------------------------
! =================================================================
    ucriv = varvi(1)*sii*htheta + varvi(2)*invar+varvi(3)
!
    if (ucriv .lt. 0.0d0) ucriv=0.0d0
!
    seuil = sii*htheta - sigc*h0c*(ucriv)**paravi(1)
!      SEUIL =-1.0D0
! =================================================================
end subroutine
