      SUBROUTINE LKCRIV (VINTR,INVAR, S, VIN, NBMAT, MATER, UCRIV,SEUIL)
C
      IMPLICIT    NONE
      INTEGER     NBMAT
      REAL*8      INVAR, S(6), MATER(NBMAT,2), VIN(7), SEUIL
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/01/2013   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C =================================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
C =================================================================
C --- BUT : CRITERE VISQUEUX --------------------------------------
C =================================================================
C IN  : VINTR  :  VIN(3) ou XIVMAX ---------------------------------
C --- : INVAR :  INVARIANT DES CONTRAINTES ------------------------
C --- : S     :  DEVIATEUR DU TENSEUR DES CONTRAINTES A T+DT ------
C --- : VIN   :  VARIABLES INTERNES -------------------------------
C --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C OUT : SEUIL :  VALEUR DE F(S) VISQUEUX  -------------------------
C =================================================================
      INTEGER   NDI, NDT
      REAL*8    SII, SIGC, PREF, LGLEPS
      REAL*8    H0E, H0C, HTHETA
      REAL*8    COS3T, RCOS3T, UCRIV
      REAL*8    PARAVI(3), VARVI(4), VINTR
C =================================================================
      COMMON /TDIM/   NDT , NDI
C =================================================================
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( LGLEPS  = 1.0D-8 )
C =================================================================
C =================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
C =================================================================
      SIGC     = MATER(3,2)
      PREF     = MATER(1,2)
C =================================================================
C --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
C =================================================================
      CALL     LCPRSC(S, S, SII)
      SII    = SQRT  (SII)
C =================================================================
C --- APPEL A HOC ET  H(THETA) ------------------------------------
C =================================================================

      RCOS3T = COS3T (S, PREF, LGLEPS)
      CALL LKHTET (NBMAT, MATER, RCOS3T, H0E, H0C, HTHETA)
C =================================================================
C --- APPEL AUX FONCTIONS D ECROUISSAGE DU CRITERE VISQUEUX -------
C =================================================================

      CALL LKVARV(VINTR,NBMAT, MATER, PARAVI)
      CALL LKVACV(NBMAT, MATER, PARAVI, VARVI)
C =================================================================
C ---  CRITERE ELASTOPLASTIQUE ------------------------------------
C =================================================================
      UCRIV  = VARVI(1)*SII*HTHETA + VARVI(2)*INVAR+VARVI(3)

      IF (UCRIV .LT. 0.0D0) UCRIV=0.0D0

      SEUIL  = SII*HTHETA - SIGC*H0C*(UCRIV)**PARAVI(1)
C      SEUIL =-1.0D0
C =================================================================
      END
