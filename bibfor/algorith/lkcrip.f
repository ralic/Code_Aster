      SUBROUTINE LKCRIP (INVAR, S, VIN, NBMAT, MATER,
     &                   UCRIP, SEUIL)
C
      IMPLICIT    NONE
      INTEGER     NBMAT
      REAL*8      INVAR, S(6), MATER(NBMAT,2), VIN(7), SEUIL
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- BUT : VALEUR SEUIL POUR LE CONVEXE ELASTO-PLASTIQUE ---------
C =================================================================
C IN  : INVAR :  INVARIANT DES CONTRAINTES ------------------------
C --- : S     :  DEVIATEUR DES CONTRAINTES ------------------------
C --- : VIN   :  VARIABLES INTERNES -------------------------------
C --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C OUT : UCRIP
C       SEUIL :  VALEUR DE F(S) ELASTOPLASTIQUE -------------------
C =================================================================
      INTEGER   NDI, NDT
      REAL*8    SII, SIGC, PREF, LGLEPS
      REAL*8    RCOS3T, COS3T, H0E, H0C, HTHETA ,  UCRIP
      REAL*8    PARAEP(3), VARPL(4), ZERO
C =================================================================
      COMMON /TDIM/   NDT , NDI
C =================================================================
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( LGLEPS  = 1.0D-8 )
      PARAMETER       ( ZERO  = 0.D0 )
C =================================================================
C =================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
C =================================================================
      SIGC   = MATER(3,2)
      PREF   = MATER(1,2)
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
C --- APPEL AUX FONCTIONS D ECROUISSAGE DU CRITERE ELASTOPLASTIQUE-
C =================================================================

      CALL LKVARP(VIN, NBMAT, MATER, PARAEP)

      CALL LKVACP(NBMAT, MATER, PARAEP, VARPL)

C =================================================================
C ---  CRITERE ELASTOPLASTIQUE ------------------------------------
C =================================================================
      UCRIP  = VARPL(1)*SII*HTHETA + VARPL(2)*INVAR+VARPL(3)
      IF (UCRIP .LT. ZERO) GOTO 100

      SEUIL  = SII*HTHETA - SIGC*H0C*(UCRIP)**PARAEP(1)
C =================================================================
100   CONTINUE
      END
