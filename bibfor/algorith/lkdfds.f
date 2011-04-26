      SUBROUTINE LKDFDS(NBMAT,MATER,S,PARA,VAR,DS2HDS,
     &                  UCRI,DFDSIG)
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        MATER(NBMAT,2)
      REAL*8        S(6), PARA(3), VAR(4), UCRI
      REAL*8        DS2HDS(6), DFDSIG(6)
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
C --- BUT : CALCUL DE DF/DSIG -------------------------------------
C =================================================================
C IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE -------------------
C --- : MATER  : PARAMETRES DU MODELE -----------------------------
C     : S      : TENSEUR DU DEVIATEUR DES CONTRAINTES -------------
C     : PARA   : VARIABLE D'ECROUISSAGE ---------------------------
C ------------ : PARA(1)=AXI --------------------------------------
C ------------ : PARA(2)=SXI --------------------------------------
C ------------ : PARA(3)=MXI --------------------------------------
C     : VAR  : ADXI, BDXI, DDXI -----------------------------------
C     : DS2HDS: d(sII*h(THETA))/dsig ------------------------------
C     : UCRI  : LE TERME SOUS LA PUISSANCE DANS LE CRITERE --------
C OUT : DFDSIG : dF/dsig ------------------------------------------
C =================================================================
      INTEGER NDI, NDT, I
      REAL*8  PREF, SIGC, COS3T, RCOS3T, H0C, H0E, HTHETA
      REAL*8  ZERO, UN, LGLEPS
      REAL*8  A(6), KRON(6)
      REAL*8  FACT1, FACT3,R8PREM
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( ZERO    =  0.0D0   )
      PARAMETER       ( UN      =  1.0D0   )
      PARAMETER       ( LGLEPS  =  1.0D-8  )
C =================================================================
      COMMON /TDIM/   NDT , NDI
C =================================================================
      DATA    KRON    /UN  ,UN  ,UN  ,ZERO  ,ZERO  ,ZERO/
C =================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
C =================================================================
      SIGC   = MATER(3,2)
      PREF   = MATER(1,2)
C =================================================================
C --- CALCUL DE h(THETA), H0E ET H0C, -----------------------------
C =================================================================
      RCOS3T = COS3T (S, PREF, LGLEPS)
      CALL LKHTET (NBMAT, MATER, RCOS3T, H0E, H0C, HTHETA)
C =================================================================
C --- CALCUL DES TERMES INTERMEDIARES
C =================================================================
      FACT1 = PARA(1) * SIGC * H0C
      FACT3 = PARA(1) - UN
C =================================================================
C --- RESULTAT FINAL
C =================================================================
      CALL R8INIR(6,0.D0,A,1)
      CALL R8INIR(6,0.D0,DFDSIG,1)

      DO 10 I = 1, NDT
      A(I) = VAR(1) * DS2HDS(I) + VAR(2)* KRON (I)
 10   CONTINUE

      DO 20 I = 1, NDT
      IF (UCRI .LE. R8PREM()) THEN
      DFDSIG(I) =  DS2HDS(I)
      ELSE
      DFDSIG(I) =  DS2HDS(I) - FACT1*((UCRI)**FACT3)*A(I)
      ENDIF
 20   CONTINUE
C =================================================================
      END
