      SUBROUTINE LKDHDS(NBMAT, MATER,INVAR,S,DHDS,RETCOM)
C
      IMPLICIT      NONE
      INTEGER       NBMAT,RETCOM
      REAL*8        MATER(NBMAT,2),INVAR,S(6), DHDS(6)
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/10/2008   AUTEUR ELGHARIB J.EL-GHARIB 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- BUT : CALCUL DES DERICEES dh(THETA)/ds ----------------------
C =================================================================
C IN  :  NBMAT :  NOMBRE DE PARAMETRES MATERIAU -------------------
C --- :  MATER :  COEFFICIENTS MATERIAU A T+DT --------------------
C -----------  :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES --------
C -----------  :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES --------
C --- :  INVAR : INVARINAT DES CONTRAINTES ------------------------
C --- :  S     : DEVIATEUR DES CONTRAINTES ------------------------
C OUT : DHDS: dh(theta)/ds ----------------------------------------
C     : RETCOM : CODE RETOUR POUR REDECOUPAGE ---------------------
C =================================================================
      INTEGER NDT, NDI, II
      REAL*8  GAMCJS, PREF
      REAL*8  T(6), DEVT(6)
      REAL*8  SII, COS3T, RCOS3T, LKHLOD, RHLODE, H5
      REAL*8  DEUX, CINQ, SIX, LGLEPS, PTIT, R8MIEM
      REAL*8  FACT1, FACT2
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( DEUX    =  2.0D0   )
      PARAMETER       ( CINQ    =  5.0D0   )
      PARAMETER       ( SIX     =  6.0D0   )
      PARAMETER       ( LGLEPS  =  1.0D-8  )
C -----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C -----------------------------------------------------------------
C =================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
C =================================================================
      GAMCJS = MATER(5,2)
      PREF   = MATER(1,2)
C =================================================================
C --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
C =================================================================
      RETCOM = 0
      PTIT = R8MIEM()
      CALL     LCPRSC(S, S, SII)
      SII    = SQRT  (SII)
      IF (SII .LT. PTIT) THEN
       CALL U2MESS('A','COMPOR1_29')
       RETCOM = 1
       GOTO 1000
      ENDIF
C =================================================================
C --- CALCUL DE h(THETA) ------------------------------------------
C =================================================================
      RCOS3T = COS3T (S, PREF, LGLEPS)
      RHLODE = LKHLOD (GAMCJS, RCOS3T)
      H5     = (RHLODE)**CINQ

      CALL CJST(S,T)
             
C =================================================================
C --- VARIABLES INTERMEDIAIRES-------------------------------------
C =================================================================
      FACT1 = GAMCJS*RCOS3T/DEUX/H5/SII**2
      FACT2 = GAMCJS*SQRT(54.D0)/SIX/H5/SII**3
C =================================================================
C --- CALCUL FINAL ------------------------------------------------
C =================================================================
      DO 10 II=1,NDT
         DHDS(II) = FACT1*S(II)-FACT2*T(II)
 10   CONTINUE
C =================================================================
1000  CONTINUE
      END
