      SUBROUTINE LKDS2H(NBMAT,MATER,INVAR,S,DHDS,DS2HDS,RETCOM)
C
      IMPLICIT      NONE
      INTEGER       NBMAT,RETCOM
      REAL*8        INVAR,MATER(NBMAT,2),S(6), DHDS(6),DS2HDS(6)
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
C --- BUT : CALCUL DES DERICEES d(sII*h(THETA))/dsig --------------
C =================================================================
C IN  : NBMAT  : NOMBRE DE PARAMETRES DU MODELE -------------------
C --- : MATER  : PARAMETRES DU MODELE -----------------------------
C --- : INVAR :  INVARIANT DES CONTRAINTES ------------------------
C --- : S     :  DEVIATEUR DES CONTRAINTES ------------------------
C     : DHDS   : dh(THETA)/ds--------------------------------------
C OUT : DS2HDS: d(sII*h(THETA))/dsig-------------------------------
C =================================================================
      INTEGER NDT, NDI, I, K
      REAL*8  H0EXT, PREF,H0E, H0C, HTHETA
      REAL*8  T(6), DEVT(6), KRON(6), IDEN6(6,6)
      REAL*8  A(6), B(6,6) , BT(6,6) 
      REAL*8  SII, COS3T, RCOS3T
      REAL*8  ZERO, UN, TROIS,  LGLEPS, PTIT, R8MIEM
      REAL*8  FACT1
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( ZERO    =  0.0D0   )
      PARAMETER       ( UN      =  1.0D0   )
      PARAMETER       ( TROIS   =  3.0D0   )
      PARAMETER       ( LGLEPS  =  1.0D-8  )
C -----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C -----------------------------------------------------------------
      DATA    KRON    /UN     ,UN     ,UN     ,ZERO   ,ZERO  ,ZERO/
      DATA    IDEN6   /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     &                 ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     &                 ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     &                 ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     &                 ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     &                 ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/
C =================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
C =================================================================
      H0EXT  = MATER(4,2)
      PREF   = MATER(1,2)
C =================================================================
C --- CALCUL DU DEVIATEUR ET DU PREMIER INVARIANT DES CONTRAINTES -
C =================================================================
      RETCOM = 0
      PTIT = R8MIEM()
      CALL     LCPRSC(S, S, SII)
      SII    = SQRT  (SII)
      IF (SII .LT. PTIT) THEN
        CALL U2MESS('A','COMPOR1_30')
       RETCOM = 1
       GOTO 1000
      ENDIF
       
C =================================================================
C --- CALCUL DE h(THETA), H0E ET H0C, -----------------------------
C =================================================================
      RCOS3T = COS3T (S, PREF, LGLEPS)
      CALL LKHTET (NBMAT, MATER,  RCOS3T, H0E, H0C, HTHETA)
      
      FACT1 = (H0C - H0EXT)/(H0C - H0E)
C =================================================================
C --- CALCUL DU PREMIER TERME      
C =================================================================

      CALL R8INIR(6,0.D0,A,1)      
      DO 10 I = 1, NDT
      A(I)  =  FACT1*DHDS(I)*SII + HTHETA*S(I)/SII      
 10   CONTINUE

C =================================================================
C --- CALCUL DU SECOND  TERME      
C =================================================================
      CALL R8INIR(6*6,0.D0,B,1)      
      DO 20 I = 1, NDT
      DO 30 K = 1, NDT
      B(I,K) = IDEN6(I,K) - KRON(I)*KRON(K)/TROIS      
 30   CONTINUE
 20   CONTINUE

C =================================================================
C --- RESULTAT FINAL      
C =================================================================
      CALL R8INIR(6,0.D0,DS2HDS,1)      

      CALL LCTRMA(B,BT)
      CALL LCPRMV(BT,A,DS2HDS)

C =================================================================
1000  CONTINUE
      END
