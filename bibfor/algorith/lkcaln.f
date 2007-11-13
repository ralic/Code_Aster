      SUBROUTINE LKCALN(S, B, VECN,RETCOM)
C
      IMPLICIT      NONE
      INTEGER       RETCOM
      REAL*8        B, S(6), VECN(6),DDOT
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/11/2007   AUTEUR ELGHARIB J.EL-GHARIB 
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
C --- BUT : CALCUL DE N -------------------------------------------
C =================================================================
C IN  : S      : DEVIATEUR DES CONTRAINTES ------------------------
C --- : B      : PARAMETRE DU CALCUL DE LA NORMALE ----------------
C OUT : VECN   : N = (B*S/SII-I)/SQRT(B**2+3) ---------------------
C =================================================================
      INTEGER I, NDT, NDI
      REAL*8  SII, RACINE, UN, TROIS, KRON(6), ZERO, LGLEPS
C =================================================================
C --- INITIALISATION DE PARAMETRE ---------------------------------
C =================================================================
      PARAMETER       ( UN      =   1.0D0  )
      PARAMETER       ( TROIS   =   3.0D0  )
      PARAMETER       ( ZERO    =   0.0D0  )
      PARAMETER       ( LGLEPS  =  1.0D-8  )
C =================================================================
      COMMON /TDIM/   NDT , NDI
C =================================================================
      DATA    KRON    /UN     ,UN     ,UN     ,ZERO   ,ZERO  ,ZERO/
C --- INITIALISATION ----------------------------------------------
C =================================================================
      CALL     LCPRSC(S, S, SII)
      SII    = SQRT  (SII)
      IF (SII .LT. LGLEPS) THEN
        CALL U2MESS('A','COMPOR1_31')
       RETCOM = 1
       GOTO 1000
      ENDIF
C =================================================================
C --- CALCUL DE N -------------------------------------------------
C =================================================================
      RACINE = SQRT(B*B + TROIS)
      DO 10 I=1,NDT
         VECN(I) = (B*S(I)/SII-KRON(I))/RACINE
 10   CONTINUE
C =================================================================
1000  CONTINUE
      END
