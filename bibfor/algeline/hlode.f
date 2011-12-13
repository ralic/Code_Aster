      FUNCTION HLODE (GAMCJS, RCOS3T)
C
      IMPLICIT NONE
      REAL*8   GAMCJS, RCOS3T, HLODE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/03/2002   AUTEUR GJBHHEL E.LORENTZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : CALCUL DE H(T) OU T DESIGNE L'ANGLE DE LODE ----------------
C ======================================================================
C IN  : GAMCJS : PARAMETRE DE FORME DE LA SURFACDE DE CHARGE -----------
C ------------ : DANS LE PLAN DEVIATOIRE -------------------------------
C --- : RCOS3T : COS(3T) -----------------------------------------------
C OUT : HLODE  = (1+GAMMA_CJS*COS3T)**(1/6) ----------------------------
C ======================================================================
      REAL*8  UN, SIX
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( SIX    =  6.0D0  )
C ======================================================================
      HLODE = (UN+GAMCJS*RCOS3T)**(UN/SIX)
C ======================================================================
      END
