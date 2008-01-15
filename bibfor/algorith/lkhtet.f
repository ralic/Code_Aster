      SUBROUTINE LKHTET (NBMAT, MATER, RCOS3T, H0E, H0C, HTHETA)
C
      IMPLICIT NONE
      INTEGER  NBMAT
      REAL*8   MATER(NBMAT,2), RCOS3T, HTHETA
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/01/2008   AUTEUR PROIX J-M.PROIX 
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
C --- BUT : CALCUL DE H(THETA) 
C =================================================================
C IN  : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : RCOS3T : COS(3T) ------------------------------------------
C OUT : H0E    : PARAMETRE UTILIE DANS LE CRITERE------------------
C     : H0C    : PARAMETRE UTILIE DANS LE CRITERE------------------
C     : HTHETA : H(THETA ------------------------------------------
C =================================================================
      REAL*8  UN, DEUX, SIX
      REAL*8  GAMCJS, H0EXT, H0C, H0E, HLODE, LKHLOD
      REAL*8  FACT1, FACT2, FACT3
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( SIX    =  6.0D0  )
C =================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
C =================================================================
      H0EXT  = MATER(4,2)
      GAMCJS = MATER(5,2)
C =================================================================
C ---- CALCUL DE H0C
C =================================================================
      H0C = (UN - GAMCJS )**(UN/SIX)
C =================================================================
C ---- CALCUL DE H0E     
C =================================================================
      H0E = (UN + GAMCJS )**(UN/SIX)
C =================================================================
C ---- CALCUL DE H(THETA)     
C =================================================================
      FACT1  = (H0C + H0EXT)/DEUX
      FACT2  = (H0C - H0EXT)/DEUX
      
      HLODE  = LKHLOD(GAMCJS,RCOS3T)
      
      FACT3  = (DEUX*HLODE-(H0C+H0E))/(H0C-H0E) 
 
      HTHETA = FACT1 + FACT2*FACT3
C =================================================================
      END
