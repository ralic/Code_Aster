      SUBROUTINE LKVARV(VINTR, NBMAT, MATER, PARAVI)
C
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        PARAVI(3), MATER(NBMAT,2)
C ==================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/11/2007   AUTEUR ELGHARIB J.EL-GHARIB 
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
C ==================================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE---------------------------
C ==================================================================
C --- BUT : CALCUL DES FONCTIONS D'ECROUISSAGE VISQUEUSE------------
C ==================================================================
C IN  : VINTR    : VARIABLE INTERNE (ICI XIV) ----------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES DU MODELE --------------------
C --- : MATER  : PARAMETRES DU MODELE ------------------------------
C OUT : PARAVI : VARIABLE D'ECROUISSAGE ----------------------------
C ------------ : AXIV, SXIV, MXIV ----------------------------------
C ==================================================================
      REAL*8  M0, A0, S0, AVMAX, MVMAX, SVMAX, XIVMAX
      REAL*8  SXIV, AXIV, MXIV, XIV
      REAL*8  FACT1,  VINTR
C ==================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE -------------------------
C ==================================================================
      M0     = MATER(13,2)
      A0     = MATER( 8,2)
      S0     = MATER(11,2)
      
      AVMAX  = 1.D0
      MVMAX  = MATER(20,2)
      SVMAX  = S0
      
      XIVMAX = MATER(21,2)
C ==================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIV < XIVMAX------
C ==================================================================
      XIV = VINTR      
      IF (XIV.LT. XIVMAX) THEN
      
         FACT1 = XIV/XIVMAX

         AXIV  = A0 + (AVMAX - A0)*FACT1
C
         SXIV  = S0 + (SVMAX - S0)*FACT1
C       
         MXIV  = M0 + (MVMAX - M0)*FACT1
C ==================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIV >= XIVMAX   --
C ==================================================================
      ELSE
         AXIV  = AVMAX
         
         SXIV  = SVMAX
                 
         MXIV  = MVMAX
C ==================================================================
C CALCUL DES VARIABLES D'ECROUISSAGES POUR LE CAS XIE< XIP < XIULT--
C ==================================================================
      ENDIF
C ==================================================================
C --- STOCKAGE -----------------------------------------------------
C ==================================================================
      PARAVI(1) = AXIV
      PARAVI(2) = SXIV
      PARAVI(3) = MXIV
C ==================================================================
      END
