      SUBROUTINE LKVPHI (NBMAT, MATER,SEUILV,  PHI)
C
      IMPLICIT    NONE
      INTEGER     NBMAT
      REAL*8      MATER(NBMAT,2),SEUILV, PHI
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2007   AUTEUR ELGHARIB J.EL-GHARIB 
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
C --- BUT : AMPLITUDE DES DEFORMATIONS IRREVERSIBLES --------------
C =================================================================
C IN  : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C OUT :  SEUILV : SEUIL VISCOPLASTIQUE ----------------------------
C         PHI :   AMPLITUDE DES DEFORMATIONS IRREVERSIBLES  -------
C         PHI    = A*(fv(SIG,XIV)/PA)**n---------------------------
C =================================================================
      REAL*8    PA , A, N , ZERO
      PARAMETER       (ZERO  =  0.D0 )
C =================================================================
C --- RECUPERATION DE PARAMETRES DU MODELE ------------------------
C =================================================================
      PA     = MATER(1,2)
      A      = MATER(22,2)
      N      = MATER(23,2)      
C =================================================================
C --- CALCUL DE PHI ------------------------------------
C =================================================================
      IF ( SEUILV. LE. ZERO) THEN
      PHI = ZERO
      ELSE
      PHI = A * (SEUILV/PA)**N
      ENDIF
C =================================================================
      END
