      FUNCTION LGLCOV (SEUIL,TOLER)
C
      IMPLICIT NONE
      LOGICAL  LGLCOV
      REAL*8   SEUIL, TOLER
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/02/2003   AUTEUR CIBHHBC R.FERNANDES 
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
C --- BUT : TESTE DE CONVERGENCE ---------------------------------------
C ======================================================================
C IN  : SEUIL  : VALEUR DU SEUIL (F(N+1)) ------------------------------
C ----- SIGC   : PARAMETRE MATERIAU ------------------------------------
C ----- TOLER  : TOLERANCE DE CONVERGENCE ------------------------------
C OUT : LGLCOV : .TRUE.   SI CONVERGENCE -------------------------------
C ------------ : .FALSE.  SINON ----------------------------------------
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      IF (ABS(SEUIL).LE.TOLER) THEN
         LGLCOV = .TRUE.
      ELSE
         LGLCOV = .FALSE.
      ENDIF
C ======================================================================
      END
