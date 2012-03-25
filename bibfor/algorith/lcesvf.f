      REAL*8 FUNCTION LCESVF(MODE,A) 

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE      
      INTEGER MODE
      REAL*8  A
C ----------------------------------------------------------------------
C  CALCUL DES FONCTIONS R(A) POUR LA ENDO_SCALAIRE AVEC GRAD_VARI
C ----------------------------------------------------------------------
C  MODE    FONCTION RECHERCHEE
C           0: R(A)
C           1: DRDA(A)
C           2: D2RDA2(A)
C  A       VALEUR DE L'ARGUMENT A
C ----------------------------------------------------------------------
      REAL*8 N,D,DN,DD,D2N,D2D
C ----------------------------------------------------------------------
      REAL*8 PK,PM,PP
      COMMON /LCES/ PK,PM,PP
C ----------------------------------------------------------------------
      
      N = (1-A)**2
      D = 1+(PM-2)*A+(1+PP*PM)*A**2
      
      IF (MODE.EQ.0) THEN
        LCESVF = N/D
        GOTO 9999
      END IF
      
      DN = -2*(1-A)
      DD = PM-2 + 2*(1+PP*PM)*A 
      
      IF (MODE.EQ.1) THEN
        LCESVF = (DN*D-DD*N)/D**2
        GOTO 9999
      END IF
      
      D2N = 2
      D2D = 2*(1+PM*PP)

      IF (MODE.EQ.2) THEN
        LCESVF = ((D2N*D-N*D2D)*D+2*DD*(N*DD-DN*D))/D**3
        GOTO 9999
      END IF
      
      CALL ASSERT(.FALSE.)
        
 9999 CONTINUE      
      END
