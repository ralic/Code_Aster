      SUBROUTINE GFRESG ( X, FVEC, LAMEQ, FLUID, GEOM1, CFPCD1,
     +                    EPS, EPS0, Z0, Z, DZ )
      IMPLICIT NONE
      INTEGER             EPS, EPS0
      REAL*8              LAMEQ, FLUID(*), GEOM1(*), CFPCD1(*), 
     +                    FVEC(*), X(*), Z0, Z, DZ
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2003   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     CALCUL DE LA VALEUR DE LA FONCTION  GFRESG
C-----------------------------------------------------------------------
      REAL*8    CD0, CD1, CD2, ROC, P0, P1, P2, A, A0, A1, AA0, AC, DH,
     +          UN
C     ------------------------------------------------------------------
C
      ROC = FLUID(1)
      P0  = FLUID(5)
      P1  = FLUID(6)
      P2  = FLUID(7)
C
      A   = GEOM1(4)
      A0  = GEOM1(5)
      A1  = GEOM1(6)
      AA0 = GEOM1(7)
      AC  = GEOM1(8)
      DH  = GEOM1(10)
C
      CD0 = CFPCD1(1)
      CD1 = CFPCD1(2)
      CD2 = CFPCD1(3)
C
      UN = 1.0D0
C
      FVEC(1) = AC/A*DZ - X(1) - A1*EPS/A*X(2) - EPS0*A0/A*X(3)
      FVEC(2) = 2*(P1-P2)/ROC + (UN+EPS*CD1)*X(2)**2 
     +                        - (UN+LAMEQ*(Z+Z0)/DH+CD2)*X(1)**2
      FVEC(3) = 2*(P0-P1)/ROC + ((A0/AA0)**2 + EPS0*CD0)*X(3)**2
     +                        - (UN+EPS*CD1)*X(2)**2
C
      END     
