      SUBROUTINE VTMV(N,V,A,R)
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     ------------------------------------------------------------------
      INTEGER         N
      REAL*8            V(N),A(N,N),R
C     ------------------------------------------------------------------
C IN  N      :  DIMENSION DU VECTEUR
C IN  V      :  VECTEUR
C IN  A      :  MATRICE
C OUT R      :  REEL, RESULTAT DE VT*A*V
C     ------------------------------------------------------------------
      R = 0.D0
      DO 10 I = 1,N
      DO 10 J = 1,N
         R = R + V(I)*A(I,J)*V(J)
 10   CONTINUE
C
      END
