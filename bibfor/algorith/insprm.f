      SUBROUTINE INSPRM ( A , B , C , N , L , M )
        IMPLICIT REAL*8 (A-H,O-Z)
C       -----------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/11/96   AUTEUR INBHHOM O.MERABET 
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
C       -----------------------------------------------------------
C       NADAI_B :
C                 PRODUIT DE DEUX MATRICES
C       IN
C           A , B : MATRICES
C           L , N , M : DIMENSION MATRICES
C
C       OUT
C           C  :  MATRICE C(N,M)
C       -----------------------------------------------------------
      INTEGER   L , M , N
      REAL*8    A(N,L),B(L,M),C(N,M)
C       ------------------------------------------------------------
      DO 1 I = 1 , N
      DO 1 J = 1 , M
      C(I,J) = 0.D0
      DO 2 K = 1 , L
      C(I,J) = C(I,J) + A(I,K) * B(K,J)
    2 CONTINUE
    1 CONTINUE
      END
