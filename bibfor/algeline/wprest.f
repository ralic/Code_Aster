      SUBROUTINE WPREST(A,X,N,M,Y)
      IMPLICIT NONE
C
      COMPLEX*16 Y(*)
      REAL*8     A(N,*),X(*)
      INTEGER    N,M
C     --------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
C     RESTITUTION DES VECTEUR PROPRES DU PB QUADRATIQUE
C     IE :         Y := A*X
C     --------------------------------------------------------------
C IN  A : R : MATRICE DES VECTEUR DE LANCZOS
C IN  X : C : MATRICE DES VECTEUR PROPRES DU PB REDUIT
C IN  N : I : TAILLE DES VECTEURS PROPRES DU PB QUADRATIQUE
C IN  M : I : TAILLE DES VECTEURS PROPRES DU PB REDUIT
C IN  Y : C : MATRICE DES VECTEUR PROPRES DU PB QUADRATIQUE
C     --------------------------------------------------------------
      COMPLEX*16 CVAL,CZERO
      INTEGER    I,J
C     --------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER K 
C-----------------------------------------------------------------------
      CZERO = DCMPLX(0.0D0,0.0D0)
      DO 100, I = 1, N, 1
         CVAL = CZERO
         K = 1
         DO 110, J = 1, M, 1
            CVAL = CVAL + A(I,J)*DCMPLX(X(K),X(K+1))
            K = K+2
110      CONTINUE
         Y(I) = CVAL
100   CONTINUE
C
      END
