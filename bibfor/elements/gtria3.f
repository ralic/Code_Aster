      SUBROUTINE GTRIA3 ( XYZL , CARAT3 )
      IMPLICIT NONE
      REAL*8  XYZL(3,*), CARAT3(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2005   AUTEUR CIBHHLV L.VIVAN 
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
C     GRANDEURS GEOMETRIQUES ET JACOBIEN SUR LE TRIA3
C     ------------------------------------------------------------------
      REAL*8   X21 , X32 , X13 , Y21 , Y32 , Y13
C     ------------------------------------------------------------------
      X21 = XYZL(1,2) - XYZL(1,1)
      X32 = XYZL(1,3) - XYZL(1,2)
      X13 = XYZL(1,1) - XYZL(1,3)
      Y21 = XYZL(2,2) - XYZL(2,1)
      Y32 = XYZL(2,3) - XYZL(2,2)
      Y13 = XYZL(2,1) - XYZL(2,3)
      CARAT3(1) = X21
      CARAT3(2) = X32
      CARAT3(3) = X13
      CARAT3(4) = Y21
      CARAT3(5) = Y32
      CARAT3(6) = Y13
C     -------------- JACOBIEN -----------------------------------------
      CARAT3(7) = - X21 * Y13 + Y21 * X13
C     ------------ AIRE DU TRIANGLE -----------------------------------
      CARAT3(8)   = CARAT3(7)/2.D0
C     ------- MATRICE JACOBIENNE INVERSE ------------------------------
      CARAT3( 9) = - Y13 / CARAT3(7)
      CARAT3(10) = - Y21 / CARAT3(7)
      CARAT3(11) =   X13 / CARAT3(7)
      CARAT3(12) =   X21 / CARAT3(7)
C     --------- LONGUEURS DES COTES -----------------------------------
      CARAT3(13) = SQRT(X21*X21 + Y21*Y21)
      CARAT3(14) = SQRT(X32*X32 + Y32*Y32)
      CARAT3(15) = SQRT(X13*X13 + Y13*Y13)
C     --------- COSINUS DIRECTEURS -------------------------------------
      CARAT3(16) = X21 / CARAT3(13)
      CARAT3(17) = X32 / CARAT3(14)
      CARAT3(18) = X13 / CARAT3(15)
      CARAT3(19) = Y21 / CARAT3(13)
      CARAT3(20) = Y32 / CARAT3(14)
      CARAT3(21) = Y13 / CARAT3(15)
      END
