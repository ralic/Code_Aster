      SUBROUTINE GQUAD4 ( XYZL , CARAQ4 )
      IMPLICIT NONE
      REAL*8              XYZL(3,*), CARAQ4(*)
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
C     GRANDEURS GEOMETRIQUES SUR LE QUAD4
C     ------------------------------------------------------------------
      REAL*8  X21 , X32 , X43 , X14 , Y21 , Y32 , Y43 , Y14
      REAL*8  X31 , X42 ,             Y31 , Y42
C     ------------------------------------------------------------------
C     -------- PROJECTION DES COTES ------------------------------------
      X21 = XYZL(1,2) - XYZL(1,1)
      X32 = XYZL(1,3) - XYZL(1,2)
      X43 = XYZL(1,4) - XYZL(1,3)
      X14 = XYZL(1,1) - XYZL(1,4)
      Y21 = XYZL(2,2) - XYZL(2,1)
      Y32 = XYZL(2,3) - XYZL(2,2)
      Y43 = XYZL(2,4) - XYZL(2,3)
      Y14 = XYZL(2,1) - XYZL(2,4)
      CARAQ4(1) = X21
      CARAQ4(2) = X32
      CARAQ4(3) = X43
      CARAQ4(4) = X14
      CARAQ4(5) = Y21
      CARAQ4(6) = Y32
      CARAQ4(7) = Y43
      CARAQ4(8) = Y14
C     -------- PROJECTION DES DIAGONALES -------------------------------
      X31 = XYZL(1,3) - XYZL(1,1)
      X42 = XYZL(1,4) - XYZL(1,2)
      Y31 = XYZL(2,3) - XYZL(2,1)
      Y42 = XYZL(2,4) - XYZL(2,2)
C     --------- LONGUEURS DES COTES -----------------------------------
      CARAQ4( 9) = SQRT(X21*X21 + Y21*Y21)
      CARAQ4(10) = SQRT(X32*X32 + Y32*Y32)
      CARAQ4(11) = SQRT(X43*X43 + Y43*Y43)
      CARAQ4(12) = SQRT(X14*X14 + Y14*Y14)
C     --------- COSINUS DIRECTEURS -------------------------------------
      CARAQ4(13) = X21 / CARAQ4( 9)
      CARAQ4(14) = X32 / CARAQ4(10)
      CARAQ4(15) = X43 / CARAQ4(11)
      CARAQ4(16) = X14 / CARAQ4(12)
      CARAQ4(17) = Y21 / CARAQ4( 9)
      CARAQ4(18) = Y32 / CARAQ4(10)
      CARAQ4(19) = Y43 / CARAQ4(11)
      CARAQ4(20) = Y14 / CARAQ4(12)
C     ----------- AIRE DU QUADRANGLE ----------------------------------
      CARAQ4(21) = (  X31 * Y42 - Y31 * X42)/2.D0
C     --------- AIRE DES 4 TRIANGLES -----------------------------------
      CARAQ4(22) = (- X21 * Y14 + Y21 * X14)/2.D0
      CARAQ4(23) = (- X32 * Y21 + Y32 * X21)/2.D0
      CARAQ4(24) = (- X43 * Y32 + Y43 * X32)/2.D0
      CARAQ4(25) = (- X14 * Y43 + Y14 * X43)/2.D0
C
      END
