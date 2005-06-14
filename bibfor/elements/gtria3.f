      SUBROUTINE GTRIA3 ( XYZL , R)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/08/95   AUTEUR B8BHHHH J.R.LEVESQUE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8  XYZL(3,*)
      REAL*8  R(*)
C     ------------------------------------------------------------------
C     GRANDEURS GEOMETRIQUES ET JACOBIEN SUR LE TRIA3
C     ------------------------------------------------------------------
      REAL*8  X21 , X32 , X13 , Y21 , Y32 , Y13
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER NPG , NC , NNO
      INTEGER LDETJ,LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
      INTEGER LAIRE
               PARAMETER (NPG   = 3)
               PARAMETER (NNO   = 3)
               PARAMETER (NC    = 3)
               PARAMETER (LDETJ = 1)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI  + NPG + NNO )
               PARAMETER (LWGT  = LETA  + NPG + NNO )
               PARAMETER (LXYC  = LWGT  + NPG)
               PARAMETER (LCOTE = LXYC  + 2*NC)
               PARAMETER (LCOS  = LCOTE + NC)
               PARAMETER (LSIN  = LCOS  + NC)
               PARAMETER (LAIRE = LSIN  + NC)
C     ------------------------------------------------------------------
      X21 = XYZL(1,2) - XYZL(1,1)
      X32 = XYZL(1,3) - XYZL(1,2)
      X13 = XYZL(1,1) - XYZL(1,3)
      Y21 = XYZL(2,2) - XYZL(2,1)
      Y32 = XYZL(2,3) - XYZL(2,2)
      Y13 = XYZL(2,1) - XYZL(2,3)
      R(LXYC)    = X21
      R(LXYC+1)  = X32
      R(LXYC+2)  = X13
      R(LXYC+3)  = Y21
      R(LXYC+4)  = Y32
      R(LXYC+5)  = Y13
C     -------------- JACOBIEN -----------------------------------------
      R(LDETJ)   = - X21 * Y13 + Y21 * X13
C     ------------ AIRE DU TRIANGLE -----------------------------------
      R(LAIRE)   = R(LDETJ)/2.D0
C     ------- MATRICE JACOBIENNE INVERSE ------------------------------
      R(LJACO)   = - Y13 / R(LDETJ)
      R(LJACO+1) = - Y21 / R(LDETJ)
      R(LJACO+2) =   X13 / R(LDETJ)
      R(LJACO+3) =   X21 / R(LDETJ)
C     --------- LONGUEURS DES COTES -----------------------------------
      R(LCOTE)   = SQRT(X21*X21 + Y21*Y21)
      R(LCOTE+1) = SQRT(X32*X32 + Y32*Y32)
      R(LCOTE+2) = SQRT(X13*X13 + Y13*Y13)
C     --------- COSINUS DIRECTEURS -------------------------------------
      R(LCOS)   = X21 / R(LCOTE)
      R(LCOS+1) = X32 / R(LCOTE+1)
      R(LCOS+2) = X13 / R(LCOTE+2)
      R(LSIN)   = Y21 / R(LCOTE)
      R(LSIN+1) = Y32 / R(LCOTE+1)
      R(LSIN+2) = Y13 / R(LCOTE+2)
      END
