      SUBROUTINE GQUAD4 ( XYZL , R )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/01/98   AUTEUR CIBHHLB L.BOURHRARA 
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
      INTEGER INT
      REAL*8  XYZL(3,*)
      REAL*8  R(*)
C     ------------------------------------------------------------------
C     GRANDEURS GEOMETRIQUES SUR LE QUAD4
C     ------------------------------------------------------------------
      REAL*8  X21 , X32 , X43 , X14 , Y21 , Y32 , Y43 , Y14
      REAL*8  X31 , X42 ,             Y31 , Y42
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN,
     &        LAIRE,LAIRN
               PARAMETER (NPG   = 4)
               PARAMETER (NNO   = 4)
               PARAMETER (NC    = 4)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI + NPG + NNO + 2*NC)
               PARAMETER (LWGT  = LETA + NPG + NNO + 2*NC)
               PARAMETER (LXYC  = LWGT + NPG)
               PARAMETER (LCOTE = LXYC + 2*NC)
               PARAMETER (LCOS  = LCOTE + NC)
               PARAMETER (LSIN  = LCOS + NC)
               PARAMETER (LAIRE = LSIN  + NC)
               PARAMETER (LAIRN = LAIRE + 1)
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
      R(LXYC)   = X21
      R(LXYC+1) = X32
      R(LXYC+2) = X43
      R(LXYC+3) = X14
      R(LXYC+4) = Y21
      R(LXYC+5) = Y32
      R(LXYC+6) = Y43
      R(LXYC+7) = Y14
C     -------- PROJECTION DES DIAGONALES -------------------------------
      X31 = XYZL(1,3) - XYZL(1,1)
      X42 = XYZL(1,4) - XYZL(1,2)
      Y31 = XYZL(2,3) - XYZL(2,1)
      Y42 = XYZL(2,4) - XYZL(2,2)
C     --------- LONGUEURS DES COTES -----------------------------------
      R(LCOTE)   = SQRT(X21*X21 + Y21*Y21)
      R(LCOTE+1) = SQRT(X32*X32 + Y32*Y32)
      R(LCOTE+2) = SQRT(X43*X43 + Y43*Y43)
      R(LCOTE+3) = SQRT(X14*X14 + Y14*Y14)
C     --------- COSINUS DIRECTEURS -------------------------------------
      R(LCOS)   = X21 / R(LCOTE)
      R(LCOS+1) = X32 / R(LCOTE+1)
      R(LCOS+2) = X43 / R(LCOTE+2)
      R(LCOS+3) = X14 / R(LCOTE+3)
      R(LSIN)   = Y21 / R(LCOTE)
      R(LSIN+1) = Y32 / R(LCOTE+1)
      R(LSIN+2) = Y43 / R(LCOTE+2)
      R(LSIN+3) = Y14 / R(LCOTE+3)
C     ----------- AIRE DU QUADRANGLE ----------------------------------
      R(LAIRE)    = (  X31 * Y42 - Y31 * X42)/2.D0
C     --------- AIRE DES 4 TRIANGLES -----------------------------------
      R(LAIRN)   = (- X21 * Y14 + Y21 * X14)/2.D0
      R(LAIRN+1) = (- X32 * Y21 + Y32 * X21)/2.D0
      R(LAIRN+2) = (- X43 * Y32 + Y43 * X32)/2.D0
      R(LAIRN+3) = (- X14 * Y43 + Y14 * X43)/2.D0
      END
