      SUBROUTINE PTMA10  (   M     , RHO , A , XL , XIY , XIZ )
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8                 M(*)  , RHO , A , XL , XIY , XIZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DE LA MATRICE DE MASSE DES ELEMENTS DE POUTRE
C          - COURBE A SECTION CONSTANTE
C     PAR LA METHODE
C          - DES MASSES CONCENTREES
C     ------------------------------------------------------------------
C OUT M        -(78) MATRICE DE MASSE ELEMENT
C IN  RHO        - MASSE VOLUMIQUE DU MATERIAU
C IN  A          - AIRE DE LA SECTION DROITE
C IN  XL         - LONGUEUR DE L ELEMENT
C IN  XIY        - MOMENT D INERTIE / Y PRINCIPAL
C IN  XIZ        - MOMENT D INERTIE / Z PRINCIPAL
C     ------------------------------------------------------------------
      REAL*8  ZAIRE
      REAL*8  C, CC
      REAL*8  ZERO , C002 , C015 , C048 , C105
      INTEGER IP(12)
      DATA    IP/0,1,3,6,10,15,21,28,36,45,55,66/
C
C     INITIALISATION
      ZERO =   0.D0
      DO 10 I=1,78
        M(I) = ZERO
10    CONTINUE
      C002 =   2.D0
      C015 =  15.D0
      C048 =  48.D0
      C105 = 105.D0
C
C     MASSES CONCENTREES FORMULATION S.D.R.C.   DANS TOUS LES CAS
      ZAIRE = RHO * A * XL / C002
      M( IP( 1)+ 1) = ZAIRE
      M( IP( 2)+ 2) = ZAIRE
      M( IP( 3)+ 3) = ZAIRE
C--   M( IP( 4)+ 4) = ZINEX
C--   M( IP( 5)+ 5) = ZINEY
C--   M( IP( 6)+ 6) = ZINEZ
C
      M( IP( 7)+ 7) = ZAIRE
      M( IP( 8)+ 8) = ZAIRE
      M( IP( 9)+ 9) = ZAIRE
C---  M( IP(10)+10) = ZINEX
C---  M( IP(11)+11) = ZINEY
C---  M( IP(12)+12) = ZINEZ
C
      CC = M( IP( 1)+ 1) + M( IP( 7)+ 7)
      C  = MIN ( CC * XL * XL / C105 , CC * XL /  C048 )
C
      M( IP( 4)+ 4) = ( XIY + XIZ ) * XL * RHO / C002
      M( IP( 5)+ 5) = C + RHO * XIY * XL * C002 / C015
      M( IP( 6)+ 6) = C + RHO * XIZ * XL * C002 / C015
C
      M( IP(10)+10) = M( IP( 4)+ 4)
      M( IP(11)+11) = M( IP( 5)+ 5)
      M( IP(12)+12) = M( IP( 6)+ 6)
C
      END
