      FUNCTION R8SQRT (A, B)
      IMPLICIT NONE
      REAL *8 A, B
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
C     CALCUL DE SQRT(A**2+B**2) SANS UNDER OU OVERFLOW
C-----------------------------------------------------------------------
C IN  : A    : PREMIER PARAMETRE.
C     : B    : SECOND PARAMETRE.
C OUT : HYPOT: SQRT(A**2+B**2).
C-----------------------------------------------------------------------
      REAL *8 P, R, S, T, U
C
C-----------------------------------------------------------------------
      REAL*8 R8SQRT 
C-----------------------------------------------------------------------
      P = MAX(ABS(A),ABS(B))
      IF (P .EQ. 0.0D0) GO TO 20
      R = (MIN(ABS(A),ABS(B))/P)**2
   10 CONTINUE
      T = 4.0D0 + R
      IF (T .EQ. 4.0D0) GO TO 20
      S = R/T
      U = 1.0D0 + 2.0D0*S
      P = U*P
      R = (S/U)**2*R
      GO TO 10
   20 CONTINUE
      R8SQRT = P
      END
