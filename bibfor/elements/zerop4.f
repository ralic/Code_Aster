      SUBROUTINE ZEROP4(B,C,D,E,X,N)
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      REAL*8  B,C,D,E,X(4)
      INTEGER N
C ----------------------------------------------------------------------
C RESOLUTION D'UN POL. DEGRE 4 : X**4 + B X**3 + C X**2 + D X + E = 0
C METHODE DE FERRARI
C ----------------------------------------------------------------------
C IN  B,C,D,E COEFFICIENTS DU POLYNOME
C OUT X       RACINES DANS L'ORDRE DECROISSANT
C OUT N       NOMBRE DE RACINES
C ----------------------------------------------------------------------
      INTEGER I,J,NRAC,N1,N2
      REAL*8 P,Q,R,X0,Y0,A0,B0,TMP
      REAL*8 RAC(3),R1(2),R2(2),Z(4)
C ----------------------------------------------------------------------

      P = -3/8.D0 * B**2 + C
      Q = (B/2)**3 - B*C/2 + D
      R = -3*(B/4)**4 + C*(B/4)**2 - B*D/4 + E

      CALL ZEROP3(2*P,P**2-4*R,-Q**2,RAC,NRAC)
      X0 = RAC(1)
      Y0 = (X0+P)/2
      A0 = SQRT(X0)
      B0 = - Q/(2*A0)

      CALL ZEROP2(-A0,Y0-B0,R1,N1)
      CALL ZEROP2( A0,Y0+B0,R2,N2)
      DO 10 I = 1,N1
        Z(I) = R1(I)
 10   CONTINUE
      DO 20 I = 1,N2
        Z(I+N1)= R2(I)
 20   CONTINUE
      N = N1 + N2

C    TRI DES SOLUTIONS PAR ORDRE DECROISSANT
      DO 100 I = 2,N
        DO 110 J = I,2,-1
          IF (Z(J).GT.Z(J-1)) THEN
            TMP = Z(J)
            Z(J) = Z(J-1)
            Z(J-1) = TMP
          END IF
 110    CONTINUE
 100  CONTINUE


      DO 30 I = 1,N
        X(I) = Z(I) - B/4
 30   CONTINUE

      END
