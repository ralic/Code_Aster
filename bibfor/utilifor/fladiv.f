      SUBROUTINE FLADIV( A, B, C, D, P, Q )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 12/12/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK CALCULANT UNE DIVISION COMPLEXE EN ARITHMETIQUE
C     REELLE.
C-----------------------------------------------------------------------
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C
C  PURPOSE
C  =======
C
C  FLADIV PERFORMS COMPLEX DIVISION IN  REAL ARITHMETIC
C
C                        A + I*B
C             P + I*Q = ---------
C                        C + I*D
C
C  THE ALGORITHM IS DUE TO ROBERT L. SMITH AND CAN BE FOUND
C  IN D. KNUTH, THE ART OF COMPUTER PROGRAMMING, VOL.2, P.195
C
C  ARGUMENTS
C  =========
C
C  A       (INPUT) REAL*8
C  B       (INPUT) REAL*8
C  C       (INPUT) REAL*8
C  D       (INPUT) REAL*8
C          THE SCALARS A, B, C, AND D IN THE ABOVE EXPRESSION.
C
C  P       (OUTPUT) REAL*8
C  Q       (OUTPUT) REAL*8
C          THE SCALARS P AND Q IN THE ABOVE EXPRESSION.
C
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C            ABS.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      REAL*8   A, B, C, D, P, Q
C
C     .. LOCAL SCALARS ..
      REAL*8   E, F
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( ABS( D ).LT.ABS( C ) ) THEN
         E = D / C
         F = C + D*E
         P = ( A+B*E ) / F
         Q = ( B-A*E ) / F
      ELSE
         E = C / D
         F = D + C*E
         P = ( B+A*E ) / F
         Q = ( -A+B*E ) / F
      END IF
C
C     END OF FLADIV
C
      END
