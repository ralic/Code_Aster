      REAL*8 FUNCTION BLSDOT (N, SX, INCX, SY, INCY)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 08/07/97   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C REMPLACE LA BLAS SDOT SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C
      IMPLICIT REAL*8  (A-H,O-Z)
C***BEGIN PROLOGUE  SDOT
C***PURPOSE  Compute the inner product of two vectors.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1A4
C***TYPE      SINGLE PRECISION (BLSDOT-S, DDOT-D, CDOTU-C)
C***KEYWORDS  BLAS, INNER PRODUCT, LINEAR ALGEBRA, VECTOR
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C           Kincaid, D. R., (U. of Texas)
C           Krogh, F. T., (JPL)
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements
C     INCY  storage spacing between elements of SY
C
C     --Output--
C     SDOT  single precision dot product (zero if N .LE. 0)
C
C     Returns the dot product of single precision SX and SY.
C     SDOT = sum for I = 0 to N-1 of  SX(LX+I*INCX) * SY(LY+I*INCY),
C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
C     defined in a similar way using INCY.
C
C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
C                 Krogh, Basic linear algebra subprograms for Fortran
C                 usage, Algorithm No. 539, Transactions on Mathematical
C                 Software 5, 3 (September 1979), pp. 308-323.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791001  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C   970701  Type des arguments SX et SY, suppression des
C           ordres RETURN (JPL)

C***END PROLOGUE  BLSDOT
      REAL*8 SX(*), SY(*)
C***FIRST EXECUTABLE STATEMENT  BLSDOT
      BLSDOT = 0.0D0
      IF (N .LE. 0) GOTO 9999
      IF (INCX .EQ. INCY) IF (INCX-1) 5,20,60
C
C     Code for unequal or nonpositive increments.
C
    5 CONTINUE
      IX = 1
      IY = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        BLSDOT = BLSDOT + SX(IX)*SY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      GOTO 9999
C
C     Code for both increments equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 5.
C
   20 CONTINUE
      M = MOD(N,5)
      IF (M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        BLSDOT = BLSDOT + SX(I)*SY(I)
   30 CONTINUE
      IF (N .LT. 5) GOTO 9999
   40 CONTINUE
      MP1 = M + 1
      DO 50 I = MP1,N,5
      BLSDOT =BLSDOT + SX(I)*SY(I) + SX(I+1)*SY(I+1) + SX(I+2)*SY(I+2) +
     1                 SX(I+3)*SY(I+3) + SX(I+4)*SY(I+4)
   50 CONTINUE
      GOTO 9999
C
C     Code for equal, positive, non-unit increments.
C
   60 CONTINUE
      NS = N*INCX
      DO 70 I = 1,NS,INCX
        BLSDOT = BLSDOT + SX(I)*SY(I)
   70 CONTINUE
9999  CONTINUE
      END
