      SUBROUTINE DLARNV( IDIST, ISEED, N, X )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/02/2000   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C 
C     SUBROUTINE LAPACK FOURNISSANT DES VECTEURS ALEATOIRES.
C-----------------------------------------------------------------------
C
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     SEPTEMBER 30, 1994 
C
C  PURPOSE
C  =======
C
C  DLARNV RETURNS A VECTOR OF N RANDOM REAL NUMBERS FROM A UNIFORM OR
C  NORMAL DISTRIBUTION.
C
C  ARGUMENTS
C  =========
C
C  IDIST   (INPUT) INTEGER
C          SPECIFIES THE DISTRIBUTION OF THE RANDOM NUMBERS:
C          = 1:  UNIFORM (0,1)
C          = 2:  UNIFORM (-1,1)
C          = 3:  NORMAL (0,1)
C
C  ISEED   (INPUT/OUTPUT) INTEGER ARRAY, DIMENSION (4)
C          ON ENTRY, THE SEED OF THE RANDOM NUMBER GENERATOR, THE ARRAY
C          ELEMENTS MUST BE BETWEEN 0 AND 4095, AND ISEED(4) MUST BE
C          ODD.
C          ON EXIT, THE SEED IS UPDATED.
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF RANDOM NUMBERS TO BE GENERATED.
C
C  X       (OUTPUT) REAL*8 ARRAY, DIMENSION (N)
C          THE GENERATED RANDOM NUMBERS.
C
C  FURTHER DETAILS
C  ===============
C
C  THIS ROUTINE CALLS THE AUXILIARY ROUTINE DLARUV TO GENERATE RANDOM
C  REAL NUMBERS FROM A UNIFORM (0,1) DISTRIBUTION, IN BATCHES OF UP TO
C  128 USING VECTORISABLE CODE. THE BOX-MULLER METHOD IS USED TO
C  TRANSFORM NUMBERS FROM A UNIFORM TO A NORMAL DISTRIBUTION.
C
C  =====================================================================
C
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER.
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      INTEGER            IDIST, N
C     ..
C     .. ARRAY ARGUMENTS ..
      INTEGER            ISEED( 4 )
      REAL*8   X( * )
      
C     .. PARAMETERS ..
      REAL*8   ONE, TWO
      PARAMETER          ( ONE = 1.0D+0, TWO = 2.0D+0 )
      INTEGER            LV
      PARAMETER          ( LV = 128 )
      REAL*8   TWOPI
      PARAMETER          ( TWOPI = 6.2831853071795864769252867663D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, IL, IL2, IV
C     ..
C     .. LOCAL ARRAYS ..
      REAL*8   U( LV )
C     ..
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      DO 40 IV = 1, N, LV / 2
         IL = MIN( LV / 2, N-IV+1 )
         IF( IDIST.EQ.3 ) THEN
            IL2 = 2*IL
         ELSE
            IL2 = IL
         END IF
C
C        CALL DLARUV TO GENERATE IL2 NUMBERS FROM A UNIFORM (0,1)
C        DISTRIBUTION (IL2 <= LV)
C
         CALL DLARUV( ISEED, IL2, U )
C
         IF( IDIST.EQ.1 ) THEN
C
C           COPY GENERATED NUMBERS
C
            DO 10 I = 1, IL
               X( IV+I-1 ) = U( I )
   10       CONTINUE
         ELSE IF( IDIST.EQ.2 ) THEN
C
C           CONVERT GENERATED NUMBERS TO UNIFORM (-1,1) DISTRIBUTION
C
            DO 20 I = 1, IL
               X( IV+I-1 ) = TWO*U( I ) - ONE
   20       CONTINUE
         ELSE IF( IDIST.EQ.3 ) THEN
C
C           CONVERT GENERATED NUMBERS TO NORMAL (0,1) DISTRIBUTION
C
            DO 30 I = 1, IL
               X( IV+I-1 ) = SQRT( -TWO*LOG( U( 2*I-1 ) ) )*
     $                       COS( TWOPI*U( 2*I ) )
   30       CONTINUE
         END IF
   40 CONTINUE
C
C     END OF DLARNV
C
      END
