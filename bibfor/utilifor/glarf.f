      SUBROUTINE GLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     SEPTEMBER 30, 1994
C
C
C  PURPOSE
C  =======
C
C  GLARF APPLIES A COMPLEX ELEMENTARY REFLECTOR H TO A COMPLEX M-BY-N
C  MATRIX C, FROM EITHER THE LEFT OR THE RIGHT. H IS REPRESENTED IN THE
C  FORM
C
C        H = I - TAU * V * V'
C
C  WHERE TAU IS A COMPLEX SCALAR AND V IS A COMPLEX VECTOR.
C
C  IF TAU = 0, THEN H IS TAKEN TO BE THE UNIT MATRIX.
C
C  TO APPLY H' (THE CONJUGATE TRANSPOSE OF H), SUPPLY CONJG(TAU) INSTEAD
C  TAU.
C
C  ARGUMENTS
C  =========
C
C  SIDE    (INPUT) CHARACTER*1
C          = 'L': FORM  H * C
C          = 'R': FORM  C * H
C
C  M       (INPUT) INTEGER
C          THE NUMBER OF ROWS OF THE MATRIX C.
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF COLUMNS OF THE MATRIX C.
C
C  V       (INPUT) COMPLEX*16 ARRAY, DIMENSION
C                     (1 + (M-1)*ABS(INCV)) IF SIDE = 'L'
C                  OR (1 + (N-1)*ABS(INCV)) IF SIDE = 'R'
C          THE VECTOR V IN THE REPRESENTATION OF H. V IS NOT USED IF
C          TAU = 0.
C
C  INCV    (INPUT) INTEGER
C          THE INCREMENT BETWEEN ELEMENTS OF V. INCV <> 0.
C
C  TAU     (INPUT) COMPLEX*16
C          THE VALUE TAU IN THE REPRESENTATION OF H.
C
C  C       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDC,N)
C          ON ENTRY, THE M-BY-N MATRIX C.
C          ON EXIT, C IS OVERWRITTEN BY THE MATRIX H * C IF SIDE = 'L',
C          OR C * H IF SIDE = 'R'.
C
C  LDC     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY C. LDC >= MAX(1,M).
C
C  WORK    (WORKSPACE) COMPLEX*16 ARRAY, DIMENSION
C                         (N) IF SIDE = 'L'
C                      OR (M) IF SIDE = 'R'
C
C  =====================================================================
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        SIDE
      INTEGER            INCV, LDC, M, N
      COMPLEX*16         TAU
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         C( LDC, * ), V( * ), WORK( * )
C     ..
C     .. PARAMETERS ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
C     ..
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      IF( LLSAME( SIDE, 'L' ) ) THEN
C
C        FORM  H * C
C
         IF( TAU.NE.ZERO ) THEN
C
C           W := C' * V
C
            CALL GLGEMV( 'C', M, N, ONE, C, LDC, V,
     $                  INCV, ZERO, WORK, 1 )
C
C           C := C - V * W'
C
            CALL GLGERC( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
C
C        FORM  C * H
C
         IF( TAU.NE.ZERO ) THEN
C
C           W := C * V
C
            CALL GLGEMV( 'N', M, N, ONE, C, LDC, V, INCV,
     $                  ZERO, WORK, 1 )
C
C           C := C - W * V'
C
            CALL GLGERC( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
 1000 CONTINUE
C
C     END OF GLARF
C
      END
