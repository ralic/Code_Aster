      SUBROUTINE FLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 20/09/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK CALCULANT LA PROJECTION D'UNE MATRICE.
C-----------------------------------------------------------------------
C
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     FEBRUARY 29, 1992
C
C  PURPOSE
C  =======
C
C  FLARF APPLIES A REAL ELEMENTARY REFLECTOR H TO A REAL M BY N MATRIX
C  C, FROM EITHER THE LEFT OR THE RIGHT. H IS REPRESENTED IN THE FORM
C
C        H = I - TAU * V * V'
C
C  WHERE TAU IS A REAL SCALAR AND V IS A REAL VECTOR.
C
C  IF TAU = 0, THEN H IS TAKEN TO BE THE UNIT MATRIX.
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
C  V       (INPUT) REAL*8 ARRAY, DIMENSION
C                     (1 + (M-1)*ABS(INCV)) IF SIDE = 'L'
C                  OR (1 + (N-1)*ABS(INCV)) IF SIDE = 'R'
C          THE VECTOR V IN THE REPRESENTATION OF H. V IS NOT USED IF
C          TAU = 0.
C
C  INCV    (INPUT) INTEGER
C          THE INCREMENT BETWEEN ELEMENTS OF V. INCV <> 0.
C
C  TAU     (INPUT) REAL*8
C          THE VALUE TAU IN THE REPRESENTATION OF H.
C
C  C       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDC,N)
C          ON ENTRY, THE M BY N MATRIX C.
C          ON EXIT, C IS OVERWRITTEN BY THE MATRIX H * C IF SIDE = 'L',
C          OR C * H IF SIDE = 'R'.
C
C  LDC     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY C. LDC >= MAX(1,M).
C
C  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION
C                         (N) IF SIDE = 'L'
C                      OR (M) IF SIDE = 'R'
C
C-----------------------------------------------------------------------
C
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        SIDE
      INTEGER            INCV, LDC, M, N
      REAL*8   TAU
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   C( LDC, * ), V( * ), WORK( * )

C     .. PARAMETERS ..
      REAL*8   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
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
C DUE TO CRP102 CALL BLGEMV('TRANSPOSE',M,N,ONE,C,LDC,V,INCV,ZERO,
            CALL BLGEMV( 'T', M, N, ONE, C, LDC, V, INCV, ZERO,
     &                  WORK, 1 )
C
C           C := C - V * W'
C
            CALL BLSGER( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
C
C        FORM  C * H
C
         IF( TAU.NE.ZERO ) THEN
C
C           W := C * V
C
C DUE TO CRP102 CALL BLGEMV('NO TRANSPOSE',M,N,ONE,C,LDC,V,INCV,
            CALL BLGEMV( 'N', M, N, ONE, C, LDC, V, INCV,
     &                  ZERO, WORK, 1 )
C
C           C := C - W * V'
C
            CALL BLSGER( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
C
C     END OF FLARF
C
      END
