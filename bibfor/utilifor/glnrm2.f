      REAL*8  FUNCTION GLNRM2( N, X, INCX )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) BLAS
C ======================================================================
C  GLNRM2 RETURNS THE EUCLIDEAN NORM OF A VECTOR VIA THE FUNCTION
C  NAME, SO THAT
C
C     GLNRM2 := SQRT( CONJG( X' )*X )
C
C
C
C  -- THIS VERSION WRITTEN ON 25-OCTOBER-1982.
C     MODIFIED ON 14-OCTOBER-1993 TO INLINE THE CALL TO ZLASSQ.
C     SVEN HAMMARLING, NAG LTD.
C
C ======================================================================
C REMPLACE LA BLAS DZNRM2 SUR LES MACHINES OU ELLE N'EST PAS DISPONIBLE
C DANS LES LIBRAIRIES SYSTEME
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C     .. SCALAR ARGUMENTS ..
      INTEGER                           INCX, N
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16                        X( * )
C     ..
C
C     .. PARAMETERS ..
      REAL*8      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     .. LOCAL SCALARS ..
      INTEGER               IX
      REAL*8                NORM, SCALE, SSQ, TEMP
C     ..
C     .. EXECUTABLE STATEMENTS ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE
         SCALE = ZERO
         SSQ   = ONE
C        THE FOLLOWING LOOP IS EQUIVALENT TO THIS CALL TO THE LAPACK
C        AUXILIARY ROUTINE:
C        CALL ZLASSQ( N, X, INCX, SCALE, SSQ )
C
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( DBLE( X( IX ) ).NE.ZERO )THEN
               TEMP = ABS( DBLE( X( IX ) ) )
               IF( SCALE.LT.TEMP )THEN
                  SSQ   = ONE   + SSQ*( SCALE/TEMP )**2
                  SCALE = TEMP
               ELSE
                  SSQ   = SSQ   +     ( TEMP/SCALE )**2
               END IF
            END IF
            IF( DIMAG( X( IX ) ).NE.ZERO )THEN
               TEMP = ABS( DIMAG( X( IX ) ) )
               IF( SCALE.LT.TEMP )THEN
                  SSQ   = ONE   + SSQ*( SCALE/TEMP )**2
                  SCALE = TEMP
               ELSE
                  SSQ   = SSQ   +     ( TEMP/SCALE )**2
               END IF
            END IF
   10    CONTINUE
         NORM  = SCALE * SQRT( SSQ )
      END IF
C
      GLNRM2 = NORM
9999  CONTINUE
C
C     END OF GLNRM2.
C
      END
