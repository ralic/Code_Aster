      SUBROUTINE DMOUT
     &  ( LOUT, M, N, A, LDA, IDIGIT, IFMT )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/09/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C
C     SUBROUTINE ARPACK ECRIVANT DES MATRICES.
C-----------------------------------------------------------------------
C  PURPOSE:    REAL MATRIX OUTPUT ROUTINE.
C
C  USAGE:      CALL DMOUT (LOUT, M, N, A, LDA, IDIGIT, IFMT)
C
C  ARGUMENTS
C     M      - NUMBER OF ROWS OF A.  (INPUT)
C     N      - NUMBER OF COLUMNS OF A.  (INPUT)
C     A      - REAL M BY N MATRIX TO BE PRINTED.  (INPUT)
C     LDA    - LEADING DIMENSION OF A EXACTLY AS SPECIFIED IN THE
C              DIMENSION STATEMENT OF THE CALLING PROGRAM.  (INPUT)
C     IFMT   - FORMAT TO BE USED IN PRINTING MATRIX A.  (INPUT)
C     IDIGIT - PRINT UP TO IABS(IDIGIT) DECIMAL DIGITS PER NUMBER.  (IN)
C              IF IDIGIT .LT. 0, PRINTING IS DONE WITH 72 COLUMNS.
C              IF IDIGIT .GT. 0, PRINTING IS DONE WITH 132 COLUMNS.
C  INTRINSIC FUNCTIONS
C     MIN, LEN.
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER.
C            REMPLACEMENT DE 1 RETURN PAR 1 GOTO 1000.
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      CHARACTER*( * )    IFMT
      INTEGER            IDIGIT, LDA, LOUT, M, N

C     .. ARRAY ARGUMENTS ..
      REAL*8   A( LDA, * )

C     .. LOCAL SCALARS ..
      CHARACTER*80       LINE
      INTEGER            I, J, K1, K2, LLL, NDIGIT

C     .. LOCAL ARRAYS ..
      CHARACTER*1          ICOL( 3 )

C     .. DATA STATEMENTS ..
      DATA               ICOL( 1 ), ICOL( 2 ), ICOL( 3 ) / 'C', 'O',
     $                   'L' /

C     .. EXECUTABLE STATEMENTS ..

C     ... FIRST EXECUTABLE STATEMENT

      LLL = MIN( LEN( IFMT ), 80 )
      DO 10 I = 1, LLL
         LINE( I: I ) = '-'
   10 CONTINUE

      DO 20 I = LLL + 1, 80
         LINE( I: I ) = ' '
   20 CONTINUE

      WRITE( LOUT, FMT = 9999 )IFMT, LINE( 1: LLL )
 9999 FORMAT( / 1X, A, / 1X, A )

      IF( M.LE.0 .OR. N.LE.0 .OR. LDA.LE.0 )
     $   GOTO 1000
      NDIGIT = IDIGIT
      IF( IDIGIT.EQ.0 )
     $   NDIGIT = 4

C=======================================================================
C             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
C=======================================================================

      IF( IDIGIT.LT.0 ) THEN
         NDIGIT = -IDIGIT
         IF( NDIGIT.LE.4 ) THEN
            DO 40 K1 = 1, N, 5
               K2 = MIN( N, K1+4 )
               WRITE( LOUT, FMT = 9998 )( ICOL, I, I = K1, K2 )
               DO 30 I = 1, M
                  WRITE( LOUT, FMT = 9994 )I, ( A( I, J ), J = K1, K2 )
   30          CONTINUE
   40       CONTINUE

         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 60 K1 = 1, N, 4
               K2 = MIN( N, K1+3 )
               WRITE( LOUT, FMT = 9997 )( ICOL, I, I = K1, K2 )
               DO 50 I = 1, M
                  WRITE( LOUT, FMT = 9993 )I, ( A( I, J ), J = K1, K2 )
   50          CONTINUE
   60       CONTINUE

         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 80 K1 = 1, N, 3
               K2 = MIN( N, K1+2 )
               WRITE( LOUT, FMT = 9996 )( ICOL, I, I = K1, K2 )
               DO 70 I = 1, M
                  WRITE( LOUT, FMT = 9992 )I, ( A( I, J ), J = K1, K2 )
   70          CONTINUE
   80       CONTINUE

         ELSE
            DO 100 K1 = 1, N, 2
               K2 = MIN( N, K1+1 )
               WRITE( LOUT, FMT = 9995 )( ICOL, I, I = K1, K2 )
               DO 90 I = 1, M
                  WRITE( LOUT, FMT = 9991 )I, ( A( I, J ), J = K1, K2 )
   90          CONTINUE
  100       CONTINUE
         END IF

C=======================================================================
C             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
C=======================================================================

      ELSE
         IF( NDIGIT.LE.4 ) THEN
            DO 120 K1 = 1, N, 10
               K2 = MIN( N, K1+9 )
               WRITE( LOUT, FMT = 9998 )( ICOL, I, I = K1, K2 )
               DO 110 I = 1, M
                  WRITE( LOUT, FMT = 9994 )I, ( A( I, J ), J = K1, K2 )
  110          CONTINUE
  120       CONTINUE

         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 140 K1 = 1, N, 8
               K2 = MIN( N, K1+7 )
               WRITE( LOUT, FMT = 9997 )( ICOL, I, I = K1, K2 )
               DO 130 I = 1, M
                  WRITE( LOUT, FMT = 9993 )I, ( A( I, J ), J = K1, K2 )
  130          CONTINUE
  140       CONTINUE

         ELSE IF( NDIGIT.LE.10 ) THEN
            DO 160 K1 = 1, N, 6
               K2 = MIN( N, K1+5 )
               WRITE( LOUT, FMT = 9996 )( ICOL, I, I = K1, K2 )
               DO 150 I = 1, M
                  WRITE( LOUT, FMT = 9992 )I, ( A( I, J ), J = K1, K2 )
  150          CONTINUE
  160       CONTINUE

         ELSE
            DO 180 K1 = 1, N, 5
               K2 = MIN( N, K1+4 )
               WRITE( LOUT, FMT = 9995 )( ICOL, I, I = K1, K2 )
               DO 170 I = 1, M
                  WRITE( LOUT, FMT = 9991 )I, ( A( I, J ), J = K1, K2 )
  170          CONTINUE
  180       CONTINUE
         END IF
      END IF
      WRITE( LOUT, FMT = 9990 )

 9998 FORMAT( 10X, 10( 4X, 3A1, I4, 1X ) )
 9997 FORMAT( 10X, 8( 5X, 3A1, I4, 2X ) )
 9996 FORMAT( 10X, 6( 7X, 3A1, I4, 4X ) )
 9995 FORMAT( 10X, 5( 9X, 3A1, I4, 6X ) )
 9994 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P, 10D12.3 )
 9993 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P, 8D14.5 )
 9992 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P, 6D18.9 )
 9991 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P, 5D22.13 )
 9990 FORMAT( 1X, ' ' )

 1000 CONTINUE
      END
