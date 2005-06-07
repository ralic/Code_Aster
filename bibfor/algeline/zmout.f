      SUBROUTINE ZMOUT( LOUT, M, N, A, LDA, IDIGIT, IFMT )
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C
C     SUBROUTINE ARPACK ECRIVANT DES MATRICES COMPLEXES.
C-----------------------------------------------------------------------
C
C  ROUTINE:    ZMOUT
C
C  PURPOSE:    COMPLEX*16 MATRIX OUTPUT ROUTINE.
C
C  USAGE:      CALL ZMOUT (LOUT, M, N, A, LDA, IDIGIT, IFMT)
C
C  ARGUMENTS
C     M      - NUMBER OF ROWS OF A.  (INPUT)
C     N      - NUMBER OF COLUMNS OF A.  (INPUT)
C     A      - COMPLEX*16 M BY N MATRIX TO BE PRINTED.  (INPUT)
C     LDA    - LEADING DIMENSION OF A EXACTLY AS SPECIFIED IN THE
C              DIMENSION STATEMENT OF THE CALLING PROGRAM.  (INPUT)
C     IFMT   - FORMAT TO BE USED IN PRINTING MATRIX A.  (INPUT)
C     IDIGIT - PRINT UP TO IABS(IDIGIT) DECIMAL DIGITS PER NUMBER.  (IN)
C              IF IDIGIT .LT. 0, PRINTING IS DONE WITH 72 COLUMNS.
C              IF IDIGIT .GT. 0, PRINTING IS DONE WITH 132 COLUMNS.
C
C\SCCS INFORMATION: @(#)
C FILE: ZMOUT.F   SID: 2.1   DATE OF SID: 11/16/95   RELEASE: 2
C
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     ... SPECIFICATIONS FOR ARGUMENTS
      INTEGER            M, N, IDIGIT, LDA, LOUT
      COMPLEX*16         A( LDA, * )
      CHARACTER*(*)      IFMT

C     ... SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER            I, J, NDIGIT, K1, K2, LLL
      CHARACTER*1        ICOL( 3 )
      CHARACTER*80       LINE
C     ...
C
      DATA               ICOL( 1 ), ICOL( 2 ), ICOL( 3 ) / 'C', 'O',
     $                   'L' /
C     ...
C     ... FIRST EXECUTABLE STATEMENT
C
      LLL = MIN( LEN( IFMT ), 80 )
      DO 10 I = 1, LLL
         LINE( I: I ) = '-'
   10 CONTINUE
C
      DO 20 I = LLL + 1, 80
         LINE( I: I ) = ' '
   20 CONTINUE
C
      WRITE( LOUT, 9999 )IFMT, LINE( 1: LLL )
 9999 FORMAT( / 1X, A / 1X, A )
C
      IF( M.LE.0 .OR. N.LE.0 .OR. LDA.LE.0 )
     $   GOTO 1000
      NDIGIT = IDIGIT
      IF( IDIGIT.EQ.0 )
     $   NDIGIT = 4
C
C=======================================================================
C             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
C=======================================================================
C
      IF( IDIGIT.LT.0 ) THEN
         NDIGIT = -IDIGIT
         IF( NDIGIT.LE.4 ) THEN
            DO 40 K1 = 1, N, 2
               K2 = MIN( N, K1+1 )
               WRITE( LOUT, 9998 )( ICOL, I, I = K1, K2 )
               DO 30 I = 1, M
                  IF (K1.NE.N) THEN
                     WRITE( LOUT, 9994 )I, ( A( I, J ), J = K1, K2 )
                  ELSE
                     WRITE( LOUT, 9984 )I, ( A( I, J ), J = K1, K2 ) 
                  END IF
   30          CONTINUE
   40       CONTINUE
C
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 60 K1 = 1, N, 2 
               K2 = MIN( N, K1+1 )
               WRITE( LOUT, 9997 )( ICOL, I, I = K1, K2 )
               DO 50 I = 1, M
                  IF (K1.NE.N) THEN
                     WRITE( LOUT, 9993 )I, ( A( I, J ), J = K1, K2 )
                  ELSE 
                     WRITE( LOUT, 9983 )I, ( A( I, J ), J = K1, K2 ) 
                  END IF
   50          CONTINUE
   60       CONTINUE
C
         ELSE IF( NDIGIT.LE.8 ) THEN
            DO 80 K1 = 1, N, 2 
               K2 = MIN( N, K1+1 )
               WRITE( LOUT, 9996 )( ICOL, I, I = K1, K2 )
               DO 70 I = 1, M
                  IF (K1.NE.N) THEN
                     WRITE( LOUT, 9992 )I, ( A( I, J ), J = K1, K2 )
                  ELSE
                     WRITE( LOUT, 9982 )I, ( A( I, J ), J = K1, K2 ) 
                  END IF 
   70          CONTINUE
   80       CONTINUE
C
         ELSE
            DO 100 K1 = 1, N
               WRITE( LOUT, 9995 ) ICOL, K1
               DO 90 I = 1, M
                  WRITE( LOUT, 9991 )I, A( I, K1 )
   90          CONTINUE
  100       CONTINUE
         END IF
C
C=======================================================================
C             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
C=======================================================================
C
      ELSE
         IF( NDIGIT.LE.4 ) THEN
            DO 120 K1 = 1, N, 4
               K2 = MIN( N, K1+3 )
               WRITE( LOUT, 9998 )( ICOL, I, I = K1, K2 )
               DO 110 I = 1, M
                  IF ((K1+3).LE.N) THEN 
                     WRITE( LOUT, 9974 )I, ( A( I, J ), J = K1, K2 )
                  ELSE IF ((K1+3-N).EQ.1) THEN
                     WRITE( LOUT, 9964 )I, ( A( I, J ), J = K1, K2 )
                  ELSE IF ((K1+3-N).EQ.2) THEN
                     WRITE( LOUT, 9954 )I, ( A( I, J ), J = K1, K2 )
                  ELSE IF ((K1+3-N).EQ.3) THEN
                     WRITE( LOUT, 9944 )I, ( A( I, J ), J = K1, K2 ) 
                  END IF
  110          CONTINUE
  120       CONTINUE
C
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 140 K1 = 1, N, 3 
               K2 = MIN( N, K1+ 2)
               WRITE( LOUT, 9997 )( ICOL, I, I = K1, K2 )
               DO 130 I = 1, M
                  IF ((K1+2).LE.N) THEN
                     WRITE( LOUT, 9973 )I, ( A( I, J ), J = K1, K2 )
                  ELSE IF ((K1+2-N).EQ.1) THEN
                     WRITE( LOUT, 9963 )I, ( A( I, J ), J = K1, K2 )
                  ELSE IF ((K1+2-N).EQ.2) THEN
                     WRITE( LOUT, 9953 )I, ( A( I, J ), J = K1, K2 )
                  END IF
  130          CONTINUE
  140       CONTINUE
C
         ELSE IF( NDIGIT.LE.8 ) THEN
            DO 160 K1 = 1, N, 3
               K2 = MIN( N, K1+2 )
                  WRITE( LOUT, 9996 )( ICOL, I, I = K1, K2 )
               DO 150 I = 1, M
                  IF ((K1+2).LE.N) THEN
                     WRITE( LOUT, 9972 )I, ( A( I, J ), J = K1, K2 )
                  ELSE IF ((K1+2-N).EQ.1) THEN
                     WRITE( LOUT, 9962 )I, ( A( I, J ), J = K1, K2 )
                  ELSE IF ((K1+2-N).EQ.2) THEN
                     WRITE( LOUT, 9952 )I, ( A( I, J ), J = K1, K2 )
                  END IF
  150          CONTINUE
  160       CONTINUE
C
         ELSE
            DO 180 K1 = 1, N, 2
               K2 = MIN( N, K1+1 )
               WRITE( LOUT, 9995 )( ICOL, I, I = K1, K2 )
               DO 170 I = 1, M
                  IF ((K1+1).LE.N) THEN
                     WRITE( LOUT, 9971 )I, ( A( I, J ), J = K1, K2 )
                  ELSE
                     WRITE( LOUT, 9961 )I, ( A( I, J ), J = K1, K2 )
                  END IF
  170          CONTINUE
  180       CONTINUE
         END IF
      END IF
      WRITE( LOUT, 9990 )
C
 9998 FORMAT( 11X, 4( 9X, 3A1, I4, 9X ) )
 9997 FORMAT( 10X, 4( 11X, 3A1, I4, 11X ) )
 9996 FORMAT( 10X, 3( 13X, 3A1, I4, 13X ) )
 9995 FORMAT( 12X, 2( 18X, 3A1, I4, 18X ) ) 
C
C========================================================
C              FORMAT FOR 72 COLUMN
C========================================================
C
C            DISPLAY 4 SIGNIFICANT DIGITS
C 
 9994 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,2('(',D10.3,',',D10.3,')  ') )
 9984 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,1('(',D10.3,',',D10.3,')  ') )
C
C            DISPLAY 6 SIGNIFICANT DIGITS
C
 9993 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,2('(',D12.5,',',D12.5,')  ') )
 9983 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,1('(',D12.5,',',D12.5,')  ') )
C
C            DISPLAY 8 SIGNIFICANT DIGITS
C
 9992 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,2('(',D14.7,',',D14.7,')  ') )
 9982 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,1('(',D14.7,',',D14.7,')  ') )
C
C            DISPLAY 13 SIGNIFICANT DIGITS
C
 9991 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,1('(',D20.13,',',D20.13,')') )
 9990 FORMAT( 1X, ' ' )
C
C
C========================================================
C              FORMAT FOR 132 COLUMN
C========================================================
C
C            DISPLAY 4 SIGNIFICANT DIGIT
C
 9974 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,4('(',D10.3,',',D10.3,')  ') )
 9964 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,3('(',D10.3,',',D10.3,')  ') )
 9954 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,2('(',D10.3,',',D10.3,')  ') )
 9944 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,1('(',D10.3,',',D10.3,')  ') )
C
C            DISPLAY 6 SIGNIFICANT DIGIT
C
 9973 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,3('(',D12.5,',',D12.5,')  ') )
 9963 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,2('(',D12.5,',',D12.5,')  ') )
 9953 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,1('(',D12.5,',',D12.5,')  ') )
C
C            DISPLAY 8 SIGNIFICANT DIGIT
C
 9972 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,3('(',D14.7,',',D14.7,')  ') )
 9962 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,2('(',D14.7,',',D14.7,')  ') )
 9952 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,1('(',D14.7,',',D14.7,')  ') )
C
C            DISPLAY 13 SIGNIFICANT DIGIT
C
 9971 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,2('(',D20.13,',',D20.13,
     &        ')  '))
 9961 FORMAT( 1X, ' ROW', I4, ':', 1X, 1P,1('(',D20.13,',',D20.13,
     &        ')  '))

C
C
 1000 CONTINUE
      END
