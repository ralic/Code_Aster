      SUBROUTINE ZVOUT
     &  (LOUT, N, CX, IDIGIT, IFMT)
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
C     SUBROUTINE ARPACK ECRIVANT DES VECTEURS DE COMPLEXE.
C-----------------------------------------------------------------------
C  Routine:    ZVOUT
C
C  Purpose:    Complex*16 vector output routine.
C
C  Usage:      CALL ZVOUT (LOUT, N, CX, IDIGIT, IFMT)
C
C  Arguments
C     N      - Length of array CX.  (Input)
C     CX     - Complex*16 array to be printed.  (Input)
C     IFMT   - Format to be used in printing array CX.  (Input)
C     IDIGIT - Print up to IABS(IDIGIT) decimal digits per number.  (In)
C              If IDIGIT .LT. 0, printing is done with 72 columns.
C              If IDIGIT .GT. 0, printing is done with 132 columns.
C
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER.
C            REMPLACEMENT DE 1 RETURN PAR 1 GOTO 1005.
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
      IMPLICIT NONE
C     ... SPECIFICATIONS FOR ARGUMENTS
      INTEGER            N, IDIGIT, LOUT
      COMPLEX*16         CX(*)
      CHARACTER*(*)      IFMT

C     ... SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER            I, NDIGIT, K1, K2, LLL
      CHARACTER*80       LINE

C     ... FIRST EXECUTABLE STATEMENT


      LLL = MIN( LEN( IFMT ), 80 )
      DO 1 I = 1, LLL
         LINE( I: I ) = '-'
   1  CONTINUE

      DO 2 I = LLL + 1, 80
         LINE( I: I ) = ' '
   2  CONTINUE

      WRITE( LOUT, 2000 )IFMT, LINE( 1: LLL )
 2000 FORMAT ( /1X, A  /1X, A )

      IF (N .LE. 0) GOTO 1005
      NDIGIT = IDIGIT
      IF (IDIGIT .EQ. 0) NDIGIT = 4

C=======================================================================
C             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
C=======================================================================

      IF( IDIGIT.LT.0 ) THEN
         NDIGIT = -IDIGIT
         IF( NDIGIT.LE.4 ) THEN
            DO 30 K1 = 1, N, 2
               K2 = MIN( N, K1+1 )
               IF (K1.NE.N) THEN
                  WRITE( LOUT, 9998 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE
                  WRITE( LOUT, 9997 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 ) 
               END IF
   30       CONTINUE
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 40 K1 = 1, N, 2
               K2 = MIN( N, K1+1 )
               IF (K1.NE.N) THEN
                  WRITE( LOUT, 9988 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE
                  WRITE( LOUT, 9987 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               END IF
   40       CONTINUE
         ELSE IF( NDIGIT.LE.8 ) THEN
            DO 50 K1 = 1, N, 2
               K2 = MIN( N, K1+1 )
               IF (K1.NE.N) THEN
                  WRITE( LOUT, 9978 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE
                  WRITE( LOUT, 9977 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 ) 
               END IF
   50       CONTINUE
         ELSE
            DO 60 K1 = 1, N
               WRITE( LOUT, 9968 )K1, K1, CX( I )
   60       CONTINUE
         END IF

C=======================================================================
C             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
C=======================================================================

      ELSE
         IF( NDIGIT.LE.4 ) THEN
            DO 70 K1 = 1, N, 4 
               K2 = MIN( N, K1+3 )
               IF ((K1+3).LE.N) THEN
                  WRITE( LOUT, 9958 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE IF ((K1+3-N) .EQ. 1) THEN
                  WRITE( LOUT, 9957 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE IF ((K1+3-N) .EQ. 2) THEN
                  WRITE( LOUT, 9956 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE IF ((K1+3-N) .EQ. 1) THEN
                  WRITE( LOUT, 9955 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               END IF
   70       CONTINUE
         ELSE IF( NDIGIT.LE.6 ) THEN
            DO 80 K1 = 1, N, 3 
               K2 = MIN( N, K1+2 )
               IF ((K1+2).LE.N) THEN
                  WRITE( LOUT, 9948 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE IF ((K1+2-N) .EQ. 1) THEN
                  WRITE( LOUT, 9947 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE IF ((K1+2-N) .EQ. 2) THEN
                  WRITE( LOUT, 9946 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               END IF
   80       CONTINUE
         ELSE IF( NDIGIT.LE.8 ) THEN
            DO 90 K1 = 1, N, 3 
               K2 = MIN( N, K1+2 )
               IF ((K1+2).LE.N) THEN
                  WRITE( LOUT, 9938 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE IF ((K1+2-N) .EQ. 1) THEN
                  WRITE( LOUT, 9937 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE IF ((K1+2-N) .EQ. 2) THEN
                  WRITE( LOUT, 9936 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               END IF
   90       CONTINUE
         ELSE
            DO 100 K1 = 1, N, 2
               K2 = MIN( N, K1+1 )
               IF ((K1+2).LE.N) THEN
                  WRITE( LOUT, 9928 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               ELSE IF ((K1+2-N) .EQ. 1) THEN
                  WRITE( LOUT, 9927 )K1, K2, ( CX( I ), 
     $                   I = K1, K2 )
               END IF
  100       CONTINUE
         END IF
      END IF
      WRITE( LOUT, 9994 )
      GOTO 1005

C=======================================================================
C                   FORMAT FOR 72 COLUMNS
C=======================================================================

C                 DISPLAY 4 SIGNIFICANT DIGITS

 9998 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,2('(',D10.3,',',D10.3,')  ') ) 
 9997 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,1('(',D10.3,',',D10.3,')  ') )

C                 DISPLAY 6 SIGNIFICANT DIGITS
 
 9988 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,2('(',D12.5,',',D12.5,')  ') )
 9987 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,1('(',D12.5,',',D12.5,')  ') )

C                 DISPLAY 8 SIGNIFICANT DIGITS

 9978 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,2('(',D14.7,',',D14.7,')  ') )
 9977 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,1('(',D14.7,',',D14.7,')  ') )

C                 DISPLAY 13 SIGNIFICANT DIGITS

 9968 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,1('(',D20.13,',',D20.13,')  ') ) 

C=====================================================================
C                   FORMAT FOR 132 COLUMNS
C=====================================================================

C                 DISPLAY 4 SIGNIFICANT DIGITS

 9958 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,4('(',D10.3,',',D10.3,')  ') )
 9957 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,3('(',D10.3,',',D10.3,')  ') )
 9956 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,2('(',D10.3,',',D10.3,')  ') )
 9955 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,1('(',D10.3,',',D10.3,')  ') )

C                 DISPLAY 6 SIGNIFICANT DIGITS

 9948 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,3('(',D12.5,',',D12.5,')  ') )
 9947 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,2('(',D12.5,',',D12.5,')  ') )
 9946 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,1('(',D12.5,',',D12.5,')  ') )

C                 DISPLAY 8 SIGNIFICANT DIGITS

 9938 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,3('(',D14.7,',',D14.7,')  ') )
 9937 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,2('(',D14.7,',',D14.7,')  ') )
 9936 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,1('(',D14.7,',',D14.7,')  ') )

C                 DISPLAY 13 SIGNIFICANT DIGITS

 9928 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,2('(',D20.13,',',D20.13,')  ') )
 9927 FORMAT( 1X, I4, ' - ', I4, ':', 1X,
     $        1P,1('(',D20.13,',',D20.13,')  ') )


 9994 FORMAT( 1X, ' ' )
 1005 CONTINUE
      END
