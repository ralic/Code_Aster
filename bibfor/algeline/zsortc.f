      SUBROUTINE ZSORTC (WHICH, APPLY, N, X, Y)
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 06/11/2006   AUTEUR MCOURTOI M.COURTOIS 
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
C     SUBROUTINE ARPACK TRIANT DES VECTEURS COMPLEXES.
C-----------------------------------------------------------------------
C\BEGINDOC
C
C\NAME: ZSORTC
C
C\DESCRIPTION:
C  SORTS THE COMPLEX*16 ARRAY IN X INTO THE ORDER 
C  SPECIFIED BY WHICH AND OPTIONALLY APPLIES THE PERMUTATION TO THE
C  DOUBLE PRECISION  ARRAY Y. 
C
C\USAGE:
C  CALL ZSORTC
C     ( WHICH, APPLY, N, X, Y )
C
C\ARGUMENTS
C  WHICH   CHARACTER*2.  (INPUT)
C          'LM' -> SORT X INTO INCREASING ORDER OF MAGNITUDE.
C          'SM' -> SORT X INTO DECREASING ORDER OF MAGNITUDE.
C          'LR' -> SORT X WITH REAL(X) IN INCREASING ALGEBRAIC ORDER 
C          'SR' -> SORT X WITH REAL(X) IN DECREASING ALGEBRAIC ORDER
C          'LI' -> SORT X WITH IMAG(X) IN INCREASING ALGEBRAIC ORDER
C          'SI' -> SORT X WITH IMAG(X) IN DECREASING ALGEBRAIC ORDER
C
C  APPLY   LOGICAL.  (INPUT)
C          APPLY = .TRUE.  -> APPLY THE SORTED ORDER TO ARRAY Y.
C          APPLY = .FALSE. -> DO NOT APPLY THE SORTED ORDER TO ARRAY Y.
C
C  N       INTEGER.  (INPUT)
C          SIZE OF THE ARRAYS.
C
C  X       COMPLEX*16 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C          THIS IS THE ARRAY TO BE SORTED.
C
C  Y       COMPLEX*16 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C
C\ENDDOC
C
C-----------------------------------------------------------------------
C
C\BEGINLIB
C
C\ROUTINES CALLED:
C     DLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
C
C\AUTHOR
C     DANNY SORENSEN               PHUONG VU
C     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
C     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
C     APPLIED MATHEMATICS 
C     RICE UNIVERSITY           
C     HOUSTON, TEXAS 
C
C     ADAPTED FROM THE SORT ROUTINE IN LANSO.
C
C\SCCS INFORMATION: @(#)
C FILE: SORTC.F   SID: 2.2   DATE OF SID: 4/20/96   RELEASE: 2
C
C\ENDLIB
C
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%

      CHARACTER*2 WHICH
      LOGICAL APPLY
      INTEGER N
C
C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%
C
      COMPLEX*16 X(0:N-1), Y(0:N-1)
C
C     %---------------%
C     | LOCAL SCALARS |
C     %---------------%
C
      INTEGER    I, IGAP, J
      COMPLEX*16 TEMP
      REAL*8     TEMP1, TEMP2
C
C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%
C
      REAL*8  DLAPY2
C
C
C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%
C
      CALL MATFPE(-1)
      IGAP = N / 2
C 
      IF (WHICH .EQ. 'LM') THEN
C
C        %--------------------------------------------%
C        | SORT X INTO INCREASING ORDER OF MAGNITUDE. |
C        %--------------------------------------------%
C
   10    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000
C
         DO 30 I = IGAP, N-1
            J = I-IGAP
   20       CONTINUE
C
            IF (J.LT.0) GO TO 30
C
            TEMP1 = DLAPY2(DBLE(X(J)),DIMAG(X(J)))
            TEMP2 = DLAPY2(DBLE(X(J+IGAP)),DIMAG(X(J+IGAP)))
C
            IF (TEMP1.GT.TEMP2) THEN
                TEMP = X(J)
                X(J) = X(J+IGAP)
                X(J+IGAP) = TEMP
C
                IF (APPLY) THEN
                    TEMP = Y(J)
                    Y(J) = Y(J+IGAP)
                    Y(J+IGAP) = TEMP
                END IF
            ELSE
                GO TO 30
            END IF
            J = J-IGAP
            GO TO 20
   30    CONTINUE
         IGAP = IGAP / 2
         GO TO 10
C
      ELSE IF (WHICH .EQ. 'SM') THEN
C
C        %--------------------------------------------%
C        | SORT X INTO DECREASING ORDER OF MAGNITUDE. |
C        %--------------------------------------------%
C
   40    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000
C
         DO 60 I = IGAP, N-1
            J = I-IGAP
   50       CONTINUE
C
            IF (J .LT. 0) GO TO 60
C
            TEMP1 = DLAPY2(DBLE(X(J)),DIMAG(X(J)))
            TEMP2 = DLAPY2(DBLE(X(J+IGAP)),DIMAG(X(J+IGAP)))
C
            IF (TEMP1.LT.TEMP2) THEN
               TEMP = X(J)
               X(J) = X(J+IGAP)
               X(J+IGAP) = TEMP
C 
               IF (APPLY) THEN
                  TEMP = Y(J)
                  Y(J) = Y(J+IGAP)
                  Y(J+IGAP) = TEMP
               END IF
            ELSE
               GO TO 60
            ENDIF
            J = J-IGAP
            GO TO 50
   60    CONTINUE
         IGAP = IGAP / 2
         GO TO 40
C 
      ELSE IF (WHICH .EQ. 'LR') THEN
C
C        %------------------------------------------------%
C        | SORT XREAL INTO INCREASING ORDER OF ALGEBRAIC. |
C        %------------------------------------------------%
C
   70    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000
C
         DO 90 I = IGAP, N-1
            J = I-IGAP
   80       CONTINUE
C
            IF (J.LT.0) GO TO 90
C
            IF (DBLE(X(J)).GT.DBLE(X(J+IGAP))) THEN
               TEMP = X(J)
               X(J) = X(J+IGAP)
               X(J+IGAP) = TEMP
C 
               IF (APPLY) THEN
                  TEMP = Y(J)
                  Y(J) = Y(J+IGAP)
                  Y(J+IGAP) = TEMP
               END IF
            ELSE
               GO TO 90
            ENDIF
            J = J-IGAP
            GO TO 80
   90    CONTINUE
         IGAP = IGAP / 2
         GO TO 70
C 
      ELSE IF (WHICH .EQ. 'SR') THEN
C
C        %------------------------------------------------%
C        | SORT XREAL INTO DECREASING ORDER OF ALGEBRAIC. |
C        %------------------------------------------------%
C
  100    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000
         DO 120 I = IGAP, N-1
            J = I-IGAP
  110       CONTINUE
C
            IF (J.LT.0) GO TO 120
C
            IF (DBLE(X(J)).LT.DBLE(X(J+IGAP))) THEN
               TEMP = X(J)
               X(J) = X(J+IGAP)
               X(J+IGAP) = TEMP
C 
               IF (APPLY) THEN
                  TEMP = Y(J)
                  Y(J) = Y(J+IGAP)
                  Y(J+IGAP) = TEMP
               END IF
            ELSE
               GO TO 120
            ENDIF
            J = J-IGAP
            GO TO 110
  120    CONTINUE
         IGAP = IGAP / 2
         GO TO 100
C 
      ELSE IF (WHICH .EQ. 'LI') THEN
C
C        %--------------------------------------------%
C        | SORT XIMAG INTO INCREASING ALGEBRAIC ORDER |
C        %--------------------------------------------%
C
  130    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000
         DO 150 I = IGAP, N-1
            J = I-IGAP
  140       CONTINUE
C
            IF (J.LT.0) GO TO 150
C
            IF (DIMAG(X(J)).GT.DIMAG(X(J+IGAP))) THEN
               TEMP = X(J)
               X(J) = X(J+IGAP)
               X(J+IGAP) = TEMP
C
               IF (APPLY) THEN
                  TEMP = Y(J)
                  Y(J) = Y(J+IGAP)
                  Y(J+IGAP) = TEMP
               END IF
            ELSE
               GO TO 150
            ENDIF
            J = J-IGAP
            GO TO 140
  150    CONTINUE
         IGAP = IGAP / 2
         GO TO 130
C 
      ELSE IF (WHICH .EQ. 'SI') THEN
C
C        %---------------------------------------------%
C        | SORT XIMAG INTO DECREASING ALGEBRAIC ORDER  |
C        %---------------------------------------------%
C
  160    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000
         DO 180 I = IGAP, N-1
            J = I-IGAP
  170       CONTINUE
C
            IF (J.LT.0) GO TO 180
C
            IF (DIMAG(X(J)).LT.DIMAG(X(J+IGAP))) THEN
               TEMP = X(J)
               X(J) = X(J+IGAP)
               X(J+IGAP) = TEMP
C 
               IF (APPLY) THEN
                  TEMP = Y(J)
                  Y(J) = Y(J+IGAP)
                  Y(J+IGAP) = TEMP
               END IF
            ELSE
               GO TO 180
            ENDIF
            J = J-IGAP
            GO TO 170
  180    CONTINUE
         IGAP = IGAP / 2
         GO TO 160
      END IF
C 
 9000 CONTINUE
      CALL MATFPE(1)
C
C     %---------------%
C     | END OF ZSORTC |
C     %---------------%
C
      END
