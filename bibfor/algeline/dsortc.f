      SUBROUTINE DSORTC
     &  (WHICH, APPLY, N, XREAL, XIMAG, Y)
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 31/01/2005   AUTEUR REZETTE C.REZETTE 
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
C     SUBROUTINE ARPACK TRIANT DES VECTEURS.
C-----------------------------------------------------------------------
C BEGINDOC
C
C DESCRIPTION:
C  SORTS THE COMPLEX ARRAY IN XREAL AND XIMAG INTO THE ORDER
C  SPECIFIED BY WHICH AND OPTIONALLY APPLIES THE PERMUTATION TO THE
C  REAL ARRAY Y. IT IS ASSUMED THAT IF AN ELEMENT OF XIMAG IS
C  NONZERO, THEN ITS NEGATIVE IS ALSO AN ELEMENT. IN OTHER WORDS,
C  BOTH MEMBERS OF A COMPLEX CONJUGATE PAIR ARE TO BE SORTED AND THE
C  PAIRS ARE KEPT ADJACENT TO EACH OTHER.
C
C ARGUMENTS
C  WHICH   CHARACTER*2.  (INPUT)
C          'LM' -> SORT XREAL,XIMAG INTO INCREASING ORDER OF MAGNITUDE.
C          'SM' -> SORT XREAL,XIMAG INTO DECREASING ORDER OF MAGNITUDE.
C          'LR' -> SORT XREAL INTO INCREASING ORDER OF ALGEBRAIC.
C          'SR' -> SORT XREAL INTO DECREASING ORDER OF ALGEBRAIC.
C          'LI' -> SORT XIMAG INTO INCREASING ORDER OF MAGNITUDE.
C          'SI' -> SORT XIMAG INTO DECREASING ORDER OF MAGNITUDE.
C          NOTE: IF AN ELEMENT OF XIMAG IS NON-ZERO, THEN ITS NEGATIVE
C                IS ALSO AN ELEMENT.
C
C  APPLY   LOGICAL.  (INPUT)
C          APPLY = .TRUE.  -> APPLY THE SORTED ORDER TO ARRAY Y.
C          APPLY = .FALSE. -> DO NOT APPLY THE SORTED ORDER TO ARRAY Y.
C
C  N       INTEGER.  (INPUT)
C          SIZE OF THE ARRAYS.
C
C  XREAL,  REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C  XIMAG   REAL AND IMAGINARY PART OF THE ARRAY TO BE SORTED.
C
C  Y       REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C
C ENDDOC
C-----------------------------------------------------------------------
C BEGINLIB
C
C AUTHOR
C     DANNY SORENSEN               PHUONG VU
C     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
C     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
C     APPLIED MATHEMATICS
C     RICE UNIVERSITY
C     HOUSTON, TEXAS
C
C REVISION HISTORY:
C     XX/XX/92: VERSION ' 2.1'
C               ADAPTED FROM THE SORT ROUTINE IN LANSO.
C
C FILE: SORTC.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            IMPLICIT NONE.
C ENDLIB
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%

      CHARACTER*2 WHICH
      LOGICAL APPLY
      INTEGER N

C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%

      REAL*8 XREAL(0:N-1), XIMAG(0:N-1), Y(0:N-1)

C     %---------------%
C     | LOCAL SCALARS |
C     %---------------%C
      INTEGER I, IGAP, J
      REAL*8 TEMP, TEMP1, TEMP2

C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%

      REAL*8 DLAPY2

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%

      IGAP = N / 2

      IF (WHICH .EQ. 'LM') THEN

C        %------------------------------------------------------%
C        | SORT XREAL,XIMAG INTO INCREASING ORDER OF MAGNITUDE. |
C        %------------------------------------------------------%

   10    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000

         DO 30 I = IGAP, N-1
            J = I-IGAP
   20       CONTINUE

            IF (J.LT.0) GO TO 30

            TEMP1 = DLAPY2(XREAL(J),XIMAG(J))
            TEMP2 = DLAPY2(XREAL(J+IGAP),XIMAG(J+IGAP))

            IF (TEMP1.GT.TEMP2) THEN
                TEMP = XREAL(J)
                XREAL(J) = XREAL(J+IGAP)
                XREAL(J+IGAP) = TEMP

                TEMP = XIMAG(J)
                XIMAG(J) = XIMAG(J+IGAP)
                XIMAG(J+IGAP) = TEMP

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

      ELSE IF (WHICH .EQ. 'SM') THEN

C        %------------------------------------------------------%
C        | SORT XREAL,XIMAG INTO DECREASING ORDER OF MAGNITUDE. |
C        %------------------------------------------------------%

   40    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000

         DO 60 I = IGAP, N-1
            J = I-IGAP
   50       CONTINUE

            IF (J .LT. 0) GO TO 60

            TEMP1 = DLAPY2(XREAL(J),XIMAG(J))
            TEMP2 = DLAPY2(XREAL(J+IGAP),XIMAG(J+IGAP))

            IF (TEMP1.LT.TEMP2) THEN
               TEMP = XREAL(J)
               XREAL(J) = XREAL(J+IGAP)
               XREAL(J+IGAP) = TEMP

               TEMP = XIMAG(J)
               XIMAG(J) = XIMAG(J+IGAP)
               XIMAG(J+IGAP) = TEMP

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

      ELSE IF (WHICH .EQ. 'LR') THEN

C        %------------------------------------------------%
C        | SORT XREAL INTO INCREASING ORDER OF ALGEBRAIC. |
C        %------------------------------------------------%

   70    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000

         DO 90 I = IGAP, N-1
            J = I-IGAP
   80       CONTINUE

            IF (J.LT.0) GO TO 90

            IF (XREAL(J).GT.XREAL(J+IGAP)) THEN
               TEMP = XREAL(J)
               XREAL(J) = XREAL(J+IGAP)
               XREAL(J+IGAP) = TEMP

               TEMP = XIMAG(J)
               XIMAG(J) = XIMAG(J+IGAP)
               XIMAG(J+IGAP) = TEMP

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

      ELSE IF (WHICH .EQ. 'SR') THEN

C        %------------------------------------------------%
C        | SORT XREAL INTO DECREASING ORDER OF ALGEBRAIC. |
C        %------------------------------------------------%

  100    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000
         DO 120 I = IGAP, N-1
            J = I-IGAP
  110       CONTINUE

            IF (J.LT.0) GO TO 120

            IF (XREAL(J).LT.XREAL(J+IGAP)) THEN
               TEMP = XREAL(J)
               XREAL(J) = XREAL(J+IGAP)
               XREAL(J+IGAP) = TEMP

               TEMP = XIMAG(J)
               XIMAG(J) = XIMAG(J+IGAP)
               XIMAG(J+IGAP) = TEMP

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

      ELSE IF (WHICH .EQ. 'LI') THEN

C        %------------------------------------------------%
C        | SORT XIMAG INTO INCREASING ORDER OF MAGNITUDE. |
C        %------------------------------------------------%

  130    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000
         DO 150 I = IGAP, N-1
            J = I-IGAP
  140       CONTINUE

            IF (J.LT.0) GO TO 150

            IF (ABS(XIMAG(J)).GT.ABS(XIMAG(J+IGAP))) THEN
               TEMP = XREAL(J)
               XREAL(J) = XREAL(J+IGAP)
               XREAL(J+IGAP) = TEMP

               TEMP = XIMAG(J)
               XIMAG(J) = XIMAG(J+IGAP)
               XIMAG(J+IGAP) = TEMP

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

      ELSE IF (WHICH .EQ. 'SI') THEN

C        %------------------------------------------------%
C        | SORT XIMAG INTO DECREASING ORDER OF MAGNITUDE. |
C        %------------------------------------------------%

  160    CONTINUE
         IF (IGAP .EQ. 0) GO TO 9000
         DO 180 I = IGAP, N-1
            J = I-IGAP
  170       CONTINUE

            IF (J.LT.0) GO TO 180

            IF (ABS(XIMAG(J)).LT.ABS(XIMAG(J+IGAP))) THEN
               TEMP = XREAL(J)
               XREAL(J) = XREAL(J+IGAP)
               XREAL(J+IGAP) = TEMP

               TEMP = XIMAG(J)
               XIMAG(J) = XIMAG(J+IGAP)
               XIMAG(J+IGAP) = TEMP

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

 9000 CONTINUE

C     %---------------%
C     | END OF DSORTC |
C     %---------------%

      END
