      SUBROUTINE IVOUT
     &  (LOUT, N, IX, IDIGIT, IFMT)
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
C     SUBROUTINE ARPACK ECRIVANT DES VECTEURS D'ENTIERS.
C-----------------------------------------------------------------------
C  ROUTINE:    IVOUT
C
C  PURPOSE:    INTEGER VECTOR OUTPUT ROUTINE.
C
C  ARGUMENTS
C     N      - LENGTH OF ARRAY IX. (INPUT)
C     IX     - INTEGER ARRAY TO BE PRINTED. (INPUT)
C     IFMT   - FORMAT TO BE USED IN PRINTING ARRAY IX. (INPUT)
C     IDIGIT - PRINT UP TO ABS(IDIGIT) DECIMAL DIGITS / NUMBER. (INPUT)
C              IF IDIGIT .LT. 0, PRINTING IS DONE WITH 72 COLUMNS.
C              IF IDIGIT .GT. 0, PRINTING IS DONE WITH 132 COLUMNS.
C INTRINSIC FUNCTIONS
C     MIN, LEN
C
C-----------------------------------------------------------------------
C ASTER INFORMATION
C 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER.
C            REMPLACEMENT DE 1 RETURN PAR 1 GOTO 1005.
C            IMPLICIT NONE.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     ... SPECIFICATIONS FOR ARGUMENTS
      INTEGER  IX(*), N, IDIGIT, LOUT
      CHARACTER*(*)  IFMT

C     ... SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, NDIGIT, K1, K2, LLL
      CHARACTER*80 LINE

      LLL = MIN ( LEN ( IFMT ), 80 )
      DO 1 I = 1, LLL
          LINE(I:I) = '-'
    1 CONTINUE

      DO 2 I = LLL+1, 80
          LINE(I:I) = ' '
    2 CONTINUE

      WRITE ( LOUT, 2000 ) IFMT, LINE(1:LLL)
 2000 FORMAT ( /1X, A  /1X, A )
C
      IF (N .LE. 0) GOTO 1005
      NDIGIT = IDIGIT
      IF (IDIGIT .EQ. 0) NDIGIT = 4

C=======================================================================
C             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
C=======================================================================

      IF (IDIGIT .LT. 0) THEN

      NDIGIT = -IDIGIT
      IF (NDIGIT .LE. 4) THEN
         DO 10 K1 = 1, N, 10
            K2 = MIN(N,K1+9)
            WRITE(LOUT,1000) K1,K2,(IX(I),I=K1,K2)
   10    CONTINUE

      ELSE IF (NDIGIT .LE. 6) THEN
         DO 30 K1 = 1, N, 7
            K2 = MIN(N,K1+6)
            WRITE(LOUT,1001) K1,K2,(IX(I),I=K1,K2)
   30    CONTINUE

      ELSE IF (NDIGIT .LE. 10) THEN
         DO 50 K1 = 1, N, 5
            K2 = MIN(N,K1+4)
            WRITE(LOUT,1002) K1,K2,(IX(I),I=K1,K2)
   50    CONTINUE

      ELSE
         DO 70 K1 = 1, N, 3
            K2 = MIN(N,K1+2)
            WRITE(LOUT,1003) K1,K2,(IX(I),I=K1,K2)
   70    CONTINUE
      END IF

C=======================================================================
C             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
C=======================================================================

      ELSE

      IF (NDIGIT .LE. 4) THEN
         DO 90 K1 = 1, N, 20
            K2 = MIN(N,K1+19)
            WRITE(LOUT,1000) K1,K2,(IX(I),I=K1,K2)
   90    CONTINUE

      ELSE IF (NDIGIT .LE. 6) THEN
         DO 110 K1 = 1, N, 15
            K2 = MIN(N,K1+14)
            WRITE(LOUT,1001) K1,K2,(IX(I),I=K1,K2)
  110    CONTINUE

      ELSE IF (NDIGIT .LE. 10) THEN
         DO 130 K1 = 1, N, 10
            K2 = MIN(N,K1+9)
            WRITE(LOUT,1002) K1,K2,(IX(I),I=K1,K2)
  130    CONTINUE

      ELSE
         DO 150 K1 = 1, N, 7
            K2 = MIN(N,K1+6)
            WRITE(LOUT,1003) K1,K2,(IX(I),I=K1,K2)
  150    CONTINUE
      END IF
      END IF
      WRITE (LOUT,1004)

 1000 FORMAT(1X,I4,' - ',I4,':',20(1X,I5))
 1001 FORMAT(1X,I4,' - ',I4,':',15(1X,I7))
 1002 FORMAT(1X,I4,' - ',I4,':',10(1X,I11))
 1003 FORMAT(1X,I4,' - ',I4,':',7(1X,I15))
 1004 FORMAT(1X,' ')

 1005 CONTINUE
      END
