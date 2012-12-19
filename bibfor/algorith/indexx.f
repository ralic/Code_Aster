      SUBROUTINE INDEXX(N,ARR,INDX)
C
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C----------------------------------------------------------------------
C ****************** DECLARATION DES VARIABLES ************************
C
      IMPLICIT NONE
C
      INTEGER N, M, NSTACK
      PARAMETER ( M = 7, NSTACK = 50)
C
      INTEGER I, J, INDX(N), IR, L, JSTACK, ISTACK(NSTACK), K, ITEMP
      INTEGER INDXT
      REAL*8  ARR(N), A
C
C ******************* DEBUT DU CODE EXECUTABLE ************************
C
C
      DO 11 J=1,N
        INDX(J) = J
 11   CONTINUE
C
      JSTACK = 0
C
      L = 1
      IR = N
 1    CONTINUE
      IF (IR-L .LT. M) THEN
        DO 13 J=L+1,IR
          INDXT = INDX(J)
          A = ARR(INDXT)
          DO 12 I=J-1,1,-1
            IF (ARR(INDX(I)) .LE. A) GOTO 2
              INDX(I+1) = INDX(I)
 12       CONTINUE
          I = 0
 2        CONTINUE
          INDX(I+1) = INDXT
 13     CONTINUE
        IF (JSTACK .EQ. 0 ) GOTO 9999
        IR = ISTACK(JSTACK)
        L  = ISTACK(JSTACK-1)
        JSTACK = JSTACK - 2
      ELSE
        K = (L+IR)/2
        ITEMP = INDX(K)
        INDX(K) = INDX(L+1)
        INDX(L+1) = ITEMP
        IF (ARR(INDX(L+1)) .GT. ARR(INDX(IR))) THEN
          ITEMP = INDX(L+1)
          INDX(L+1) = INDX(IR)
          INDX(IR) = ITEMP
        ENDIF
        IF (ARR(INDX(L)) .GT. ARR(INDX(IR))) THEN
          ITEMP = INDX(L)
          INDX(L) = INDX(IR)
          INDX(IR) = ITEMP
        ENDIF
        IF (ARR(INDX(L+1)) .GT. ARR(INDX(L))) THEN
          ITEMP = INDX(L+1)
          INDX(L+1) = INDX(L)
          INDX(L) = ITEMP
        ENDIF
        I = L+1
        J = IR
        INDXT = INDX(L)
        A = ARR(INDXT)
 3    CONTINUE
      I = I+ 1
      IF (ARR(INDX(I)) .LT. A) GOTO 3
 4    CONTINUE
      J = J-1
      IF (ARR(INDX(J)) .GT. A) GOTO 4
      IF (J.LT.I) GOTO 5
      ITEMP = INDX(I)
      INDX(I) = INDX(J)
      INDX(J) = ITEMP
      GOTO 3
 5    CONTINUE
      INDX(L) = INDX(J)
      INDX(J) = INDXT
      JSTACK = JSTACK + 2
      IF (IR-I+1 .GE. J-L) THEN
        ISTACK(JSTACK) = IR
        ISTACK(JSTACK-1) = I
        IR = J-1
      ELSE
        ISTACK(JSTACK) = J-1
        ISTACK(JSTACK-1) = L
      ENDIF
      ENDIF
      GOTO 1
9999  CONTINUE
      END
