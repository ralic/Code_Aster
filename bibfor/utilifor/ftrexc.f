      SUBROUTINE FTREXC( COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, WORK,
     &                   INFO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 20/09/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) LAPACK
C ======================================================================
C
C     SUBROUTINE LAPACK REORDONNANT LA FACTORISATION DE SCHUR REELLE
C     D'UNE MATRICE REELLE QUELCONQUE.
C-----------------------------------------------------------------------
C  -- LAPACK ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     MARCH 31, 1993
C
C  PURPOSE
C  =======
C
C  FTREXC REORDERS THE REAL SCHUR FACTORIZATION OF A REAL MATRIX
C  A = Q*T*Q**T, SO THAT THE DIAGONAL BLOCK OF T WITH ROW INDEX IFST IS
C  MOVED TO ROW ILST.
C
C  THE REAL SCHUR FORM T IS REORDERED BY AN ORTHOGONAL SIMILARITY
C  TRANSFORMATION Z**T*T*Z, AND OPTIONALLY THE MATRIX Q OF SCHUR VECTORS
C  IS UPDATED BY POSTMULTIPLYING IT WITH Z.
C
C  T MUST BE IN SCHUR CANONICAL FORM (AS RETURNED BY DHSEQR), THAT IS,
C  BLOCK UPPER TRIANGULAR WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS, EACH
C  2-BY-2 DIAGONAL BLOCK HAS ITS DIAGONAL ELEMENTS EQUAL AND ITS
C  OFF-DIAGONAL ELEMENTS OF OPPOSITE SIGN.
C
C  ARGUMENTS
C  =========
C
C  COMPQ   (INPUT) CHARACTER*1
C          = 'V':  UPDATE THE MATRIX Q OF SCHUR VECTORS,
C          = 'N':  DO NOT UPDATE Q.
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX T. N >= 0.
C
C  T       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDT,N)
C          ON ENTRY, THE UPPER QUASI-TRIANGULAR MATRIX T, IN SCHUR
C          SCHUR CANONICAL FORM.
C          ON EXIT, THE REORDERED UPPER QUASI-TRIANGULAR MATRIX, AGAIN
C          IN SCHUR CANONICAL FORM.
C
C  LDT     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
C
C  Q       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDQ,N)
C          ON ENTRY, IF COMPQ = 'V', THE MATRIX Q OF SCHUR VECTORS.
C          ON EXIT, IF COMPQ = 'V', Q HAS BEEN POSTMULTIPLIED BY THE
C          ORTHOGONAL TRANSFORMATION MATRIX Z WHICH REORDERS T.
C          IF COMPQ = 'N', Q IS NOT REFERENCED.
C
C  LDQ     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY Q.  LDQ >= MAX(1,N).
C
C  IFST    (INPUT/OUTPUT) INTEGER
C  ILST    (INPUT/OUTPUT) INTEGER
C          SPECIFY THE REORDERING OF THE DIAGONAL BLOCKS OF T.
C          THE BLOCK WITH ROW INDEX IFST IS MOVED TO ROW ILST, BY A
C          SEQUENCE OF TRANSPOSITIONS BETWEEN ADJACENT BLOCKS.
C          ON EXIT, IF IFST POINTED ON ENTRY TO THE SECOND ROW OF A
C          2-BY-2 BLOCK, IT IS CHANGED TO POINT TO THE FIRST ROW, ILST
C          ALWAYS POINTS TO THE FIRST ROW OF THE BLOCK IN ITS FINAL
C          POSITION (WHICH MAY DIFFER FROM ITS INPUT VALUE BY +1 OR -1).
C          1 <= IFST <= N, 1 <= ILST <= N.
C
C  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION (N)
C
C  INFO    (OUTPUT) INTEGER
C          = 0:  SUCCESSFUL EXIT
C          < 0:  IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
C          = 1:  TWO ADJACENT BLOCKS WERE TOO CLOSE TO SWAP (THE PROBLEM
C                IS VERY ILL-CONDITIONED), T MAY HAVE BEEN PARTIALLY
C                REORDERED, AND ILST POINTS TO THE FIRST ROW OF THE
C                CURRENT POSITION OF THE BLOCK BEING MOVED.
C
C ASTER INFORMATION
C 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C            REMPLACEMENT DE 9 RETURN PAR GOTO 1000,
C            IMPLICIT NONE.
C INTRINSIC FUNCTION
C   MAX
C ENDLIB
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        COMPQ
      INTEGER            IFST, ILST, INFO, LDQ, LDT, N
C     ..
C     .. ARRAY ARGUMENTS ..
      REAL*8   Q( LDQ, * ), T( LDT, * ), WORK( * )

C     .. PARAMETERS ..
      REAL*8   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            WANTQ
      INTEGER            HERE, NBF, NBL, NBNEXT
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LLSAME
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     DECODE AND TEST THE INPUT ARGUMENTS.
C
      INFO = 0
      WANTQ = LLSAME( COMPQ, 'V' )
      IF( .NOT.WANTQ .AND. .NOT.LLSAME( COMPQ, 'N' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDT.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LDQ.LT.1 .OR. ( WANTQ .AND. LDQ.LT.MAX( 1, N ) ) ) THEN
         INFO = -6
      ELSE IF( IFST.LT.1 .OR. IFST.GT.N ) THEN
         INFO = -7
      ELSE IF( ILST.LT.1 .OR. ILST.GT.N ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'FTREXC', -INFO )
         GOTO 1000
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( N.LE.1 )
     &   GOTO 1000
C
C     DETERMINE THE FIRST ROW OF SPECIFIED BLOCK
C     AND FIND OUT IT IS 1 BY 1 OR 2 BY 2.
C
      IF( IFST.GT.1 ) THEN
         IF( T( IFST, IFST-1 ).NE.ZERO )
     &      IFST = IFST - 1
      END IF
      NBF = 1
      IF( IFST.LT.N ) THEN
         IF( T( IFST+1, IFST ).NE.ZERO )
     &      NBF = 2
      END IF
C
C     DETERMINE THE FIRST ROW OF THE FINAL BLOCK
C     AND FIND OUT IT IS 1 BY 1 OR 2 BY 2.
C
      IF( ILST.GT.1 ) THEN
         IF( T( ILST, ILST-1 ).NE.ZERO )
     &      ILST = ILST - 1
      END IF
      NBL = 1
      IF( ILST.LT.N ) THEN
         IF( T( ILST+1, ILST ).NE.ZERO )
     &      NBL = 2
      END IF
C
      IF( IFST.EQ.ILST )
     &   GOTO 1000
C
      IF( IFST.LT.ILST ) THEN
C
C        UPDATE ILST
C
         IF( NBF.EQ.2 .AND. NBL.EQ.1 )
     &      ILST = ILST - 1
         IF( NBF.EQ.1 .AND. NBL.EQ.2 )
     &      ILST = ILST + 1
C
         HERE = IFST
C
   10    CONTINUE
C
C        SWAP BLOCK WITH NEXT ONE BELOW
C
         IF( NBF.EQ.1 .OR. NBF.EQ.2 ) THEN
C
C           CURRENT BLOCK EITHER 1 BY 1 OR 2 BY 2
C
            NBNEXT = 1
            IF( HERE+NBF+1.LE.N ) THEN
               IF( T( HERE+NBF+1, HERE+NBF ).NE.ZERO )
     &            NBNEXT = 2
            END IF
            CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE, NBF, NBNEXT,
     &                   WORK, INFO )
            IF( INFO.NE.0 ) THEN
               ILST = HERE
               GOTO 1000
            END IF
            HERE = HERE + NBNEXT
C
C           TEST IF 2 BY 2 BLOCK BREAKS INTO TWO 1 BY 1 BLOCKS
C
            IF( NBF.EQ.2 ) THEN
               IF( T( HERE+1, HERE ).EQ.ZERO )
     &            NBF = 3
            END IF
C
         ELSE
C
C           CURRENT BLOCK CONSISTS OF TWO 1 BY 1 BLOCKS EACH OF WHICH
C           MUST BE SWAPPED INDIVIDUALLY
C
            NBNEXT = 1
            IF( HERE+3.LE.N ) THEN
               IF( T( HERE+3, HERE+2 ).NE.ZERO )
     &            NBNEXT = 2
            END IF
            CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE+1, 1, NBNEXT,
     &                   WORK, INFO )
            IF( INFO.NE.0 ) THEN
               ILST = HERE
               GOTO 1000
            END IF
            IF( NBNEXT.EQ.1 ) THEN
C
C              SWAP TWO 1 BY 1 BLOCKS, NO PROBLEMS POSSIBLE
C
               CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE, 1, NBNEXT,
     &                      WORK, INFO )
               HERE = HERE + 1
            ELSE
C
C              RECOMPUTE NBNEXT IN CASE 2 BY 2 SPLIT
C
               IF( T( HERE+2, HERE+1 ).EQ.ZERO )
     &            NBNEXT = 1
               IF( NBNEXT.EQ.2 ) THEN
C
C                 2 BY 2 BLOCK DID NOT SPLIT
C
                  CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE, 1,
     &                         NBNEXT, WORK, INFO )
                  IF( INFO.NE.0 ) THEN
                     ILST = HERE
                     GOTO 1000
                  END IF
                  HERE = HERE + 2
               ELSE
C
C                 2 BY 2 BLOCK DID SPLIT
C
                  CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE, 1, 1,
     &                         WORK, INFO )
                  CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE+1, 1, 1,
     &                         WORK, INFO )
                  HERE = HERE + 2
               END IF
            END IF
         END IF
         IF( HERE.LT.ILST )
     &      GO TO 10
C
      ELSE
C
         HERE = IFST
   20    CONTINUE
C
C        SWAP BLOCK WITH NEXT ONE ABOVE
C
         IF( NBF.EQ.1 .OR. NBF.EQ.2 ) THEN
C
C           CURRENT BLOCK EITHER 1 BY 1 OR 2 BY 2
C
            NBNEXT = 1
            IF( HERE.GE.3 ) THEN
               IF( T( HERE-1, HERE-2 ).NE.ZERO )
     &            NBNEXT = 2
            END IF
            CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE-NBNEXT, NBNEXT,
     &                   NBF, WORK, INFO )
            IF( INFO.NE.0 ) THEN
               ILST = HERE
               GOTO 1000
            END IF
            HERE = HERE - NBNEXT
C
C           TEST IF 2 BY 2 BLOCK BREAKS INTO TWO 1 BY 1 BLOCKS
C
            IF( NBF.EQ.2 ) THEN
               IF( T( HERE+1, HERE ).EQ.ZERO )
     &            NBF = 3
            END IF
C
         ELSE
C
C           CURRENT BLOCK CONSISTS OF TWO 1 BY 1 BLOCKS EACH OF WHICH
C           MUST BE SWAPPED INDIVIDUALLY
C
            NBNEXT = 1
            IF( HERE.GE.3 ) THEN
               IF( T( HERE-1, HERE-2 ).NE.ZERO )
     &            NBNEXT = 2
            END IF
            CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE-NBNEXT, NBNEXT,
     &                   1, WORK, INFO )
            IF( INFO.NE.0 ) THEN
               ILST = HERE
               GOTO 1000
            END IF
            IF( NBNEXT.EQ.1 ) THEN
C
C              SWAP TWO 1 BY 1 BLOCKS, NO PROBLEMS POSSIBLE
C
               CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE, NBNEXT, 1,
     &                      WORK, INFO )
               HERE = HERE - 1
            ELSE
C
C              RECOMPUTE NBNEXT IN CASE 2 BY 2 SPLIT
C
               IF( T( HERE, HERE-1 ).EQ.ZERO )
     &            NBNEXT = 1
               IF( NBNEXT.EQ.2 ) THEN
C
C                 2 BY 2 BLOCK DID NOT SPLIT
C
                  CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE-1, 2, 1,
     &                         WORK, INFO )
                  IF( INFO.NE.0 ) THEN
                     ILST = HERE
                     GOTO 1000
                  END IF
                  HERE = HERE - 2
               ELSE
C
C                 2 BY 2 BLOCK DID SPLIT
C
                  CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE, 1, 1,
     &                         WORK, INFO )
                  CALL FLAEXC( WANTQ, N, T, LDT, Q, LDQ, HERE-1, 1, 1,
     &                         WORK, INFO )
                  HERE = HERE - 2
               END IF
            END IF
         END IF
         IF( HERE.GT.ILST )
     &      GO TO 20
      END IF
      ILST = HERE
C
 1000 CONTINUE
C
C     END OF FTREXC
C
      END
