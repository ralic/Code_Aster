      SUBROUTINE BLGEMV (TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y,
     &   INCY)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 20/09/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C SUBROUTINE BLAS 2 EFFECTUANT UNE OPERATION MATRICE-VECTEUR
C PARTICULIERE. ELLE REMPLACE LA BLAS SGEMV POUR LES MACHINES OU ELLE
C N'EST PAS DISPONIBLE DANS LES LIBRAIRIES SYSTEME
C---------------------------------------------------------------------
C***BEGIN PROLOGUE  SGEMV
C***PURPOSE  MULTIPLY A REAL VECTOR BY A REAL GENERAL MATRIX.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1B4
C***TYPE      SINGLE PRECISION (SGEMV-S, BLGEMV-D, CGEMV-C)
C***KEYWORDS  LEVEL 2 BLAS, LINEAR ALGEBRA
C***AUTHOR  DONGARRA, J. J., (ANL)
C           DU CROZ, J., (NAG)
C           HAMMARLING, S., (NAG)
C           HANSON, R. J., (SNLA)
C***DESCRIPTION
C
C  SGEMV  PERFORMS ONE OF THE MATRIX-VECTOR OPERATIONS
C
C     Y := ALPHA*A*X + BETA*Y,   OR   Y := ALPHA*A'*X + BETA*Y,
C
C  WHERE ALPHA AND BETA ARE SCALARS, X AND Y ARE VECTORS AND A IS AN
C  M BY N MATRIX.
C
C  PARAMETERS
C  ==========
C
C  TRANS  - CHARACTER*1.
C           ON ENTRY, TRANS SPECIFIES THE OPERATION TO BE PERFORMED AS
C           FOLLOWS:
C
C              TRANS = 'N' OR 'N'   Y := ALPHA*A*X + BETA*Y.
C
C              TRANS = 'T' OR 'T'   Y := ALPHA*A'*X + BETA*Y.
C
C              TRANS = 'C' OR 'C'   Y := ALPHA*A'*X + BETA*Y.
C
C           UNCHANGED ON EXIT.
C
C  M      - INTEGER.
C           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
C           M MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
C           N MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  ALPHA  - REAL            .
C           ON ENTRY, ALPHA SPECIFIES THE SCALAR ALPHA.
C           UNCHANGED ON EXIT.
C
C  A      - REAL             ARRAY OF DIMENSION ( LDA, N ).
C           BEFORE ENTRY, THE LEADING M BY N PART OF THE ARRAY A MUST
C           CONTAIN THE MATRIX OF COEFFICIENTS.
C           UNCHANGED ON EXIT.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM. LDA MUST BE AT LEAST
C           MAX( 1, M ).
C           UNCHANGED ON EXIT.
C
C  X      - REAL             ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( N - 1 )*ABS( INCX ) ) WHEN TRANS = 'N' OR 'N'
C           AND AT LEAST
C           ( 1 + ( M - 1 )*ABS( INCX ) ) OTHERWISE.
C           BEFORE ENTRY, THE INCREMENTED ARRAY X MUST CONTAIN THE
C           VECTOR X.
C           UNCHANGED ON EXIT.
C
C  INCX   - INTEGER.
C           ON ENTRY, INCX SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           X. INCX MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C  BETA   - REAL            .
C           ON ENTRY, BETA SPECIFIES THE SCALAR BETA. WHEN BETA IS
C           SUPPLIED AS ZERO THEN Y NEED NOT BE SET ON INPUT.
C           UNCHANGED ON EXIT.
C
C  Y      - REAL             ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( M - 1 )*ABS( INCY ) ) WHEN TRANS = 'N' OR 'N'
C           AND AT LEAST
C           ( 1 + ( N - 1 )*ABS( INCY ) ) OTHERWISE.
C           BEFORE ENTRY WITH BETA NON-ZERO, THE INCREMENTED ARRAY Y
C           MUST CONTAIN THE VECTOR Y. ON EXIT, Y IS OVERWRITTEN BY THE
C           UPDATED VECTOR Y.
C
C  INCY   - INTEGER.
C           ON ENTRY, INCY SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           Y. INCY MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C***REFERENCES  DONGARRA, J. J., DU CROZ, J., HAMMARLING, S., AND
C                 HANSON, R. J.  AN EXTENDED SET OF FORTRAN BASIC LINEAR
C                 ALGEBRA SUBPROGRAMS.  ACM TOMS, VOL. 14, NO. 1,
C                 PP. 1-17, MARCH 1988.
C***REVISION HISTORY  (YYMMDD)
C   861022  DATE WRITTEN
C   910605  MODIFIED TO MEET SLATEC PROLOGUE STANDARDS.  ONLY COMMENT
C           LINES WERE MODIFIED.  (BKS)
C   970701  TYPE DES ARGUMENTS, SUPPRESSION DES ORDRES RETURN (JPL)
C   000121  CORRECTION DU TEST IF (TRANS.NE.'N') THEN LENX=N ... (OB)
C           TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
C           IMPLICIT NONE.
C INTRINSIC FUNCTIONS
C   NONE.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
C     .. SCALAR ARGUMENTS ..
      REAL*8             ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
C     .. ARRAY ARGUMENTS ..
      REAL*8             A( LDA, 1 ), X( 1 ), Y( 1 )
C     .. PARAMETERS ..
      REAL*8             ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     .. LOCAL SCALARS ..
      REAL*8             TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
C***FIRST EXECUTABLE STATEMENT  SGEMV
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      IF     ( (TRANS.NE.'N').AND.
     &         (TRANS.NE.'T').AND.
     &         (TRANS.NE.'C')      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         GOTO 9999
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     &    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     &   GOTO 9999
C
C     SET  LENX  AND  LENY, THE LENGTHS OF THE VECTORS X AND Y, AND SET
C     UP THE START POINTS IN  X  AND  Y.
C
C OB DEBUT
      IF(TRANS.EQ.'N')THEN
C OB FIN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
C
C     START THE OPERATIONS. IN THIS VERSION THE ELEMENTS OF A ARE
C     ACCESSED SEQUENTIALLY WITH ONE PASS THROUGH A.
C
C     FIRST FORM  Y := BETA*Y.
C
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     &   GOTO 9999
      IF( (TRANS.EQ.'N') )THEN
C
C        FORM  Y := ALPHA*A*X + Y.
C
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
C
C        FORM  Y := ALPHA*A'*X + Y.
C
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
C
9999  CONTINUE
C
C     END OF SGEMV .
C
      END
