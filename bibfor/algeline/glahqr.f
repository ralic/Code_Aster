      SUBROUTINE GLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILOZ,
     $                   IHIZ, Z, LDZ, INFO )
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
C TOLE CRP_20
C
C     SUBROUTINE ARPACK CALCULANT LES VALEURS PROPRES ET LA DECOMPOSI
C     TION DE SCHUR D'UNE MATRICE DE HESSENBERG.
C-----------------------------------------------------------------------
C
C  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     SEPTEMBER 30, 1994
C
C
C  PURPOSE
C  =======
C
C  GLAHQR IS AN AUXILIARY ROUTINE CALLED BY CHSEQR TO UPDATE THE
C  EIGENVALUES AND SCHUR DECOMPOSITION ALREADY COMPUTED BY CHSEQR, BY
C  DEALING WITH THE HESSENBERG SUBMATRIX IN ROWS AND COLUMNS ILO TO IHI.
C
C  ARGUMENTS
C  =========
C
C  WANTT   (INPUT) LOGICAL
C          = .TRUE. : THE FULL SCHUR FORM T IS REQUIRED;
C          = .FALSE.: ONLY EIGENVALUES ARE REQUIRED.
C
C  WANTZ   (INPUT) LOGICAL
C          = .TRUE. : THE MATRIX OF SCHUR VECTORS Z IS REQUIRED;
C          = .FALSE.: SCHUR VECTORS ARE NOT REQUIRED.
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX H.  N >= 0.
C
C  ILO     (INPUT) INTEGER
C  IHI     (INPUT) INTEGER
C          IT IS ASSUMED THAT H IS ALREADY UPPER TRIANGULAR IN ROWS AND
C          COLUMNS IHI+1:N, AND THAT H(ILO,ILO-1) = 0 (UNLESS ILO = 1).

C          GLAHQR WORKS PRIMARILY WITH THE HESSENBERG SUBMATRIX IN ROWS
C          AND COLUMNS ILO TO IHI, BUT APPLIES TRANSFORMATIONS TO ALL OF
C          H IF WANTT IS .TRUE..
C          1 <= ILO <= MAX(1,IHI); IHI <= N.
C
C  H       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDH,N)
C          ON ENTRY, THE UPPER HESSENBERG MATRIX H.
C          ON EXIT, IF WANTT IS .TRUE., H IS UPPER TRIANGULAR IN ROWS
C          AND COLUMNS ILO:IHI, WITH ANY 2-BY-2 DIAGONAL BLOCKS IN
C          STANDARD FORM. IF WANTT IS .FALSE., THE CONTENTS OF H ARE
C          UNSPECIFIED ON EXIT.
C
C  LDH     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY H. LDH >= MAX(1,N).
C
C  W       (OUTPUT) COMPLEX*16 ARRAY, DIMENSION (N)
C          THE COMPUTED EIGENVALUES ILO TO IHI ARE STORED IN THE
C          CORRESPONDING ELEMENTS OF W. IF WANTT IS .TRUE., THE
C          EIGENVALUES ARE STORED IN THE SAME ORDER AS ON THE DIAGONAL
C          OF THE SCHUR FORM RETURNED IN H, WITH W(I) = H(I,I).
C
C  ILOZ    (INPUT) INTEGER
C  IHIZ    (INPUT) INTEGER
C          SPECIFY THE ROWS OF Z TO WHICH TRANSFORMATIONS MUST BE
C          APPLIED IF WANTZ IS .TRUE..
C          1 <= ILOZ <= ILO; IHI <= IHIZ <= N.
C
C  Z       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDZ,N)
C          IF WANTZ IS .TRUE., ON ENTRY Z MUST CONTAIN THE CURRENT
C          MATRIX Z OF TRANSFORMATIONS ACCUMULATED BY CHSEQR, AND ON
C          EXIT Z HAS BEEN UPDATED; TRANSFORMATIONS ARE APPLIED ONLY TO
C          THE SUBMATRIX Z(ILOZ:IHIZ,ILO:IHI).
C          IF WANTZ IS .FALSE., Z IS NOT REFERENCED.
C
C  LDZ     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY Z. LDZ >= MAX(1,N).
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          > 0: IF INFO = I, GLAHQR FAILED TO COMPUTE ALL THE
C               EIGENVALUES ILO TO IHI IN A TOTAL OF 30*(IHI-ILO+1)
C               ITERATIONS; ELEMENTS I+1:IHI OF W CONTAIN THOSE
C               EIGENVALUES WHICH HAVE BEEN SUCCESSFULLY COMPUTED.
C
C  =====================================================================
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     .. SCALAR ARGUMENTS ..
      LOGICAL            WANTT, WANTZ
      INTEGER            IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, N
C     ..
C     .. ARRAY ARGUMENTS ..
      COMPLEX*16         H( LDH, * ), W( * ), Z( LDZ, * )
C     ..
C     .. PARAMETERS ..
      COMPLEX*16         ZERO, ONE
      PARAMETER       (ZERO=(0.0D+0,0.0D+0),ONE=(1.0D+0,0.0D+0))
      REAL*8   RZERO, HALF
      PARAMETER          ( RZERO = 0.0D+0, HALF = 0.5D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, I1, I2, ITN, ITS, J, K, L, M, NH, NZ
      REAL*8             H10, H21, RTEMP, S, SMLNUM, T2, TST1, ULP,UNFL
      COMPLEX*16         CDUM, H11, H11S, H22, SUM, T, T1, TEMP, U, V2,
     $                   X, Y
C     ..
C     .. LOCAL ARRAYS ..
      REAL*8   RWORK
      COMPLEX*16         V( 2 )
C     ..
C     .. EXTERNAL FUNCTIONS ..
      REAL*8   GLANHS, DLAMCH, R8PREM, R8MIEM
      COMPLEX*16         GLADIV
      INTEGER ISBAEM
C     ..
C     ..
C     .. STATEMENT FUNCTIONS ..
      REAL*8   CABS1
C     ..
C     .. STATEMENT FUNCTION DEFINITIONS ..
      CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
      INFO = 0
      RWORK=0.D0
C
C     QUICK RETURN IF POSSIBLE
C
      IF( N.EQ.0 )
     &   GOTO 1000
      IF( ILO.EQ.IHI ) THEN
         W( ILO ) = H( ILO, ILO )
         GOTO 1000
      END IF
C
      NH = IHI - ILO + 1
      NZ = IHIZ - ILOZ + 1
C
C     SET MACHINE-DEPENDENT CONSTANTS FOR THE STOPPING CRITERION.
C     IF NORM(H) <= SQRT(OVFL), OVERFLOW SHOULD NOT OCCUR.
C
      UNFL = R8MIEM()
C DUE TO CRS512      OVFL = ONE / UNFL
      ULP = R8PREM() * 0.5D0 * ISBAEM()
      SMLNUM = UNFL*( NH / ULP )
C
C     I1 AND I2 ARE THE INDICES OF THE FIRST ROW AND LAST COLUMN OF H
C     TO WHICH TRANSFORMATIONS MUST BE APPLIED. IF EIGENVALUES ONLY ARE
C     BEING COMPUTED, I1 AND I2 ARE SET INSIDE THE MAIN LOOP.
C
      IF( WANTT ) THEN
         I1 = 1
         I2 = N
      END IF
C
C     ITN IS THE TOTAL NUMBER OF QR ITERATIONS ALLOWED.
C
      ITN = 30*NH
C
C     THE MAIN LOOP BEGINS HERE. I IS THE LOOP INDEX AND DECREASES FROM
C     IHI TO ILO IN STEPS OF 1. EACH ITERATION OF THE LOOP WORKS
C     WITH THE ACTIVE SUBMATRIX IN ROWS AND COLUMNS L TO I.
C     EIGENVALUES I+1 TO IHI HAVE ALREADY CONVERGED. EITHER L = ILO, OR
C     H(L,L-1) IS NEGLIGIBLE SO THAT THE MATRIX SPLITS.
C
      I = IHI
   10 CONTINUE
      IF( I.LT.ILO )
     &   GO TO 130
C
C     PERFORM QR ITERATIONS ON ROWS AND COLUMNS ILO TO I UNTIL A
C     SUBMATRIX OF ORDER 1 SPLITS OFF AT THE BOTTOM BECAUSE A
C     SUBDIAGONAL ELEMENT HAS BECOME NEGLIGIBLE.
C
      L = ILO
      DO 110 ITS = 0, ITN
C
C        LOOK FOR A SINGLE SMALL SUBDIAGONAL ELEMENT.
C
         DO 20 K = I, L + 1, -1
            TST1 = CABS1( H( K-1, K-1 ) ) + CABS1( H( K, K ) )
            IF( TST1.EQ.RZERO )
     $         TST1 = GLANHS( '1', I-L+1, H( L, L ), LDH, RWORK )
            IF( ABS( DBLE( H( K, K-1 ) ) ).LE.MAX( ULP*TST1, SMLNUM ) )
     $         GO TO 30
   20    CONTINUE
   30    CONTINUE
         L = K
         IF( L.GT.ILO ) THEN
C
C           H(L,L-1) IS NEGLIGIBLE
C
            H( L, L-1 ) = ZERO
         END IF
C
C        EXIT FROM LOOP IF A SUBMATRIX OF ORDER 1 HAS SPLIT OFF.
C
         IF( L.GE.I )
     $      GO TO 120
C
C        NOW THE ACTIVE SUBMATRIX IS IN ROWS AND COLUMNS L TO I. IF
C        EIGENVALUES ONLY ARE BEING COMPUTED, ONLY THE ACTIVE SUBMATRIX
C        NEED BE TRANSFORMED.
C
         IF( .NOT.WANTT ) THEN
            I1 = L
            I2 = I
         END IF
C
         IF( ITS.EQ.10 .OR. ITS.EQ.20 ) THEN
C
C           EXCEPTIONAL SHIFT.
C
            T = ABS( DBLE( H( I, I-1 ) ) ) +
     $          ABS( DBLE( H( I-1, I-2 ) ) )
         ELSE
C
C           WILKINSON'S SHIFT.
C
            T = H( I, I )
            U = H( I-1, I )*DBLE( H( I, I-1 ) )
            IF( U.NE.ZERO ) THEN
               X = HALF*( H( I-1, I-1 )-T )
               Y = SQRT( X*X+U )
               IF( DBLE( X )*DBLE( Y )+DIMAG( X )*DIMAG( Y ).LT.RZERO )
     $            Y = -Y
               T = T - GLADIV( U, ( X+Y ) )
            END IF
         END IF
C
C        LOOK FOR TWO CONSECUTIVE SMALL SUBDIAGONAL ELEMENTS.
C
         DO 40 M = I - 1, L + 1, -1
C
C           DETERMINE THE EFFECT OF STARTING THE SINGLE-SHIFT QR
C           ITERATION AT ROW M, AND SEE IF THIS WOULD MAKE H(M,M-1)
C           NEGLIGIBLE.
C
            H11 = H( M, M )
            H22 = H( M+1, M+1 )
            H11S = H11 - T
            H21 = H( M+1, M )
            S = CABS1( H11S ) + ABS( H21 )
            H11S = H11S / S
            H21 = H21 / S
            V( 1 ) = H11S
            V( 2 ) = H21
            H10 = H( M, M-1 )
            TST1 = CABS1( H11S )*( CABS1( H11 )+CABS1( H22 ) )
            IF( ABS( H10*H21 ).LE.ULP*TST1 )
     &         GO TO 50
   40    CONTINUE
         H11 = H( L, L )
         H22 = H( L+1, L+1 )
         H11S = H11 - T
         H21 = H( L+1, L )
         S = CABS1( H11S ) + ABS( H21 )
         H11S = H11S / S
         H21 = H21 / S
         V( 1 ) = H11S
         V( 2 ) = H21
   50    CONTINUE
C
C        SINGLE-SHIFT QR STEP
C
         DO 100 K = M, I - 1
C
C           THE FIRST ITERATION OF THIS LOOP DETERMINES A REFLECTION G
C           FROM THE VECTOR V AND APPLIES IT FROM LEFT AND RIGHT TO H,
C           THUS CREATING A NONZERO BULGE BELOW THE SUBDIAGONAL.
C
C           EACH SUBSEQUENT ITERATION DETERMINES A REFLECTION G TO
C           RESTORE THE HESSENBERG FORM IN THE (K-1)TH COLUMN, AND THUS
C           CHASES THE BULGE ONE STEP TOWARD THE BOTTOM OF THE ACTIVE
C           SUBMATRIX.
C
C           V(2) IS ALWAYS REAL BEFORE THE CALL TO GLARFG, AND HENCE
C           AFTER THE CALL T2 ( = T1*V(2) ) IS ALSO REAL.
C
            IF( K.GT.M )
     &         CALL GLCOPY( 2, H( K, K-1 ), 1, V, 1 )
            CALL GLARFG( 2, V( 1 ), V( 2 ), 1, T1 )
            IF( K.GT.M ) THEN
               H( K, K-1 ) = V( 1 )
               H( K+1, K-1 ) = ZERO
            END IF
            V2 = V( 2 )
            T2 = DBLE( T1*V2 )
C
C           APPLY G FROM THE LEFT TO TRANSFORM THE ROWS OF THE MATRIX
C           IN COLUMNS K TO I2.
C
            DO 60 J = K, I2
               SUM = DCONJG( T1 )*H( K, J ) + T2*H( K+1, J )
               H( K, J ) = H( K, J ) - SUM
               H( K+1, J ) = H( K+1, J ) - SUM*V2
   60       CONTINUE
C
C           APPLY G FROM THE RIGHT TO TRANSFORM THE COLUMNS OF THE
C           MATRIX IN ROWS I1 TO MIN(K+2,I).
C
            DO 70 J = I1, MIN( K+2, I )
               SUM = T1*H( J, K ) + T2*H( J, K+1 )
               H( J, K ) = H( J, K ) - SUM
               H( J, K+1 ) = H( J, K+1 ) - SUM*DCONJG( V2 )
   70       CONTINUE
C
            IF( WANTZ ) THEN
C
C              ACCUMULATE TRANSFORMATIONS IN THE MATRIX Z
C
               DO 80 J = ILOZ, IHIZ
                  SUM = T1*Z( J, K ) + T2*Z( J, K+1 )
                  Z( J, K ) = Z( J, K ) - SUM
                  Z( J, K+1 ) = Z( J, K+1 ) - SUM*DCONJG( V2 )
   80          CONTINUE
            END IF
C
            IF( K.EQ.M .AND. M.GT.L ) THEN
C
C              IF THE QR STEP WAS STARTED AT ROW M > L BECAUSE TWO
C              CONSECUTIVE SMALL SUBDIAGONALS WERE FOUND, THEN EXTRA
C              SCALING MUST BE PERFORMED TO ENSURE THAT H(M,M-1) REMAINS
C              REAL.
C
               TEMP = ONE - T1
               TEMP = TEMP / ABS( TEMP )
               H( M+1, M ) = H( M+1, M )*DCONJG( TEMP )
               IF( M+2.LE.I )
     &            H( M+2, M+1 ) = H( M+2, M+1 )*TEMP
               DO 90 J = M, I
                  IF( J.NE.M+1 ) THEN
                     IF( I2.GT.J )
     &                  CALL ZLSCAL( I2-J, TEMP, H( J, J+1 ), LDH )
                     CALL ZLSCAL( J-I1, DCONJG( TEMP ), H( I1, J ), 1 )
                     IF( WANTZ ) THEN
                        CALL ZLSCAL( NZ, DCONJG( TEMP ),
     &                              Z( ILOZ, J ), 1 )
                     END IF
                  END IF
   90          CONTINUE
            END IF
  100    CONTINUE
C
C        ENSURE THAT H(I,I-1) IS REAL.
C
         TEMP = H( I, I-1 )
         IF( DIMAG( TEMP ).NE.RZERO ) THEN
            RTEMP = ABS( TEMP )
            H( I, I-1 ) = RTEMP
            TEMP = TEMP / RTEMP
            IF( I2.GT.I )
     &         CALL ZLSCAL( I2-I, DCONJG( TEMP ), H( I, I+1 ), LDH )
            CALL ZLSCAL( I-I1, TEMP, H( I1, I ), 1 )
            IF( WANTZ ) THEN
               CALL ZLSCAL( NZ, TEMP, Z( ILOZ, I ), 1 )
            END IF
         END IF
C
  110 CONTINUE
C
C     FAILURE TO CONVERGE IN REMAINING NUMBER OF ITERATIONS
C
      INFO = I
      GOTO 1000
C
  120 CONTINUE
C
C     H(I,I-1) IS NEGLIGIBLE: ONE EIGENVALUE HAS CONVERGED.
C
      W( I ) = H( I, I )
C
C     DECREMENT NUMBER OF REMAINING ITERATIONS, AND RETURN TO START OF
C     THE MAIN LOOP WITH NEW VALUE OF I.
C
      ITN = ITN - ITS
      I = L - 1
      GO TO 10
C
  130 CONTINUE
 1000 CONTINUE
C     END OF GLAHQR
C
      END
