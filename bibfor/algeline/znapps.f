      SUBROUTINE ZNAPPS
     &   ( N, KEV, NP, SHIFT, V, LDV, H, LDH, RESID, Q, LDQ, 
     &     WORKL, WORKD )
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
C     SUBROUTINE ARPACK PREPARANT LE RESTART VIA UN QR IMPLICITE POUR
C     ELIMINER LES NP MODES PROPRES INDESIRABLES.
C-----------------------------------------------------------------------
C\BEGINDOC
C
C\NAME: ZNAPPS
C
C\DESCRIPTION:
C  GIVEN THE ARNOLDI FACTORIZATION
C
C     A*V_{K} - V_{K}*H_{K} = R_{K+P}*E_{K+P}^T,
C
C  APPLY NP IMPLICIT SHIFTS RESULTING IN
C
C     A*(V_{K}*Q) - (V_{K}*Q)*(Q^T* H_{K}*Q) = R_{K+P}*E_{K+P}^T * Q
C
C  WHERE Q IS AN ORTHOGONAL MATRIX WHICH IS THE PRODUCT OF ROTATIONS
C  AND REFLECTIONS RESULTING FROM THE NP BULGE CHANGE SWEEPS.
C  THE UPDATED ARNOLDI FACTORIZATION BECOMES:
C
C     A*VNEW_{K} - VNEW_{K}*HNEW_{K} = RNEW_{K}*E_{K}^T.
C
C\USAGE:
C  CALL ZNAPPS
C     ( N, KEV, NP, SHIFT, V, LDV, H, LDH, RESID, Q, LDQ, 
C       WORKL, WORKD )
C
C\ARGUMENTS
C  N       INTEGER.  (INPUT)
C          PROBLEM SIZE, I.E. SIZE OF MATRIX A.
C
C  KEV     INTEGER.  (INPUT/OUTPUT)
C          KEV+NP IS THE SIZE OF THE INPUT MATRIX H.
C          KEV IS THE SIZE OF THE UPDATED MATRIX HNEW. 
C
C  NP      INTEGER.  (INPUT)
C          NUMBER OF IMPLICIT SHIFTS TO BE APPLIED.
C
C  SHIFT   COMPLEX*16 ARRAY OF LENGTH NP.  (INPUT)
C          THE SHIFTS TO BE APPLIED.
C
C  V       COMPLEX*16 N BY (KEV+NP) ARRAY.  (INPUT/OUTPUT)
C          ON INPUT, V CONTAINS THE CURRENT KEV+NP ARNOLDI VECTORS.
C          ON OUTPUT, V CONTAINS THE UPDATED KEV ARNOLDI VECTORS
C          IN THE FIRST KEV COLUMNS OF V.
C
C  LDV     INTEGER.  (INPUT)
C          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  H       COMPLEX*16 (KEV+NP) BY (KEV+NP) ARRAY.  (INPUT/OUTPUT)
C          ON INPUT, H CONTAINS THE CURRENT KEV+NP BY KEV+NP UPPER 
C          HESSENBERG MATRIX OF THE ARNOLDI FACTORIZATION.
C          ON OUTPUT, H CONTAINS THE UPDATED KEV BY KEV UPPER HESSENBERG
C          MATRIX IN THE KEV LEADING SUBMATRIX.
C
C  LDH     INTEGER.  (INPUT)
C          LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  RESID   COMPLEX*16 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
C          ON INPUT, RESID CONTAINS THE THE RESIDUAL VECTOR R_{K+P}.
C          ON OUTPUT, RESID IS THE UPDATE RESIDUAL VECTOR RNEW_{K} 
C          IN THE FIRST KEV LOCATIONS.
C
C  Q       COMPLEX*16 KEV+NP BY KEV+NP WORK ARRAY.  (WORKSPACE)
C          WORK ARRAY USED TO ACCUMULATE THE ROTATIONS AND REFLECTIONS
C          DURING THE BULGE CHASE SWEEP.
C
C  LDQ     INTEGER.  (INPUT)
C          LEADING DIMENSION OF Q EXACTLY AS DECLARED IN THE CALLING
C          PROGRAM.
C
C  WORKL   COMPLEX*16 WORK ARRAY OF LENGTH (KEV+NP).  (WORKSPACE)
C          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
C          THE FRONT END.
C
C  WORKD   COMPLEX*16 WORK ARRAY OF LENGTH 2*N.  (WORKSPACE)
C          DISTRIBUTED ARRAY USED IN THE APPLICATION OF THE ACCUMULATED
C          ORTHOGONAL MATRIX Q.
C
C\ENDDOC
C
C-----------------------------------------------------------------------
C
C\BEGINLIB
C
C\LOCAL VARIABLES:
C     XXXXXX  COMPLEX*16
C
C\REFERENCES:
C  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
C     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
C     PP 357-385.
C
C\ROUTINES CALLED:
C     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
C     SECOND  ARPACK UTILITY ROUTINE FOR TIMING.
C     ZMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
C     ZVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
C     GLACPY  LAPACK MATRIX COPY ROUTINE.
C     HLANHS  LAPACK ROUTINE THAT COMPUTES VARIOUS NORMS OF A MATRIX.
C     GLARTG  LAPACK GIVENS ROTATION CONSTRUCTION ROUTINE.
C     GLASET  LAPACK MATRIX INITIALIZATION ROUTINE.
C     DLABAD  LAPACK ROUTINE FOR DEFINING THE UNDERFLOW AND OVERFLOW
C             LIMITS.
C     DLAMCH  LAPACK ROUTINE THAT DETERMINES MACHINE CONSTANTS.
C     FLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
C     ZGEMV   LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
C     ZAXPY   LEVEL 1 BLAS THAT COMPUTES A VECTOR TRIAD.
C     ZCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER.
C     ZLSCAL   LEVEL 1 BLAS THAT SCALES A VECTOR.
C
C\AUTHOR
C     DANNY SORENSEN               PHUONG VU
C     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
C     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
C     APPLIED MATHEMATICS 
C     RICE UNIVERSITY           
C     HOUSTON, TEXAS 
C
C\SCCS INFORMATION: @(#)
C FILE: NAPPS.F   SID: 2.3   DATE OF SID: 3/28/97   RELEASE: 2
C
C\REMARKS
C  1. IN THIS VERSION, EACH SHIFT IS APPLIED TO ALL THE SUBLOCKS OF
C     THE HESSENBERG MATRIX H AND NOT JUST TO THE SUBMATRIX THAT IT
C     COMES FROM. DEFLATION AS IN LAPACK ROUTINE ZLAHQR (QR ALGORITHM
C     FOR UPPER HESSENBERG MATRICES ) IS USED.
C     UPON OUTPUT, THE SUBDIAGONALS OF H ARE ENFORCED TO BE NON-NEGATIVE
C     REAL NUMBERS.
C
C\ENDLIB
C
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C     %-----------------------------%
C     | INCLUDE FILES FOR DEBUGGING |
C     %-----------------------------%

      INTEGER LOGFIL, NDIGIT, MGETV0,
     &  MNAUPD, MNAUP2, MNAITR, MNEIGH, MNAPPS, MNGETS, MNEUPD
      COMMON /DEBUG/
     &  LOGFIL, NDIGIT, MGETV0,
     &  MNAUPD, MNAUP2, MNAITR, MNEIGH, MNAPPS, MNGETS, MNEUPD

C     %------------------%
C     | SCALAR ARGUMENTS |
C     %------------------%
C
      INTEGER    KEV, LDH, LDQ, LDV, N, NP
C
C     %-----------------%
C     | ARRAY ARGUMENTS |
C     %-----------------%
C
      COMPLEX*16 H(LDH,KEV+NP), RESID(N), SHIFT(NP), 
     &           V(LDV,KEV+NP), Q(LDQ,KEV+NP), WORKD(2*N), WORKL(KEV+NP)
C
C     %------------%
C     | PARAMETERS |
C     %------------%
C
      COMPLEX*16 ONE, ZERO
      REAL*8     RZERO
      PARAMETER (ONE = (1.0D+0, 0.0D+0), ZERO = (0.0D+0, 0.0D+0),
     &           RZERO = 0.0D+0)
C
C     %------------------------%
C     | LOCAL SCALARS & ARRAYS |
C     %------------------------%
C
      INTEGER    I, IEND, ISTART, J, JJ, KPLUSP, MSGLVL
      LOGICAL    FIRST
      COMPLEX*16 CDUM, F, G, H11, H21, R, S, SIGMA, T
      REAL*8     C,  OVFL, SMLNUM, ULP, UNFL, TST1
      SAVE       FIRST, OVFL, SMLNUM, ULP, UNFL 
C
C
C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%
C
      INTEGER ISBAEM
      REAL*8  HLANHS, FLAPY2, R8PREM, R8MIEM
C
C     %---------------------%
C     | STATEMENT FUNCTIONS |
C     %---------------------%
C
      REAL*8  ZABS1
      ZABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
C
C     %----------------%
C     | DATA STATMENTS |
C     %----------------%
C
      DATA       FIRST / .TRUE. /
C
C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%
C
      IF (FIRST) THEN
C
C        %-----------------------------------------------%
C        | SET MACHINE-DEPENDENT CONSTANTS FOR THE       |
C        | STOPPING CRITERION. IF NORM(H) <= SQRT(OVFL), |
C        | OVERFLOW SHOULD NOT OCCUR.                    |
C        | REFERENCE: LAPACK SUBROUTINE ZLAHQR           |
C        %-----------------------------------------------%
C

         UNFL = R8MIEM()
C DUE RO CRS512         OVFL = ONE / UNFL
         ULP = R8PREM() *0.5D0 * ISBAEM()
         SMLNUM = UNFL*( N / ULP )
         FIRST = .FALSE.
      END IF
C
C     %-------------------------------%
C     | INITIALIZE TIMING STATISTICS  |
C     | & MESSAGE LEVEL FOR DEBUGGING |
C     %-------------------------------%
C
      MSGLVL = MNAPPS
C 
      KPLUSP = KEV + NP 
C 
C     %--------------------------------------------%
C     | INITIALIZE Q TO THE IDENTITY TO ACCUMULATE |
C     | THE ROTATIONS AND REFLECTIONS              |
C     %--------------------------------------------%
C
      CALL GLASET ('A', KPLUSP, KPLUSP, ZERO, ONE, Q, LDQ)
C
C     %----------------------------------------------%
C     | QUICK RETURN IF THERE ARE NO SHIFTS TO APPLY |
C     %----------------------------------------------%
C
      IF (NP .EQ. 0) GO TO 9000
C
C     %----------------------------------------------%
C     | CHASE THE BULGE WITH THE APPLICATION OF EACH |
C     | IMPLICIT SHIFT. EACH SHIFT IS APPLIED TO THE |
C     | WHOLE MATRIX INCLUDING EACH BLOCK.           |
C     %----------------------------------------------%
C
      DO 110 JJ = 1, NP
         SIGMA = SHIFT(JJ)
C
         IF (MSGLVL .GT. 2 ) THEN
            CALL IVOUT (LOGFIL, 1, JJ, NDIGIT, 
     &               '_NAPPS: SHIFT NUMBER.')
            CALL ZVOUT (LOGFIL, 1, SIGMA, NDIGIT, 
     &               '_NAPPS: VALUE OF THE SHIFT ')
         END IF
C
         ISTART = 1
   20    CONTINUE
C
         DO 30 I = ISTART, KPLUSP-1
C
C           %----------------------------------------%
C           | CHECK FOR SPLITTING AND DEFLATION. USE |
C           | A STANDARD TEST AS IN THE QR ALGORITHM |
C           | REFERENCE: LAPACK SUBROUTINE ZLAHQR    |
C           %----------------------------------------%
C
            TST1 = ZABS1( H( I, I ) ) + ZABS1( H( I+1, I+1 ) )
            IF( TST1.EQ.RZERO )
     &         TST1 = HLANHS( '1', KPLUSP-JJ+1, H, LDH, WORKL )
            IF ( ABS(DBLE(H(I+1,I))) 
     &           .LE. MAX(ULP*TST1, SMLNUM) )  THEN
               IF (MSGLVL .GT. 0) THEN
                  CALL IVOUT (LOGFIL, 1, I, NDIGIT, 
     &                 '_NAPPS: MATRIX SPLITTING AT ROW/COLUMN NO.')
                  CALL IVOUT (LOGFIL, 1, JJ, NDIGIT, 
     &                 '_NAPPS: MATRIX SPLITTING WITH SHIFT NUMBER.')
                  CALL ZVOUT (LOGFIL, 1, H(I+1,I), NDIGIT, 
     &                 '_NAPPS: OFF DIAGONAL ELEMENT.')
               END IF
               IEND = I
               H(I+1,I) = ZERO
               GO TO 40
            END IF
   30    CONTINUE
         IEND = KPLUSP
   40    CONTINUE
C
         IF (MSGLVL .GT. 2) THEN
             CALL IVOUT (LOGFIL, 1, ISTART, NDIGIT, 
     &                   '_NAPPS: START OF CURRENT BLOCK ')
             CALL IVOUT (LOGFIL, 1, IEND, NDIGIT, 
     &                   '_NAPPS: END OF CURRENT BLOCK ')
         END IF
C
C        %------------------------------------------------%
C        | NO REASON TO APPLY A SHIFT TO BLOCK OF ORDER 1 |
C        | OR IF THE CURRENT BLOCK STARTS AFTER THE POINT |
C        | OF COMPRESSION SINCE WE'LL DISCARD THIS STUFF  |
C        %------------------------------------------------%
C
         IF ( ISTART .EQ. IEND .OR. ISTART .GT. KEV) GO TO 100
C
         H11 = H(ISTART,ISTART)
         H21 = H(ISTART+1,ISTART)
         F = H11 - SIGMA
         G = H21
C 
         DO 80 I = ISTART, IEND-1
C
C           %------------------------------------------------------%
C           | CONSTRUCT THE PLANE ROTATION G TO ZERO OUT THE BULGE |
C           %------------------------------------------------------%
C
            CALL GLARTG (F, G, C, S, R)
            IF (I .GT. ISTART) THEN
               H(I,I-1) = R
               H(I+1,I-1) = ZERO
            END IF
C
C           %---------------------------------------------%
C           | APPLY ROTATION TO THE LEFT OF H;  H <- G'*H |
C           %---------------------------------------------%
C
            DO 50 J = I, KPLUSP
               T        =  C*H(I,J) + S*H(I+1,J)
               H(I+1,J) = -DCONJG(S)*H(I,J) + C*H(I+1,J)
               H(I,J)   = T   
   50       CONTINUE
C
C           %---------------------------------------------%
C           | APPLY ROTATION TO THE RIGHT OF H;  H <- H*G |
C           %---------------------------------------------%
C
            DO 60 J = 1, MIN(I+2,IEND)
               T        =  C*H(J,I) + DCONJG(S)*H(J,I+1)
               H(J,I+1) = -S*H(J,I) + C*H(J,I+1)
               H(J,I)   = T   
   60       CONTINUE
C
C           %-----------------------------------------------------%
C           | ACCUMULATE THE ROTATION IN THE MATRIX Q;  Q <- Q*G' |
C           %-----------------------------------------------------%
C
            DO 70 J = 1, MIN(I+JJ, KPLUSP)
               T        =   C*Q(J,I) + DCONJG(S)*Q(J,I+1)
               Q(J,I+1) = - S*Q(J,I) + C*Q(J,I+1)
               Q(J,I)   = T   
   70       CONTINUE
C
C           %---------------------------%
C           | PREPARE FOR NEXT ROTATION |
C           %---------------------------%
C
            IF (I .LT. IEND-1) THEN
               F = H(I+1,I)
               G = H(I+2,I)
            END IF
   80    CONTINUE
C
C        %-------------------------------%
C        | FINISHED APPLYING THE SHIFT.  |
C        %-------------------------------%
C 
  100    CONTINUE
C
C        %---------------------------------------------------------%
C        | APPLY THE SAME SHIFT TO THE NEXT BLOCK IF THERE IS ANY. |
C        %---------------------------------------------------------%
C
         ISTART = IEND + 1
         IF (IEND .LT. KPLUSP) GO TO 20
C
C        %---------------------------------------------%
C        | LOOP BACK TO THE TOP TO GET THE NEXT SHIFT. |
C        %---------------------------------------------%
C
  110 CONTINUE
C
C     %---------------------------------------------------%
C     | PERFORM A SIMILARITY TRANSFORMATION THAT MAKES    |
C     | SURE THAT THE COMPRESSED H WILL HAVE NON-NEGATIVE |
C     | REAL SUBDIAGONAL ELEMENTS.                        |
C     %---------------------------------------------------%
C
      DO 120 J=1,KEV
         IF ( DBLE( H(J+1,J) ) .LT. RZERO .OR.
     &        DIMAG( H(J+1,J) ) .NE. RZERO ) THEN
            T = H(J+1,J) / FLAPY2(DBLE(H(J+1,J)),DIMAG(H(J+1,J)))
            CALL ZLSCAL( KPLUSP-J+1, DCONJG(T), H(J+1,J), LDH )
            CALL ZLSCAL( MIN(J+2, KPLUSP), T, H(1,J+1), 1 )
            CALL ZLSCAL( MIN(J+NP+1,KPLUSP), T, Q(1,J+1), 1 )
            H(J+1,J) = DCMPLX( DBLE( H(J+1,J) ), RZERO )
         END IF
  120 CONTINUE
C
      DO 130 I = 1, KEV
C
C        %--------------------------------------------%
C        | FINAL CHECK FOR SPLITTING AND DEFLATION.   |
C        | USE A STANDARD TEST AS IN THE QR ALGORITHM |
C        | REFERENCE: LAPACK SUBROUTINE ZLAHQR.       |
C        | NOTE: SINCE THE SUBDIAGONALS OF THE        |
C        | COMPRESSED H ARE NONNEGATIVE REAL NUMBERS, |
C        | WE TAKE ADVANTAGE OF THIS.                 |
C        %--------------------------------------------%
C
         TST1 = ZABS1( H( I, I ) ) + ZABS1( H( I+1, I+1 ) )
         IF( TST1 .EQ. RZERO )
     &       TST1 = HLANHS( '1', KEV, H, LDH, WORKL )
         IF( DBLE( H( I+1,I ) ) .LE. MAX( ULP*TST1, SMLNUM ) ) 
     &       H(I+1,I) = ZERO
 130  CONTINUE
C
C     %-------------------------------------------------%
C     | COMPUTE THE (KEV+1)-ST COLUMN OF (V*Q) AND      |
C     | TEMPORARILY STORE THE RESULT IN WORKD(N+1:2*N). |
C     | THIS IS NEEDED IN THE RESIDUAL UPDATE SINCE WE  |
C     | CANNOT GUARANTEE THAT THE CORRESPONDING ENTRY   |
C     | OF H WOULD BE ZERO AS IN EXACT ARITHMETIC.      |
C     %-------------------------------------------------%
C
      IF ( DBLE( H(KEV+1,KEV) ) .GT. RZERO )
     &   CALL ZGEMV ('N', N, KPLUSP, ONE, V, LDV, Q(1,KEV+1), 1, ZERO, 
     &                WORKD(N+1), 1)
C 
C     %----------------------------------------------------------%
C     | COMPUTE COLUMN 1 TO KEV OF (V*Q) IN BACKWARD ORDER       |
C     | TAKING ADVANTAGE OF THE UPPER HESSENBERG STRUCTURE OF Q. |
C     %----------------------------------------------------------%
C
      DO 140 I = 1, KEV
         CALL ZGEMV ('N', N, KPLUSP-I+1, ONE, V, LDV,
     &               Q(1,KEV-I+1), 1, ZERO, WORKD, 1)
         CALL ZCOPY (N, WORKD, 1, V(1,KPLUSP-I+1), 1)
  140 CONTINUE
C
C     %-------------------------------------------------%
C     |  MOVE V(:,KPLUSP-KEV+1:KPLUSP) INTO V(:,1:KEV). |
C     %-------------------------------------------------%
C
      CALL GLACPY ('A', N, KEV, V(1,KPLUSP-KEV+1), LDV, V, LDV)
C 
C     %--------------------------------------------------------------%
C     | COPY THE (KEV+1)-ST COLUMN OF (V*Q) IN THE APPROPRIATE PLACE |
C     %--------------------------------------------------------------%
C
      IF ( DBLE( H(KEV+1,KEV) ) .GT. RZERO )
     &   CALL ZCOPY (N, WORKD(N+1), 1, V(1,KEV+1), 1)
C 
C     %-------------------------------------%
C     | UPDATE THE RESIDUAL VECTOR:         |
C     |    R <- SIGMAK*R + BETAK*V(:,KEV+1) |
C     | WHERE                               |
C     |    SIGMAK = (E_{KEV+P}'*Q)*E_{KEV}  |
C     |    BETAK = E_{KEV+1}'*H*E_{KEV}     |
C     %-------------------------------------%
C
      CALL ZLSCAL (N, Q(KPLUSP,KEV), RESID, 1)
      IF ( DBLE( H(KEV+1,KEV) ) .GT. RZERO )
     &   CALL ZAXPY (N, H(KEV+1,KEV), V(1,KEV+1), 1, RESID, 1)
C
      IF (MSGLVL .GT. 1) THEN
         CALL ZVOUT (LOGFIL, 1, Q(KPLUSP,KEV), NDIGIT,
     &        '_NAPPS: SIGMAK = (E_(KEV+P)T*Q)*E_(KEV)')
         CALL ZVOUT (LOGFIL, 1, H(KEV+1,KEV), NDIGIT,
     &        '_NAPPS: BETAK = E_(KEV+1)T*H*E_(KEV)')
         CALL IVOUT (LOGFIL, 1, KEV, NDIGIT, 
     &               '_NAPPS: ORDER OF THE FINAL HESSENBERG MATRIX ')
         IF (MSGLVL .GT. 2) THEN
            CALL ZMOUT (LOGFIL, KEV, KEV, H, LDH, NDIGIT,
     &      '_NAPPS: UPDATED HESSENBERG MATRIX H FOR NEXT ITERATION')
         END IF
C
      END IF
C
 9000 CONTINUE
C 
C
C     %---------------%
C     | END OF ZNAPPS |
C     %---------------%
C
      END
