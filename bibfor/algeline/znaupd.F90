!---------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =====================================================================
!
!     SUBROUTINE ARPACK CALCULANT LES VALEURS PROPRES DE (OP), ELLE
!     APPELLE LA ROUTINE DE TRAVAIL ZNAUPD2.
!---------------------------------------------------------------------
!\BEGINDOC
!
!\NAME: ZNAUPD
!
!\DESCRIPTION:
!  REVERSE COMMUNICATION INTERFACE FOR THE IMPLICITLY RESTARTED ARNOLDI
!  ITERATION. THIS IS INTENDED TO BE USED TO FIND A FEW EIGENPAIRS OF A
!  COMPLEX LINEAR OPERATOR OP WITH RESPECT TO A SEMI-INNER PRODUCT
!  DEFINED BY A HERMITIAN POSITIVE SEMI-DEFINITE REAL MATRIX B. B MAY
!  BE THE IDENTITY MATRIX.  NOTE: IF BOTH OP AND B ARE REAL, THEN
!  DSAUPD  OR DNAUPD  SHOULD BE USED.
!
!
!  THE COMPUTED APPROXIMATE EIGENVALUES ARE CALLED RITZ VALUES AND
!  THE CORRESPONDING APPROXIMATE EIGENVECTORS ARE CALLED RITZ VECTORS.
!
!  ZNAUPD  IS USUALLY CALLED ITERATIVELY TO SOLVE ONE OF THE
!  FOLLOWING PROBLEMS:
!
!  MODE 1:  A*X = LAMBDA*X.
!           ===> OP = A  AND  B = I.
!
!  MODE 2:  A*X = LAMBDA*M*X, M HERMITIAN POSITIVE DEFINITE
!           ===> OP = INV[M]*A  AND  B = M.
!           ===> (IF M CAN BE FACTORED SEE REMARK 3 BELOW)
!
!  MODE 3:  A*X = LAMBDA*M*X, M HERMITIAN SEMI-DEFINITE
!           ===> OP =  INV[A - SIGMA*M]*M   AND  B = M.
!           ===> SHIFT-AND-INVERT MODE
!           IF OP*X = AMU*X, THEN LAMBDA = SIGMA + 1/AMU.
!
!
!  NOTE: THE ACTION OF W <- INV[A - SIGMA*M]*V OR W <- INV[M]*V
!        SHOULD BE ACCOMPLISHED EITHER BY A DIRECT METHOD
!        USING A SPARSE MATRIX FACTORIZATION AND SOLVING
!
!           [A - SIGMA*M]*W = V  OR M*W = V,
!
!        OR THROUGH AN ITERATIVE METHOD FOR SOLVING THESE
!        SYSTEMS.  IF AN ITERATIVE METHOD IS USED, THE
!        CONVERGENCE TEST MUST BE MORE STRINGENT THAN
!        THE ACCURACY REQUIREMENTS FOR THE EIGENVALUE
!        APPROXIMATIONS.
!
!\USAGE:
!  CALL ZNAUPD
!     ( IDO, BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM,
!       IPNTR, WORKD, WORKL, LWORKL, RWORK, INFO )
!
!\ARGUMENTS
!  IDO     INTEGER.  (INPUT/OUTPUT)
!          REVERSE COMMUNICATION FLAG.  IDO MUST BE ZERO ON THE FIRST
!          CALL TO ZNAUPD .  IDO WILL BE SET INTERNALLY TO
!          INDICATE THE TYPE OF OPERATION TO BE PERFORMED.  CONTROL IS
!          THEN GIVEN BACK TO THE CALLING ROUTINE WHICH HAS THE
!          RESPONSIBILITY TO CARRY OUT THE REQUESTED OPERATION AND CALL
!          ZNAUPD  WITH THE RESULT.  THE OPERAND IS GIVEN IN
!          WORKD(IPNTR(1)), THE RESULT MUST BE PUT IN WORKD(IPNTR(2)).
!          -------------------------------------------------------------
!          IDO =  0: FIRST CALL TO THE REVERSE COMMUNICATION INTERFACE
!          IDO = -1: COMPUTE  Y = OP * X  WHERE
!                    IPNTR(1) IS THE POINTER INTO WORKD FOR X,
!                    IPNTR(2) IS THE POINTER INTO WORKD FOR Y.
!                    THIS IS FOR THE INITIALIZATION PHASE TO FORCE THE
!                    STARTING VECTOR INTO THE RANGE OF OP.
!          IDO =  1: COMPUTE  Y = OP * X  WHERE
!                    IPNTR(1) IS THE POINTER INTO WORKD FOR X,
!                    IPNTR(2) IS THE POINTER INTO WORKD FOR Y.
!                    IN MODE 3, THE VECTOR B * X IS ALREADY
!                    AVAILABLE IN WORKD(IPNTR(3)).  IT DOES NOT
!                    NEED TO BE RECOMPUTED IN FORMING OP * X.
!          IDO =  2: COMPUTE  Y = M * X  WHERE
!                    IPNTR(1) IS THE POINTER INTO WORKD FOR X,
!                    IPNTR(2) IS THE POINTER INTO WORKD FOR Y.
!          IDO =  3: COMPUTE AND RETURN THE SHIFTS IN THE FIRST
!                    NP LOCATIONS OF WORKL.
!          IDO = 99: DONE
!          -------------------------------------------------------------
!          AFTER THE INITIALIZATION PHASE, WHEN THE ROUTINE IS USED IN
!          THE "SHIFT-AND-INVERT" MODE, THE VECTOR M * X IS ALREADY
!          AVAILABLE AND DOES NOT NEED TO BE RECOMPUTED IN FORMING OP*X.
!
!  BMAT    CHARACTER*1.  (INPUT)
!          BMAT SPECIFIES THE TYPE OF THE MATRIX B THAT DEFINES THE
!          SEMI-INNER PRODUCT FOR THE OPERATOR OP.
!          BMAT = 'I' -> STANDARD EIGENVALUE PROBLEM A*X = LAMBDA*X
!          BMAT = 'G' -> GENERALIZED EIGENVALUE PROBLEM A*X = LAMBDA*M*X
!
!  N       INTEGER.  (INPUT)
!          DIMENSION OF THE EIGENPROBLEM.
!
!  WHICH   CHARACTER*2.  (INPUT)
!          'LM' -> WANT THE NEV EIGENVALUES OF LARGEST MAGNITUDE.
!          'SM' -> WANT THE NEV EIGENVALUES OF SMALLEST MAGNITUDE.
!          'LR' -> WANT THE NEV EIGENVALUES OF LARGEST REAL PART.
!          'SR' -> WANT THE NEV EIGENVALUES OF SMALLEST REAL PART.
!          'LI' -> WANT THE NEV EIGENVALUES OF LARGEST IMAGINARY PART.
!          'SI' -> WANT THE NEV EIGENVALUES OF SMALLEST IMAGINARY PART.
!
!  NEV     INTEGER.  (INPUT)
!          NUMBER OF EIGENVALUES OF OP TO BE COMPUTED. 0 < NEV < N-1.
!
!  TOL     DOUBLE PRECISION   SCALAR.  (INPUT)
!          STOPPING CRITERIA: THE RELATIVE ACCURACY OF THE RITZ VALUE
!          IS CONSIDERED ACCEPTABLE IF BOUNDS(I) .LE. TOL*ABS(RITZ(I))
!          WHERE ABS(RITZ(I)) IS THE MAGNITUDE WHEN RITZ(I) IS COMPLEX.
!          DEFAULT = DLAMCH ('EPS')  (MACHINE PRECISION AS COMPUTED
!                    BY THE LAPACK AUXILIARY SUBROUTINE DLAMCH ).
!
!  RESID   COMPLEX*16  ARRAY OF LENGTH N.  (INPUT/OUTPUT)
!          ON INPUT:
!          IF INFO .EQ. 0, A RANDOM INITIAL RESIDUAL VECTOR IS USED.
!          IF INFO .NE. 0, RESID CONTAINS THE INITIAL RESIDUAL VECTOR,
!                          POSSIBLY FROM A PREVIOUS RUN.
!          ON OUTPUT:
!          RESID CONTAINS THE FINAL RESIDUAL VECTOR.
!
!  NCV     INTEGER.  (INPUT)
!          NUMBER OF COLUMNS OF THE MATRIX V. NCV MUST SATISFY THE TWO
!          INEQUALITIES 1 <= NCV-NEV AND NCV <= N.
!          THIS WILL INDICATE HOW MANY ARNOLDI VECTORS ARE GENERATED
!          AT EACH ITERATION.  AFTER THE STARTUP PHASE IN WHICH NEV
!          ARNOLDI VECTORS ARE GENERATED, THE ALGORITHM GENERATES
!          APPROXIMATELY NCV-NEV ARNOLDI VECTORS AT EACH SUBSEQUENT
!          UPDATE
!          ITERATION. MOST OF THE COST IN GENERATING EACH ARNOLDI
!          VECTOR IS
!          IN THE MATRIX-VECTOR OPERATION OP*X. (SEE REMARK 4 BELOW.)
!
!  V       COMPLEX*16  ARRAY N BY NCV.  (OUTPUT)
!          CONTAINS THE FINAL SET OF ARNOLDI BASIS VECTORS.
!
!  LDV     INTEGER.  (INPUT)
!          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  IPARAM  INTEGER ARRAY OF LENGTH 11.  (INPUT/OUTPUT)
!          IPARAM(1) = ISHIFT: METHOD FOR SELECTING THE IMPLICIT SHIFTS.
!          THE SHIFTS SELECTED AT EACH ITERATION ARE USED TO FILTER OUT
!          THE COMPONENTS OF THE UNWANTED EIGENVECTOR.
!          -------------------------------------------------------------
!          ISHIFT = 0: THE SHIFTS ARE TO BE PROVIDED BY THE USER VIA
!                      REVERSE COMMUNICATION.  THE NCV EIGENVALUES OF
!                      THE HESSENBERG MATRIX H ARE RETURNED IN THE PART
!                      OF WORKL ARRAY CORRESPONDING TO RITZ.
!          ISHIFT = 1: EXACT SHIFTS WITH RESPECT TO THE CURRENT
!                      HESSENBERG MATRIX H.  THIS IS EQUIVALENT TO
!                      RESTARTING THE ITERATION FROM THE BEGINNING
!                      AFTER UPDATING THE STARTING VECTOR WITH A LINEAR
!                      COMBINATION OF RITZ VECTORS ASSOCIATED WITH THE
!                      "WANTED" EIGENVALUES.
!          ISHIFT = 2: OTHER CHOICE OF INTERNAL SHIFT TO BE DEFINED.
!          -------------------------------------------------------------
!
!          IPARAM(2) = NO LONGER REFERENCED
!
!          IPARAM(3) = MXITER
!          ON INPUT:  MAXIMUM NUMBER OF ARNOLDI UPDATE ITERATIONS
!                     ALLOWED.
!          ON OUTPUT: ACTUAL NUMBER OF ARNOLDI UPDATE ITERATIONS TAKEN.
!
!          IPARAM(4) = NB: BLOCKSIZE TO BE USED IN THE RECURRENCE.
!          THE CODE CURRENTLY WORKS ONLY FOR NB = 1.
!
!          IPARAM(5) = NCONV: NUMBER OF "CONVERGED" RITZ VALUES.
!          THIS REPRESENTS THE NUMBER OF RITZ VALUES THAT SATISFY
!          THE CONVERGENCE CRITERION.
!
!          IPARAM(6) = IUPD (NOT USED)
!          NO LONGER REFERENCED. IMPLICIT RESTARTING IS ALWAYS USED.
!
!          IPARAM(7) = MODE
!          ON INPUT DETERMINES WHAT TYPE OF EIGENPROBLEM IS BEING
!          SOLVED.
!          MUST BE 1,2,3; SEE UNDER \DESCRIPTION OF ZNAUPD  FOR THE
!          FOUR MODES AVAILABLE.
!
!          IPARAM(8) = NP
!          WHEN IDO = 3 AND THE USER PROVIDES SHIFTS THROUGH REVERSE
!          COMMUNICATION (IPARAM(1)=0), _NAUPD RETURNS NP, THE NUMBER
!          OF SHIFTS THE USER IS TO PROVIDE. 0 < NP < NCV-NEV.
!
!          IPARAM(9) = NUMOP, IPARAM(10) = NUMOPB, IPARAM(11) = NUMREO,
!          OUTPUT: NUMOP  = TOTAL NUMBER OF OP*X OPERATIONS,
!                  NUMOPB = TOTAL NUMBER OF B*X OPERATIONS IF BMAT='G',
!                  NUMREO = TOTAL NUMBER OF STEPS OF
!                           RE-ORTHOGONALIZATION.
!
!  IPNTR   INTEGER ARRAY OF LENGTH 14.  (OUTPUT)
!          POINTER TO MARK THE STARTING LOCATIONS IN THE WORKD AND WORKL
!          ARRAYS FOR MATRICES/VECTORS USED BY THE ARNOLDI ITERATION.
!          -------------------------------------------------------------
!          IPNTR(1): POINTER TO THE CURRENT OPERAND VECTOR X IN WORKD.
!          IPNTR(2): POINTER TO THE CURRENT RESULT VECTOR Y IN WORKD.
!          IPNTR(3): POINTER TO THE VECTOR B * X IN WORKD WHEN USED IN
!                    THE SHIFT-AND-INVERT MODE.
!          IPNTR(4): POINTER TO THE NEXT AVAILABLE LOCATION IN WORKL
!                    THAT IS UNTOUCHED BY THE PROGRAM.
!          IPNTR(5): POINTER TO THE NCV BY NCV UPPER HESSENBERG
!                    MATRIX H IN WORKL.
!          IPNTR(6): POINTER TO THE  RITZ VALUE ARRAY  RITZ
!          IPNTR(7): POINTER TO THE (PROJECTED) RITZ VECTOR ARRAY Q
!          IPNTR(8): POINTER TO THE ERROR BOUNDS ARRAY IN WORKL.
!          IPNTR(14): POINTER TO THE NP SHIFTS IN WORKL. SEE REMARK 5
!                     BELOW.
!
!          NOTE: IPNTR(9:13) IS ONLY REFERENCED BY ZNEUPD . SEE
!               REMARK 2 BELOW.
!
!          IPNTR(9): POINTER TO THE NCV RITZ VALUES OF THE
!                    ORIGINAL SYSTEM.
!          IPNTR(10): NOT USED
!          IPNTR(11): POINTER TO THE NCV CORRESPONDING ERROR BOUNDS.
!          IPNTR(12): POINTER TO THE NCV BY NCV UPPER TRIANGULAR
!                     SCHUR MATRIX FOR H.
!          IPNTR(13): POINTER TO THE NCV BY NCV MATRIX OF EIGENVECTORS
!                     OF THE UPPER HESSENBERG MATRIX H. ONLY REFERENCED
!                     BY ZNEUPD  IF RVEC = .TRUE. SEE REMARK 2 BELOW.
!
!          -------------------------------------------------------------
!
!  WORKD   COMPLEX*16  WORK ARRAY OF LENGTH 3*N. (REVERSE COMMUNICATION)
!          DISTRIBUTED ARRAY TO BE USED IN THE BASIC ARNOLDI ITERATION
!          FOR REVERSE COMMUNICATION.  THE USER SHOULD NOT USE WORKD
!          AS TEMPORARY WORKSPACE DURING THE ITERATION !!!!!!!!!!
!          SEE DATA DISTRIBUTION NOTE BELOW.
!
!  WORKL   COMPLEX*16  WORK ARRAY OF LENGTH LWORKL.  (OUTPUT/WORKSPACE)
!          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
!          THE FRONT END.  SEE DATA DISTRIBUTION NOTE BELOW.
!
!  LWORKL  INTEGER.  (INPUT)
!          LWORKL MUST BE AT LEAST 3*NCV**2 + 5*NCV.
!
!  RWORK   DOUBLE PRECISION   WORK ARRAY OF LENGTH NCV (WORKSPACE)
!          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
!          THE FRONT END.
!
!
!  INFO    INTEGER.  (INPUT/OUTPUT)
!          IF INFO .EQ. 0, A RANDOMLY INITIAL RESIDUAL VECTOR IS USED.
!          IF INFO .NE. 0, RESID CONTAINS THE INITIAL RESIDUAL VECTOR,
!                          POSSIBLY FROM A PREVIOUS RUN.
!          ERROR FLAG ON OUTPUT.
!          =  0: NORMAL EXIT.
!          =  1: MAXIMUM NUMBER OF ITERATIONS TAKEN.
!                ALL POSSIBLE EIGENVALUES OF OP HAS BEEN FOUND.
!                IPARAM(5)
!                RETURNS THE NUMBER OF WANTED CONVERGED RITZ VALUES.
!          =  2: NO LONGER AN INFORMATIONAL ERROR. DEPRECATED STARTING
!                WITH RELEASE 2 OF ARPACK.
!          =  3: NO SHIFTS COULD BE APPLIED DURING A CYCLE OF THE
!                IMPLICITLY RESTARTED ARNOLDI ITERATION. ONE POSSIBILITY
!                IS TO INCREASE THE SIZE OF NCV RELATIVE TO NEV.
!                SEE REMARK 4 BELOW.
!          = -1: N MUST BE POSITIVE.
!          = -2: NEV MUST BE POSITIVE.
!          = -3: NCV-NEV >= 2 AND LESS THAN OR EQUAL TO N.
!          = -4: THE MAXIMUM NUMBER OF ARNOLDI UPDATE ITERATION
!                MUST BE GREATER THAN ZERO.
!          = -5: WHICH MUST BE ONE OF 'LM', 'SM', 'LR', 'SR', 'LI', 'SI'
!          = -6: BMAT MUST BE ONE OF 'I' OR 'G'.
!          = -7: LENGTH OF PRIVATE WORK ARRAY IS NOT SUFFICIENT.
!          = -8: ERROR RETURN FROM LAPACK EIGENVALUE CALCULATION;
!          = -9: STARTING VECTOR IS ZERO.
!          = -10: IPARAM(7) MUST BE 1,2,3.
!          = -11: IPARAM(7) = 1 AND BMAT = 'G' ARE INCOMPATIBLE.
!          = -12: IPARAM(1) MUST BE EQUAL TO 0 OR 1.
!          = -9999: COULD NOT BUILD AN ARNOLDI FACTORIZATION.
!                   USER INPUT ERROR HIGHLY LIKELY.  PLEASE
!                   CHECK ACTUAL ARRAY DIMENSIONS AND LAYOUT.
!                   IPARAM(5) RETURNS THE SIZE OF THE CURRENT ARNOLDI
!                   FACTORIZATION.
!
!\REMARKS
!  1. THE COMPUTED RITZ VALUES ARE APPROXIMATE EIGENVALUES OF OP. THE
!     SELECTION OF WHICH SHOULD BE MADE WITH THIS IN MIND WHEN USING
!     MODE = 3.  WHEN OPERATING IN MODE = 3 SETTING WHICH = 'LM' WILL
!     COMPUTE THE NEV EIGENVALUES OF THE ORIGINAL PROBLEM THAT ARE
!     CLOSEST TO THE SHIFT SIGMA . AFTER CONVERGENCE, APPROXIMATE
!     EIGENVALUES OF THE ORIGINAL PROBLEM MAY BE OBTAINED WITH THE
!     ARPACK SUBROUTINE ZNEUPD .
!
!  2. IF A BASIS FOR THE INVARIANT SUBSPACE CORRESPONDING TO THE
!     CONVERGED RITZ VALUES IS NEEDED, THE USER MUST CALL ZNEUPD
!     IMMEDIATELY FOLLOWING COMPLETION OF ZNAUPD . THIS IS NEW
!     STARTING WITH RELEASE 2 OF ARPACK.
!
!  3. IF M CAN BE FACTORED INTO A CHOLESKY FACTORIZATION M = LL`
!     THEN MODE = 2 SHOULD NOT BE SELECTED.  INSTEAD ONE SHOULD USE
!     MODE = 1 WITH  OP = INV(L)*A*INV(L`).  APPROPRIATE TRIANGULAR
!     LINEAR SYSTEMS SHOULD BE SOLVED WITH L AND L` RATHER
!     THAN COMPUTING INVERSES.  AFTER CONVERGENCE, AN APPROXIMATE
!     EIGENVECTOR Z OF THE ORIGINAL PROBLEM IS RECOVERED BY SOLVING
!     L`Z = X  WHERE X IS A RITZ VECTOR OF OP.
!
!  4. AT PRESENT THERE IS NO A-PRIORI ANALYSIS TO GUIDE THE SELECTION
!     OF NCV RELATIVE TO NEV.  THE ONLY FORMAL REQUIREMENT IS THAT
!     NCV > NEV + 1.
!     HOWEVER, IT IS RECOMMENDED THAT NCV .GE. 2*NEV.  IF MANY PROBLEMS
!     OF THE SAME TYPE ARE TO BE SOLVED, ONE SHOULD EXPERIMENT WITH
!     INCREASING NCV WHILE KEEPING NEV FIXED FOR A GIVEN TEST PROBLEM.
!     THIS WILL USUALLY DECREASE THE REQUIRED NUMBER OF OP*X OPERATIONS
!     BUT IT ALSO INCREASES THE WORK AND STORAGE REQUIRED TO MAINTAIN
!     THE ORTHOGONAL BASIS VECTORS.  THE OPTIMAL "CROSS-OVER" WITH
!     RESPECT TO CPU TIME
!     IS PROBLEM DEPENDENT AND MUST BE DETERMINED EMPIRICALLY.
!     SEE CHAPTER 8 OF REFERENCE 2 FOR FURTHER INFORMATION.
!
!  5. WHEN IPARAM(1) = 0, AND IDO = 3, THE USER NEEDS TO PROVIDE THE
!     NP = IPARAM(8) COMPLEX SHIFTS IN LOCATIONS
!     WORKL(IPNTR(14)), WORKL(IPNTR(14)+1), ... , WORKL(IPNTR(14)+NP).
!     EIGENVALUES OF THE CURRENT UPPER HESSENBERG MATRIX ARE LOCATED IN
!     WORKL(IPNTR(6)) THROUGH WORKL(IPNTR(6)+NCV-1). THEY ARE ORDERED
!     ACCORDING TO THE ORDER DEFINED BY WHICH.  THE ASSOCIATED RITZ
!     ESTIMATES ARE LOCATED IN WORKL(IPNTR(8)), WORKL(IPNTR(8)+1), ...,
!     WORKL(IPNTR(8)+NCV-1).
!
!-----------------------------------------------------------------------
!
!\DATA DISTRIBUTION NOTE:
!
!  FORTRAN-D SYNTAX:
!  ================
!  COMPLEX*16  RESID(N), V(LDV,NCV), WORKD(3*N), WORKL(LWORKL)
!  DECOMPOSE  D1(N), D2(N,NCV)
!  ALIGN      RESID(I) WITH D1(I)
!  ALIGN      V(I,J)   WITH D2(I,J)
!  ALIGN      WORKD(I) WITH D1(I)     RANGE (1:N)
!  ALIGN      WORKD(I) WITH D1(I-N)   RANGE (N+1:2*N)
!  ALIGN      WORKD(I) WITH D1(I-2*N) RANGE (2*N+1:3*N)
!  DISTRIBUTE D1(BLOCK), D2(BLOCK,:)
!  REPLICATED WORKL(LWORKL)
!
!  CRAY MPP SYNTAX:
!  ===============
!  COMPLEX*16  RESID(N), V(LDV,NCV), WORKD(N,3), WORKL(LWORKL)
!  SHARED     RESID(BLOCK), V(BLOCK,:), WORKD(BLOCK,:)
!  REPLICATED WORKL(LWORKL)
!
!  CM2/CM5 SYNTAX:
!  ==============
!
!-----------------------------------------------------------------------
!
!     INCLUDE   'EX-NONSYM.DOC'
!
!-----------------------------------------------------------------------
!
!\BEGINLIB
!
!\LOCAL VARIABLES:
!     XXXXXX  COMPLEX*16
!
!\REFERENCES:
!  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
!     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
!     PP 357-385.
!  2. R.B. LEHOUCQ, "ANALYSIS AND IMPLEMENTATION OF AN IMPLICITLY
!     RESTARTED ARNOLDI ITERATION", RICE UNIVERSITY TECHNICAL REPORT
!     TR95-13, DEPARTMENT OF COMPUTATIONAL AND APPLIED MATHEMATICS.
!  3. B.N. PARLETT & Y. SAAD, "_COMPLEX_ SHIFT AND INVERT STRATEGIES FOR
!     _REAL_ MATRICES", LINEAR ALGEBRA AND ITS APPLICATIONS, VOL 88/89,
!     PP 575-595, (1987).
!
!\ROUTINES CALLED:
!     ZNAUP2   ARPACK ROUTINE THAT IMPLEMENTS THE IMPLICITLY RESTARTED
!             ARNOLDI ITERATION.
!     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
!     ZVOUT    ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
!
!\AUTHOR
!     DANNY SORENSEN               PHUONG VU
!     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
!     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
!     APPLIED MATHEMATICS
!     RICE UNIVERSITY
subroutine znaupd(ido, bmat, n, which, nev,&
                  tol, resid, ncv, v, ldv,&
                  iparam, ipntr, workd, workl, lworkl,&
                  rwork, info, neqact, alpha)
!     HOUSTON, TEXAS
!
!\SCCS INFORMATION: @(#)
! FILE: NAUPD.F   SID: 2.8   DATE OF SID: 04/10/01   RELEASE: 2
!
!\REMARKS
!
!\ENDLIB
!
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1501
    implicit none
!     %--------------------------------------%
!     | INCLUDE FILES FOR DEBUGGING AND INFO |
!     %--------------------------------------%
!
#include "asterc/r8prem.h"
#include "asterfort/ivout.h"
#include "asterfort/u2mesi.h"
#include "asterfort/znaup2.h"
#include "asterfort/zvout.h"
    integer :: logfil, ndigit, mgetv0, mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, mneupd
    common /debug/&
     &  logfil, ndigit, mgetv0,&
     &  mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, mneupd
    integer :: nopx, nbx, nrorth, nitref, nrstrt
    common /infor/&
     &  nopx, nbx, nrorth, nitref, nrstrt
!
!
!     %------------------%
!     | SCALAR ARGUMENTS |
!     %------------------%
!
    character(len=1) :: bmat
    character(len=2) :: which
    integer :: ido, info, ldv, lworkl, n, ncv, nev, neqact
    real(kind=8) :: tol, alpha
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    integer :: iparam(11), ipntr(14), vali(6)
    complex(kind=8) :: resid(n), v(ldv, ncv), workd(3*n), workl(lworkl)
    real(kind=8) :: rwork(ncv)
!
!     %------------%
!     | PARAMETERS |
!     %------------%
!
    complex(kind=8) :: zero
    parameter (zero = (0.0d+0, 0.0d+0))
!
!     %---------------%
!     | LOCAL SCALARS |
!     %---------------%
!
    integer :: bounds, ierr, ih, iq, ishift, iw, ldh, ldq, mode, msglvl, mxiter, nb, nev0, next
    integer :: np, ritz, j
    save       bounds, ih, iq, ishift, iw,&
     &           ldh, ldq, mode, msglvl, mxiter, nb,&
     &           nev0, next, np, ritz
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
    if (ido .eq. 0) then
!        %-------------------------------%
!        | INITIALIZE TIMING STATISTICS  |
!        | & MESSAGE LEVEL FOR DEBUGGING |
!        %-------------------------------%
!
        msglvl = mnaupd
!
!        %----------------%
!        | ERROR CHECKING |
!        %----------------%
!
        ierr = 0
        ishift = iparam(1)
! DUE TO CRS512  LEVEC  = IPARAM(2)
        mxiter = iparam(3)
        nb = iparam(4)
!
!        %--------------------------------------------%
!        | REVISION 2 PERFORMS ONLY IMPLICIT RESTART. |
!        %--------------------------------------------%
!
        mode = iparam(7)
!
        if (n .le. 0) then
            ierr = -1
        else if (nev .le. 0) then
            ierr = -2
        else if (ncv .le. nev .or. ncv .gt. n) then
            if (msglvl .gt. 0) then
                write(logfil,*)
                write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                write(logfil,*)'& FLAG ERREUR -3 DEBRANCHE DANS ZNAUPD &'
                write(logfil,*)'& NBVECT < NBFREQ + 2 OU NBVECT > NBEQ &'
                write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                write(logfil,*)
            endif
        else if (mxiter .le. 0) then
            ierr = -4
            else if (which .ne. 'LM' .and. which .ne. 'SM' .and. which&
        .ne. 'LR' .and. which .ne. 'SR' .and. which .ne. 'LI' .and.&
        which .ne. 'SI') then
            ierr = -5
        else if (bmat .ne. 'I' .and. bmat .ne. 'G') then
            ierr = -6
        else if (lworkl .lt. 3*ncv**2 + 5*ncv) then
            ierr = -7
        else if (mode .lt. 1 .or. mode .gt. 3) then
            ierr = -10
        else if (mode .eq. 1 .and. bmat .eq. 'G') then
            ierr = -11
        endif
!
!        %------------%
!        | ERROR EXIT |
!        %------------%
!
        if (ierr .ne. 0) then
            info = ierr
            ido = 99
            goto 9000
        endif
!
!        %------------------------%
!        | SET DEFAULT PARAMETERS |
!        %------------------------%
!
        if (nb .le. 0) nb = 1
        if (tol .le. 0.d0) tol = r8prem()*0.5d0
        if (ishift .ne. 0 .and. ishift .ne. 1 .and. ishift .ne. 2) ishift = 1
!
!        %----------------------------------------------%
!        | NP IS THE NUMBER OF ADDITIONAL STEPS TO      |
!        | EXTEND THE LENGTH NEV LANCZOS FACTORIZATION. |
!        | NEV0 IS THE LOCAL VARIABLE DESIGNATING THE   |
!        | SIZE OF THE INVARIANT SUBSPACE DESIRED.      |
!        %----------------------------------------------%
!
        np = ncv - nev
        nev0 = nev
!
!        %-----------------------------%
!        | ZERO OUT INTERNAL WORKSPACE |
!        %-----------------------------%
!
        do 10 j = 1, 3*ncv**2 + 5*ncv
            workl(j) = zero
10      continue
!
!       %-------------------------------------------------------------%
!      | POINTER INTO WORKL FOR ADDRESS OF H, RITZ, BOUNDS, Q        |
!      | ETC... AND THE REMAINING WORKSPACE.                         |
!      | ALSO UPDATE POINTER TO BE USED ON OUTPUT.                   |
!      | MEMORY IS LAID OUT AS FOLLOWS:                              |
!      | WORKL(1:NCV*NCV) := GENERATED HESSENBERG MATRIX             |
!      | WORKL(NCV*NCV+1:NCV*NCV+NCV) := THE RITZ VALUES             |
!      | WORKL(NCV*NCV+NCV+1:NCV*NCV+2*NCV)   := ERROR BOUNDS        |
!      | WORKL(NCV*NCV+2*NCV+1:2*NCV*NCV+2*NCV) := ROTATION MATRIX Q |
!      | WORKL(2*NCV*NCV+2*NCV+1:3*NCV*NCV+5*NCV) := WORKSPACE       |
!      | THE FINAL WORKSPACE IS NEEDED BY SUBROUTINE ZNEIGH  CALLED   |
!     | BY ZNAUP2 . SUBROUTINE ZNEIGH  CALLS LAPACK ROUTINES FOR      |
!      | CALCULATING EIGENVALUES AND THE LAST ROW OF THE EIGENVECTOR |
!      | MATRIX.                                                     |
!        %-------------------------------------------------------------%
!
        ldh = ncv
        ldq = ncv
        ih = 1
        ritz = ih + ldh*ncv
        bounds = ritz + ncv
        iq = bounds + ncv
        iw = iq + ldq*ncv
        next = iw + ncv**2 + 3*ncv
!
        ipntr(4) = next
        ipntr(5) = ih
        ipntr(6) = ritz
        ipntr(7) = iq
        ipntr(8) = bounds
        ipntr(14) = iw
    endif
!
!     %-------------------------------------------------------%
!     | CARRY OUT THE IMPLICITLY RESTARTED ARNOLDI ITERATION. |
!     %-------------------------------------------------------%
!
    call znaup2(ido, bmat, n, which, nev0,&
                np, tol, resid, ishift, mxiter,&
                v, ldv, workl(ih), ldh, workl(ritz),&
                workl(bounds), workl(iq), ldq, workl(iw), ipntr,&
                workd, rwork, info, neqact, alpha)
!
!     %--------------------------------------------------%
!     | IDO .NE. 99 IMPLIES USE OF REVERSE COMMUNICATION |
!     | TO COMPUTE OPERATIONS INVOLVING OP.              |
!     %--------------------------------------------------%
!
    if (ido .eq. 3) iparam(8) = np
    if (ido .ne. 99) goto 9000
!
    iparam(3) = mxiter
    iparam(5) = np
    iparam(9) = nopx
    iparam(10) = nbx
    iparam(11) = nrorth
!
!     %------------------------------------%
!     | EXIT IF THERE WAS AN INFORMATIONAL |
!     | ERROR WITHIN ZNAUP2 .               |
!     %------------------------------------%
!
    if (info .lt. 0) goto 9000
    if (info .eq. 2) info = 3
!
    if (msglvl .gt. 0) then
        call ivout(logfil, 1, mxiter, ndigit, '_NAUPD: NUMBER OF UPDATE ITERATIONS TAKEN')
        call ivout(logfil, 1, np, ndigit, '_NAUPD: NUMBER OF WANTED "CONVERGED" RITZ VALUES')
        call zvout(logfil, np, workl(ritz), ndigit, '_NAUPD: THE FINAL RITZ VALUES')
        call zvout(logfil, np, workl(bounds), ndigit, '_NAUPD: ASSOCIATED RITZ ESTIMATES')
    endif
!        %--------------------------------%
!        | VERSION NUMBER & VERSION DATE  |
!        %--------------------------------%
    vali(1) = mxiter
    vali(2) = nopx
    vali(3) = nbx
    vali(4) = nrorth
    vali(5) = nitref
    vali(6) = nrstrt
    call u2mesi('I', 'ALGELINE6_27', 6, vali)
!
    mxiter=0
    nopx=0
    nbx=0
    nrorth=0
    nitref=0
    nrstrt=0
!
9000  continue
!
!     %---------------%
!     | END OF ZNAUPD |
!     %---------------%
!
end subroutine
