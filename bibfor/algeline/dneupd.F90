!---------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) LAPACK
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     SUBROUTINE ARPACK CALCULANT LES MODES PROPRES DU PROBLEME
!     INITIAL.
!---------------------------------------------------------------------
! BEGINDOC
!
! NAME: DNEUPD
!
! DESCRIPTION:
!
!  THIS SUBROUTINE RETURNS THE CONVERGED APPROXIMATIONS TO EIGENVALUES
!  OF A*Z = LAMBDA*B*Z AND (OPTIONALLY):
!
!      (1) THE CORRESPONDING APPROXIMATE EIGENVECTORS,
!
!      (2) AN ORTHONORMAL BASIS FOR THE ASSOCIATED APPROXIMATE
!          INVARIANT SUBSPACE,
!
!      (3) BOTH.
!
!  THERE IS NEGLIGIBLE ADDITIONAL COST TO OBTAIN EIGENVECTORS. AN
!  ORTHONORMAL BASIS IS ALWAYS COMPUTED.
!  THERE IS AN ADDITIONAL STORAGE COST OF N*NEV IF BOTH ARE REQUESTED
!  (IN THIS CASE A SEPARATE ARRAY Z MUST BE SUPPLIED).
!
!  THE APPROXIMATE EIGENVALUES AND EIGENVECTORS OF  A*Z = LAMBDA*B*Z
!  ARE DERIVED FROM APPROXIMATE EIGENVALUES AND EIGENVECTORS OF
!  OF THE LINEAR OPERATOR OP PRESCRIBED BY THE MODE SELECTION IN THE
!  CALL TO DNAUPD. DNAUPD MUST BE CALLED BEFORE THIS ROUTINE IS CALLED.
!  THESE APPROXIMATE EIGENVALUES AND VECTORS ARE COMMONLY CALLED RITZ
!  VALUES AND RITZ VECTORS RESPECTIVELY.  THEY ARE REFERRED TO AS SUCH
!  IN THE COMMENTS THAT FOLLOW. THE COMPUTED ORTHONORMAL BASIS FOR THE
!  INVARIANT SUBSPACE CORRESPONDING TO THESE RITZ VALUES IS REFERRED
!  TO AS A SCHUR BASIS.
!
!  SEE DOCUMENTATION IN THE HEADER OF THE SUBROUTINE DNAUPD FOR
!  DEFINITION OF OP AS WELL AS OTHER TERMS AND THE RELATION OF COMPUTED
!  RITZ VALUES AND RITZ VECTORS OF OP WITH RESPECT TO THE GIVEN PROBLEM
!  A*Z = LAMBDA*B*Z.  FOR A BRIEF DESCRIPTION, SEE DEFINITIONS OF
!  IPARAM(7), MODE AND WHICH IN THE DOCUMENTATION OF DNAUPD.
!
! ARGUMENTS:
!  RVEC  LOGICAL  (INPUT)
!    SPECIFIES WHETHER A BASIS FOR THE INVARIANT SUBSPACE CORRESPONDING
!    TO THE CONVERGED RITZ VALUE APPROXIMATIONS FOR THE EIGENPROBLEM
!    A*Z = LAMBDA*B*Z IS COMPUTED.
!
!             RVEC = .FALSE. COMPUTE RITZ VALUES ONLY.
!
!             RVEC = .TRUE.  COMPUTE THE RITZ VECTORS OR SCHUR VECTORS.
!                            SEE REMARKS BELOW.
!
!  HOWMNY  CHARACTER*1  (INPUT)
!     SPECIFIES THE FORM OF THE BASIS FOR THE INVARIANT SUBSPACE
!     CORRESPONDING TO THE CONVERGED RITZ VALUES THAT IS TO BE COMPUTED.
!
!          = 'A': COMPUTE NEV RITZ VECTORS,
!          = 'P': COMPUTE NEV SCHUR VECTORS,
!          = 'S': COMPUTE SOME OF THE RITZ VECTORS, SPECIFIED
!                 BY THE LOGICAL ARRAY SELECT.
!
!  SELECT  LOGICAL ARRAY OF DIMENSION NCV.  (INPUT)
!          IF HOWMNY = 'S', SELECT SPECIFIES THE RITZ VECTORS TO BE
!          COMPUTED. TO SELECT THE RITZ VECTOR CORRESPONDING TO A
!          RITZ VALUE (DR(J), DI(J)), SELECT(J) MUST BE SET TO .TRUE..
!          IF HOWMNY = 'A' OR 'P', SELECT IS USED AS INTERNAL WORKSPACE.
!
!  DR   REAL*8 ARRAY OF DIMENSION NEV+1.  (OUTPUT)
!     IF IPARAM(7) = 1,2 OR 3 AND SIGMAI=0.0  THEN ON EXIT: DR CONTAINS
!     THE REAL PART OF THE RITZ  APPROXIMATIONS TO THE EIGENVALUES OF
!     A*Z = LAMBDA*B*Z.
!     IF IPARAM(7) = 3, 4 AND SIGMAI IS NOT EQUAL TO ZERO, THEN ON EXIT:
!     DR CONTAINS THE REAL PART OF THE RITZ VALUES OF OP COMPUTED BY
!     DNAUPD. A FURTHER COMPUTATION MUST BE PERFORMED BY THE USER
!     TO TRANSFORM THE RITZ VALUES COMPUTED FOR OP BY DNAUPD TO THOSE
!     OF THE ORIGINAL SYSTEM A*Z = LAMBDA*B*Z. SEE REMARK 3 BELOW.
!
!  DI REAL*8 ARRAY OF DIMENSION NEV+1.  (OUTPUT)
!     ON EXIT, DI CONTAINS THE IMAGINARY PART OF THE RITZ VALUE
!     APPROXIMATIONS TO THE EIGENVALUES OF A*Z = LAMBDA*B*Z ASSOCIATED
!     WITH DR.
!
!     NOTE: WHEN RITZ VALUES ARE COMPLEX, THEY WILL COME IN COMPLEX
!           CONJUGATE PAIRS.  IF EIGENVECTORS ARE REQUESTED, THE
!           CORRESPONDING RITZ VECTORS WILL ALSO COME IN CONJUGATE
!           PAIRS AND THE REAL AND IMAGINARY PARTS OF THESE ARE
!           REPRESENTED IN TWO CONSECUTIVE COLUMNS OF THE ARRAY Z
!           (SEE BELOW).
!
!  Z REAL*8 N BY NEV+1 ARRAY IF RVEC = .TRUE. AND HOWMNY = 'A'. (OUTPUT)
!    ON EXIT, IF RVEC = .TRUE. AND HOWMNY = 'A', THEN THE COLUMNS OF
!    Z REPRESENT APPROXIMATE EIGENVECTORS (RITZ VECTORS) CORRESPONDING
!    TO THE NCONV=IPARAM(5) RITZ VALUES FOR EIGENSYSTEM
!    A*Z = LAMBDA*B*Z.
!
!    THE COMPLEX RITZ VECTOR ASSOCIATED WITH THE RITZ VALUE
!    WITH POSITIVE IMAGINARY PART IS STORED IN TWO CONSECUTIVE
!    COLUMNS.  THE FIRST COLUMN HOLDS THE REAL PART OF THE RITZ
!    VECTOR AND THE SECOND COLUMN HOLDS THE IMAGINARY PART.  THE
!    RITZ VECTOR ASSOCIATED WITH THE RITZ VALUE WITH NEGATIVE
!    IMAGINARY PART IS SIMPLY THE COMPLEX CONJUGATE OF THE RITZ VECTOR
!    ASSOCIATED WITH THE POSITIVE IMAGINARY PART.
!
!    IF  RVEC = .FALSE. OR HOWMNY = 'P', THEN Z IS NOT REFERENCED.
!
!    NOTE: IF IF RVEC = .TRUE. AND A SCHUR BASIS IS NOT REQUIRED,
!    THE ARRAY Z MAY BE SET EQUAL TO FIRST NEV+1 COLUMNS OF THE ARNOLDI
!    BASIS ARRAY V COMPUTED BY DNAUPD.  IN THIS CASE THE ARNOLDI BASIS
!    WILL BE DESTROYED AND OVERWRITTEN WITH THE EIGENVECTOR BASIS.
!
!  LDZ INTEGER.  (INPUT)
!     THE LEADING DIMENSION OF THE ARRAY Z.  IF RITZ VECTORS ARE
!     DESIRED, THEN  LDZ >= MAX( 1, N ).  IN ANY CASE,  LDZ >= 1.
!
!  SIGMAR  REAL*8  (INPUT)
!        IF IPARAM(7) = 3 OR 4, REPRESENTS THE REAL PART OF THE SHIFT.
!        NOT REFERENCED IF IPARAM(7) = 1 OR 2.
!
!  SIGMAI  REAL*8  (INPUT)
!    IF IPARAM(7) = 3 OR 4, REPRESENTS THE IMAGINARY PART OF THE SHIFT.
!    NOT REFERENCED IF IPARAM(7) = 1 OR 2. SEE REMARK 3 BELOW.
!
!  WORKEV  REAL*8 WORK ARRAY OF DIMENSION 3*NCV.  (WORKSPACE)
!
!  **** THE REMAINING ARGUMENTS MUST BE THE SAME AS FOR THE   ****
!  **** CALL TO DNAUPD THAT WAS JUST COMPLETED.               ****
!
!  NOTE: THE REMAINING ARGUMENTS
!
!       BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR,
!       WORKD, WORKL, LWORKL, INFO
!
!       MUST BE PASSED DIRECTLY TO DNEUPD FOLLOWING THE LAST CALL
!       TO DNAUPD.  THESE ARGUMENTS MUST NOT BE MODIFIED BETWEEN
!       THE THE LAST CALL TO DNAUPD AND THE CALL TO DNEUPD.
!
!  THREE PARAMETERS (V, WORKL, INFO) ARE ALSO OUTPUT PARAMETERS:
!
!  V  REAL*8 N BY NCV ARRAY.  (INPUT/OUTPUT)
!
!     UPON INPUT: THE NCV COLUMNS OF V CONTAIN THE ARNOLDI BASIS
!                 VECTORS FOR OP AS CONSTRUCTED BY DNAUPD .
!
!     UPON OUTPUT: IF RVEC = .TRUE. THE FIRST NCONV=IPARAM(5) COLUMNS
!                  CONTAIN APPROXIMATE SCHUR VECTORS THAT SPAN THE
!                  DESIRED INVARIANT SUBSPACE.  SEE REMARK 2 BELOW.
!
!     NOTE: IF THE ARRAY Z HAS BEEN SET EQUAL TO FIRST NEV+1 COLUMNS
!     OF THE ARRAY V AND RVEC=.TRUE. AND HOWMNY= 'A', THEN THE
!     ARNOLDI BASIS HELD BY V HAS BEEN OVERWRITTEN BY THE DESIRED
!     RITZ VECTORS.  IF A SEPARATE ARRAY Z HAS BEEN PASSED THEN
!     THE FIRST NCONV=IPARAM(5) COLUMNS OF V WILL CONTAIN APPROXIMATE
!     SCHUR VECTORS THAT SPAN THE DESIRED INVARIANT SUBSPACE.
!
!  WORKL   REAL*8 WORK ARRAY OF LENGTH LWORKL.  (OUTPUT/WORKSPACE)
!     WORKL(1:NCV*NCV+3*NCV) CONTAINS INFORMATION OBTAINED IN
!     DNAUPD.  THEY ARE NOT CHANGED BY DNEUPD.
!     WORKL(NCV*NCV+3*NCV+1:3*NCV*NCV+6*NCV) HOLDS THE
!     REAL AND IMAGINARY PART OF THE UNTRANSFORMED RITZ VALUES,
!     THE UPPER QUASI-TRIANGULAR MATRIX FOR H, AND THE
!     ASSOCIATED MATRIX REPRESENTATION OF THE INVARIANT SUBSPACE FOR H.
!
!     NOTE: IPNTR(9:13) CONTAINS THE POINTER INTO WORKL FOR ADDRESSES
!     OF THE ABOVE INFORMATION COMPUTED BY DNEUPD.
!     -------------------------------------------------------------
!     IPNTR(9):  POINTER TO THE REAL PART OF THE NCV RITZ VALUES OF THE
!                ORIGINAL SYSTEM.
!     IPNTR(10): POINTER TO THE IMAGINARY PART OF THE NCV RITZ VALUES OF
!                THE ORIGINAL SYSTEM.
!     IPNTR(11): POINTER TO THE NCV CORRESPONDING ERROR BOUNDS.
!     IPNTR(12): POINTER TO THE NCV BY NCV UPPER QUASI-TRIANGULAR
!                SCHUR MATRIX FOR H.
!     IPNTR(13): POINTER TO THE NCV BY NCV MATRIX OF EIGENVECTORS
!                OF THE UPPER HESSENBERG MATRIX H. ONLY REFERENCED BY
!                DNEUPD IF RVEC = .TRUE. SEE REMARK 2 BELOW.
!      -------------------------------------------------------------
!
!  INFO  INTEGER.  (OUTPUT)
!        ERROR FLAG ON OUTPUT.
!
!        =  0: NORMAL EXIT.
!
!        =  1: THE SCHUR FORM COMPUTED BY LAPACK ROUTINE DLAHQR
!          COULD NOT BE REORDERED BY LAPACK ROUTINE DTRSEN.
!          RE-ENTER SUBROUTINE DNEUPD WITH IPARAM(5)=NCV AND
!          INCREASE THE SIZE OF THE ARRAYS DR AND DI TO HAVE
!          DIMENSION AT LEAST DIMENSION NCV AND ALLOCATE AT LEAST NCV
!          COLUMNS FOR Z. NOTE: NOT NECESSARY IF Z AND V SHARE
!          THE SAME SPACE. PLEASE NOTIFY THE AUTHORS IF THIS ERROR
!          OCCURS.
!
!        = -1: N MUST BE POSITIVE.
!        = -2: NEV MUST BE POSITIVE.
!        = -3: NCV-NEV >= 2 AND LESS THAN OR EQUAL TO N.
subroutine dneupd(rvec, howmny, select, dr, di,&
                  z, ldz, sigmar, sigmai, workev,&
                  bmat, n, which, nev, tol,&
                  resid, ncv, v, ldv, iparam,&
                  ipntr, workd, workl, lworkl, info)
!        = -5: WHICH MUST BE ONE OF 'LM', 'SM', 'LR', 'SR', 'LI', 'SI'
!        = -6: BMAT MUST BE ONE OF 'I' OR 'G'.
!        = -7: LENGTH OF PRIVATE WORK WORKL ARRAY IS NOT SUFFICIENT.
!        = -8: ERROR RETURN FROM CALCULATION OF A REAL SCHUR FORM.
!              INFORMATIONAL ERROR FROM LAPACK ROUTINE DLAHQR.
!        = -9: ERROR RETURN FROM CALCULATION OF EIGENVECTORS.
!              INFORMATIONAL ERROR FROM LAPACK ROUTINE DTREVC.
!        = -10: IPARAM(7) MUST BE 1,2,3,4.
!        = -11: IPARAM(7) = 1 AND BMAT = 'G' ARE INCOMPATIBLE.
!        = -12: HOWMNY = 'S' NOT YET IMPLEMENTED
!        = -13: HOWMNY MUST BE ONE OF 'A' OR 'P' IF RVEC = .TRUE.
!        = -14: DNAUPD DID NOT FIND ANY EIGENVALUES TO SUFFICIENT
!               ACCURACY.
!
! BEGINLIB
!-----------------------------------------------------------------------
! REFERENCES:
!  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
!     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
!     PP 357-385.
!  2. R.B. LEHOUCQ, "ANALYSIS AND IMPLEMENTATION OF AN IMPLICITLY
!     RESTARTED ARNOLDI ITERATION", RICE UNIVERSITY TECHNICAL REPORT
!     TR95-13, DEPARTMENT OF COMPUTATIONAL AND APPLIED MATHEMATICS.
!  3. B.N. PARLETT & Y. SAAD, "COMPLEX SHIFT AND INVERT STRATEGIES FOR
!     REAL MATRICES", LINEAR ALGEBRA AND ITS APPLICATIONS, VOL 88/89,
!     PP 575-595, (1987).
!
! ROUTINES CALLED:
!     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
!     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
!     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
!     DGEQR2  LAPACK ROUTINE THAT COMPUTES THE QR FACTORIZATION OF
!             A MATRIX.
!     DLACPY  LAPACK MATRIX COPY ROUTINE.
!     DLAHQR  LAPACK ROUTINE TO COMPUTE THE REAL SCHUR FORM OF AN
!             UPPER HESSENBERG MATRIX.
!     DLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
!     DLASET  LAPACK MATRIX INITIALIZATION ROUTINE.
!     DORM2R  LAPACK ROUTINE THAT APPLIES AN ORTHOGONAL MATRIX IN
!             FACTORED FORM.
!     DTREVC  LAPACK ROUTINE TO COMPUTE THE EIGENVECTORS OF A MATRIX
!             IN UPPER QUASI-TRIANGULAR FORM.
!     DTRSEN  LAPACK ROUTINE THAT RE-ORDERS THE SCHUR FORM.
!     DTRMM   LEVEL 3 BLAS MATRIX TIMES AN UPPER TRIANGULAR MATRIX.
!     DGER    LEVEL 2 BLAS RANK ONE UPDATE TO A MATRIX.
!     DCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
!     DNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
!     DSCAL   LEVEL 1 BLAS THAT SCALES A VECTOR.
!
!     R8PREM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE PRECISION.
!     R8MIEM  ASTER UTILITY ROUTINE THAT GIVES THE MINIMUN VALUES.
!
! INTRINSIC FUNCTIONS
!     ABS, MAX, MIN
!
! REMARKS
!
!  1. CURRENTLY ONLY HOWMNY = 'A' AND 'P' ARE IMPLEMENTED.
!
!     LET X' DENOTE THE TRANSPOSE OF X.
!
!  2. SCHUR VECTORS ARE AN ORTHOGONAL REPRESENTATION FOR THE BASIS OF
!   RITZ VECTORS. THUS, THEIR NUMERICAL PROPERTIES ARE OFTEN SUPERIOR.
!   IF RVEC = .TRUE. THEN THE RELATIONSHIP
!             A * V(:,1:IPARAM(5)) = V(:,1:IPARAM(5)) * T, AND
!   V(:,1:IPARAM(5))' * V(:,1:IPARAM(5)) = I ARE APPROXIMATELY
!   SATISFIED.
!   HERE T IS THE LEADING SUBMATRIX OF ORDER IPARAM(5) OF THE REAL
!   UPPER QUASI-TRIANGULAR MATRIX STORED WORKL(IPNTR(12)). THAT IS,
!   T IS BLOCK UPPER TRIANGULAR WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS,
!   EACH 2-BY-2 DIAGONAL BLOCK HAS ITS DIAGONAL ELEMENTS EQUAL AND ITS
!   OFF-DIAGONAL ELEMENTS OF OPPOSITE SIGN.CORRESPONDING TO EACH 2-BY-2
!   DIAGONAL BLOCK IS A COMPLEX CONJUGATE PAIR OF RITZ VALUES. THE REAL
!   RITZ VALUES ARE STORED ON THE DIAGONAL OF T.
!
!  3. IF IPARAM(7) = 3 OR 4 AND SIGMAI IS NOT EQUAL ZERO, THEN THE USER
!   MUST FORM THE IPARAM(5) RAYLEIGH QUOTIENTS IN ORDER TO TRANSFORM
!   RITZ VALUES COMPUTED BY DNAUPD FOR OP TO THOSE OF A*Z = LAMBDA*B*Z.
!   SET RVEC = .TRUE. AND HOWMNY = 'A', AND
!   COMPUTE
!         Z(:,I)' * A * Z(:,I) IF DI(I) = 0.
!   IF DI(I) IS NOT EQUAL TO ZERO AND DI(I+1) = - D(I),
!   THEN THE DESIRED REAL AND IMAGINARY PARTS OF THE RITZ VALUE ARE
!       Z(:,I)' * A * Z(:,I) +  Z(:,I+1)' * A * Z(:,I+1),
!       Z(:,I)' * A * Z(:,I+1) -  Z(:,I+1)' * A * Z(:,I), RESPECTIVELY.
!     ANOTHER POSSIBILITY IS TO SET RVEC = .TRUE. AND HOWMNY = 'P' AND
!     COMPUTE V(:,1:IPARAM(5))' * A * V(:,1:IPARAM(5)) AND THEN AN UPPER
!     QUASI-TRIANGULAR MATRIX OF ORDER IPARAM(5) IS COMPUTED. SEE REMARK
!     2 ABOVE.
!
! AUTHORS
!     DANNY SORENSEN               PHUONG VU
!     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
!     CHAO YANG                    HOUSTON, TEXAS
!     DEPT. OF COMPUTATIONAL&
!     APPLIED MATHEMATICS
!     RICE UNIVERSITY
!     HOUSTON, TEXAS
!
! FILE: NEUPD.F   SID: 2.5   DATE OF SID: 7/31/96   RELEASE: 2
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE DLAMCH,
!            COMMON TIMING REMPLACE PAR COMMON INFORMATION,
!            UTILISATION DE R8PREM() ET R8MIEM(),
!            RAJOUT DU REEL EPS,
!            SHUNTAGE MESSAGE ERREUR -3 ET NOUVEAU MESSAGE,
!            RAJOUT 4 MESSAGES DIVISION PAR ZERO,
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1304,W1504
    implicit none
!
!     %-----------------------------%
!     | INCLUDE FILES FOR DEBUGGING |
!     %-----------------------------%
!
#include "asterf_types.h"
#include "asterc/matfpe.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/dmout.h"
#include "asterfort/dvout.h"
#include "asterfort/ar_dgeqr2.h"
#include "asterfort/ar_dlahqr.h"
#include "asterfort/ar_dtrevc.h"
#include "asterfort/ar_dtrsen.h"
#include "asterfort/ivout.h"
#include "blas/dcopy.h"
#include "blas/dgemv.h"
#include "blas/dger.h"
#include "blas/dlacpy.h"
#include "blas/dlapy2.h"
#include "blas/dlaset.h"
#include "blas/dnrm2.h"
#include "blas/dorm2r.h"
#include "blas/dscal.h"
#include "blas/dtrmm.h"
    integer :: logfil, ndigit, mgetv0, mnaupd, mnaup2, mnaitr, mneigh, mnapps
    integer :: mngets, mneupd
    common /debug/&
     &  logfil, ndigit, mgetv0,&
     &  mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, mneupd
!
!     %------------------%
!     | SCALAR ARGUMENTS |
!     %------------------%
!
    character(len=1) :: bmat, howmny
    character(len=2) :: which
    aster_logical :: rvec
    integer :: info, ldz, ldv, lworkl, n, ncv, nev
    real(kind=8) :: sigmar, sigmai, tol
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    integer :: iparam(11), ipntr(14)
    aster_logical :: select(ncv)
    real(kind=8) :: dr(nev+1), di(nev+1), resid(n), v(ldv, ncv), z(ldz, *)
    real(kind=8) :: workd(3*n), workl(lworkl), workev(3*ncv)
!
!     %------------%
!     | PARAMETERS |
!     %------------%
!
    real(kind=8) :: one, zero
    parameter (one = 1.0d+0, zero = 0.0d+0)
!
!     %---------------%
!     | LOCAL SCALARS |
!     %---------------%
!
    character(len=6) :: type
    integer(kind=4) :: ierr4
    integer :: bounds, ierr, ih, ihbds, iheigr, iheigi, iconj, nconv, invsub
    integer :: iuptri, iwork(1), j, k, ktrord, ldh, ldq, mode, msglvl, outncv
    integer :: ritzr, ritzi, irr, iri, ibd
! DUE TO CRS512 INTEGER IWEV, WRR, WRI
    aster_logical :: reord
    real(kind=8) :: conds, rnorm, sep, temp, thres, vl(1, 1), temp1, eps23, eps
!
!     %--------------------%
!     | EXTERNAL FUNCTIONS |
!     %--------------------%
!
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
    call matfpe(-1)
!
!     %------------------------%
!     | SET DEFAULT PARAMETERS |
!     %------------------------%
!
    msglvl = mneupd
    eps = r8miem()**(2.0d+0 / 3.0d+0)
    mode = iparam(7)
    nconv = iparam(5)
    info = 0
!
!     %---------------------------------%
!     | GET MACHINE DEPENDENT CONSTANT. |
!     %---------------------------------%
!
    eps23 = r8prem()*0.5d0
    eps23 = eps23**(2.0d+0 / 3.0d+0)
!
!     %--------------%
!     | QUICK RETURN |
!     %--------------%
!
    ierr = 0
!
    if (nconv .le. 0) then
        ierr = -14
    else if (n .le. 0) then
        ierr = -1
    else if (nev .le. 0) then
        ierr = -2
    else if (ncv .le. nev+1 .or. ncv .gt. n) then
        if (msglvl .gt. 0) then
            write(logfil,*)
            write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
            write(logfil,*)'& FLAG ERREUR -3 DEBRANCHE DANS DNEUPD &'
            write(logfil,*)'& NBVECT < NBFREQ + 2 OU NBVECT > NBEQ &'
            write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
            write(logfil,*)
        endif
        else if (which .ne. 'LM' .and. which .ne. 'SM' .and. which .ne.&
    'LR' .and. which .ne. 'SR' .and. which .ne. 'LI' .and. which .ne.&
    'SI') then
        ierr = -5
    else if (bmat .ne. 'I' .and. bmat .ne. 'G') then
        ierr = -6
    else if (lworkl .lt. 3*ncv**2 + 6*ncv) then
        ierr = -7
        else if ( (howmny .ne. 'A' .and. howmny .ne. 'P' .and. howmny&
    .ne. 'S') .and. rvec ) then
        ierr = -13
    else if (howmny .eq. 'S') then
        ierr = -12
    endif
!
    if (mode .eq. 1 .or. mode .eq. 2) then
        type = 'REGULR'
    else if (mode .eq. 3 .and. sigmai .eq. zero) then
        type = 'SHIFTI'
    else if (mode .eq. 3) then
        type = 'REALPT'
    else if (mode .eq. 4) then
        type = 'IMAGPT'
    else
        ierr = -10
    endif
    if (mode .eq. 1 .and. bmat .eq. 'G') ierr = -11
!
!     %------------%
!     | ERROR EXIT |
!     %------------%
!
    if (ierr .ne. 0) then
        info = ierr
        goto 9000
    endif
!
!     %--------------------------------------------------------%
!     | POINTER INTO WORKL FOR ADDRESS OF H, RITZ, BOUNDS, Q   |
!     | ETC... AND THE REMAINING WORKSPACE.                    |
!     | ALSO UPDATE POINTER TO BE USED ON OUTPUT.              |
!     | MEMORY IS LAID OUT AS FOLLOWS:                         |
!     | WORKL(1:NCV*NCV) := GENERATED HESSENBERG MATRIX        |
!     | WORKL(NCV*NCV+1:NCV*NCV+2*NCV) := REAL AND IMAGINARY   |
!     |                                   PARTS OF RITZ VALUES |
!     | WORKL(NCV*NCV+2*NCV+1:NCV*NCV+3*NCV) := ERROR BOUNDS   |
!     %--------------------------------------------------------%
!
!     %-----------------------------------------------------------%
!     | THE FOLLOWING IS USED AND SET BY DNEUPD.                  |
!     | WORKL(NCV*NCV+3*NCV+1:NCV*NCV+4*NCV) := THE UNTRANSFORMED |
!     |                             REAL PART OF THE RITZ VALUES. |
!     | WORKL(NCV*NCV+4*NCV+1:NCV*NCV+5*NCV) := THE UNTRANSFORMED |
!     |                        IMAGINARY PART OF THE RITZ VALUES. |
!     | WORKL(NCV*NCV+5*NCV+1:NCV*NCV+6*NCV) := THE UNTRANSFORMED |
!     |                           ERROR BOUNDS OF THE RITZ VALUES |
!     | WORKL(NCV*NCV+6*NCV+1:2*NCV*NCV+6*NCV) := HOLDS THE UPPER |
!     |                             QUASI-TRIANGULAR MATRIX FOR H |
!     | WORKL(2*NCV*NCV+6*NCV+1: 3*NCV*NCV+6*NCV) := HOLDS THE    |
!     |       ASSOCIATED MATRIX REPRESENTATION OF THE INVARIANT   |
!     |       SUBSPACE FOR H.                                     |
!     | GRAND TOTAL OF NCV * ( 3 * NCV + 6 ) LOCATIONS.           |
!     %-----------------------------------------------------------%
!
    ih = ipntr(5)
    ritzr = ipntr(6)
    ritzi = ipntr(7)
    bounds = ipntr(8)
    ldh = ncv
    ldq = ncv
    iheigr = bounds + ldh
    iheigi = iheigr + ldh
    ihbds = iheigi + ldh
    iuptri = ihbds + ldh
    invsub = iuptri + ldh*ncv
    ipntr(9) = iheigr
    ipntr(10) = iheigi
    ipntr(11) = ihbds
    ipntr(12) = iuptri
    ipntr(13) = invsub
! DUE TO CRS512      WRR = 1
! DUE TO CRS512      WRI = NCV + 1
! DUE TO CRS512      IWEV = WRI + NCV
!
!     %-----------------------------------------%
!     | IRR POINTS TO THE REAL PART OF THE RITZ |
!     |     VALUES COMPUTED BY _NEIGH BEFORE    |
!     |     EXITING _NAUP2.                     |
!     | IRI POINTS TO THE IMAGINARY PART OF THE |
!     |     RITZ VALUES COMPUTED BY _NEIGH      |
!     |     BEFORE EXITING _NAUP2.              |
!     | IBD POINTS TO THE RITZ ESTIMATES        |
!     |     COMPUTED BY _NEIGH BEFORE EXITING   |
!     |     _NAUP2.                             |
!     %-----------------------------------------%
!
    irr = ipntr(14)+ncv*ncv
    iri = irr+ncv
    ibd = iri+ncv
!
!     %------------------------------------%
!     | RNORM IS B-NORM OF THE RESID(1:N). |
!     %------------------------------------%
!
    rnorm = workl(ih+2)
    workl(ih+2) = zero
!
    if (rvec) then
!
!        %-------------------------------------------%
!        | GET CONVERGED RITZ VALUE ON THE BOUNDARY. |
!        | NOTE: CONVERGED RITZ VALUES HAVE BEEN     |
!        | PLACED IN THE FIRST NCONV LOCATIONS IN    |
!        | WORKL(RITZR) AND WORKL(RITZI).  THEY HAVE |
!        | BEEN SORTED (IN _NAUP2) ACCORDING TO THE  |
!        | WHICH SELECTION CRITERION.                |
!        %-------------------------------------------%
!
        if (which .eq. 'LM' .or. which .eq. 'SM') then
            thres = dlapy2( workl(ritzr), workl(ritzi) )
        else if (which .eq. 'LR' .or. which .eq. 'SR') then
            thres = workl(ritzr)
        else if (which .eq. 'LI' .or. which .eq. 'SI') then
            thres = abs( workl(ritzi) )
        endif
!
        if (msglvl .gt. 2) then
            call dvout(logfil, 1, [thres], ndigit,&
                       '_NEUPD: THRESHOLD EIGENVALUE USED FOR RE-ORDERING')
        endif
!
!        %----------------------------------------------------------%
!        | CHECK TO SEE IF ALL CONVERGED RITZ VALUES APPEAR AT THE  |
!        | TOP OF THE UPPER QUASI-TRIANGULAR MATRIX COMPUTED BY     |
!        | _NEIGH IN _NAUP2.  THIS IS DONE IN THE FOLLOWING WAY:    |
!        |                                                          |
!        | 1) FOR EACH RITZ VALUE OBTAINED FROM _NEIGH, COMPARE IT  |
!        |    WITH THE THRESHOLD RITZ VALUE COMPUTED ABOVE TO       |
!        |    DETERMINE WHETHER IT IS A WANTED ONE.                 |
!        |                                                          |
!        | 2) IF IT IS WANTED, THEN CHECK THE CORRESPONDING RITZ    |
!        |    ESTIMATE TO SEE IF IT HAS CONVERGED.  IF IT HAS, SET  |
!        |    CORREPONDING ENTRY IN THE LOGICAL ARRAY SELECT TO     |
!        |    .TRUE..                                               |
!        |                                                          |
!        | IF SELECT(J) = .TRUE. AND J > NCONV, THEN THERE IS A     |
!        | CONVERGED RITZ VALUE THAT DOES NOT APPEAR AT THE TOP OF  |
!        | THE UPPER QUASI-TRIANGULAR MATRIX COMPUTED BY _NEIGH IN  |
!        | _NAUP2.  REORDERING IS NEEDED.                           |
!        %----------------------------------------------------------%
!
        reord = .false.
        ktrord = 0
        do j = 0, ncv-1
            select(j+1) = .false.
            if (which .eq. 'LM') then
                if (dlapy2(workl(irr+j), workl(iri+j)) .ge. thres) then
                    temp1 = max( eps23, dlapy2( workl(irr+j), workl( iri+j) ) )
                    if (workl(ibd+j) .le. tol*temp1) select(j+1) = .true.
                endif
            else if (which .eq. 'SM') then
                if (dlapy2(workl(irr+j), workl(iri+j)) .le. thres) then
                    temp1 = max( eps23, dlapy2( workl(irr+j), workl( iri+j) ) )
                    if (workl(ibd+j) .le. tol*temp1) select(j+1) = .true.
                endif
            else if (which .eq. 'LR') then
                if (workl(irr+j) .ge. thres) then
                    temp1 = max( eps23, dlapy2( workl(irr+j), workl( iri+j) ) )
                    if (workl(ibd+j) .le. tol*temp1) select(j+1) = .true.
                endif
            else if (which .eq. 'SR') then
                if (workl(irr+j) .le. thres) then
                    temp1 = max( eps23, dlapy2( workl(irr+j), workl( iri+j) ) )
                    if (workl(ibd+j) .le. tol*temp1) select(j+1) = .true.
                endif
            else if (which .eq. 'LI') then
                if (abs(workl(iri+j)) .ge. thres) then
                    temp1 = max( eps23, dlapy2( workl(irr+j), workl( iri+j) ) )
                    if (workl(ibd+j) .le. tol*temp1) select(j+1) = .true.
                endif
            else if (which .eq. 'SI') then
                if (abs(workl(iri+j)) .le. thres) then
                    temp1 = max( eps23, dlapy2( workl(irr+j), workl( iri+j) ) )
                    if (workl(ibd+j) .le. tol*temp1) select(j+1) = .true.
                endif
            endif
            if (j+1 .gt. nconv) reord = ( select(j+1) .or. reord )
            if (select(j+1)) ktrord = ktrord + 1
        end do
!
        if (msglvl .gt. 2) then
            call ivout(logfil, 1, [ktrord], ndigit, '_NEUPD: NUMBER OF SPECIFIED EIGENVALUES')
            call ivout(logfil, 1, [nconv], ndigit, '_NEUPD: NUMBER OF "CONVERGED" EIGENVALUES')
        endif
!
!        %-----------------------------------------------------------%
!        | CALL LAPACK ROUTINE DLAHQR TO COMPUTE THE REAL SCHUR FORM |
!        | OF THE UPPER HESSENBERG MATRIX RETURNED BY DNAUPD.        |
!        | MAKE A COPY OF THE UPPER HESSENBERG MATRIX.               |
!        | INITIALIZE THE SCHUR VECTOR MATRIX Q TO THE IDENTITY.     |
!        %-----------------------------------------------------------%
!
        call dcopy(ldh*ncv, workl(ih), 1, workl(iuptri), 1)
! DUE TO CRP_102 CALL DLASET ('ALL', NCV, NCV, ZERO, ONE,
! WORKL(INVSUB), LDQ)
        call dlaset('A', ncv, ncv, zero, one,&
                    workl(invsub), ldq)
        call ar_dlahqr(.true._1, .true._1, ncv, 1, ncv,&
                    workl(iuptri), ldh, workl(iheigr), workl(iheigi), 1,&
                    ncv, workl(invsub), ldq, ierr)
        call dcopy(ncv, workl(invsub+ncv-1), ldq, workl(ihbds), 1)
!
        if (ierr .ne. 0) then
            info = -8
            goto 9000
        endif
!
        if (msglvl .gt. 1) then
            call dvout(logfil, ncv, workl(iheigr), ndigit,&
                       '_NEUPD: REAL PART OF THE EIGENVALUES OF H')
            call dvout(logfil, ncv, workl(iheigi), ndigit,&
                       '_NEUPD: IMAGINARY PART OF THE EIGENVALUES OF H')
            call dvout(logfil, ncv, workl(ihbds), ndigit,&
                       '_NEUPD: LAST ROW OF THE SCHUR VECTOR MATRIX')
            if (msglvl .gt. 3) then
                call dmout(logfil, ncv, ncv, workl(iuptri), ldh,&
                           ndigit, '_NEUPD: THE UPPER QUASI-TRIANGULAR MATRIX ')
            endif
        endif
!
        if (reord) then
!
!           %-----------------------------------------------------%
!           | REORDER THE COMPUTED UPPER QUASI-TRIANGULAR MATRIX. |
!           %-----------------------------------------------------%
!
            call ar_dtrsen('N', 'V', select, ncv, workl(iuptri),&
                        ldh, workl(invsub), ldq, workl(iheigr), workl(iheigi),&
                        nconv, conds, sep, workl(ihbds), ncv,&
                        iwork, 1, ierr)
            if (ierr .eq. 1) then
                info = 1
                goto 9000
            endif
!
            if (msglvl .gt. 2) then
                call dvout(logfil, ncv, workl(iheigr), ndigit,&
                           '_NEUPD: REAL PART OF THE EIGENVALUES OF H--REORDERED')
                call dvout(logfil, ncv, workl(iheigi), ndigit,&
                           '_NEUPD: IMAG PART OF THE EIGENVALUES OF H--REORDERED')
                if (msglvl .gt. 3) then
                    call dmout(logfil, ncv, ncv, workl(iuptri), ldq,&
                               ndigit, '_NEUPD: QUASI-TRIANGULAR MATRIX AFTER RE-ORDERING')
                endif
            endif
        endif
!
!        %---------------------------------------%
!        | COPY THE LAST ROW OF THE SCHUR VECTOR |
!        | INTO WORKL(IHBDS).  THIS WILL BE USED |
!        | TO COMPUTE THE RITZ ESTIMATES OF      |
!        | CONVERGED RITZ VALUES.                |
!        %---------------------------------------%
!
        call dcopy(ncv, workl(invsub+ncv-1), ldq, workl(ihbds), 1)
!
!        %----------------------------------------------------%
!        | PLACE THE COMPUTED EIGENVALUES OF H INTO DR AND DI |
!        | IF A SPECTRAL TRANSFORMATION WAS NOT USED.         |
!        %----------------------------------------------------%
!
        if (type .eq. 'REGULR') then
            call dcopy(nconv, workl(iheigr), 1, dr, 1)
            call dcopy(nconv, workl(iheigi), 1, di, 1)
        endif
!
!        %----------------------------------------------------------%
!        | COMPUTE THE QR FACTORIZATION OF THE MATRIX REPRESENTING  |
!        | THE WANTED INVARIANT SUBSPACE LOCATED IN THE FIRST NCONV |
!        | COLUMNS OF WORKL(INVSUB,LDQ).                            |
!        %----------------------------------------------------------%
!
        call ar_dgeqr2(ncv, nconv, workl(invsub), ldq, workev,&
                    workev( ncv+1), ierr)
!
!        %---------------------------------------------------------%
!        | * POSTMULTIPLY V BY Q USING DORM2R.                     |
!        | * COPY THE FIRST NCONV COLUMNS OF VQ INTO Z.            |
!        | * POSTMULTIPLY Z BY R.                                  |
!        | THE N BY NCONV MATRIX Z IS NOW A MATRIX REPRESENTATION  |
!        | OF THE APPROXIMATE INVARIANT SUBSPACE ASSOCIATED WITH   |
!        | THE RITZ VALUES IN WORKL(IHEIGR) AND WORKL(IHEIGI)      |
!        | THE FIRST NCONV COLUMNS OF V ARE NOW APPROXIMATE SCHUR  |
!        | VECTORS ASSOCIATED WITH THE REAL UPPER QUASI-TRIANGULAR |
!        | MATRIX OF ORDER NCONV IN WORKL(IUPTRI)                  |
!        %---------------------------------------------------------%
! DUE TO CRP_102 CALL DORM2R ('RIGHT', 'NOTRANSPOSE', N, NCV, NCONV,
!
        call dorm2r('R', 'N', n, ncv, nconv,&
                    workl(invsub), ldq, workev, v, ldv,&
                    workd(n+1), ierr4)
        ierr=ierr4
        call dlacpy('A', n, nconv, v, ldv,&
                    z, ldz)
!
        do j = 1, nconv
!
!           %---------------------------------------------------%
!           | PERFORM BOTH A COLUMN AND ROW SCALING IF THE      |
!           | DIAGONAL ELEMENT OF WORKL(INVSUB,LDQ) IS NEGATIVE |
!           | I'M LAZY AND DON'T TAKE ADVANTAGE OF THE UPPER    |
!           | QUASI-TRIANGULAR FORM OF WORKL(IUPTRI,LDQ)        |
!           | NOTE THAT SINCE Q IS ORTHOGONAL, R IS A DIAGONAL  |
!           | MATRIX CONSISTING OF PLUS OR MINUS ONES           |
!           %---------------------------------------------------%
!
            if (workl(invsub+(j-1)*ldq+j-1) .lt. zero) then
                call dscal(nconv, -one, workl(iuptri+j-1), ldq)
                call dscal(nconv, -one, workl(iuptri+(j-1)*ldq), 1)
            endif
!
        end do
!
        if (howmny .eq. 'A') then
!
!           %--------------------------------------------%
!           | COMPUTE THE NCONV WANTED EIGENVECTORS OF T |
!           | LOCATED IN WORKL(IUPTRI,LDQ).              |
!           %--------------------------------------------%
!
            do j = 1, ncv
                if (j .le. nconv) then
                    select(j) = .true.
                else
                    select(j) = .false.
                endif
            end do
!
            call ar_dtrevc('R', 'S', select, ncv, workl(iuptri),&
                        ldq, vl, 1, workl(invsub), ldq,&
                        ncv, outncv, workev, ierr)
!
            if (ierr .ne. 0) then
                info = -9
                goto 9000
            endif
!
!           %------------------------------------------------%
!           | SCALE THE RETURNING EIGENVECTORS SO THAT THEIR |
!           | EUCLIDEAN NORMS ARE ALL ONE. LAPACK SUBROUTINE |
!           | DTREVC RETURNS EACH EIGENVECTOR NORMALIZED SO  |
!           | THAT THE ELEMENT OF LARGEST MAGNITUDE HAS      |
!           | MAGNITUDE 1,                                   |
!           %------------------------------------------------%
!
            iconj = 0
            do j = 1, nconv
!
                if (workl(iheigi+j-1) .eq. zero) then
!
!                 %----------------------%
!                 | REAL EIGENVALUE CASE |
!                 %----------------------%
!
                    temp = dnrm2( ncv, workl(invsub+(j-1)*ldq), 1 )
                    call dscal(ncv, one / temp, workl(invsub+(j-1)* ldq), 1)
!
                else
!
!                 %-------------------------------------------%
!                 | COMPLEX CONJUGATE PAIR CASE. NOTE THAT    |
!                 | SINCE THE REAL AND IMAGINARY PART OF      |
!                 | THE EIGENVECTOR ARE STORED IN CONSECUTIVE |
!                 | COLUMNS, WE FURTHER NORMALIZE BY THE      |
!                 | SQUARE ROOT OF TWO.                       |
!                 %-------------------------------------------%
!
                    if (iconj .eq. 0) then
                        temp = dlapy2(&
                               dnrm2( ncv, workl(invsub+(j-1)* ldq), 1 ),&
                               dnrm2( ncv, workl(invsub+j*ldq), 1)&
                               )
                        call dscal(ncv, one / temp, workl(invsub+(j- 1)*ldq), 1)
                        call dscal(ncv, one / temp, workl(invsub+j* ldq), 1)
                        iconj = 1
                    else
                        iconj = 0
                    endif
!
                endif
!
            end do
!
            call dgemv('T', ncv, nconv, one, workl(invsub),&
                       ldq, workl(ihbds), 1, zero, workev,&
                       1)
!
            iconj = 0
            do j = 1, nconv
                if (workl(iheigi+j-1) .ne. zero) then
!
!                 %-------------------------------------------%
!                 | COMPLEX CONJUGATE PAIR CASE. NOTE THAT    |
!                 | SINCE THE REAL AND IMAGINARY PART OF      |
!                 | THE EIGENVECTOR ARE STORED IN CONSECUTIVE |
!                 %-------------------------------------------%
!
                    if (iconj .eq. 0) then
                        workev(j) = dlapy2(workev(j), workev(j+1))
                        workev(j+1) = workev(j)
                        iconj = 1
                    else
                        iconj = 0
                    endif
                endif
            end do
!
            if (msglvl .gt. 2) then
                call dcopy(ncv, workl(invsub+ncv-1), ldq, workl(ihbds), 1)
                call dvout(logfil, ncv, workl(ihbds), ndigit,&
                           '_NEUPD: LAST ROW OF THE EIGENVECTOR MATRIX FOR T')
                if (msglvl .gt. 3) then
                    call dmout(logfil, ncv, ncv, workl(invsub), ldq,&
                               ndigit, '_NEUPD: THE EIGENVECTOR MATRIX FOR T')
                endif
            endif
!
!           %---------------------------------------%
!           | COPY RITZ ESTIMATES INTO WORKL(IHBDS) |
!           %---------------------------------------%
!
            call dcopy(nconv, workev, 1, workl(ihbds), 1)
!
!           %---------------------------------------------------------%
!           | COMPUTE THE QR FACTORIZATION OF THE EIGENVECTOR MATRIX  |
!           | ASSOCIATED WITH LEADING PORTION OF T IN THE FIRST NCONV |
!           | COLUMNS OF WORKL(INVSUB,LDQ).                           |
!           %---------------------------------------------------------%
!
            call ar_dgeqr2(ncv, nconv, workl(invsub), ldq, workev,&
                        workev(ncv+1), ierr)
!
!           %----------------------------------------------%
!           | * POSTMULTIPLY Z BY Q.                       |
!           | * POSTMULTIPLY Z BY R.                       |
!           | THE N BY NCONV MATRIX Z IS NOW CONTAINS THE  |
!           | RITZ VECTORS ASSOCIATED WITH THE RITZ VALUES |
!           | IN WORKL(IHEIGR) AND WORKL(IHEIGI).          |
!           %----------------------------------------------%
! DUE TO CRP102
!          CALL DORM2R ('RIGHT', 'NOTRANSPOSE', N, NCV, NCONV,
!     &         WORKL(INVSUB), LDQ, WORKEV, Z, LDZ, WORKD(N+1), IERR)
            call dorm2r('R', 'N', n, ncv, nconv,&
                        workl(invsub), ldq, workev, z, ldz,&
                        workd(n+1), ierr4)
            ierr=ierr4
!
! DUE TO CRP102 CALL DTRMM('RIGHT','UPPER','NO TRANSPOSE','NON-UNIT',
            call dtrmm('R', 'U', 'N', 'N', n,&
                       nconv, one, workl( invsub), ldq, z,&
                       ldz)
!
        endif
!
    else
!
!        %------------------------------------------------------%
!        | AN APPROXIMATE INVARIANT SUBSPACE IS NOT NEEDED.     |
!        | PLACE THE RITZ VALUES COMPUTED DNAUPD INTO DR AND DI |
!        %------------------------------------------------------%
!
        call dcopy(nconv, workl(ritzr), 1, dr, 1)
        call dcopy(nconv, workl(ritzi), 1, di, 1)
        call dcopy(nconv, workl(ritzr), 1, workl(iheigr), 1)
        call dcopy(nconv, workl(ritzi), 1, workl(iheigi), 1)
        call dcopy(nconv, workl(bounds), 1, workl(ihbds), 1)
    endif
!
!     %------------------------------------------------%
!     | TRANSFORM THE RITZ VALUES AND POSSIBLY VECTORS |
!     | AND CORRESPONDING ERROR BOUNDS OF OP TO THOSE  |
!     | OF A*X = LAMBDA*B*X.                           |
!     %------------------------------------------------%
!
    if (type .eq. 'REGULR') then
!
        if (rvec) call dscal(ncv, rnorm, workl(ihbds), 1)
!
    else
!
!        %---------------------------------------%
!        |   A SPECTRAL TRANSFORMATION WAS USED. |
!        | * DETERMINE THE RITZ ESTIMATES OF THE |
!        |   RITZ VALUES IN THE ORIGINAL SYSTEM. |
!        %---------------------------------------%
!
        if (type .eq. 'SHIFTI') then
!
            if (rvec) call dscal(ncv, rnorm, workl(ihbds), 1)
!
            do k = 1, ncv
                temp = dlapy2( workl(iheigr+k-1), workl(iheigi+k-1) )
!
                if (temp * temp .le. eps) then
                    if (msglvl .gt. 0) then
                        write(logfil,*)
                        write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                        write(logfil,*)'&         DNEUPD_1                 &'
                        write(logfil,*)'& DIV PAR EPS AU LIEU DE TEMP*2    &'
                        write(logfil,*)'& EPS    = ',eps
                        write(logfil,*)'& TEMP*2 = ',temp*temp
                        write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                        write(logfil,*)
                    endif
                    workl(ihbds+k-1)=abs(workl(ihbds+k-1))/eps
                else
                    workl(ihbds+k-1) = abs( workl(ihbds+k-1) ) / temp / temp
                endif
            end do
!
        else if (type .eq. 'REALPT') then
!
            do k = 1, ncv
            end do
!
        else if (type .eq. 'IMAGPT') then
!
            do k = 1, ncv
            end do
!
        endif
!
!        %-----------------------------------------------------------%
!        | *  TRANSFORM THE RITZ VALUES BACK TO THE ORIGINAL SYSTEM. |
!        |    FOR TYPE = 'SHIFTI' THE TRANSFORMATION IS              |
!        |             LAMBDA = 1/THETA + SIGMA                      |
!        |    FOR TYPE = 'REALPT' OR 'IMAGPT' THE USER MUST FROM     |
!        |    RAYLEIGH QUOTIENTS OR A PROJECTION. SEE REMARK 3 ABOVE.|
!        | NOTES:                                                    |
!        | *THE RITZ VECTORS ARE NOT AFFECTED BY THE TRANSFORMATION. |
!        %-----------------------------------------------------------%
!
        if (type .eq. 'SHIFTI') then
!
            do k = 1, ncv
                temp = dlapy2( workl(iheigr+k-1), workl(iheigi+k-1) )
                if (temp * temp .le. eps) then
                    workl(iheigr+k-1) = workl(iheigr+k-1) / eps + sigmar
                    workl(iheigi+k-1) = -workl(iheigi+k-1) / eps + sigmai
                    if (msglvl .gt. 0) then
                        write(logfil,*)
                        write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                        write(logfil,*)'&         DNEUPD_2                 &'
                        write(logfil,*)'& DIV PAR EPS AU LIEU DE TEMP*2    &'
                        write(logfil,*)'& EPS    = ',eps
                        write(logfil,*)'& TEMP*2 = ',temp*temp
                        write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                        write(logfil,*)
                    endif
                else
                    workl(iheigr+k-1) = workl(iheigr+k-1 ) / temp / temp + sigmar
                    workl(iheigi+k-1) = -workl(iheigi+k-1 ) / temp / temp + sigmai
                endif
!
            end do
!
            call dcopy(nconv, workl(iheigr), 1, dr, 1)
            call dcopy(nconv, workl(iheigi), 1, di, 1)
!
        else if (type .eq. 'REALPT' .or. type .eq. 'IMAGPT') then
!
            call dcopy(nconv, workl(iheigr), 1, dr, 1)
            call dcopy(nconv, workl(iheigi), 1, di, 1)
!
        endif
!
    endif
!
    if (type .eq. 'SHIFTI' .and. msglvl .gt. 1) then
        call dvout(logfil, nconv, dr, ndigit,&
                   '_NEUPD: UNTRANSFORMED REAL PART OF THE RITZ VALUESS.')
        call dvout(logfil, nconv, di, ndigit,&
                   '_NEUPD: UNTRANSFORMED IMAG PART OF THE RITZ VALUESS.')
        call dvout(logfil, nconv, workl(ihbds), ndigit,&
                   '_NEUPD: RITZ ESTIMATES OF UNTRANSFORMED RITZ VALUES.')
    else if (type .eq. 'REGULR' .and. msglvl .gt. 1) then
        call dvout(logfil, nconv, dr, ndigit, '_NEUPD: REAL PARTS OF CONVERGED RITZ VALUES.')
        call dvout(logfil, nconv, di, ndigit, '_NEUPD: IMAG PARTS OF CONVERGED RITZ VALUES.')
        call dvout(logfil, nconv, workl(ihbds), ndigit, '_NEUPD: ASSOCIATED RITZ ESTIMATES.')
    endif
!
!     %-------------------------------------------------%
!     | EIGENVECTOR PURIFICATION STEP. FORMALLY PERFORM |
!     | ONE OF INVERSE SUBSPACE ITERATION. ONLY USED    |
!     | FOR MODE = 2.                                   |
!     %-------------------------------------------------%
!
    if (rvec .and. howmny .eq. 'A' .and. type .eq. 'SHIFTI') then
!
!        %------------------------------------------------%
!        | PURIFY THE COMPUTED RITZ VECTORS BY ADDING A   |
!        | LITTLE BIT OF THE RESIDUAL VECTOR:             |
!        |                      T                         |
!        |          RESID(:)*( E    S ) / THETA           |
!        |                      NCV                       |
!        | WHERE H S = S THETA. REMEMBER THAT WHEN THETA  |
!        | HAS NONZERO IMAGINARY PART, THE CORRESPONDING  |
!        | RITZ VECTOR IS STORED ACROSS TWO COLUMNS OF Z. |
!        %------------------------------------------------%
!
        iconj = 0
        do j = 1, nconv
            if (workl(iheigi+j-1) .eq. zero) then
                if (abs(workl(iheigr+j-1)) .le. eps) then
                    workev(j) = workl(invsub+(j-1)*ldq+ncv-1) / eps
                    if (msglvl .gt. 0) then
                        write(logfil,*)
                        write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                        write(logfil,*)'&         DNEUPD_3                 &'
                        write(logfil,*)'& DIV PAR EPS AU LIEU DE WORKL     &'
                        write(logfil,*)'& EPS    = ',eps
                        write(logfil,*)'& WORKL  = ',workl(iheigr+j-1)
                        write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                        write(logfil,*)
                    endif
                else
                    workev(j) = workl( invsub+(j-1)*ldq+ncv-1) / workl(iheigr+j-1)
                endif
!
            else if (iconj .eq. 0) then
                temp = dlapy2( workl(iheigr+j-1), workl(iheigi+j-1) )
!
                if (temp * temp .le. eps) then
                    workev(j) = (&
                                workl(&
                                invsub+(j-1)*ldq+ncv-1) * workl(iheigr+j-1) + workl(invsub+j*ldq+&
                                &ncv-1) * workl(iheigi+j-1&
                                )&
                                ) / eps
                    workev(j+1) = (&
                                  workl(invsub+j*ldq+ncv-1) * workl(iheigr+j-1) - workl(invsub+(j&
                                  &-1)*ldq+ncv-1) * workl(iheigi+j-1)&
                                  ) / eps
                    if (msglvl .gt. 0) then
                        write(logfil,*)
                        write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                        write(logfil,*)'&         DNEUPD_4                 &'
                        write(logfil,*)'& DIV PAR EPS AU LIEU DE TEMP*2    &'
                        write(logfil,*)'& EPS    = ',eps
                        write(logfil,*)'& TEMP*2 = ',temp*temp
                        write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                        write(logfil,*)
                    endif
                else
                    workev(j) = (&
                                workl(&
                                invsub+(j-1)*ldq+ncv-1) * workl(iheigr+j-1) + workl(invsub+j*ldq+&
                                &ncv-1) * workl(iheigi+j-1&
                                )&
                                ) / temp / temp
                    workev(j+1) = (&
                                  workl(invsub+j*ldq+ncv-1) * workl(iheigr+j-1) - workl(invsub+(j&
                                  &-1)*ldq+ncv-1) * workl(iheigi+j-1)&
                                  ) / temp / temp
                endif
                iconj = 1
            else
                iconj = 0
            endif
        end do
!
!        %---------------------------------------%
!        | PERFORM A RANK ONE UPDATE TO Z AND    |
!        | PURIFY ALL THE RITZ VECTORS TOGETHER. |
!        %---------------------------------------%
!
        call dger(n, nconv, one, resid, 1,&
                  workev, 1, z, ldz)
!
    endif
!
9000 continue
!
    call matfpe(1)
!
!     %---------------%
!     | END OF DNEUPD |
!     %---------------%
!
end subroutine
