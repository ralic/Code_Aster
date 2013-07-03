!---------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!
!     SUBROUTINE ARPACK CALCULANT LES MODES PROPRES DU PROBLEME
!     INITIAL.
!---------------------------------------------------------------------
!\BEGINDOC
!
!\NAME: ZNEUPD
!
!\DESCRIPTION:
!  THIS SUBROUTINE RETURNS THE CONVERGED APPROXIMATIONS TO EIGENVALUES
!  OF A*Z = LAMBDA*B*Z AND (OPTIONALLY):
!
!      (1) THE CORRESPONDING APPROXIMATE EIGENVECTORS;
!
!      (2) AN ORTHONORMAL BASIS FOR THE ASSOCIATED APPROXIMATE
!          INVARIANT SUBSPACE;
!
!      (3) BOTH.
!
!  THERE IS NEGLIGIBLE ADDITIONAL COST TO OBTAIN EIGENVECTORS.  AN
!  ORTHONORMAL
!  BASIS IS ALWAYS COMPUTED.  THERE IS AN ADDITIONAL STORAGE COST OF
!  N*NEV
!  IF BOTH ARE REQUESTED (IN THIS CASE A SEPARATE ARRAY Z MUST BE
!  SUPPLIED).
!
!  THE APPROXIMATE EIGENVALUES AND EIGENVECTORS OF  A*Z = LAMBDA*B*Z
!  ARE DERIVED FROM APPROXIMATE EIGENVALUES AND EIGENVECTORS OF
!  OF THE LINEAR OPERATOR OP PRESCRIBED BY THE MODE SELECTION IN THE
!  CALL TO ZNAUPD .  ZNAUPD  MUST BE CALLED BEFORE THIS ROUTINE IS
!  CALLED.
!  THESE APPROXIMATE EIGENVALUES AND VECTORS ARE COMMONLY CALLED RITZ
!  VALUES AND RITZ VECTORS RESPECTIVELY.  THEY ARE REFERRED TO AS SUCH
!  IN THE COMMENTS THAT FOLLOW.   THE COMPUTED ORTHONORMAL BASIS FOR
!  THE
!  INVARIANT SUBSPACE CORRESPONDING TO THESE RITZ VALUES IS REFERRED TO
!  AS A SCHUR BASIS.
!
!  THE DEFINITION OF OP AS WELL AS OTHER TERMS AND THE RELATION OF
!  COMPUTED
!  RITZ VALUES AND VECTORS OF OP WITH RESPECT TO THE GIVEN PROBLEM
!  A*Z = LAMBDA*B*Z MAY BE FOUND IN THE HEADER OF ZNAUPD .  FOR A BRIEF
!  DESCRIPTION, SEE DEFINITIONS OF IPARAM(7), MODE AND WHICH IN THE
!  DOCUMENTATION OF ZNAUPD .
!
!\USAGE:
!  CALL ZNEUPD
!     ( RVEC, HOWMNY, SELECT, D, Z, LDZ, SIGMA, WORKEV, BMAT,
!       N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD,
!       WORKL, LWORKL, RWORK, INFO )
!
!\ARGUMENTS:
!  RVEC    LOGICAL  (INPUT)
!          SPECIFIES WHETHER A BASIS FOR THE INVARIANT SUBSPACE
!          CORRESPONDING
!          TO THE CONVERGED RITZ VALUE APPROXIMATIONS FOR THE
!          EIGENPROBLEM
!          A*Z = LAMBDA*B*Z IS COMPUTED.
!
!             RVEC = .FALSE.     COMPUTE RITZ VALUES ONLY.
!
!             RVEC = .TRUE.      COMPUTE RITZ VECTORS OR SCHUR VECTORS.
!                                SEE REMARKS BELOW.
!
!  HOWMNY  CHARACTER*1  (INPUT)
!          SPECIFIES THE FORM OF THE BASIS FOR THE INVARIANT SUBSPACE
!          CORRESPONDING TO THE CONVERGED RITZ VALUES THAT IS TO BE
!          COMPUTED.
!
!          = 'A': COMPUTE NEV RITZ VECTORS;
!          = 'P': COMPUTE NEV SCHUR VECTORS;
!          = 'S': COMPUTE SOME OF THE RITZ VECTORS, SPECIFIED
!                 BY THE LOGICAL ARRAY SELECT.
!
!  SELECT  LOGICAL ARRAY OF DIMENSION NCV.  (INPUT)
!          IF HOWMNY = 'S', SELECT SPECIFIES THE RITZ VECTORS TO BE
!          COMPUTED. TO SELECT THE  RITZ VECTOR CORRESPONDING TO A
!          RITZ VALUE D(J), SELECT(J) MUST BE SET TO .TRUE..
!          IF HOWMNY = 'A' OR 'P', SELECT NEED NOT BE INITIALIZED
!          BUT IT IS USED AS INTERNAL WORKSPACE.
!
!  D       COMPLEX*16  ARRAY OF DIMENSION NEV+1.  (OUTPUT)
!          ON EXIT, D CONTAINS THE  RITZ  APPROXIMATIONS
!          TO THE EIGENVALUES LAMBDA FOR A*Z = LAMBDA*B*Z.
!
!  Z       COMPLEX*16  N BY NEV ARRAY    (OUTPUT)
!          ON EXIT, IF RVEC = .TRUE. AND HOWMNY = 'A', THEN THE COLUMNS
!          OF Z REPRESENTS APPROXIMATE EIGENVECTORS (RITZ VECTORS)
!          CORRESPONDING
!          TO THE NCONV=IPARAM(5) RITZ VALUES FOR EIGENSYSTEM
!          A*Z = LAMBDA*B*Z.
!
!          IF RVEC = .FALSE. OR HOWMNY = 'P', THEN Z IS NOT REFERENCED.
!
!          NOTE: IF IF RVEC = .TRUE. AND A SCHUR BASIS IS NOT REQUIRED,
!          THE ARRAY Z MAY BE SET EQUAL TO FIRST NEV+1 COLUMNS OF THE
!          ARNOLDI
!          BASIS ARRAY V COMPUTED BY ZNAUPD .  IN THIS CASE THE ARNOLDI
!          BASIS WILL BE DESTROYED AND OVERWRITTEN WITH THE EIGENVECTOR
!          BASIS.
!
!  LDZ     INTEGER.  (INPUT)
!          THE LEADING DIMENSION OF THE ARRAY Z.  IF RITZ VECTORS ARE
!          DESIRED, THEN  LDZ .GE.  MAX( 1, N ) IS REQUIRED.
!          IN ANY CASE,  LDZ .GE. 1 IS REQUIRED.
!
!  SIGMA   COMPLEX*16   (INPUT)
!          IF IPARAM(7) = 3 THEN SIGMA REPRESENTS THE SHIFT.
!          NOT REFERENCED IF IPARAM(7) = 1 OR 2.
!
!  WORKEV  COMPLEX*16  WORK ARRAY OF DIMENSION 2*NCV.  (WORKSPACE)
!
!  **** THE REMAINING ARGUMENTS MUST BE THE SAME AS FOR THE   ****
!  **** CALL TO ZNAUPD  THAT WAS JUST COMPLETED.               ****
!
!  NOTE: THE REMAINING ARGUMENTS
!
!           BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR,
!           WORKD, WORKL, LWORKL, RWORK, INFO
!
!         MUST BE PASSED DIRECTLY TO ZNEUPD  FOLLOWING THE LAST CALL
!         TO ZNAUPD .  THESE ARGUMENTS MUST NOT BE MODIFIED BETWEEN
!         THE THE LAST CALL TO ZNAUPD  AND THE CALL TO ZNEUPD .
!
!  THREE OF THESE PARAMETERS (V, WORKL AND INFO) ARE ALSO OUTPUT
!  PARAMETERS:
!
!  V       COMPLEX*16  N BY NCV ARRAY.  (INPUT/OUTPUT)
!
!          UPON INPUT: THE NCV COLUMNS OF V CONTAIN THE ARNOLDI BASIS
!                      VECTORS FOR OP AS CONSTRUCTED BY ZNAUPD  .
!
!          UPON OUTPUT: IF RVEC = .TRUE. THE FIRST NCONV=IPARAM(5)
!                       COLUMNS
!                       CONTAIN APPROXIMATE SCHUR VECTORS THAT SPAN THE
!                       DESIRED INVARIANT SUBSPACE.
!
!          NOTE: IF THE ARRAY Z HAS BEEN SET EQUAL TO FIRST NEV+1
!          COLUMNS
!          OF THE ARRAY V AND RVEC=.TRUE. AND HOWMNY= 'A', THEN THE
!          ARNOLDI BASIS HELD BY V HAS BEEN OVERWRITTEN BY THE DESIRED
!          RITZ VECTORS.  IF A SEPARATE ARRAY Z HAS BEEN PASSED THEN
!          THE FIRST NCONV=IPARAM(5) COLUMNS OF V WILL CONTAIN
!          APPROXIMATE
!          SCHUR VECTORS THAT SPAN THE DESIRED INVARIANT SUBSPACE.
!
!  WORKL   DOUBLE PRECISION  WORK ARRAY OF LENGTH LWORKL.
!          (OUTPUT/WORKSPACE)
!          WORKL(1:NCV*NCV+2*NCV) CONTAINS INFORMATION OBTAINED IN
!          ZNAUPD .  THEY ARE NOT CHANGED BY ZNEUPD .
!          WORKL(NCV*NCV+2*NCV+1:3*NCV*NCV+4*NCV) HOLDS THE
!          UNTRANSFORMED RITZ VALUES, THE UNTRANSFORMED ERROR
!          ESTIMATES OF
!          THE RITZ VALUES, THE UPPER TRIANGULAR MATRIX FOR H, AND THE
!          ASSOCIATED MATRIX REPRESENTATION OF THE INVARIANT SUBSPACE
!          FOR H.
!
!          NOTE: IPNTR(9:13) CONTAINS THE POINTER INTO WORKL FOR
!          ADDRESSES OF THE ABOVE INFORMATION COMPUTED BY ZNEUPD .
!         -------------------------------------------------------------
!          IPNTR(9):  POINTER TO THE NCV RITZ VALUES OF THE
!                     ORIGINAL SYSTEM.
!          IPNTR(10): NOT USED
!          IPNTR(11): POINTER TO THE NCV CORRESPONDING ERROR ESTIMATES.
!          IPNTR(12): POINTER TO THE NCV BY NCV UPPER TRIANGULAR
!                     SCHUR MATRIX FOR H.
!          IPNTR(13): POINTER TO THE NCV BY NCV MATRIX OF EIGENVECTORS
!                     OF THE UPPER HESSENBERG MATRIX H. ONLY REFERENCED
!                     BY ZNEUPD  IF RVEC = .TRUE. SEE REMARK 2 BELOW.
!        -------------------------------------------------------------
!
!  INFO    INTEGER.  (OUTPUT)
!          ERROR FLAG ON OUTPUT.
!          =  0: NORMAL EXIT.
!
!          =  1: THE SCHUR FORM COMPUTED BY LAPACK ROUTINE CSHEQR
!                COULD NOT BE REORDERED BY LAPACK ROUTINE GTRSEN .
!                RE-ENTER SUBROUTINE ZNEUPD  WITH IPARAM(5)=NCV AND
!                INCREASE THE SIZE OF THE ARRAY D TO HAVE
!                DIMENSION AT LEAST DIMENSION NCV AND ALLOCATE AT LEAST
!                NCV
subroutine zneupd(rvec, howmny, select, d, z,&
                  ldz, sigma, workev, bmat, n,&
                  which, nev, tol, resid, ncv,&
                  v, ldv, iparam, ipntr, workd,&
                  workl, lworkl, rwork, info)
!                COLUMNS FOR Z. NOTE: NOT NECESSARY IF Z AND V SHARE
!                THE SAME SPACE. PLEASE NOTIFY THE AUTHORS IF THIS ERROR
!                OCCURS.
!
!          = -1: N MUST BE POSITIVE.
!          = -2: NEV MUST BE POSITIVE.
!          = -3: NCV-NEV >= 2 AND LESS THAN OR EQUAL TO N.
!          = -5: WHICH MUST BE ONE OF 'LM', 'SM', 'LR', 'SR', 'LI', 'SI'
!          = -6: BMAT MUST BE ONE OF 'I' OR 'G'.
!          = -7: LENGTH OF PRIVATE WORK WORKL ARRAY IS NOT SUFFICIENT.
!          = -8: ERROR RETURN FROM LAPACK EIGENVALUE CALCULATION.
!                THIS SHOULD NEVER HAPPENED.
!          = -9: ERROR RETURN FROM CALCULATION OF EIGENVECTORS.
!                INFORMATIONAL ERROR FROM LAPACK ROUTINE GTREVC .
!          = -10: IPARAM(7) MUST BE 1,2,3
!          = -11: IPARAM(7) = 1 AND BMAT = 'G' ARE INCOMPATIBLE.
!          = -12: HOWMNY = 'S' NOT YET IMPLEMENTED
!          = -13: HOWMNY MUST BE ONE OF 'A' OR 'P' IF RVEC = .TRUE.
!          = -14: ZNAUPD  DID NOT FIND ANY EIGENVALUES TO SUFFICIENT
!                 ACCURACY.
!          = -15: ZNEUPD  GOT A DIFFERENT COUNT OF THE NUMBER OF
!                 CONVERGED RITZ VALUES THAN ZNAUPD  GOT.  THIS
!                 INDICATES THE USER
!                 PROBABLY MADE AN ERROR IN PASSING DATA FROM ZNAUPD TO
!                 ZNEUPD  OR THAT THE DATA WAS MODIFIED BEFORE ENTERING
!                 ZNEUPD
!
!\BEGINLIB
!
!\REFERENCES:
!  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
!     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
!     PP 357-385.
!  2. R.B. LEHOUCQ, "ANALYSIS AND IMPLEMENTATION OF AN IMPLICITLY
!     RESTARTED ARNOLDI ITERATION", RICE UNIVERSITY TECHNICAL REPORT
!     TR95-13, DEPARTMENT OF COMPUTATIONAL AND APPLIED MATHEMATICS.
!  3. B. NOUR-OMID, B. N. PARLETT, T. ERICSSON AND P. S. JENSEN,
!     "HOW TO IMPLEMENT THE SPECTRAL TRANSFORMATION", MATH COMP.,
!     VOL. 48, NO. 178, APRIL, 1987 PP. 664-673.
!
!\ROUTINES CALLED:
!     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
!     ZMOUT    ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
!     ZVOUT    ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
!     ZGEQR2   LAPACK ROUTINE THAT COMPUTES THE QR FACTORIZATION OF
!             A MATRIX.
!     ZLACPY   LAPACK MATRIX COPY ROUTINE.
!     ZLAHQR   LAPACK ROUTINE THAT COMPUTES THE SCHUR FORM OF A
!             UPPER HESSENBERG MATRIX.
!     ZLASET   LAPACK MATRIX INITIALIZATION ROUTINE.
!     GTREVC   LAPACK ROUTINE TO COMPUTE THE EIGENVECTORS OF A MATRIX
!             IN UPPER TRIANGULAR FORM.
!     GTRSEN   LAPACK ROUTINE THAT RE-ORDERS THE SCHUR FORM.
!     ZUNM2R   LAPACK ROUTINE THAT APPLIES AN ORTHOGONAL MATRIX IN
!             FACTORED FORM.
!     ZTRMM    LEVEL 3 BLAS MATRIX TIMES AN UPPER TRIANGULAR MATRIX.
!     ZGERU    LEVEL 2 BLAS RANK ONE UPDATE TO A MATRIX.
!     ZCOPY    LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
!     ZSCAL    LEVEL 1 BLAS THAT SCALES A VECTOR.
!     ZDSCAL   LEVEL 1 BLAS THAT SCALES A COMPLEX VECTOR BY A REAL
!              NUMBER.
!     DZNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A COMPLEX VECTOR.
!
!\REMARKS
!
!  1. CURRENTLY ONLY HOWMNY = 'A' AND 'P' ARE IMPLEMENTED.
!
!  2. SCHUR VECTORS ARE AN ORTHOGONAL REPRESENTATION FOR THE BASIS OF
!     RITZ VECTORS. THUS, THEIR NUMERICAL PROPERTIES ARE OFTEN SUPERIOR.
!     IF RVEC = .TRUE. THEN THE RELATIONSHIP
!             A * V(:,1:IPARAM(5)) = V(:,1:IPARAM(5)) * T, AND
!       TRANSPOSE( V(:,1:IPARAM(5)) ) * V(:,1:IPARAM(5)) = I
!     ARE APPROXIMATELY SATISFIED.
!     HERE T IS THE LEADING SUBMATRIX OF ORDER IPARAM(5) OF THE
!     UPPER TRIANGULAR MATRIX STORED WORKL(IPNTR(12)).
!
!\AUTHORS
!     DANNY SORENSEN               PHUONG VU
!     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
!     CHAO YANG                    HOUSTON, TEXAS
!     DEPT. OF COMPUTATIONAL&
!     APPLIED MATHEMATICS
!     RICE UNIVERSITY
!     HOUSTON, TEXAS
!
!\SCCS INFORMATION: @(#)
! FILE: NEUPD.F   SID: 2.7   DATE OF SID: 09/20/00   RELEASE: 2
!
!\ENDLIB
!
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1304,W1501,W1504
    implicit none
!
!     %-----------------------------%
!     | INCLUDE FILES FOR DEBUGGING |
!     %-----------------------------%
!
#include "asterc/matfpe.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/gtrevc.h"
#include "asterfort/gtrsen.h"
#include "asterfort/ivout.h"
#include "asterfort/zmout.h"
#include "asterfort/zngets.h"
#include "asterfort/zvout.h"
#include "blas/dlapy2.h"
#include "blas/dznrm2.h"
#include "blas/zcopy.h"
#include "blas/zdotc.h"
#include "blas/zdscal.h"
#include "blas/zgeqr2.h"
#include "blas/zgeru.h"
#include "blas/zlacpy.h"
#include "blas/zlahqr.h"
#include "blas/zlaset.h"
#include "blas/zscal.h"
#include "blas/ztrmm.h"
#include "blas/zunm2r.h"
    integer :: logfil, ndigit, mgetv0, mnaupd, mnaup2, mnaitr, mneigh, mnapps
    integer :: mngets, mneupd
    common /debug/&
     &  logfil, ndigit, mgetv0,&
     &  mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, mneupd
!
!
!     %------------------%
!     | SCALAR ARGUMENTS |
!     %------------------%
!
    character(len=1) :: bmat, howmny
    character(len=2) :: which
    logical :: rvec
    integer :: info, ldz, ldv, lworkl, n, ncv, nev
    complex(kind=8) :: sigma
    real(kind=8) :: tol
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    integer :: iparam(11), ipntr(14)
    logical :: select(*)
    real(kind=8) :: rwork(*)
    complex(kind=8) :: d(*), resid(*), v(ldv, *), z(ldz, *), workd(3*n)
    complex(kind=8) :: workl(lworkl), workev(2*ncv)
!
!     %------------%
!     | PARAMETERS |
!     %------------%
!
    complex(kind=8) :: one, zero
    parameter  (one = (1.0d+0, 0.0d+0) , zero = (0.0d+0, 0.0d+0) )
!
!     %---------------%
!     | LOCAL SCALARS |
!     %---------------%
!
    character(len=6) :: type
    integer(kind=4) :: ierr4
    integer :: bounds, ierr, ih, ihbds, iheig, nconv, invsub, iuptri, j, ldh
    integer :: ldq, mode, msglvl, ritz, k, irz, ibd, outncv, np, numcnv, jj
    integer :: ishift
    complex(kind=8) :: rnorm, temp, vl(1)
    real(kind=8) :: conds, sep, rtemp, eps23, eps
    logical :: reord
!
!
!     %--------------------%
!     | EXTERNAL FUNCTIONS |
!     %--------------------%
!
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
!
!     %---------------------------------%
!     | GET MACHINE DEPENDENT CONSTANT. |
!     %---------------------------------%
!
    eps23 = r8prem()*0.5d0
    eps23 = eps23**(2.0d+0 / 3.0d+0)
!
!     %-------------------------------%
!     | QUICK RETURN                  |
!     | CHECK FOR INCOMPATIBLE INPUT  |
!     %-------------------------------%
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
    else if (lworkl .lt. 3*ncv**2 + 4*ncv) then
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
    else if (mode .eq. 3) then
        type = 'SHIFTI'
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
!     | POINTER INTO WORKL FOR ADDRESS OF H, RITZ, WORKEV, Q   |
!     | ETC... AND THE REMAINING WORKSPACE.                    |
!     | ALSO UPDATE POINTER TO BE USED ON OUTPUT.              |
!     | MEMORY IS LAID OUT AS FOLLOWS:                         |
!     | WORKL(1:NCV*NCV) := GENERATED HESSENBERG MATRIX        |
!     | WORKL(NCV*NCV+1:NCV*NCV+NCV) := RITZ VALUES            |
!     | WORKL(NCV*NCV+NCV+1:NCV*NCV+2*NCV) := ERROR BOUNDS     |
!     %--------------------------------------------------------%
!
!     %-----------------------------------------------------------%
!     | THE FOLLOWING IS USED AND SET BY ZNEUPD .                 |
!     | WORKL(NCV*NCV+2*NCV+1:NCV*NCV+3*NCV) := THE UNTRANSFORMED |
!     |                                      RITZ VALUES.         |
!     | WORKL(NCV*NCV+3*NCV+1:NCV*NCV+4*NCV) := THE UNTRANSFORMED |
!     |                                      ERROR BOUNDS OF      |
!     |                                      THE RITZ VALUES      |
!     | WORKL(NCV*NCV+4*NCV+1:2*NCV*NCV+4*NCV) := HOLDS THE UPPER |
!     |                                      TRIANGULAR MATRIX    |
!     |                                      FOR H.               |
!     | WORKL(2*NCV*NCV+4*NCV+1: 3*NCV*NCV+4*NCV) := HOLDS THE    |
!     |                                      ASSOCIATED MATRIX    |
!     |                                      REPRESENTATION OF    |
!     |                                      THE INVARIANT        |
!     |                                      SUBSPACE FOR H.      |
!     | GRAND TOTAL OF NCV * ( 3 * NCV + 4 ) LOCATIONS.           |
!     %-----------------------------------------------------------%
!
    ih = ipntr(5)
    ritz = ipntr(6)
    bounds = ipntr(8)
    ldh = ncv
    ldq = ncv
    iheig = bounds + ldh
    ihbds = iheig + ldh
    iuptri = ihbds + ldh
    invsub = iuptri + ldh*ncv
    ipntr(9) = iheig
    ipntr(11) = ihbds
    ipntr(12) = iuptri
    ipntr(13) = invsub
!      WR = 1
!      IWEV = WR + NCV
!
!
!     %-----------------------------------------%
!     | IRZ POINTS TO THE RITZ VALUES COMPUTED  |
!     |     BY _NEIGH BEFORE EXITING _NAUP2.    |
!     | IBD POINTS TO THE RITZ ESTIMATES        |
!     |     COMPUTED BY _NEIGH BEFORE EXITING   |
!     |     _NAUP2.                             |
!     %-----------------------------------------%
!
    irz = ipntr(14) + ncv*ncv
    ibd = irz + ncv
!
!     %------------------------------------%
!     | RNORM IS B-NORM OF THE RESID(1:N). |
!     %------------------------------------%
!
    rnorm = workl(ih+2)
    workl(ih+2) = zero
!
    if (msglvl .gt. 2) then
        call zvout(logfil, ncv, workl(irz), ndigit, '_NEUPD: RITZ VALUES PASSED IN FROM _NAUPD.')
        call zvout(logfil, ncv, workl(ibd), ndigit,&
                   '_NEUPD: RITZ ESTIMATES PASSED IN FROM _NAUPD.')
    endif
!
    if (rvec) then
!
        reord = .false.
!
!        %---------------------------------------------------%
!        | USE THE TEMPORARY BOUNDS ARRAY TO STORE INDICES   |
!        | THESE WILL BE USED TO MARK THE SELECT ARRAY LATER |
!        %---------------------------------------------------%
!
        do 10 j = 1, ncv
            workl(bounds+j-1) = j
            select(j) = .false.
10      continue
!
!        %-------------------------------------%
!        | SELECT THE WANTED RITZ VALUES.      |
!        | SORT THE RITZ VALUES SO THAT THE    |
!        | WANTED ONES APPEAR AT THE TAILING   |
!        | NEV POSITIONS OF WORKL(IRR) AND     |
!        | WORKL(IRI).  MOVE THE CORRESPONDING |
!        | ERROR ESTIMATES IN WORKL(IBD)       |
!        | ACCORDINGLY.                        |
!        %-------------------------------------%
!
        np = ncv - nev
        ishift = 0
        call zngets(ishift, which, nev, np, workl(irz),&
                    workl( bounds))
!
        if (msglvl .gt. 2) then
            call zvout(logfil, ncv, workl(irz), ndigit,&
                       '_NEUPD: RITZ VALUES AFTER CALLING _NGETS.')
            call zvout(logfil, ncv, workl(bounds), ndigit,&
                       '_NEUPD: RITZ VALUE INDICES AFTER CALLING _NGETS.')
        endif
!
!        %-----------------------------------------------------%
!        | RECORD INDICES OF THE CONVERGED WANTED RITZ VALUES  |
!        | MARK THE SELECT ARRAY FOR POSSIBLE REORDERING       |
!        %-----------------------------------------------------%
!
        numcnv = 0
        do 11 j = 1, ncv
            rtemp = max( eps23, dlapy2 (dble (workl(irz+ncv-j)), dimag (workl(irz+ncv-j))) )
            jj = nint(dble(workl(bounds + ncv - j)))
            if (numcnv .lt. nconv .and.&
                dlapy2 (dble (workl(ibd+jj-1) ), dimag (workl(ibd+jj-1))) .le. tol*rtemp) then
                select(jj) = .true.
                numcnv = numcnv + 1
                if (jj .gt. nev) reord = .true.
            endif
11      continue
!
!        %-----------------------------------------------------------%
!        | CHECK THE COUNT (NUMCNV) OF CONVERGED RITZ VALUES WITH    |
!        | THE NUMBER (NCONV) REPORTED BY DNAUPD.  IF THESE TWO      |
!        | ARE DIFFERENT THEN THERE HAS PROBABLY BEEN AN ERROR       |
!        | CAUSED BY INCORRECT PASSING OF THE DNAUPD DATA.           |
!        %-----------------------------------------------------------%
!
        if (msglvl .gt. 2) then
            call ivout(logfil, 1, numcnv, ndigit, '_NEUPD: NUMBER OF SPECIFIED EIGENVALUES')
            call ivout(logfil, 1, nconv, ndigit, '_NEUPD: NUMBER OF "CONVERGED" EIGENVALUES')
        endif
!
        if (numcnv .ne. nconv) then
            info = -15
            goto 9000
        endif
!
!        %-------------------------------------------------------%
!        | CALL LAPACK ROUTINE ZLAHQR  TO COMPUTE THE SCHUR FORM |
!        | OF THE UPPER HESSENBERG MATRIX RETURNED BY ZNAUPD .   |
!        | MAKE A COPY OF THE UPPER HESSENBERG MATRIX.           |
!        | INITIALIZE THE SCHUR VECTOR MATRIX Q TO THE IDENTITY. |
!        %-------------------------------------------------------%
!
        call zcopy(ldh*ncv, workl(ih), 1, workl(iuptri), 1)
        call zlaset('A', ncv, ncv, zero, one,&
                    workl(invsub), ldq)
        call zlahqr(.true., .true., ncv, 1, ncv,&
                    workl(iuptri), ldh, workl(iheig), 1, ncv,&
                    workl(invsub), ldq, ierr4)
        ierr=ierr4
        call zcopy(ncv, workl(invsub+ncv-1), ldq, workl(ihbds), 1)
!
        if (ierr .ne. 0) then
            info = -8
            goto 9000
        endif
!
        if (msglvl .gt. 1) then
            call zvout(logfil, ncv, workl(iheig), ndigit, '_NEUPD: EIGENVALUES OF H')
            call zvout(logfil, ncv, workl(ihbds), ndigit,&
                       '_NEUPD: LAST ROW OF THE SCHUR VECTOR MATRIX')
            if (msglvl .gt. 3) then
                call zmout(logfil, ncv, ncv, workl(iuptri), ldh,&
                           ndigit, '_NEUPD: THE UPPER TRIANGULAR MATRIX ')
            endif
        endif
!
        if (reord) then
!
!           %-----------------------------------------------%
!           | REORDER THE COMPUTED UPPER TRIANGULAR MATRIX. |
!           %-----------------------------------------------%
!
            call gtrsen('N', 'V', select, ncv, workl(iuptri),&
                        ldh, workl(invsub), ldq, workl(iheig), nconv,&
                        conds, sep, workev, ncv, ierr)
!
            if (ierr .eq. 1) then
                info = 1
                goto 9000
            endif
!
            if (msglvl .gt. 2) then
                call zvout(logfil, ncv, workl(iheig), ndigit,&
                           '_NEUPD: EIGENVALUES OF H--REORDERED')
                if (msglvl .gt. 3) then
                    call zmout(logfil, ncv, ncv, workl(iuptri), ldq,&
                               ndigit, '_NEUPD: TRIANGULAR MATRIX AFTER RE-ORDERING')
                endif
            endif
!
        endif
!
!        %---------------------------------------------%
!        | COPY THE LAST ROW OF THE SCHUR BASIS MATRIX |
!        | TO WORKL(IHBDS).  THIS VECTOR WILL BE USED  |
!        | TO COMPUTE THE RITZ ESTIMATES OF CONVERGED  |
!        | RITZ VALUES.                                |
!        %---------------------------------------------%
!
        call zcopy(ncv, workl(invsub+ncv-1), ldq, workl(ihbds), 1)
!
!        %--------------------------------------------%
!        | PLACE THE COMPUTED EIGENVALUES OF H INTO D |
!        | IF A SPECTRAL TRANSFORMATION WAS NOT USED. |
!        %--------------------------------------------%
!
        if (type .eq. 'REGULR') then
            call zcopy(nconv, workl(iheig), 1, d, 1)
        endif
!
!        %----------------------------------------------------------%
!        | COMPUTE THE QR FACTORIZATION OF THE MATRIX REPRESENTING  |
!        | THE WANTED INVARIANT SUBSPACE LOCATED IN THE FIRST NCONV |
!        | COLUMNS OF WORKL(INVSUB,LDQ).                            |
!        %----------------------------------------------------------%
!
        call zgeqr2(ncv, nconv, workl(invsub), ldq, workev,&
                    workev(ncv+1), ierr4)
        ierr=ierr4
!
!        %--------------------------------------------------------%
!        | * POSTMULTIPLY V BY Q USING ZUNM2R .                    |
!        | * COPY THE FIRST NCONV COLUMNS OF VQ INTO Z.           |
!        | * POSTMULTIPLY Z BY R.                                 |
!        | THE N BY NCONV MATRIX Z IS NOW A MATRIX REPRESENTATION |
!        | OF THE APPROXIMATE INVARIANT SUBSPACE ASSOCIATED WITH  |
!        | THE RITZ VALUES IN WORKL(IHEIG). THE FIRST NCONV       |
!        | COLUMNS OF V ARE NOW APPROXIMATE SCHUR VECTORS         |
!        | ASSOCIATED WITH THE UPPER TRIANGULAR MATRIX OF ORDER   |
!        | NCONV IN WORKL(IUPTRI).                                |
!        %--------------------------------------------------------%
!
        call zunm2r('R', 'N', n, ncv, nconv,&
                    workl(invsub), ldq, workev, v, ldv,&
                    workd(n+1), ierr4)
        ierr=ierr4
        call zlacpy('A', n, nconv, v, ldv,&
                    z, ldz)
!
        do 20 j = 1, nconv
!
!           %---------------------------------------------------%
!           | PERFORM BOTH A COLUMN AND ROW SCALING IF THE      |
!           | DIAGONAL ELEMENT OF WORKL(INVSUB,LDQ) IS NEGATIVE |
!           | I'M LAZY AND DON'T TAKE ADVANTAGE OF THE UPPER    |
!           | TRIANGULAR FORM OF WORKL(IUPTRI,LDQ).             |
!           | NOTE THAT SINCE Q IS ORTHOGONAL, R IS A DIAGONAL  |
!           | MATRIX CONSISTING OF PLUS OR MINUS ONES.          |
!           %---------------------------------------------------%
!
            if (dble ( workl(invsub+(j-1)*ldq+j-1) ) .lt. dble (zero)) then
                call zscal(nconv, -one, workl(iuptri+j-1), ldq)
                call zscal(nconv, -one, workl(iuptri+(j-1)*ldq), 1)
            endif
!
20      continue
!
        if (howmny .eq. 'A') then
!
!           %--------------------------------------------%
!           | COMPUTE THE NCONV WANTED EIGENVECTORS OF T |
!           | LOCATED IN WORKL(IUPTRI,LDQ).              |
!           %--------------------------------------------%
!
            do 30 j = 1, ncv
                if (j .le. nconv) then
                    select(j) = .true.
                else
                    select(j) = .false.
                endif
30          continue
!
            call gtrevc('R', 'S', select, ncv, workl(iuptri),&
                        ldq, vl, 1, workl(invsub), ldq,&
                        ncv, outncv, workev, rwork, ierr)
!
            if (ierr .ne. 0) then
                info = -9
                goto 9000
            endif
!
!           %------------------------------------------------%
!           | SCALE THE RETURNING EIGENVECTORS SO THAT THEIR |
!           | EUCLIDEAN NORMS ARE ALL ONE. LAPACK SUBROUTINE |
!           | GTREVC  RETURNS EACH EIGENVECTOR NORMALIZED SO  |
!           | THAT THE ELEMENT OF LARGEST MAGNITUDE HAS      |
!           | MAGNITUDE 1.                                   |
!           %------------------------------------------------%
!
            do 40 j = 1, nconv
                rtemp = dznrm2 (ncv, workl(invsub+(j-1)*ldq), 1)
                rtemp = dble (one) / rtemp
                call zdscal(ncv, rtemp, workl(invsub+(j-1)*ldq), 1)
!
!                 %------------------------------------------%
!                 | RITZ ESTIMATES CAN BE OBTAINED BY TAKING |
!                 | THE INNER PRODUCT OF THE LAST ROW OF THE |
!                 | SCHUR BASIS OF H WITH EIGENVECTORS OF T. |
!                 | NOTE THAT THE EIGENVECTOR MATRIX OF T IS |
!                 | UPPER TRIANGULAR, THUS THE LENGTH OF THE |
!                 | INNER PRODUCT CAN BE SET TO J.           |
!                 %------------------------------------------%
!
                workev(j) = zdotc ( j, workl(ihbds), 1, workl(invsub+( j-1)*ldq), 1)
40          continue
!
            if (msglvl .gt. 2) then
                call zcopy(nconv, workl(invsub+ncv-1), ldq, workl( ihbds), 1)
                call zvout(logfil, nconv, workl(ihbds), ndigit,&
                           '_NEUPD: LAST ROW OF THE EIGENVECTOR MATRIX FOR T')
                if (msglvl .gt. 3) then
                    call zmout(logfil, ncv, ncv, workl(invsub), ldq,&
                               ndigit, '_NEUPD: THE EIGENVECTOR MATRIX FOR T')
                endif
            endif
!
!           %---------------------------------------%
!           | COPY RITZ ESTIMATES INTO WORKL(IHBDS) |
!           %---------------------------------------%
!
            call zcopy(nconv, workev, 1, workl(ihbds), 1)
!
!           %----------------------------------------------%
!           | THE EIGENVECTOR MATRIX Q OF T IS TRIANGULAR. |
!           | FORM Z*Q.                                    |
!           %----------------------------------------------%
!
            call ztrmm('R', 'U', 'N', 'N', n,&
                       nconv, one, workl( invsub), ldq, z,&
                       ldz)
        endif
!
    else
!
!        %--------------------------------------------------%
!        | AN APPROXIMATE INVARIANT SUBSPACE IS NOT NEEDED. |
!        | PLACE THE RITZ VALUES COMPUTED ZNAUPD  INTO D.    |
!        %--------------------------------------------------%
!
        call zcopy(nconv, workl(ritz), 1, d, 1)
        call zcopy(nconv, workl(ritz), 1, workl(iheig), 1)
        call zcopy(nconv, workl(bounds), 1, workl(ihbds), 1)
!
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
        if (rvec) call zscal(ncv, rnorm, workl(ihbds), 1)
!
    else
!
!        %---------------------------------------%
!        |   A SPECTRAL TRANSFORMATION WAS USED. |
!        | * DETERMINE THE RITZ ESTIMATES OF THE |
!        |   RITZ VALUES IN THE ORIGINAL SYSTEM. |
!        %---------------------------------------%
!
        if (rvec) call zscal(ncv, rnorm, workl(ihbds), 1)
!
        do 50 k = 1, ncv
            temp = workl(iheig+k-1)
            if (abs(temp * temp) .le. eps) then
                if (msglvl .gt. 0) then
                    write(logfil,*)
                    write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                    write(logfil,*)'&         ZNEUPD_1                 &'
                    write(logfil,*)'& DIV PAR EPS AU LIEU DE TEMP*2    &'
                    write(logfil,*)'& EPS    = ',eps
                    write(logfil,*)'& TEMP*2 = ',temp*temp
                    write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                    write(logfil,*)
                endif
                workl(ihbds+k-1) = workl(ihbds+k-1)/eps
            else
                workl(ihbds+k-1) = workl(ihbds+k-1) / temp / temp
            endif
50      continue
!
    endif
!
!     %-----------------------------------------------------------%
!     | *  TRANSFORM THE RITZ VALUES BACK TO THE ORIGINAL SYSTEM. |
!     |    FOR TYPE = 'SHIFTI' THE TRANSFORMATION IS              |
!     |             LAMBDA = 1/THETA + SIGMA                      |
!     | NOTES:                                                    |
!     | *THE RITZ VECTORS ARE NOT AFFECTED BY THE TRANSFORMATION. |
!     %-----------------------------------------------------------%
!
    if (type .eq. 'SHIFTI') then
        do 60 k = 1, nconv
            d(k) = one / workl(iheig+k-1) + sigma
60      continue
    endif
!
    if (type .ne. 'REGULR' .and. msglvl .gt. 1) then
        call zvout(logfil, nconv, d, ndigit, '_NEUPD: UNTRANSFORMED RITZ VALUES.')
        call zvout(logfil, nconv, workl(ihbds), ndigit,&
                   '_NEUPD: RITZ ESTIMATES OF THE UNTRANSFORMED RITZ VALUES.')
    else if (msglvl .gt. 1) then
        call zvout(logfil, nconv, d, ndigit, '_NEUPD: CONVERGED RITZ VALUES.')
        call zvout(logfil, nconv, workl(ihbds), ndigit, '_NEUPD: ASSOCIATED RITZ ESTIMATES.')
    endif
!
!     %-------------------------------------------------%
!     | EIGENVECTOR PURIFICATION STEP. FORMALLY PERFORM |
!     | ONE OF INVERSE SUBSPACE ITERATION. ONLY USED    |
!     | FOR MODE = 3. SEE REFERENCE 3.                  |
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
!        | WHERE H S = S THETA.                           |
!        %------------------------------------------------%
!
        do 100 j = 1, nconv
            if (workl(iheig+j-1) .ne. zero) then
                workev(j) = workl(invsub+(j-1)*ldq+ncv-1) / workl( iheig+j-1)
            endif
100      continue
!
!        %---------------------------------------%
!        | PERFORM A RANK ONE UPDATE TO Z AND    |
!        | PURIFY ALL THE RITZ VECTORS TOGETHER. |
!        %---------------------------------------%
!
        call zgeru(n, nconv, one, resid, 1,&
                   workev, 1, z, ldz)
!
    endif
!
9000  continue
    call matfpe(1)
!
!
!     %---------------%
!     | END OF ZNEUPD |
!     %---------------%
!
end subroutine
