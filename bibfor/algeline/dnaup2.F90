subroutine dnaup2(ido, bmat, n, which, nev,&
                  np, tol, resid, ishift, mxiter,&
                  v, ldv, h, ldh, ritzr,&
                  ritzi, bounds, q, ldq, workl,&
                  ipntr, workd, info, neqact, alpha)
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
!     SUBROUTINE ARPACK CALCULANT LES VALEURS PROPRES DE (OP) VIA
!     IRAM.
!---------------------------------------------------------------------
! BEGINDOC
!
! DESCRIPTION:
!  INTERMEDIATE LEVEL INTERFACE CALLED BY DNAUPD.
!
! ARGUMENTS
!
!  IDO, BMAT, N, WHICH, NEV, TOL, RESID: SAME AS DEFINED IN DNAUPD.
!  ISHIFT, MXITER: SEE THE DEFINITION OF IPARAM IN DNAUPD.
!
!  NP    INTEGER.  (INPUT/OUTPUT)
!        CONTAINS THE NUMBER OF IMPLICIT SHIFTS TO APPLY DURING
!        EACH ARNOLDI ITERATION.
!        IF ISHIFT=1, NP IS ADJUSTED DYNAMICALLY AT EACH ITERATION
!        TO ACCELERATE CONVERGENCE AND PREVENT STAGNATION.
!        THIS IS ALSO ROUGHLY EQUAL TO THE NUMBER OF MATRIX-VECTOR
!        PRODUCTS (INVOLVING THE OPERATOR OP) PER ARNOLDI ITERATION.
!        THE LOGIC FOR ADJUSTING IS CONTAINED WITHIN THE CURRENT
!        SUBROUTINE.
!        IF ISHIFT=0, NP IS THE NUMBER OF SHIFTS THE USER NEEDS
!        TO PROVIDE VIA REVERSE COMUNICATION. 0 < NP < NCV-NEV.
!        NP MAY BE LESS THAN NCV-NEV FOR TWO REASONS. THE FIRST, IS
!        TO KEEP COMPLEX CONJUGATE PAIRS OF "WANTED" RITZ VALUES
!        TOGETHER. THE SECOND, IS THAT A LEADING BLOCK OF THE CURRENT
!        UPPER HESSENBERG MATRIX HAS SPLIT OFF AND CONTAINS "UNWANTED"
!        RITZ VALUES.
!        UPON TERMINATION OF THE IRA ITERATION, NP CONTAINS NUMBER
!        OF "CONVERGED" WANTED RITZ VALUES.
!
!  V     DOUBLE PRECISION N BY (NEV+NP) ARRAY.  (INPUT/OUTPUT)
!        THE ARNOLDI BASIS VECTORS ARE RETURNED IN THE FIRST NEV
!        COLUMNS OF V.
!
!  LDV   INTEGER.  (INPUT)
!        LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
!        PROGRAM.
!
!  H     DOUBLE PRECISION (NEV+NP) BY (NEV+NP) ARRAY.  (OUTPUT)
!        H IS USED TO STORE THE GENERATED UPPER HESSENBERG MATRIX
!
!  LDH   INTEGER.  (INPUT)
!        LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
!        PROGRAM.
!
!  RITZR, DOUBLE PRECISION ARRAYS OF LENGTH NEV+NP.  (OUTPUT)
!  RITZI  RITZR(1:NEV) (RESP. RITZI(1:NEV)) CONTAINS THE REAL (RESP.
!         IMAGINARY) PART OF THE COMPUTED RITZ VALUES OF OP.
!
!  BOUNDS DOUBLE PRECISION ARRAY OF LENGTH NEV+NP.  (OUTPUT)
!         BOUNDS(1:NEV) CONTAIN THE ERROR BOUNDS CORRESPONDING TO
!         THE COMPUTED RITZ VALUES.
!
!  Q     DOUBLE PRECISION (NEV+NP) BY (NEV+NP) ARRAY.  (WORKSPACE)
!        PRIVATE (REPLICATED) WORK ARRAY USED TO ACCUMULATE THE
!        ROTATION IN THE SHIFT APPLICATION STEP.
!
!  LDQ   INTEGER.  (INPUT)
!        LEADING DIMENSION OF Q EXACTLY AS DECLARED IN THE CALLING
!        PROGRAM.
!
!  WORKL DOUBLE PRECISION WORK ARRAY OF LENGTH AT LEAST
!        (NEV+NP)**2 + 3*(NEV+NP).  (INPUT/WORKSPACE)
!        PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
!        THE FRONT END.  IT IS USED IN SHIFTS CALCULATION, SHIFTS
!        APPLICATION AND CONVERGENCE CHECKING.
!
!        ON EXIT, THE LAST 3*(NEV+NP) LOCATIONS OF WORKL CONTAIN
!        THE RITZ VALUES (REAL,IMAGINARY) AND ASSOCIATED RITZ
!        ESTIMATES OF THE CURRENT HESSENBERG MATRIX.  THEY ARE
!        LISTED IN THE SAME ORDER AS RETURNED FROM DNEIGH.
!
!        IF ISHIFT .EQ. O AND IDO .EQ. 3, THE FIRST 2*NP LOCATIONS
!        OF WORKL ARE USED IN REVERSE COMMUNICATION TO HOLD THE USER
!        SUPPLIED SHIFTS.
!
!  IPNTR INTEGER ARRAY OF LENGTH 3.  (OUTPUT)
!        POINTER TO MARK THE STARTING LOCATIONS IN THE WORKD FOR
!        VECTORS USED BY THE ARNOLDI ITERATION.
!        ----------------------------------------------------------
!        IPNTR(1): POINTER TO THE CURRENT OPERAND VECTOR X.
!        IPNTR(2): POINTER TO THE CURRENT RESULT VECTOR Y.
!        IPNTR(3): POINTER TO THE VECTOR B * X WHEN USED IN THE
!                  SHIFT-AND-INVERT MODE.  X IS THE CURRENT OPERAND.
!        ----------------------------------------------------------
!
!  WORKD DOUBLE PRECISION WORK ARRAY OF LENGTH 3*N.  (WORKSPACE)
!        DISTRIBUTED ARRAY TO BE USED IN THE BASIC ARNOLDI ITERATION
!        FOR REVERSE COMMUNICATION.  THE USER SHOULD NOT USE WORKD
!        AS TEMPORARY WORKSPACE DURING THE ITERATION !!!!!!!!!!
!        SEE DATA DISTRIBUTION NOTE IN DNAUPD.
!
!  INFO  INTEGER.  (INPUT/OUTPUT)
!        IF INFO .EQ. 0, A RANDOMLY INITIAL RESIDUAL VECTOR IS USED.
!        IF INFO .NE. 0, RESID CONTAINS THE INITIAL RESIDUAL VECTOR,
!                        POSSIBLY FROM A PREVIOUS RUN.
!        ERROR FLAG ON OUTPUT.
!        =     0: NORMAL RETURN.
!        =     1: MAXIMUM NUMBER OF ITERATIONS TAKEN.
!                 ALL POSSIBLE EIGENVALUES OF OP HAS BEEN FOUND.
!                 NP RETURNS THE NUMBER OF CONVERGED RITZ VALUES.
!        =     2: NO SHIFTS COULD BE APPLIED.
!        =    -8: ERROR RETURN FROM LAPACK EIGENVALUE CALCULATION,
!                 THIS SHOULD NEVER HAPPEN.
!        =    -9: STARTING VECTOR IS ZERO.
!        = -9999: COULD NOT BUILD AN ARNOLDI FACTORIZATION.
!                 SIZE THAT WAS BUILT IN RETURNED IN NP.
!
!  NEQACT INTEGER  (INPUT/ NEW PARAMETER INTRODUCED BY ASTER)
!         NUMBER OF PHYSICAL DEGREE OF FREEDOM
!
!  ALPHA   REAL  (INPUT/ NEW PARAMETER INTRODUCED FOR ASTER)
!          ORTHONORMALISATION PARAMETER FOR KAHAN-PARLETT ALGORITHM
!
! ENDDOC
!-----------------------------------------------------------------------
! BEGINLIB
!
! REFERENCES:
!  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
!     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
!     PP 357-385.
!  2. R.B. LEHOUCQ, "ANALYSIS AND IMPLEMENTATION OF AN IMPLICITLY
!     RESTARTED ARNOLDI ITERATION", RICE UNIVERSITY TECHNICAL REPORT
!     TR95-13, DEPARTMENT OF COMPUTATIONAL AND APPLIED MATHEMATICS.
!
! ROUTINES CALLED:
!     DGETV0  ARPACK INITIAL VECTOR GENERATION ROUTINE.
!     DNAITR  ARPACK ARNOLDI FACTORIZATION ROUTINE.
!     DNAPPS  ARPACK APPLICATION OF IMPLICIT SHIFTS ROUTINE.
!     DNCONV  ARPACK CONVERGENCE OF RITZ VALUES ROUTINE.
!     DNEIGH  ARPACK COMPUTE RITZ VALUES AND ERROR BOUNDS ROUTINE.
!     DNGETS  ARPACK REORDER RITZ VALUES AND ERROR BOUNDS ROUTINE.
!     DSORTC  ARPACK SORTING ROUTINE.
!     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
!     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
!     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
!     DLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
!     DCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
!     DDOT    LEVEL 1 BLAS THAT COMPUTES THE SCALAR PRODUCT.
!     DNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
!     DSWAP   LEVEL 1 BLAS THAT SWAPS TWO VECTORS.
!
!     R8PREM  ASTER UTILITY ROUTINE THAT GIVES MACHINE PRECISION
!
! INTRINSIC FUNCTIONS
!
!     MIN, MAX, ABS, SQRT
!
! AUTHOR
!     DANNY SORENSEN               PHUONG VU
!     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
!     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
!     APPLIED MATHEMATICS
!     RICE UNIVERSITY
!     HOUSTON, TEXAS
!
! FILE: NAUP2.F   SID: 2.4   DATE OF SID: 7/30/96   RELEASE: 2
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE SECOND ET DLAMCH,
!            COMMON TIMING REMPLACE PAR COMMON INFOR,
!            RAJOUT DU PARAMETRE NEQACT,
!            NOUVEAU MSG POUR ESPACE INVARIANT,
!            UTILISATION DE R8PREM(),
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
!
!     %-----------------------------%
!     | INCLUDE FILES FOR DEBUGGING |
!     %-----------------------------%
!
#include "asterc/matfpe.h"
#include "asterc/r8prem.h"
#include "asterfort/dgetv0.h"
#include "asterfort/dmout.h"
#include "asterfort/dnaitr.h"
#include "asterfort/dnapps.h"
#include "asterfort/dnconv.h"
#include "asterfort/dneigh.h"
#include "asterfort/dngets.h"
#include "asterfort/dsortc.h"
#include "asterfort/dvout.h"
#include "asterfort/ivout.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dlapy2.h"
#include "blas/dnrm2.h"
    integer :: logfil, ndigit, mgetv0, mnaupd, mnaup2, mnaitr, mneigh, mnapps
    integer :: mngets, mneupd
    common /debug/&
     &  logfil, ndigit, mgetv0,&
     &  mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, mneupd
    integer :: nopx, nbx, nrorth, nitref, nrstrt
    common /infor/&
     &  nopx, nbx, nrorth, nitref, nrstrt
!
!     %------------------%
!     | SCALAR ARGUMENTS |
!     %------------------%
!
    character(len=1) :: bmat
    character(len=2) :: which
    integer :: ido, info, ishift, ldh, ldq, ldv, mxiter, n, nev, np, neqact
    real(kind=8) :: tol, alpha
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    integer :: ipntr(13)
    real(kind=8) :: bounds(nev+np), h(ldh, nev+np), q(ldq, nev+np), resid(n)
    real(kind=8) :: ritzi(nev+np), ritzr(nev+np), v(ldv, nev+np), workd(3*n)
    real(kind=8) :: workl( (nev+np)*(nev+np+3) )
!
!     %------------%
!     | PARAMETERS |
!     %------------%
!
    real(kind=8) :: zero
    parameter (zero = 0.0d+0)
!
!     %---------------%
!     | LOCAL SCALARS |
!     %---------------%
!
    character(len=2) :: wprime
    logical :: cnorm, getv0, initv, update, ushift
    integer :: ierr, iter, j, kplusp, msglvl, nconv, nevbef, nev0, np0, nptemp
    integer :: numcnv
    real(kind=8) :: rnorm, temp, eps23
!
!     %-----------------------%
!     | LOCAL ARRAY ARGUMENTS |
!     %-----------------------%
!
    integer :: kp(4)
    save
!
!     %-----------%
!     | FUNCTIONS |
!     %-----------%
!
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
    call matfpe(-1)
!
    if (ido .eq. 0) then
!
        msglvl = mnaup2
!
!        %-------------------------------------%
!        | GET THE MACHINE DEPENDENT CONSTANT. |
!        %-------------------------------------%
!
        eps23 = r8prem()*0.5d0
        eps23 = eps23**(2.0d+0 / 3.0d+0)
!
        nev0 = nev
        np0 = np
!
!        %-------------------------------------%
!        | KPLUSP IS THE BOUND ON THE LARGEST  |
!        |        LANCZOS FACTORIZATION BUILT. |
!        | NCONV IS THE CURRENT NUMBER OF      |
!        |        "CONVERGED" EIGENVLUES.      |
!        | ITER IS THE COUNTER ON THE CURRENT  |
!        |      ITERATION STEP.                |
!        %-------------------------------------%
!
        kplusp = nev + np
        nconv = 0
        iter = 0
!
!        %---------------------------------------%
!        | SET FLAGS FOR COMPUTING THE FIRST NEV |
!        | STEPS OF THE ARNOLDI FACTORIZATION.   |
!        %---------------------------------------%
!
        getv0 = .true.
        update = .false.
        ushift = .false.
        cnorm = .false.
!
        if (info .ne. 0) then
!
!           %--------------------------------------------%
!           | USER PROVIDES THE INITIAL RESIDUAL VECTOR. |
!           %--------------------------------------------%
!
            initv = .true.
            info = 0
        else
            initv = .false.
        endif
    endif
!
!     %---------------------------------------------%
!     | GET A POSSIBLY RANDOM STARTING VECTOR AND   |
!     | FORCE IT INTO THE RANGE OF THE OPERATOR OP. |
!     %---------------------------------------------%
!
!
    if (getv0) then
        call dgetv0(ido, bmat, 1, initv, n,&
                    1, v, ldv, resid, rnorm,&
                    ipntr, workd, info, alpha)
        if (ido .ne. 99) goto 9000
        if (rnorm .eq. zero) then
!
!           %-----------------------------------------%
!           | THE INITIAL VECTOR IS ZERO. ERROR EXIT. |
!           %-----------------------------------------%
!
            info = -9
            goto 1100
        endif
        getv0 = .false.
        ido = 0
    endif
!
!     %-----------------------------------%
!     | BACK FROM REVERSE COMMUNICATION : |
!     | CONTINUE WITH UPDATE STEP         |
!     %-----------------------------------%
!
    if (update) goto 20
!
!     %-------------------------------------------%
!     | BACK FROM COMPUTING USER SPECIFIED SHIFTS |
!     %-------------------------------------------%
!
    if (ushift) goto 50
!
!     %-------------------------------------%
!     | BACK FROM COMPUTING RESIDUAL NORM   |
!     | AT THE END OF THE CURRENT ITERATION |
!     %-------------------------------------%
!
    if (cnorm) goto 100
!
!     %----------------------------------------------------------%
!     | COMPUTE THE FIRST NEV STEPS OF THE ARNOLDI FACTORIZATION |
!     %----------------------------------------------------------%
!
    call dnaitr(ido, bmat, n, 0, nev,&
                resid, rnorm, v, ldv, h,&
                ldh, ipntr, workd, info, alpha)
!
!     %---------------------------------------------------%
!     | IDO .NE. 99 IMPLIES USE OF REVERSE COMMUNICATION  |
!     | TO COMPUTE OPERATIONS INVOLVING OP AND POSSIBLY B |
!     %---------------------------------------------------%
!
    if (ido .ne. 99) goto 9000
    if (info .gt. 0) then
        np = info
        mxiter = iter
        info = -9999
        goto 1200
    endif
!
!     %--------------------------------------------------------------%
!     |                                                              |
!     |           M A I N  ARNOLDI  I T E R A T I O N  L O O P       |
!     |           EACH ITERATION IMPLICITLY RESTARTS THE ARNOLDI     |
!     |           FACTORIZATION IN PLACE.                            |
!     |                                                              |
!     %--------------------------------------------------------------%
!
1000 continue
!
    iter = iter + 1
    if (msglvl .gt. 0) then
        call ivout(logfil, 1, [iter], ndigit, '_NAUP2: **** START OF MAJOR ITERATION NUMBER ****')
    endif
!
!        %-----------------------------------------------------------%
!        | COMPUTE NP ADDITIONAL STEPS OF THE ARNOLDI FACTORIZATION. |
!        | ADJUST NP SINCE NEV MIGHT HAVE BEEN UPDATED BY LAST CALL  |
!        | TO THE SHIFT APPLICATION ROUTINE DNAPPS.                  |
!        %-----------------------------------------------------------%
!
    np = kplusp - nev
    if (msglvl .gt. 1) then
        call ivout(logfil, 1, [nev], ndigit,&
                   '_NAUP2: THE LENGTH OF THE CURRENT ARNOLDI FACTORIZATION')
        call ivout(logfil, 1, [np], ndigit, '_NAUP2: EXTEND THE ARNOLDI FACTORIZATION BY')
    endif
!
!        %-----------------------------------------------------------%
!        | COMPUTE NP ADDITIONAL STEPS OF THE ARNOLDI FACTORIZATION. |
!        %-----------------------------------------------------------%
!
    ido = 0
 20 continue
    update = .true.
    call dnaitr(ido, bmat, n, nev, np,&
                resid, rnorm, v, ldv, h,&
                ldh, ipntr, workd, info, alpha)
!
!        %---------------------------------------------------%
!        | IDO .NE. 99 IMPLIES USE OF REVERSE COMMUNICATION  |
!        | TO COMPUTE OPERATIONS INVOLVING OP AND POSSIBLY B |
!        %---------------------------------------------------%
!
    if (ido .ne. 99) goto 9000
    if (info .gt. 0) then
        np = info
        mxiter = iter
        if (info .ge. neqact) then
            if (msglvl .gt. 0) then
                write(logfil,*)
                write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                write(logfil,*)'& ESPACE INVARIANT DE TAILLE &'
                write(logfil,*)'& NEQACT = ',neqact
                write(logfil,*)'& SHUNTAGE PARTIEL DE DNAUP2 &'
                write(logfil,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
                write(logfil,*)
            endif
        else
            info = -9999
            goto 1200
        endif
    endif
    update = .false.
!
    if (msglvl .gt. 1) then
        call dvout(logfil, 1, [rnorm], ndigit, '_NAUP2: CORRESPONDING B-NORM OF THE RESIDUAL')
    endif
!
!        %--------------------------------------------------------%
!        | COMPUTE THE EIGENVALUES AND CORRESPONDING ERROR BOUNDS |
!        | OF THE CURRENT UPPER HESSENBERG MATRIX.                |
!        %--------------------------------------------------------%
!
    call dneigh(rnorm, kplusp, h, ldh, ritzr,&
                ritzi, bounds, q, ldq, workl,&
                ierr)
!
    if (ierr .ne. 0) then
        info = -8
        goto 1200
    endif
!
!        %----------------------------------------------------%
!        | MAKE A COPY OF EIGENVALUES AND CORRESPONDING ERROR |
!        | BOUNDS OBTAINED FROM DNEIGH.                       |
!        %----------------------------------------------------%
!
    call dcopy(kplusp, ritzr, 1, workl(kplusp**2+1), 1)
    call dcopy(kplusp, ritzi, 1, workl(kplusp**2+kplusp+1), 1)
    call dcopy(kplusp, bounds, 1, workl(kplusp**2+2*kplusp+1), 1)
!
!        %---------------------------------------------------%
!        | SELECT THE WANTED RITZ VALUES AND THEIR BOUNDS    |
!        | TO BE USED IN THE CONVERGENCE TEST.               |
!        | THE WANTED PART OF THE SPECTRUM AND CORRESPONDING |
!        | ERROR BOUNDS ARE IN THE LAST NEV LOC. OF RITZR,   |
!        | RITZI AND BOUNDS RESPECTIVELY. THE VARIABLES NEV  |
!        | AND NP MAY BE UPDATED IF THE NEV-TH WANTED RITZ   |
!        | VALUE HAS A NON ZERO IMAGINARY PART. IN THIS CASE |
!        | NEV IS INCREASED BY ONE AND NP DECREASED BY ONE.  |
!        | NOTE: THE LAST TWO ARGUMENTS OF DNGETS ARE NO     |
!        | LONGER USED AS OF VERSION 2.1.                    |
!        %---------------------------------------------------%
!
    nev = nev0
    np = np0
    numcnv = nev
    call dngets(ishift, which, nev, np, ritzr,&
                ritzi, bounds, workl, workl(np+1))
    if (nev .eq. nev0+1) numcnv = nev0+1
!
!        %-------------------%
!        | CONVERGENCE TEST. |
!        %-------------------%
!
    call dcopy(nev, bounds(np+1), 1, workl(2*np+1), 1)
    call dnconv(nev, ritzr(np+1), ritzi(np+1), workl(2*np+1), tol,&
                nconv)
!
    if (msglvl .gt. 2) then
        kp(1) = nev
        kp(2) = np
        kp(3) = numcnv
        kp(4) = nconv
        call ivout(logfil, 4, kp, ndigit, '_NAUP2: NEV, NP, NUMCNV, NCONV ARE')
        call dvout(logfil, kplusp, ritzr, ndigit, '_NAUP2: REAL PART OF THE EIGENVALUES OF H')
        call dvout(logfil, kplusp, ritzi, ndigit,&
                   '_NAUP2: IMAGINARY PART OF THE EIGENVALUES OF H')
        call dvout(logfil, kplusp, bounds, ndigit,&
                   '_NAUP2: RITZ ESTIMATES OF THE CURRENT NCV RITZ VALUES')
    endif
!
!        %---------------------------------------------------------%
!        | COUNT THE NUMBER OF UNWANTED RITZ VALUES THAT HAVE ZERO |
!        | RITZ ESTIMATES. IF ANY RITZ ESTIMATES ARE EQUAL TO ZERO |
!        | THEN A LEADING BLOCK OF H OF ORDER EQUAL TO AT LEAST    |
!        | THE NUMBER OF RITZ VALUES WITH ZERO RITZ ESTIMATES HAS  |
!        | SPLIT OFF. NONE OF THESE RITZ VALUES MAY BE REMOVED BY  |
!        | SHIFTING. DECREASE NP THE NUMBER OF SHIFTS TO APPLY. IF |
!        | NO SHIFTS MAY BE APPLIED, THEN PREPARE TO EXIT          |
!        %---------------------------------------------------------%
!
    nptemp = np
    do j = 1, nptemp
        if (bounds(j) .eq. zero) then
            np = np - 1
            nev = nev + 1
        endif
    end do
!
    if ((nconv .ge. numcnv) .or. (iter .gt. mxiter) .or. (np .eq. 0)) then
!
        if (msglvl .gt. 4) then
            call dvout(logfil, kplusp, workl(kplusp**2+1), ndigit,&
                       '_NAUP2: REAL PART OF THE EIG COMPUTED BY _NEIGH:')
            call dvout(logfil, kplusp, workl(kplusp**2+kplusp+1), ndigit,&
                       '_NAUP2: IMAG PART OF THE EIG COMPUTED BY _NEIGH:')
            call dvout(logfil, kplusp, workl(kplusp**2+kplusp*2+1), ndigit,&
                       '_NAUP2: RITZ EISTMATES COMPUTED BY _NEIGH:')
        endif
!
!           %------------------------------------------------%
!           | PREPARE TO EXIT. PUT THE CONVERGED RITZ VALUES |
!           | AND CORRESPONDING BOUNDS IN RITZ(1:NCONV) AND  |
!           | BOUNDS(1:NCONV) RESPECTIVELY. THEN SORT. BE    |
!           | CAREFUL WHEN NCONV > NP                        |
!           %------------------------------------------------%
!
!           %------------------------------------------%
!           |  USE H( 3,1 ) AS STORAGE TO COMMUNICATE  |
!           |  RNORM TO _NEUPD IF NEEDED               |
!           %------------------------------------------%
!
        h(3,1) = rnorm
!
!           %----------------------------------------------%
!           | TO BE CONSISTENT WITH DNGETS, WE FIRST DO A  |
!           | PRE-PROCESSING SORT IN ORDER TO KEEP COMPLEX |
!           | CONJUGATE PAIRS TOGETHER.  THIS IS SIMILAR   |
!           | TO THE PRE-PROCESSING SORT USED IN DNGETS    |
!           | EXCEPT THAT THE SORT IS DONE IN THE OPPOSITE |
!           | ORDER.                                       |
!           %----------------------------------------------%
!
        if (which .eq. 'LM') wprime = 'SR'
        if (which .eq. 'SM') wprime = 'LR'
        if (which .eq. 'LR') wprime = 'SM'
        if (which .eq. 'SR') wprime = 'LM'
        if (which .eq. 'LI') wprime = 'SM'
        if (which .eq. 'SI') wprime = 'LM'
!
        call dsortc(wprime, .true., kplusp, ritzr, ritzi,&
                    bounds)
!
!           %----------------------------------------------%
!           | NOW SORT RITZ VALUES SO THAT CONVERGED RITZ  |
!           | VALUES APPEAR WITHIN THE FIRST NEV LOCATIONS |
!           | OF RITZR, RITZI AND BOUNDS, AND THE MOST     |
!           | DESIRED ONE APPEARS AT THE FRONT.            |
!           %----------------------------------------------%
!
        if (which .eq. 'LM') wprime = 'SM'
        if (which .eq. 'SM') wprime = 'LM'
        if (which .eq. 'LR') wprime = 'SR'
        if (which .eq. 'SR') wprime = 'LR'
        if (which .eq. 'LI') wprime = 'SI'
        if (which .eq. 'SI') wprime = 'LI'
!
        call dsortc(wprime, .true., kplusp, ritzr, ritzi,&
                    bounds)
!
!           %--------------------------------------------------%
!           | SCALE THE RITZ ESTIMATE OF EACH RITZ VALUE       |
!           | BY 1 / MAX(EPS23,MAGNITUDE OF THE RITZ VALUE).   |
!           %--------------------------------------------------%
!
        do j = 1, nev0
            temp = max(eps23,dlapy2(ritzr(j), ritzi(j)))
            bounds(j) = bounds(j)/temp
        end do
!
!           %----------------------------------------------------%
!           | SORT THE RITZ VALUES ACCORDING TO THE SCALED RITZ  |
!           | ESITMATES.  THIS WILL PUSH ALL THE CONVERGED ONES  |
!           | TOWARDS THE FRONT OF RITZR, RITZI, BOUNDS          |
!           | (IN THE CASE WHEN NCONV < NEV.)                    |
!           %----------------------------------------------------%
!
        wprime = 'LR'
        call dsortc(wprime, .true., nev0, bounds, ritzr,&
                    ritzi)
!
!           %----------------------------------------------%
!           | SCALE THE RITZ ESTIMATE BACK TO ITS ORIGINAL |
!           | VALUE.                                       |
!           %----------------------------------------------%
!
        do j = 1, nev0
            temp = max(eps23, dlapy2(ritzr(j), ritzi(j)))
            bounds(j) = bounds(j)*temp
        end do
!
!           %------------------------------------------------%
!           | SORT THE CONVERGED RITZ VALUES AGAIN SO THAT   |
!           | THE "THRESHOLD" VALUE APPEARS AT THE FRONT OF  |
!           | RITZR, RITZI AND BOUND.                        |
!           %------------------------------------------------%
!
        call dsortc(which, .true., nconv, ritzr, ritzi,&
                    bounds)
!
        if (msglvl .gt. 1) then
            call dvout(logfil, kplusp, ritzr, ndigit,&
                       '_NAUP2: SORTED REAL PART OF THE EIGENVALUES')
            call dvout(logfil, kplusp, ritzi, ndigit,&
                       '_NAUP2: SORTED IMAGINARY PART OF THE EIGENVALUES')
            call dvout(logfil, kplusp, bounds, ndigit, '_NAUP2: SORTED RITZ ESTIMATES.')
        endif
!
!           %------------------------------------%
!           | MAX ITERATIONS HAVE BEEN EXCEEDED. |
!           %------------------------------------%
!
        if (iter .gt. mxiter .and. nconv .lt. numcnv) info = 1
!
!           %---------------------%
!           | NO SHIFTS TO APPLY. |
!           %---------------------%
!
        if (np .eq. 0 .and. nconv .lt. numcnv) info = 2
!
        np = nconv
        goto 1100
!
    else if ((nconv .lt. numcnv) .and. (ishift .eq. 1)) then
!
!           %-------------------------------------------------%
!           | DO NOT HAVE ALL THE REQUESTED EIGENVALUES YET.  |
!           | TO PREVENT POSSIBLE STAGNATION, ADJUST THE SIZE |
!           | OF NEV.                                         |
!           %-------------------------------------------------%
!
        nevbef = nev
        nev = nev + min(nconv, np/2)
        if (nev .eq. 1 .and. kplusp .ge. 6) then
            nev = kplusp / 2
        else if (nev .eq. 1 .and. kplusp .gt. 3) then
            nev = 2
        endif
        np = kplusp - nev
!
!           %---------------------------------------%
!           | IF THE SIZE OF NEV WAS JUST INCREASED |
!           | RESORT THE EIGENVALUES.               |
!           %---------------------------------------%
!
        if (nevbef .lt. nev) call dngets(ishift, which, nev, np, ritzr,&
                                         ritzi, bounds, workl, workl(np+1))
!
    endif
!
    if (msglvl .gt. 0) then
        call ivout(logfil, 1, [nconv], ndigit,&
                   '_NAUP2: NO. OF "CONVERGED" RITZ VALUES AT THIS ITER.')
        if (msglvl .gt. 1) then
            kp(1) = nev
            kp(2) = np
            call ivout(logfil, 2, kp, ndigit, '_NAUP2: NEV AND NP ARE')
            call dvout(logfil, nev, ritzr(np+1), ndigit,&
                       '_NAUP2: "WANTED" RITZ VALUES -- REAL PART')
            call dvout(logfil, nev, ritzi(np+1), ndigit,&
                       '_NAUP2: "WANTED" RITZ VALUES -- IMAG PART')
            call dvout(logfil, nev, bounds(np+1), ndigit,&
                       '_NAUP2: RITZ ESTIMATES OF THE "WANTED" VALUES ')
        endif
    endif
!
    if (ishift .eq. 0) then
!
!           %-------------------------------------------------------%
!           | USER SPECIFIED SHIFTS: REVERSE COMMINUCATION TO       |
!           | COMPUTE THE SHIFTS. THEY ARE RETURNED IN THE FIRST    |
!           | 2*NP LOCATIONS OF WORKL.                              |
!           %-------------------------------------------------------%
!
        ushift = .true.
        ido = 3
        goto 9000
    endif
!
 50 continue
!
!        %------------------------------------%
!        | BACK FROM REVERSE COMMUNICATION,   |
!        | USER SPECIFIED SHIFTS ARE RETURNED |
!        | IN WORKL(1:2*NP)                   |
!        %------------------------------------%
!
    ushift = .false.
    if (ishift .eq. 0) then
!
!            %----------------------------------%
!            | MOVE THE NP SHIFTS FROM WORKL TO |
!            | RITZR, RITZI TO FREE UP WORKL    |
!            | FOR NON-EXACT SHIFT CASE.        |
!            %----------------------------------%
!
        call dcopy(np, workl, 1, ritzr, 1)
        call dcopy(np, workl(np+1), 1, ritzi, 1)
    endif
!
    if (msglvl .gt. 2) then
        call ivout(logfil, 1, [np], ndigit, '_NAUP2: THE NUMBER OF SHIFTS TO APPLY ')
        call dvout(logfil, np, ritzr, ndigit, '_NAUP2: REAL PART OF THE SHIFTS')
        call dvout(logfil, np, ritzi, ndigit, '_NAUP2: IMAGINARY PART OF THE SHIFTS')
        if (ishift .eq. 1) call dvout(logfil, np, bounds, ndigit,&
                                      '_NAUP2: RITZ ESTIMATES OF THE SHIFTS')
    endif
!
!        %---------------------------------------------------------%
!        | APPLY THE NP IMPLICIT SHIFTS BY QR BULGE CHASING.       |
!        | EACH SHIFT IS APPLIED TO THE WHOLE UPPER HESSENBERG     |
!        | MATRIX H.                                               |
!        | THE FIRST 2*N LOCATIONS OF WORKD ARE USED AS WORKSPACE. |
!        %---------------------------------------------------------%
!
    call dnapps(n, nev, np, ritzr, ritzi,&
                v, ldv, h, ldh, resid,&
                q, ldq, workl, workd)
!
!        %---------------------------------------------%
!        | COMPUTE THE B-NORM OF THE UPDATED RESIDUAL. |
!        | KEEP B*RESID IN WORKD(1:N) TO BE USED IN    |
!        | THE FIRST STEP OF THE NEXT CALL TO DNAITR.  |
!        %---------------------------------------------%
!
    cnorm = .true.
    if (bmat .eq. 'G') then
        nbx = nbx + 1
        call dcopy(n, resid, 1, workd(n+1), 1)
        ipntr(1) = n + 1
        ipntr(2) = 1
        ido = 2
!
!           %----------------------------------%
!           | EXIT IN ORDER TO COMPUTE B*RESID |
!           %----------------------------------%
!
        goto 9000
    else if (bmat .eq. 'I') then
        call dcopy(n, resid, 1, workd, 1)
    endif
!
100 continue
!
!        %----------------------------------%
!        | BACK FROM REVERSE COMMUNICATION, |
!        | WORKD(1:N) := B*RESID            |
!        %----------------------------------%
!
    if (bmat .eq. 'G') then
        rnorm = ddot (n, resid, 1, workd, 1)
        rnorm = sqrt(abs(rnorm))
    else if (bmat .eq. 'I') then
        rnorm = dnrm2(n, resid, 1)
    endif
    cnorm = .false.
!
    if (msglvl .gt. 2) then
        call dvout(logfil, 1, [rnorm], ndigit,&
                   '_NAUP2: B-NORM OF RESIDUAL FOR COMPRESSED FACTORIZATION')
        call dmout(logfil, nev, nev, h, ldh,&
                   ndigit, '_NAUP2: COMPRESSED UPPER HESSENBERG MATRIX H')
    endif
!
    goto 1000
!
!     %---------------------------------------------------------------%
!     |                                                               |
!     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  |
!     |                                                               |
!     %---------------------------------------------------------------%
!
1100 continue
!
    mxiter = iter
    nev = numcnv
!
1200 continue
    ido = 99
!
!
9000 continue
!
    call matfpe(1)
!
!     %---------------%
!     | END OF DNAUP2 |
!     %---------------%
!
end subroutine
