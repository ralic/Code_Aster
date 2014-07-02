subroutine dgetv0(ido, bmat, itry, initv, n,&
                  j, v, ldv, resid, rnorm,&
                  ipntr, workd, ierr, alpha)
!----------------------------------------------------------------------
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
!     SUBROUTINE ARPACK GENERANT UN VECTEUR INITIAL DANS IM(OP).
!-----------------------------------------------------------------------
! BEGINDOC
!
! DESCRIPTION:
!  GENERATE A RANDOM INITIAL RESIDUAL VECTOR FOR THE ARNOLDI PROCESS.
!  FORCE THE RESIDUAL VECTOR TO BE IN THE RANGE OF THE OPERATOR OP.
!
! ARGUMENTS
!  IDO     INTEGER.  (INPUT/OUTPUT)
!          REVERSE COMMUNICATION FLAG.  IDO MUST BE ZERO ON THE FIRST
!          CALL TO DGETV0.
!          ------------------------------------------------------------
!          IDO =  0: FIRST CALL TO THE REVERSE COMMUNICATION INTERFACE
!          IDO = -1: COMPUTE  Y = OP * X  WHERE
!                    IPNTR(1) IS THE POINTER INTO WORKD FOR X,
!                    IPNTR(2) IS THE POINTER INTO WORKD FOR Y.
!                    THIS IS FOR THE INITIALIZATION PHASE TO FORCE THE
!                    STARTING VECTOR INTO THE RANGE OF OP.
!          IDO =  2: COMPUTE  Y = B * X  WHERE
!                    IPNTR(1) IS THE POINTER INTO WORKD FOR X,
!                    IPNTR(2) IS THE POINTER INTO WORKD FOR Y.
!          IDO = 99: DONE
!          ------------------------------------------------------------
!
!  BMAT    CHARACTER*1.  (INPUT)
!          BMAT SPECIFIES THE TYPE OF THE MATRIX B IN THE (GENERALIZED)
!          EIGENVALUE PROBLEM A*X = LAMBDA*B*X.
!          B = 'I' -> STANDARD EIGENVALUE PROBLEM A*X = LAMBDA*X
!          B = 'G' -> GENERALIZED EIGENVALUE PROBLEM A*X = LAMBDA*B*X
!
!  ITRY    INTEGER.  (INPUT)
!          ITRY COUNTS THE NUMBER OF TIMES THAT DGETV0 IS CALLED.
!          IT SHOULD BE SET TO 1 ON THE INITIAL CALL TO DGETV0.
!
!  INITV   LOGICAL VARIABLE.  (INPUT)
!          .TRUE.  => THE INITIAL RESIDUAL VECTOR IS GIVEN IN RESID.
!          .FALSE. => GENERATE A RANDOM INITIAL RESIDUAL VECTOR.
!
!  N       INTEGER.  (INPUT)
!          DIMENSION OF THE PROBLEM.
!
!  J     INTEGER.  (INPUT)
!        INDEX OF THE RESIDUAL VECTOR TO BE GENERATED, WITH RESPECT TO
!        THE ARNOLDI PROCESS.  J > 1 IN CASE OF A "RESTART".
!
!  V     REAL*8 N BY J ARRAY.  (INPUT)
!        THE FIRST J-1 COLUMNS OF V CONTAIN THE CURRENT ARNOLDI BASIS
!        IF THIS IS A "RESTART".
!
!  LDV     INTEGER.  (INPUT)
!          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  RESID   REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
!          INITIAL RESIDUAL VECTOR TO BE GENERATED.  IF RESID IS
!          PROVIDED, FORCE RESID INTO THE RANGE OF THE OPERATOR OP.
!
!  RNORM   REAL*8 SCALAR.  (OUTPUT)
!          B-NORM OF THE GENERATED RESIDUAL.
!
!  IPNTR   INTEGER ARRAY OF LENGTH 3.  (OUTPUT)
!
!  WORKD   REAL*8 WORK ARRAY OF LENGTH 2*N.  (REVERSE COMMUNICATION).
!          ON EXIT, WORK(1:N) = B*RESID TO BE USED IN SSAITR.
!
!  IERR  INTEGER.  (OUTPUT)
!        =  0: NORMAL EXIT.
!        = -1: CANNOT GENERATE A NONTRIVIAL RESTARTED RESIDUAL VECTOR
!                IN THE RANGE OF THE OPERATOR OP.
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
!     DVOUT   ARPACK UTILITY ROUTINE FOR VECTOR OUTPUT.
!     DLARNV  LAPACK ROUTINE FOR GENERATING A RANDOM VECTOR.
!     DGEMV   LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
!     DCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER.
!     DDOT    LEVEL 1 BLAS THAT COMPUTES THE SCALAR PRODUCT .
!     DNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
!
! INTRINSIC FUNCTIONS
!     ABS, SQRT
!
! AUTHOR
!     DANNY SORENSEN               PHUONG VU
!     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
!     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
!     APPLIED MATHEMATICS
!     RICE UNIVERSITY
!     HOUSTON, TEXAS
!
! FILE: GETV0.F   SID: 2.6   DATE OF SID: 8/27/96   RELEASE: 2
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE SECOND,
!            RAJOUT DE ALPHA,
!            COMMON TIMING REMPLACE PAR COMMON INFOR,
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1304
    implicit none
!
!     %-----------------------------%
!     | INCLUDE FILES FOR DEBUGGING |
!     %-----------------------------%
!
#include "asterf_types.h"
#include "asterc/matfpe.h"
#include "asterfort/dvout.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dgemv.h"
#include "blas/dlarnv.h"
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
    aster_logical :: initv
    integer :: ido, ierr, itry, j, ldv, n
    real(kind=8) :: rnorm, alpha
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    integer :: ipntr(3)
    real(kind=8) :: resid(n), v(ldv, j), workd(2*n)
!
!     %------------%
!     | PARAMETERS |
!     %------------%
!
    real(kind=8) :: one, zero
    parameter (one = 1.0d+0, zero = 0.0d+0)
!
!     %------------------------%
!     | LOCAL SCALARS & ARRAYS |
!     %------------------------%
!
    aster_logical :: first, inits, orth
    integer(kind=4) :: iseed4(4)
    integer :: idist, iseed(4), iter, msglvl, jj
    real(kind=8) :: rnorm0
    save first, iseed, inits, iter, msglvl, orth, rnorm0
!
!     %-----------%
!     | FUNCTIONS |
!     %-----------%
!
!
!     %-----------------%
!     | DATA STATEMENTS |
!     %-----------------%
!
    data       inits /.true./
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
    call matfpe(-1)
!
!     %-----------------------------------%
!     | INITIALIZE THE SEED OF THE LAPACK |
!     | RANDOM NUMBER GENERATOR           |
!     %-----------------------------------%
!
    if (inits) then
        iseed(1) = 1
        iseed(2) = 3
        iseed(3) = 5
        iseed(4) = 7
        inits = .false.
    endif
!
    if (ido .eq. 0) then
!
!        %-------------------------------%
!        | INITIALIZE TIMING STATISTICS  |
!        | & MESSAGE LEVEL FOR DEBUGGING |
!        %-------------------------------%
!
        msglvl = mgetv0
        ierr = 0
        iter = 0
        first = .false.
        orth = .false.
!
!        %-----------------------------------------------------%
!        | POSSIBLY GENERATE A RANDOM STARTING VECTOR IN RESID |
!        | USE A LAPACK RANDOM NUMBER GENERATOR USED BY THE    |
!        | MATRIX GENERATION ROUTINES.                         |
!        |    IDIST = 1: UNIFORM (0,1)  DISTRIBUTION,          |
!        |    IDIST = 2: UNIFORM (-1,1) DISTRIBUTION,          |
!        |    IDIST = 3: NORMAL  (0,1)  DISTRIBUTION,          |
!        %-----------------------------------------------------%
!
        if (.not.initv) then
            idist = 2
            iseed4(1)=int(iseed(1), 4)
            iseed4(2)=int(iseed(2), 4)
            iseed4(3)=int(iseed(3), 4)
            iseed4(4)=int(iseed(4), 4)
            call dlarnv(idist, iseed4, n, resid)
            iseed(1)=iseed4(1)
            iseed(2)=iseed4(2)
            iseed(3)=iseed4(3)
            iseed(4)=iseed4(4)
        endif
!
!        %----------------------------------------------------------%
!        | FORCE THE STARTING VECTOR INTO THE RANGE OF OP TO HANDLE |
!        | THE GENERALIZED PROBLEM WHEN B IS POSSIBLY (SINGULAR).   |
!        %----------------------------------------------------------%
!
        if (bmat .eq. 'G') then
            nopx = nopx + 1
            ipntr(1) = 1
            ipntr(2) = n + 1
            call dcopy(n, resid, 1, workd, 1)
            ido = -1
            goto 9000
        endif
    endif
!
!     %-----------------------------------------%
!     | BACK FROM COMPUTING OP*(INITIAL-VECTOR) |
!     %-----------------------------------------%
!
    if (first) goto 20
!
!     %-----------------------------------------------%
!     | BACK FROM COMPUTING B*(ORTHOGONALIZED-VECTOR) |
!     %-----------------------------------------------%
!
    if (orth) goto 40
!
!     %------------------------------------------------------%
!     | STARTING VECTOR IS NOW IN THE RANGE OF OP, R = OP*R, |
!     | COMPUTE B-NORM OF STARTING VECTOR.                   |
!     %------------------------------------------------------%
!
    first = .true.
    if (bmat .eq. 'G') then
        nbx = nbx + 1
        call dcopy(n, workd(n+1), 1, resid, 1)
        ipntr(1) = n + 1
        ipntr(2) = 1
        ido = 2
        goto 9000
    else if (bmat .eq. 'I') then
        call dcopy(n, resid, 1, workd, 1)
    endif
!
 20 continue
!
    first = .false.
    if (bmat .eq. 'G') then
        rnorm0 = ddot (n, resid, 1, workd, 1)
        rnorm0 = sqrt(abs(rnorm0))
    else if (bmat .eq. 'I') then
        rnorm0 = dnrm2(n, resid, 1)
    endif
    rnorm = rnorm0
!
!     %---------------------------------------------%
!     | EXIT IF THIS IS THE VERY FIRST ARNOLDI STEP |
!     %---------------------------------------------%
!
    if (j .eq. 1) goto 50
!
!     %----------------------------------------------------------------
!     | OTHERWISE NEED TO B-ORTHOGONALIZE THE STARTING VECTOR AGAINST |
!     | THE CURRENT ARNOLDI BASIS USING GRAM-SCHMIDT WITH ITER. REF.  |
!     | THIS IS THE CASE WHERE AN INVARIANT SUBSPACE IS ENCOUNTERED   |
!     | IN THE MIDDLE OF THE ARNOLDI FACTORIZATION.                   |
!     |                                                               |
!     |       S = VT(T)*B*R,   R = R - V*S,                           |
!     |                                                               |
!     | STOPPING CRITERIA USED FOR ITER. REF. IS DISCUSSED IN         |
!     | PARLETT'S BOOK, PAGE 107 AND IN GRAGG & REICHEL TOMS PAPER.   |
!     %---------------------------------------------------------------%
!
    orth = .true.
 30 continue
!
    call dgemv('T', n, j-1, one, v,&
               ldv, workd, 1, zero, workd(n+1),&
               1)
    call dgemv('N', n, j-1, -one, v,&
               ldv, workd(n+1), 1, one, resid,&
               1)
!
!     %----------------------------------------------------------%
!     | COMPUTE THE B-NORM OF THE ORTHOGONALIZED STARTING VECTOR |
!     %----------------------------------------------------------%
!
    if (bmat .eq. 'G') then
        nbx = nbx + 1
        call dcopy(n, resid, 1, workd(n+1), 1)
        ipntr(1) = n + 1
        ipntr(2) = 1
        ido = 2
        goto 9000
    else if (bmat .eq. 'I') then
        call dcopy(n, resid, 1, workd, 1)
    endif
!
 40 continue
!
    if (bmat .eq. 'G') then
        rnorm = ddot (n, resid, 1, workd, 1)
        rnorm = sqrt(abs(rnorm))
    else if (bmat .eq. 'I') then
        rnorm = dnrm2(n, resid, 1)
    endif
!
!     %--------------------------------------%
!     | CHECK FOR FURTHER ORTHOGONALIZATION. |
!     %--------------------------------------%
!
    if (msglvl .gt. 2) then
        call dvout(logfil, 1, [rnorm0], ndigit, '_GETV0: RE-ORTHONALIZATION , RNORM0 IS')
        call dvout(logfil, 1, [rnorm], ndigit, '_GETV0: RE-ORTHONALIZATION , RNORM IS')
    endif
    if (rnorm .gt. alpha*rnorm0) goto 50
    iter = iter + 1
    if (iter .le. 5) then
!
!        %-----------------------------------%
!        | PERFORM ITERATIVE REFINEMENT STEP |
!        %-----------------------------------%
!
        rnorm0 = rnorm
        goto 30
    else
!
!        %------------------------------------%
!        | ITERATIVE REFINEMENT STEP "FAILED" |
!        %------------------------------------%
!
        do jj = 1, n
            resid(jj) = zero
        end do
        rnorm = zero
        ierr = -1
    endif
!
 50 continue
!
    if (msglvl .gt. 0) then
        call dvout(logfil, 1, [rnorm], ndigit,&
                   '_GETV0: B-NORM OF INITIAL / RESTARTED STARTING VECTOR')
    endif
    if (msglvl .gt. 2) then
        call dvout(logfil, n, [resid], ndigit, '_GETV0: INITIAL / RESTARTED STARTING VECTOR')
    endif
    ido = 99
!
9000 continue
!
    call matfpe(1)
!
!     %---------------%
!     | END OF DGETV0 |
!     %---------------%
!
end subroutine
