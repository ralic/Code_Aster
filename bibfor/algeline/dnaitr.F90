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
!     SUBROUTINE ARPACK OPERANT NP ETAPE D'ARNOLDI A PARTIR D'UNE
!     FACTORISATION D'ORDRE K.
!-----------------------------------------------------------------------
! BEGINDOC
!
! DESCRIPTION:
!  REVERSE COMMUNICATION INTERFACE FOR APPLYING NP ADDITIONAL STEPS TO
!  A K STEP NONSYMMETRIC ARNOLDI FACTORIZATION.
!
!  INPUT:  OP*V_(K)  -  V_(K)*H = R_(K)*E_(K)T
!
!          WITH (V_(K)T)*B*V_(K) = I, (V_(K)T)*B*R_(K) = 0.
!
!  OUTPUT: OP*V_(K+P)  -  V_(K+P)*H = R_(K+P)*E_(K+P)T
!
!          WITH (V_(K+P)T)*B*V_(K+P) = I, (V_(K+P)T)*B*R_(K+P) = 0.
!
!  WHERE OP AND B ARE AS IN DNAUPD.  THE B-NORM OF R_(K+P) IS ALSO
!  COMPUTED AND RETURNED.
!
! ARGUMENTS
!  IDO     INTEGER.  (INPUT/OUTPUT)
!          REVERSE COMMUNICATION FLAG.
!          -------------------------------------------------------------
!          IDO =  0: FIRST CALL TO THE REVERSE COMMUNICATION INTERFACE
!          IDO = -1: COMPUTE  Y = OP * X  WHERE
!                    IPNTR(1) IS THE POINTER INTO WORK FOR X,
!                    IPNTR(2) IS THE POINTER INTO WORK FOR Y.
!                    THIS IS FOR THE RESTART PHASE TO FORCE THE NEW
!                    STARTING VECTOR INTO THE RANGE OF OP.
!          IDO =  1: COMPUTE  Y = OP * X  WHERE
!                    IPNTR(1) IS THE POINTER INTO WORK FOR X,
!                    IPNTR(2) IS THE POINTER INTO WORK FOR Y,
!                    IPNTR(3) IS THE POINTER INTO WORK FOR B * X.
!          IDO =  2: COMPUTE  Y = B * X  WHERE
!                    IPNTR(1) IS THE POINTER INTO WORK FOR X,
!                    IPNTR(2) IS THE POINTER INTO WORK FOR Y.
!          IDO = 99: DONE
!          -------------------------------------------------------------
!          WHEN THE ROUTINE IS USED IN THE "SHIFT-AND-INVERT" MODE, THE
!          VECTOR B * Q IS ALREADY AVAILABLE AND DO NOT NEED TO BE
!          RECOMPUTE IN FORMING OP * Q.
!
!  BMAT    CHARACTER*1.  (INPUT)
!          BMAT SPECIFIES THE TYPE OF THE MATRIX B THAT DEFINES THE
!          SEMI-INNER PRODUCT FOR THE OPERATOR OP.  SEE DNAUPD.
!          B = 'I' -> STANDARD EIGENVALUE PROBLEM A*X = LAMBDA*X
!          B = 'G' -> GENERALIZED EIGENVALUE PROBLEM A*X = LAMBDA*M**X
!
!  N       INTEGER.  (INPUT)
!          DIMENSION OF THE EIGENPROBLEM.
!
!  K       INTEGER.  (INPUT)
!          CURRENT SIZE OF V AND H.
!
!  NP      INTEGER.  (INPUT)
!          NUMBER OF ADDITIONAL ARNOLDI STEPS TO TAKE.
!
!  RESID   REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
!          ON INPUT:  RESID CONTAINS THE RESIDUAL VECTOR R_(K).
!          ON OUTPUT: RESID CONTAINS THE RESIDUAL VECTOR R_(K+P).
!
!  RNORM   REAL*8 SCALAR.  (INPUT/OUTPUT)
!          B-NORM OF THE STARTING RESIDUAL ON INPUT.
!          B-NORM OF THE UPDATED RESIDUAL R_(K+P) ON OUTPUT.
!
!  V       REAL*8 N BY K+NP ARRAY.  (INPUT/OUTPUT)
!          ON INPUT:  V CONTAINS THE ARNOLDI VECTORS IN THE FIRST K
!          COLUMNS.
!          ON OUTPUT: V CONTAINS THE NEW NP ARNOLDI VECTORS IN THE NEXT
!          NP COLUMNS.  THE FIRST K COLUMNS ARE UNCHANGED.
!
!  LDV     INTEGER.  (INPUT)
!          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  H       REAL*8 (K+NP) BY (K+NP) ARRAY.  (INPUT/OUTPUT)
!          H IS USED TO STORE THE GENERATED UPPER HESSENBERG MATRIX.
!
!  LDH     INTEGER.  (INPUT)
!          LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  IPNTR   INTEGER ARRAY OF LENGTH 3.  (OUTPUT)
!          POINTER TO MARK THE STARTING LOCATIONS IN THE WORK FOR
!          VECTORS USED BY THE ARNOLDI ITERATION.
!          -------------------------------------------------------------
!          IPNTR(1): POINTER TO THE CURRENT OPERAND VECTOR X.
!          IPNTR(2): POINTER TO THE CURRENT RESULT VECTOR Y.
!          IPNTR(3): POINTER TO THE VECTOR B * X WHEN USED IN THE
!                    SHIFT-AND-INVERT MODE.  X IS THE CURRENT OPERAND.
!          -------------------------------------------------------------
!
!  WORKD   REAL*8 WORK ARRAY OF LENGTH 3*N.  (REVERSE COMMUNICATION)
!          DISTRIBUTED ARRAY TO BE USED IN THE BASIC ARNOLDI ITERATION
!          FOR REVERSE COMMUNICATION.  THE CALLING PROGRAM SHOULD NOT
!          USE WORKD AS TEMPORARY WORKSPACE DURING THE ITERATION
!          ON INPUT, WORKD(1:N) = B*RESID AND IS USED TO SAVE SOME
!          COMPUTATION AT THE FIRST STEP.
!
!  INFO    INTEGER.  (OUTPUT)
!          = 0: NORMAL EXIT.
!          > 0: SIZE OF THE SPANNING INVARIANT SUBSPACE OF OP FOUND.
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
!     DGETV0  ARPACK ROUTINE TO GENERATE THE INITIAL VECTOR.
!     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
!     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
!     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
!     DLASCL  LAPACK ROUTINE FOR CAREFUL SCALING OF A MATRIX.
!     DLANHS  LAPACK ROUTINE THAT COMPUTES VARIOUS NORMS OF A MATRIX.
!     DGEMV  LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
!     DAXPY  LEVEL 1 BLAS THAT COMPUTES A VECTOR TRIAD.
!     DSCAL  LEVEL 1 BLAS THAT SCALES A VECTOR.
!     DCOPY  LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
!     DDOT  LEVEL 1 BLAS THAT COMPUTES THE SCALAR PRODUCT OF VECTORS.
!     DNRM2  LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
!
!     R8PREM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE PRECISION
!     R8MIEM  ASTER UTILITY ROUTINE THAT GIVES THE MINIMUN VALUES
!     ISBAEM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE BASE.
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
! REVISION HISTORY:
!     XX/XX/92: VERSION ' 2.4'
!
! FILE: NAITR.F   SID: 2.4   DATE OF SID: 8/27/96   RELEASE: 2
!
! REMARKS
!  THE ALGORITHM IMPLEMENTED IS:
!
!  RESTART = .FALSE.
!  GIVEN V_(K) = (V_(1), ..., V_(K)), R_(K),
!  R_(K) CONTAINS THE INITIAL RESIDUAL VECTOR EVEN FOR K = 0,
!  ALSO ASSUME THAT RNORM = || B*R_(K) || AND B*R_(K) ARE ALREADY
!  COMPUTED BY THE CALLING PROGRAM.
!
!  BETAJ = RNORM , P_(K+1) = B*R_(K) ,
!  FOR  J = K+1, ..., K+NP  DO
!     1) IF ( BETAJ < TOL ) STOP OR RESTART DEPENDING ON J.
!        ( AT PRESENT TOL IS ZERO )
!        IF ( RESTART ) GENERATE A NEW STARTING VECTOR.
!     2) V_(J) = R(J-1)/BETAJ,  V_(J) = (V_(J-1), V_(J)),
!        P_(J) = P_(J)/BETAJ
!     3) R_(J) = OP*V_(J) WHERE OP IS DEFINED AS IN DNAUPD
!        FOR SHIFT-INVERT MODE P_(J) = B*V_(J) IS ALREADY AVAILABLE.
!        WNORM = || OP*V_(J) ||
!     4) COMPUTE THE J-TH STEP RESIDUAL VECTOR.
!        W_(J) =  V_(J)T * B * OP * V_(J)
!        R_(J) =  OP*V_(J) - V_(J) * W_(J)
!        H(:,J) = W_(J),
!        H(J,J-1) = RNORM
!        RNORM = || R_(J) ||
!        IF (RNORM > ALPHA*WNORM) ACCEPT STEP AND GO BACK TO 1)
!     5) RE-ORTHOGONALIZATION STEP:
!        S = V_(J)'*B*R_(J)
!        R_(J) = R_(J) - V_(J)*S,  RNORM1 = || R_(J) ||
!        ALPHAJ = ALPHAJ + S_(J),
!     6) ITERATIVE REFINEMENT STEP:
!        IF (RNORM1 > ALPHA*RNORM) THEN
!           RNORM = RNORM1
!           ACCEPT STEP AND GO BACK TO 1)
!        ELSE
!           RNORM = RNORM1
!           IF THIS IS THE FIRST TIME IN STEP 6), GOTO 5)
!           ELSE R_(J) LIES IN THE SPAN OF V_(J) NUMERICALLY.
!              SET R_(J) = 0 AND RNORM = 0, GOTO 1)
!        ENDIF
!  END DO
subroutine dnaitr(ido, bmat, n, k, np,&
                  resid, rnorm, v, ldv, h,&
                  ldh, ipntr, workd, info, alpha)
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE SECOND, DLABAD ET DLAMCH,
!            COMMON TIMING REMPLACE PAR COMMON INFOR,
!            UTILISATION DE R8PREM(), R8MIEM() ET ISBAEM(),
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1304,W1501
    implicit none
!
!     %-----------------------------%
!     | INCLUDE FILES FOR DEBUGGING |
!     %-----------------------------%
!
    include 'asterc/isbaem.h'
    include 'asterc/matfpe.h'
    include 'asterc/r8miem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/dgetv0.h'
    include 'asterfort/dmout.h'
    include 'asterfort/dvout.h'
    include 'asterfort/ivout.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    include 'blas/dgemv.h'
    include 'blas/dlanhs.h'
    include 'blas/dlascl.h'
    include 'blas/dnrm2.h'
    include 'blas/dscal.h'
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
    integer :: ido, info, k, ldh, ldv, n, np
    real(kind=8) :: rnorm, alpha
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    integer :: ipntr(3)
    real(kind=8) :: h(ldh, k+np), resid(n), v(ldv, k+np), workd(3*n)
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
    logical :: first, orth1, orth2, rstart, step3, step4
    integer(kind=4) :: infol4
    integer :: ierr, i, ipj, irj, ivj, iter, itry, j, msglvl, jj
    real(kind=8) :: betaj, temp1, rnorm1, smlnum, tst1, ulp, unfl, wnorm
! DUE TO CRS512      REAL*8 OVFL
! DUE TO CRS512      SAVE FIRST, ORTH1, ORTH2, RSTART, STEP3, STEP4,
!     &  IERR, IPJ, IRJ, IVJ, ITER, ITRY, J, MSGLVL, OVFL,
!     &  BETAJ, RNORM1, SMLNUM, ULP, UNFL, WNORM
    save first, orth1, orth2, rstart, step3, step4,&
     &  ierr, ipj, irj, ivj, iter, itry, j, msglvl,&
     &  betaj, rnorm1, smlnum, ulp, unfl, wnorm
!
!     %-----------------------%
!     | LOCAL ARRAY ARGUMENTS |
!     %-----------------------%
!
    real(kind=8) :: xtemp(2)
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
    data  first / .true. /
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
    call matfpe(-1)
!
    if (first) then
!
!        %-----------------------------------------%
!        | SET MACHINE-DEPENDENT CONSTANTS FOR THE |
!        | THE SPLITTING AND DEFLATION CRITERION.  |
!        | IF NORM(H) <= SQRT(OVFL),               |
!        | OVERFLOW SHOULD NOT OCCUR.              |
!        | REFERENCE: LAPACK SUBROUTINE FLAHQR     |
!        %-----------------------------------------%
!
        unfl = r8miem()
! DUE TO CRS512         OVFL = ONE / UNFL
        ulp = r8prem() * 0.5d0 * isbaem()
        smlnum = unfl*( n / ulp )
        first = .false.
    endif
!
    if (ido .eq. 0) then
!
!        %-------------------------------%
!        | INITIALIZE TIMING STATISTICS  |
!        | & MESSAGE LEVEL FOR DEBUGGING |
!        %-------------------------------%
!
        msglvl = mnaitr
!
!        %------------------------------%
!        | INITIAL CALL TO THIS ROUTINE |
!        %------------------------------%
!
        info = 0
        step3 = .false.
        step4 = .false.
        rstart = .false.
        orth1 = .false.
        orth2 = .false.
        j = k + 1
        ipj = 1
        irj = ipj + n
        ivj = irj + n
    endif
!
!     %-------------------------------------------------%
!     | WHEN IN REVERSE COMMUNICATION MODE ONE OF:      |
!     | STEP3, STEP4, ORTH1, ORTH2, RSTART              |
!     | WILL BE .TRUE. WHEN ....                        |
!     | STEP3: RETURN FROM COMPUTING OP*V_(J).          |
!     | STEP4: RETURN FROM COMPUTING B-NORM OF OP*V_(J) |
!     | ORTH1: RETURN FROM COMPUTING B-NORM OF R_(J+1)  |
!     | ORTH2: RETURN FROM COMPUTING B-NORM OF          |
!     |        CORRECTION TO THE RESIDUAL VECTOR.       |
!     | RSTART: RETURN FROM OP COMPUTATIONS NEEDED BY   |
!     |         DGETV0.                                 |
!     %-------------------------------------------------%
!
    if (step3) goto 50
    if (step4) goto 60
    if (orth1) goto 70
    if (orth2) goto 90
    if (rstart) goto 30
!
!     %-----------------------------%
!     | ELSE THIS IS THE FIRST STEP |
!     %-----------------------------%
!
!     %--------------------------------------------------------------%
!     |                                                              |
!     |        A R N O L D I     I T E R A T I O N     L O O P       |
!     |                                                              |
!     | NOTE:  B*R_(J-1) IS ALREADY IN WORKD(1:N)=WORKD(IPJ:IPJ+N-1) |
!     %--------------------------------------------------------------%
!
1000  continue
!
    if (msglvl .gt. 1) then
        call ivout(logfil, 1, j, ndigit, '_NAITR: GENERATING ARNOLDI VECTOR NUMBER')
        call dvout(logfil, 1, rnorm, ndigit, '_NAITR: B-NORM OF THE CURRENT RESIDUAL IS')
    endif
!
!        %---------------------------------------------------%
!        | STEP 1: CHECK IF THE B NORM OF J-TH RESIDUAL      |
!        | VECTOR IS ZERO. EQUIVALENT TO DETERMING WHETHER   |
!        | AN EXACT J-STEP ARNOLDI FACTORIZATION IS PRESENT. |
!        %---------------------------------------------------%
!
    betaj = rnorm
    if (rnorm .gt. zero) goto 40
!
!           %---------------------------------------------------%
!           | INVARIANT SUBSPACE FOUND, GENERATE A NEW STARTING |
!           | VECTOR WHICH IS ORTHOGONAL TO THE CURRENT ARNOLDI |
!           | BASIS AND CONTINUE THE ITERATION.                 |
!           %---------------------------------------------------%
!
    if (msglvl .gt. 0) then
        call ivout(logfil, 1, j, ndigit, '_NAITR: ****** RESTART AT STEP ******')
    endif
!
!           %---------------------------------------------%
!           | ITRY IS THE LOOP VARIABLE THAT CONTROLS THE |
!           | MAXIMUM AMOUNT OF TIMES THAT A RESTART IS   |
!           | ATTEMPTED. NRSTRT IS USED BY STAT.H         |
!           %---------------------------------------------%
!
    betaj = zero
    nrstrt = nrstrt + 1
    itry = 1
20  continue
    rstart = .true.
    ido = 0
30  continue
!
!           %--------------------------------------%
!           | IF IN REVERSE COMMUNICATION MODE AND |
!           | RSTART = .TRUE. FLOW RETURNS HERE.   |
!           %--------------------------------------%
!
    call dgetv0(ido, bmat, itry, .false., n,&
                j, v, ldv, resid, rnorm,&
                ipntr, workd, ierr, alpha)
    if (ido .ne. 99) goto 9000
    if (ierr .lt. 0) then
        itry = itry + 1
        if (itry .le. 3) goto 20
!
!              %------------------------------------------------%
!              | GIVE UP AFTER SEVERAL RESTART ATTEMPTS.        |
!              | SET INFO TO THE SIZE OF THE INVARIANT SUBSPACE |
!              | WHICH SPANS OP AND EXIT.                       |
!              %------------------------------------------------%
!
        info = j - 1
        ido = 99
        goto 9000
    endif
!
40  continue
!
!        %---------------------------------------------------------%
!        | STEP 2:  V_(J) = R_(J-1)/RNORM AND P_(J) = P_(J)/RNORM  |
!        | NOTE THAT P_(J) = B*R_(J-1). IN ORDER TO AVOID OVERFLOW |
!        | WHEN RECIPROCATING A SMALL RNORM, TEST AGAINST LOWER    |
!        | MACHINE BOUND.                                          |
!        %---------------------------------------------------------%
!
    call dcopy(n, resid, 1, v(1, j), 1)
    if (rnorm .ge. unfl) then
        temp1 = one / rnorm
        call dscal(n, temp1, v(1, j), 1)
        call dscal(n, temp1, workd(ipj), 1)
    else
!
!            %-----------------------------------------%
!            | TO SCALE BOTH V_(J) AND P_(J) CAREFULLY |
!            | USE LAPACK ROUTINE SLASCL               |
!            %-----------------------------------------%
!
! DUE TO CRP_102 CALL DLASCL ('GENERAL', I, I, RNORM, ONE, N, 1,
        call dlascl('G', i, i, rnorm, one,&
                    n, 1, v(1, j), n, infol4)
! DUE TO CRP_102 CALL DLASCL ('GENERAL', I, I, RNORM, ONE, N, 1,
        call dlascl('G', i, i, rnorm, one,&
                    n, 1, workd(ipj), n, infol4)
!
    endif
!
!        %------------------------------------------------------%
!        | STEP 3:  R_(J) = OP*V_(J), NOTE THAT P_(J) = B*V_(J) |
!        | NOTE THAT THIS IS NOT QUITE YET R_(J). SEE STEP 4    |
!        %------------------------------------------------------%
!
    step3 = .true.
    nopx = nopx + 1
    call dcopy(n, v(1, j), 1, workd(ivj), 1)
    ipntr(1) = ivj
    ipntr(2) = irj
    ipntr(3) = ipj
    ido = 1
!
!        %-----------------------------------%
!        | EXIT IN ORDER TO COMPUTE OP*V_(J) |
!        %-----------------------------------%
!
    goto 9000
50  continue
!
!        %----------------------------------%
!        | BACK FROM REVERSE COMMUNICATION, |
!        | WORKD(IRJ:IRJ+N-1) := OP*V_(J)   |
!        | IF STEP3 = .TRUE.                |
!        %----------------------------------%
!
    step3 = .false.
!
!        %------------------------------------------%
!        | PUT ANOTHER COPY OF OP*V_(J) INTO RESID. |
!        %------------------------------------------%
!
    call dcopy(n, workd(irj), 1, resid, 1)
!
!        %---------------------------------------%
!        | STEP 4:  FINISH EXTENDING THE ARNOLDI |
!        |          FACTORIZATION TO LENGTH J.   |
!        %---------------------------------------%
!
    if (bmat .eq. 'G') then
        nbx = nbx + 1
        step4 = .true.
        ipntr(1) = irj
        ipntr(2) = ipj
        ido = 2
!
!           %-------------------------------------%
!           | EXIT IN ORDER TO COMPUTE B*OP*V_(J) |
!           %-------------------------------------%
!
        goto 9000
    else if (bmat .eq. 'I') then
        call dcopy(n, resid, 1, workd(ipj), 1)
    endif
60  continue
!
!        %----------------------------------%
!        | BACK FROM REVERSE COMMUNICATION, |
!        | WORKD(IPJ:IPJ+N-1) := B*OP*V_(J) |
!        | IF STEP4 = .TRUE.                |
!        %----------------------------------%
!
    step4 = .false.
!
!        %-------------------------------------%
!        | THE FOLLOWING IS NEEDED FOR STEP 5. |
!        | COMPUTE THE B-NORM OF OP*V_(J).     |
!        %-------------------------------------%
!
    if (bmat .eq. 'G') then
        wnorm = ddot (n, resid, 1, workd(ipj), 1)
        wnorm = sqrt(abs(wnorm))
    else if (bmat .eq. 'I') then
        wnorm = dnrm2(n, resid, 1)
    endif
!
!        %-----------------------------------------%
!        | COMPUTE THE J-TH RESIDUAL CORRESPONDING |
!        | TO THE J STEP FACTORIZATION.            |
!        | USE CLASSICAL GRAM SCHMIDT AND COMPUTE: |
!        | W_(J) <-  V_(J)T * B * OP * V_(J)      |
!        | R_(J) <-  OP*V_(J) - V_(J) * W_(J)      |
!        %-----------------------------------------%
!
!        %------------------------------------------%
!        | COMPUTE THE J FOURIER COEFFICIENTS W_(J) |
!        | WORKD(IPJ:IPJ+N-1) CONTAINS B*OP*V_(J).  |
!        %------------------------------------------%
!
    call dgemv('T', n, j, one, v,&
               ldv, workd(ipj), 1, zero, h(1, j),&
               1)
!
!        %--------------------------------------%
!        | ORTHOGONALIZE R_(J) AGAINST V_(J).   |
!        | RESID CONTAINS OP*V_(J). SEE STEP 3. |
!        %--------------------------------------%
!
    call dgemv('N', n, j, -one, v,&
               ldv, h(1, j), 1, one, resid,&
               1)
!
    if (j .gt. 1) h(j,j-1) = betaj
    orth1 = .true.
    if (bmat .eq. 'G') then
        nbx = nbx + 1
        call dcopy(n, resid, 1, workd(irj), 1)
        ipntr(1) = irj
        ipntr(2) = ipj
        ido = 2
!
!           %----------------------------------%
!           | EXIT IN ORDER TO COMPUTE B*R_(J) |
!           %----------------------------------%
!
        goto 9000
    else if (bmat .eq. 'I') then
        call dcopy(n, resid, 1, workd(ipj), 1)
    endif
70  continue
!
!        %---------------------------------------------------%
!        | BACK FROM REVERSE COMMUNICATION IF ORTH1 = .TRUE. |
!        | WORKD(IPJ:IPJ+N-1) := B*R_(J).                    |
!        %---------------------------------------------------%
!
    orth1 = .false.
!
!        %------------------------------%
!        | COMPUTE THE B-NORM OF R_(J). |
!        %------------------------------%
!
    if (bmat .eq. 'G') then
        rnorm = ddot (n, resid, 1, workd(ipj), 1)
        rnorm = sqrt(abs(rnorm))
    else if (bmat .eq. 'I') then
        rnorm = dnrm2(n, resid, 1)
    endif
!
!        %-----------------------------------------------------------%
!        | STEP 5: RE-ORTHOGONALIZATION / ITERATIVE REFINEMENT PHASE |
!        | MAXIMUM NITER_ITREF TRIES.                                |
!        |                                                           |
!        |          S      = V_(J)T * B * R_(J)                      |
!        |          R_(J)  = R_(J) - V_(J)*S                         |
!        |          ALPHAJ = ALPHAJ + S_(J)                          |
!        |                                                           |
!        | THE STOPPING CRITERIA USED FOR ITERATIVE REFINEMENT IS    |
!        | DISCUSSED IN PARLETT'S BOOK SEP, PAGE 107 AND IN GRAGG &  |
!        | REICHEL ACM TOMS PAPER, ALGORITHM 686, DEC. 1990.         |
!        | DETERMINE IF WE NEED TO CORRECT THE RESIDUAL. THE GOAL IS |
!        | TO ENFORCE ||V(:,1:J)T * R_(J)|| .LE. EPS * || R_(J) ||   |
!        | THE FOLLOWING TEST DETERMINES WHETHER THE SINE OF THE     |
!        | ANGLE BETWEEN  OP*X AND THE COMPUTED RESIDUAL IS LESS     |
!        | THAN OR EQUAL TO 0.717.                                   |
!        %-----------------------------------------------------------%
!
    if (rnorm .gt. alpha*wnorm) goto 100
    iter = 0
    nrorth = nrorth + 1
!
!        %---------------------------------------------------%
!        | ENTER THE ITERATIVE REFINEMENT PHASE. IF FURTHER  |
!        | REFINEMENT IS NECESSARY, LOOP BACK HERE. THE LOOP |
!        | VARIABLE IS ITER. PERFORM A STEP OF CLASSICAL     |
!        | GRAM-SCHMIDT USING ALL THE ARNOLDI VECTORS V_(J)  |
!        %---------------------------------------------------%
!
80  continue
!
    if (msglvl .gt. 2) then
        xtemp(1) = wnorm
        xtemp(2) = rnorm
        call dvout(logfil, 2, xtemp, ndigit, '_NAITR: RE-ORTHONALIZATION, WNORM AND RNORM ARE')
        call dvout(logfil, j, h(1, j), ndigit, '_NAITR: J-TH COLUMN OF H')
    endif
!
!        %----------------------------------------------------%
!        | COMPUTE V_(J)T * B * R_(J)                         |
!        | WORKD(IRJ:IRJ+J-1) = V(:,1:J)'*WORKD(IPJ:IPJ+N-1). |
!        %----------------------------------------------------%
!
    call dgemv('T', n, j, one, v,&
               ldv, workd(ipj), 1, zero, workd(irj),&
               1)
!
!        %---------------------------------------------%
!        | COMPUTE THE CORRECTION TO THE RESIDUAL:     |
!        | R_(J) = R_(J) - V_(J) * WORKD(IRJ:IRJ+J-1). |
!        | THE CORRECTION TO H IS V(:,1:J)*H(1:J,1:J)  |
!        | + V(:,1:J)*WORKD(IRJ:IRJ+J-1)*E'_J.         |
!        %---------------------------------------------%
!
    call dgemv('N', n, j, -one, v,&
               ldv, workd(irj), 1, one, resid,&
               1)
    call daxpy(j, one, workd(irj), 1, h(1, j),&
               1)
!
    orth2 = .true.
    if (bmat .eq. 'G') then
        nbx = nbx + 1
        call dcopy(n, resid, 1, workd(irj), 1)
        ipntr(1) = irj
        ipntr(2) = ipj
        ido = 2
!
!           %-----------------------------------%
!           | EXIT IN ORDER TO COMPUTE B*R_(J). |
!           | R_(J) IS THE CORRECTED RESIDUAL.  |
!           %-----------------------------------%
!
        goto 9000
    else if (bmat .eq. 'I') then
        call dcopy(n, resid, 1, workd(ipj), 1)
    endif
90  continue
!
!        %---------------------------------------------------%
!        | BACK FROM REVERSE COMMUNICATION IF ORTH2 = .TRUE. |
!        %---------------------------------------------------%
!
!        %-----------------------------------------------------%
!        | COMPUTE THE B-NORM OF THE CORRECTED RESIDUAL R_(J). |
!        %-----------------------------------------------------%
!
    if (bmat .eq. 'G') then
        rnorm1 = ddot (n, resid, 1, workd(ipj), 1)
        rnorm1 = sqrt(abs(rnorm1))
    else if (bmat .eq. 'I') then
        rnorm1 = dnrm2(n, resid, 1)
    endif
!
    if (msglvl .gt. 0 .and. iter .gt. 0) then
        call ivout(logfil, 1, j, ndigit, '_NAITR: ITERATIVE REFINEMENT FOR ARNOLDI RESIDUAL')
        if (msglvl .gt. 2) then
            xtemp(1) = rnorm
            xtemp(2) = rnorm1
            call dvout(logfil, 2, xtemp, ndigit,&
                       '_NAITR: ITERATIVE REFINEMENT , RNORM AND RNORM1 ARE')
        endif
    endif
!
!        %-----------------------------------------%
!        | DETERMINE IF WE NEED TO PERFORM ANOTHER |
!        | STEP OF RE-ORTHOGONALIZATION.           |
!        %-----------------------------------------%
!
    if (rnorm1 .gt. alpha*rnorm) then
!
!           %---------------------------------------%
!           | NO NEED FOR FURTHER REFINEMENT.       |
!           | THE COSINE OF THE ANGLE BETWEEN THE   |
!           | CORRECTED RESIDUAL VECTOR AND THE OLD |
!           | RESIDUAL VECTOR IS GREATER THAN 0.717 |
!           | IN OTHER WORDS THE CORRECTED RESIDUAL |
!           | AND THE OLD RESIDUAL VECTOR SHARE AN  |
!           | ANGLE OF LESS THAN ARCCOS(0.717)      |
!           %---------------------------------------%
!
        rnorm = rnorm1
!
    else
!
!           %-------------------------------------------%
!           | ANOTHER STEP OF ITERATIVE REFINEMENT STEP |
!           | IS REQUIRED. NITREF IS USED BY STAT.H     |
!           %-------------------------------------------%
!
        nitref = nitref + 1
        rnorm = rnorm1
        iter = iter + 1
        if (iter .le. 1) goto 80
!
!           %-------------------------------------------------%
!           | OTHERWISE RESID IS NUMERICALLY IN THE SPAN OF V |
!           %-------------------------------------------------%
!
        do 95 jj = 1, n
            resid(jj) = zero
95      continue
        rnorm = zero
    endif
!
!        %----------------------------------------------%
!        | BRANCH HERE DIRECTLY IF ITERATIVE REFINEMENT |
!        | WASN'T NECESSARY OR AFTER AT MOST NITER_REF  |
!        | STEPS OF ITERATIVE REFINEMENT.               |
!        %----------------------------------------------%
!
100  continue
!
    rstart = .false.
    orth2 = .false.
!
!        %------------------------------------%
!        | STEP 6: UPDATE  J = J+1,  CONTINUE |
!        %------------------------------------%
    j = j + 1
    if (j .gt. k+np) then
        ido = 99
        do 110 i = max(1, k), k+np-1
!
!              %--------------------------------------------%
!              | CHECK FOR SPLITTING AND DEFLATION.         |
!              | USE A STANDARD TEST AS IN THE QR ALGORITHM |
!              | REFERENCE: LAPACK SUBROUTINE FLAHQR        |
!              %--------------------------------------------%
!
            tst1 = abs( h( i, i ) ) + abs( h( i+1, i+1 ) )
            if (tst1 .eq. zero) tst1 = dlanhs( '1', k+np, h, ldh, workd(n+1) )
            if (abs( h( i+1,i ) ) .le. max( ulp*tst1, smlnum )) h(i+1,i) = zero
110      continue
!
        if (msglvl .gt. 2) then
            call dmout(logfil, k+np, k+np, h, ldh,&
                       ndigit, '_NAITR: FINAL UPPER HESSENBERG MATRIX H OF ORDER K+NP')
        endif
!
        goto 9000
    endif
!
!        %--------------------------------------------------------%
!        | LOOP BACK TO EXTEND THE FACTORIZATION BY ANOTHER STEP. |
!        %--------------------------------------------------------%
!
    goto 1000
!
!     %---------------------------------------------------------------%
!     |                                                               |
!     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  |
!     |                                                               |
!     %---------------------------------------------------------------%
!
9000  continue
!
    call matfpe(1)
!
!     %---------------%
!     | END OF DNAITR |
!     %---------------%
!
end subroutine
