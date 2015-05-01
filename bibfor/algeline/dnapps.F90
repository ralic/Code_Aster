subroutine dnapps(n, kev, np, shiftr, shifti,&
                  v, ldv, h, ldh, resid,&
                  q, ldq, workl, workd)
!---------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) LAPACK
! COPYRIGHT (C) 2007 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
!     SUBROUTINE ARPACK PREPARANT LE RESTART VIA UN QR IMPLICITE POUR
!     ELIMINER LES NP MODES PROPRES INDESIRABLES.
!-----------------------------------------------------------------------
! BEGINDOC
!
! DESCRIPTION:
!  GIVEN THE ARNOLDI FACTORIZATION
!
!     A*V_(K) - V_(K)*H_(K) = R_(K+P)*E_(K+P)T,
!
!  APPLY NP IMPLICIT SHIFTS RESULTING IN
!
!     A*(V_(K)*Q) - (V_(K)*Q)*(QT* H_(K)*Q) = R_(K+P)*E_(K+P)T * Q
!
!  WHERE Q IS AN ORTHOGONAL MATRIX WHICH IS THE PRODUCT OF ROTATIONS
!  AND REFLECTIONS RESULTING FROM THE NP BULGE CHAGE SWEEPS.
!  THE UPDATED ARNOLDI FACTORIZATION BECOMES:
!
!     A*VNEW_(K) - VNEW_(K)*HNEW_(K) = RNEW_(K)*E_(K)T.
!
! ARGUMENTS
!  N       INTEGER.  (INPUT)
!          PROBLEM SIZE, I.E. SIZE OF MATRIX A.
!
!  KEV     INTEGER.  (INPUT/OUTPUT)
!          KEV+NP IS THE SIZE OF THE INPUT MATRIX H.
!          KEV IS THE SIZE OF THE UPDATED MATRIX HNEW.  KEV IS ONLY
!          UPDATED ON OUPUT WHEN FEWER THAN NP SHIFTS ARE APPLIED IN
!          ORDER TO KEEP THE CONJUGATE PAIR TOGETHER.
!
!  NP      INTEGER.  (INPUT)
!          NUMBER OF IMPLICIT SHIFTS TO BE APPLIED.
!
!  SHIFTR, REAL*8 ARRAY OF LENGTH NP.  (INPUT)
!  SHIFTI  REAL AND IMAGINARY PART OF THE SHIFTS TO BE APPLIED.
!          UPON, ENTRY TO DNAPPS, THE SHIFTS MUST BE SORTED SO THAT
!          THE CONJUGATE PAIRS ARE IN CONSECUTIVE LOCATIONS.
!
!  V       REAL*8 N BY (KEV+NP) ARRAY.  (INPUT/OUTPUT)
!          ON INPUT, V CONTAINS THE CURRENT KEV+NP ARNOLDI VECTORS.
!          ON OUTPUT, V CONTAINS THE UPDATED KEV ARNOLDI VECTORS
!          IN THE FIRST KEV COLUMNS OF V.
!
!  LDV     INTEGER.  (INPUT)
!          LEADING DIMENSION OF V EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  H       REAL*8 (KEV+NP) BY (KEV+NP) ARRAY.  (INPUT/OUTPUT)
!          ON INPUT, H CONTAINS THE CURRENT KEV+NP BY KEV+NP UPPER
!          HESSENBER MATRIX OF THE ARNOLDI FACTORIZATION.
!          ON OUTPUT, H CONTAINS THE UPDATED KEV BY KEV UPPER
!          HESSENBERG MATRIX IN THE KEV LEADING SUBMATRIX.
!
!  LDH     INTEGER.  (INPUT)
!          LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  RESID   REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
!          ON INPUT, RESID CONTAINS THE THE RESIDUAL VECTOR R_(K+P).
!          ON OUTPUT, RESID IS THE UPDATE RESIDUAL VECTOR RNEW_(K)
!          IN THE FIRST KEV LOCATIONS.
!
!  Q       REAL*8 KEV+NP BY KEV+NP WORK ARRAY.  (WORKSPACE)
!          WORK ARRAY USED TO ACCUMULATE THE ROTATIONS AND REFLECTIONS
!          DURING THE BULGE CHASE SWEEP.
!
!  LDQ     INTEGER.  (INPUT)
!          LEADING DIMENSION OF Q EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  WORKL   REAL*8 WORK ARRAY OF LENGTH (KEV+NP).  (WORKSPACE)
!          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
!          THE FRONT END.
!
!  WORKD   REAL*8 WORK ARRAY OF LENGTH 2*N.  (WORKSPACE)
!          DISTRIBUTED ARRAY USED IN THE APPLICATION OF THE ACCUMULATED
!          ORTHOGONAL MATRIX Q.
!
! ENDDOC
!-----------------------------------------------------------------------
! BEGINLIB
!
! REFERENCES:
!  1. D.C. SORENSEN, "IMPLICIT APPLICATION OF POLYNOMIAL FILTERS IN
!     A K-STEP ARNOLDI METHOD", SIAM J. MATR. ANAL. APPS., 13 (1992),
!     PP 357-385.
!
! ROUTINES CALLED:
!     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
!     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES.
!     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
!     DLACPY  LAPACK MATRIX COPY ROUTINE.
!     DLANHS  LAPACK ROUTINE THAT COMPUTES VARIOUS NORMS OF A MATRIX.
!     DLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
!     DLARF   LAPACK ROUTINE THAT APPLIES HOUSEHOLDER REFLECTION TO
!             A MATRIX.
!     DLARFG  LAPACK HOUSEHOLDER REFLECTION CONSTRUCTION ROUTINE.
!     DLARTG  LAPACK GIVENS ROTATION CONSTRUCTION ROUTINE.
!     DLASET  LAPACK MATRIX INITIALIZATION ROUTINE.
!     DGEMV   LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
!     DAXPY   LEVEL 1 BLAS THAT COMPUTES A VECTOR TRIAD.
!     DCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
!     DSCAL   LEVEL 1 BLAS THAT SCALES A VECTOR.
!
!     R8PREM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE PRECISION.
!     R8MIEM  ASTER UTILITY ROUTINE THAT GIVES THE MINIMUN VALUES.
!     ISBAEM  ASTER UTILITY ROUTINE THAT GIVES THE MACHINE BASE.
!
! INTRINSIC FUNCTIONS
!     ABS, MAX, MIN
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
!     XX/XX/92: VERSION ' 2.1'
!
! FILE: NAPPS.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
!
! REMARKS
!  1. IN THIS VERSION, EACH SHIFT IS APPLIED TO ALL THE SUBLOCKS OF
!     THE HESSENBERG MATRIX H AND NOT JUST TO THE SUBMATRIX THAT IT
!     COMES FROM. DEFLATION AS IN LAPACK ROUTINE DLAHQR (QR ALGORITHM
!     FOR UPPER HESSENBERG MATRICES ) IS USED.
!     THE SUBDIAGONALS OF H ARE ENFORCED TO BE NON-NEGATIVE.
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
    implicit none
!
!     %-----------------------------%
!     | INCLUDE FILES FOR DEBUGGING |
!     %-----------------------------%
!
#include "asterf_types.h"
#include "asterc/isbaem.h"
#include "asterc/matfpe.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/dmout.h"
#include "asterfort/dvout.h"
#include "asterfort/ar_dlarfg.h"
#include "asterfort/ar_dlartg.h"
#include "asterfort/ivout.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dgemv.h"
#include "blas/dlacpy.h"
#include "blas/dlanhs.h"
#include "blas/dlapy2.h"
#include "blas/dlarf.h"
#include "blas/dlaset.h"
#include "blas/dscal.h"
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
    integer :: kev, ldh, ldq, ldv, n, np
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    real(kind=8) :: h(ldh, kev+np), resid(n), shifti(np), shiftr(np)
    real(kind=8) :: v(ldv, kev+np), q(ldq, kev+np), workd(2*n), workl(kev+np)
!
!     %------------%
!     | PARAMETERS |
!     %------------%
!
    real(kind=8) :: one, zero, deux
    parameter (one = 1.0d+0, zero = 0.0d+0, deux = 2.0d+0)
!
!     %------------------------%
!     | LOCAL SCALARS & ARRAYS |
!     %------------------------%
!
    integer :: i, iend, ir, istart, j, jj, kplusp, msglvl, nr
    aster_logical :: cconj, first
    real(kind=8) :: c, f, g, h11, h12, h21, h22, h32, r, s, sigmai, sigmar
    real(kind=8) :: smlnum, ulp, unfl, u(3), t, tau, tst1
! DUE TO CRS512      REAL*8 OVFL
! DUE TO CRS512      SAVE FIRST, OVFL, SMLNUM, ULP, UNFL
    save first, smlnum, ulp, unfl
!
!     %--------------------%
!     | EXTERNAL FUNCTIONS |
!     %--------------------%
!
!
!     %----------------%
!     | DATA STATMENTS |
!     %----------------%
!
    data first / .true. /
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
    call matfpe(-1)
!
    if (first) then
!
!        %-----------------------------------------------%
!        | SET MACHINE-DEPENDENT CONSTANTS FOR THE       |
!        | STOPPING CRITERION. IF NORM(H) <= SQRT(OVFL), |
!        | OVERFLOW SHOULD NOT OCCUR.                    |
!        | REFERENCE: LAPACK SUBROUTINE DLAHQR           |
!        %-----------------------------------------------%
!
        unfl = r8miem()
! DUE RO CRS512         OVFL = ONE / UNFL
        ulp = r8prem() *0.5d0 * isbaem()
        smlnum = unfl*( n / ulp )
        first = .false.
    endif
!
!     %-------------------------------%
!     | INITIALIZE TIMING STATISTICS  |
!     | & MESSAGE LEVEL FOR DEBUGGING |
!     %-------------------------------%
!
    msglvl = mnapps
    kplusp = kev + np
!
!     %--------------------------------------------%
!     | INITIALIZE Q TO THE IDENTITY TO ACCUMULATE |
!     | THE ROTATIONS AND REFLECTIONS              |
!     %--------------------------------------------%
!
! DUE TO CRP_102 CALL DLASET ('ALL', KPLUSP, KPLUSP, ZERO,
! ONE, Q, LDQ)
    call dlaset('A', kplusp, kplusp, zero, one,&
                q, ldq)
!
!     %----------------------------------------------%
!     | QUICK RETURN IF THERE ARE NO SHIFTS TO APPLY |
!     %----------------------------------------------%
!
    if (np .eq. 0) goto 9000
!
!     %----------------------------------------------%
!     | CHASE THE BULGE WITH THE APPLICATION OF EACH |
!     | IMPLICIT SHIFT. EACH SHIFT IS APPLIED TO THE |
!     | WHOLE MATRIX INCLUDING EACH BLOCK.           |
!     %----------------------------------------------%
!
    cconj = .false.
    do jj = 1, np
        sigmar = shiftr(jj)
        sigmai = shifti(jj)
!
        if (msglvl .gt. 2) then
            call ivout(logfil, 1, [jj], ndigit, '_NAPPS: SHIFT NUMBER.')
            call dvout(logfil, 1, [sigmar], ndigit, '_NAPPS: THE REAL PART OF THE SHIFT ')
            call dvout(logfil, 1, [sigmai], ndigit, '_NAPPS: THE IMAGINARY PART OF THE SHIFT ')
        endif
!
!        %-------------------------------------------------%
!        | THE FOLLOWING SET OF CONDITIONALS IS NECESSARY  |
!        | IN ORDER THAT COMPLEX CONJUGATE PAIRS OF SHIFTS |
!        | ARE APPLIED TOGETHER OR NOT AT ALL.             |
!        %-------------------------------------------------%
!
        if (cconj) then
!
!           %-----------------------------------------%
!           | CCONJ = .TRUE. MEANS THE PREVIOUS SHIFT |
!           | HAD NON-ZERO IMAGINARY PART.            |
!           %-----------------------------------------%
!
            cconj = .false.
            goto 110
        else if (jj .lt. np .and. abs( sigmai ) .gt. zero) then
!
!           %------------------------------------%
!           | START OF A COMPLEX CONJUGATE PAIR. |
!           %------------------------------------%
!
            cconj = .true.
        else if (jj .eq. np .and. abs( sigmai ) .gt. zero) then
!
!           %----------------------------------------------%
!           | THE LAST SHIFT HAS A NONZERO IMAGINARY PART. |
!           | DON'T APPLY IT, THUS THE ORDER OF THE        |
!           | COMPRESSED H IS ORDER KEV+1 SINCE ONLY NP-1  |
!           | WERE APPLIED.                                |
!           %----------------------------------------------%
!
            kev = kev + 1
            goto 110
        endif
        istart = 1
 20     continue
!
!        %--------------------------------------------------%
!        | IF SIGMAI = 0 THEN                               |
!        |    APPLY THE JJ-TH SHIFT ...                     |
!        | ELSE                                             |
!        |    APPLY THE JJ-TH AND (JJ+1)-TH TOGETHER ...    |
!        |    (NOTE THAT JJ < NP AT THIS POINT IN THE CODE) |
!        | END                                              |
!        | TO THE CURRENT BLOCK OF H. THE NEXT DO LOOP      |
!        | DETERMINES THE CURRENT BLOCK ,                   |
!        %--------------------------------------------------%
!
        do i = istart, kplusp-1
!
!           %----------------------------------------%
!           | CHECK FOR SPLITTING AND DEFLATION. USE |
!           | A STANDARD TEST AS IN THE QR ALGORITHM |
!           | REFERENCE: LAPACK SUBROUTINE DLAHQR    |
!           %----------------------------------------%
!
            tst1 = abs( h( i, i ) ) + abs( h( i+1, i+1 ) )
            if (tst1 .eq. zero) tst1 = dlanhs('1', kplusp-jj+1, h, ldh, workl)
            if (abs( h( i+1,i ) ) .le. max( ulp*tst1, smlnum )) then
                if (msglvl .gt. 0) then
                    call ivout(logfil, 1, [i], ndigit,&
                               '_NAPPS: MATRIX SPLITTING AT ROW/COLUMN NO.')
                    call ivout(logfil, 1, [jj], ndigit,&
                               '_NAPPS: MATRIX SPLITTING WITH SHIFT NUMBER.')
                    call dvout(logfil, 1, h(i+1, i), ndigit, '_NAPPS: OFF DIAGONAL ELEMENT.')
                endif
                iend = i
                h(i+1,i) = zero
                goto 40
            endif
        end do
        iend = kplusp
 40     continue
!
        if (msglvl .gt. 2) then
            call ivout(logfil, 1, [istart], ndigit, '_NAPPS: START OF CURRENT BLOCK ')
            call ivout(logfil, 1, [iend], ndigit, '_NAPPS: END OF CURRENT BLOCK ')
        endif
!
!        %------------------------------------------------%
!        | NO REASON TO APPLY A SHIFT TO BLOCK OF ORDER 1 |
!        %------------------------------------------------%
!
        if (istart .eq. iend) goto 100
!
!        %------------------------------------------------------%
!        | IF ISTART + 1 = IEND THEN NO REASON TO APPLY A       |
!        | COMPLEX CONJUGATE PAIR OF SHIFTS ON A 2 BY 2 MATRIX. |
!        %------------------------------------------------------%
!
        if (istart + 1 .eq. iend .and. abs( sigmai ) .gt. zero) goto 100
!
        h11 = h(istart,istart)
        h21 = h(istart+1,istart)
        if (abs( sigmai ) .le. zero) then
!
!           %---------------------------------------------%
!           | REAL-VALUED SHIFT ==> APPLY SINGLE SHIFT QR |
!           %---------------------------------------------%
!
            f = h11 - sigmar
            g = h21
!
            do i = istart, iend-1
!
!              %-----------------------------------------------------%
!              | CONTRUCT THE PLANE ROTATION G TO ZERO OUT THE BULGE |
!              %-----------------------------------------------------%
!
                call ar_dlartg(f, g, c, s, r)
                if (i .gt. istart) then
!
!                 %-------------------------------------------%
!                 | THE FOLLOWING ENSURES THAT H(1:IEND-1,1), |
!                 | THE FIRST IEND-2 OFF DIAGONAL OF ELEMENTS |
!                 | H, REMAIN NON NEGATIVE.                   |
!                 %-------------------------------------------%
!
                    if (r .lt. zero) then
                        r = -r
                        c = -c
                        s = -s
                    endif
                    h(i,i-1) = r
                    h(i+1,i-1) = zero
                endif
!
!              %---------------------------------------------%
!              | APPLY ROTATION TO THE LEFT OF H,  H <- G'*H |
!              %---------------------------------------------%
!
                do j = i, kplusp
                    t = c*h(i,j) + s*h(i+1,j)
                    h(i+1,j) = -s*h(i,j) + c*h(i+1,j)
                    h(i,j) = t
                end do
!
!              %---------------------------------------------%
!              | APPLY ROTATION TO THE RIGHT OF H,  H <- H*G |
!              %---------------------------------------------%
!
                do j = 1, min(i+2, iend)
                    t = c*h(j,i) + s*h(j,i+1)
                    h(j,i+1) = -s*h(j,i) + c*h(j,i+1)
                    h(j,i) = t
                end do
!
!              %----------------------------------------------------%
!              | ACCUMULATE THE ROTATION IN THE MATRIX Q,  Q <- Q*G |
!              %----------------------------------------------------%
!
                do j = 1, min( i+jj, kplusp )
                    t = c*q(j,i) + s*q(j,i+1)
                    q(j,i+1) = - s*q(j,i) + c*q(j,i+1)
                    q(j,i) = t
                end do
!
!              %---------------------------%
!              | PREPARE FOR NEXT ROTATION |
!              %---------------------------%
!
                if (i .lt. iend-1) then
                    f = h(i+1,i)
                    g = h(i+2,i)
                endif
            end do
!
!           %-----------------------------------%
!           | FINISHED APPLYING THE REAL SHIFT. |
!           %-----------------------------------%
!
        else
!
!           %----------------------------------------------------%
!           | COMPLEX CONJUGATE SHIFTS ==> APPLY DOUBLE SHIFT QR |
!           %----------------------------------------------------%
!
            h12 = h(istart,istart+1)
            h22 = h(istart+1,istart+1)
            h32 = h(istart+2,istart+1)
!
!           %---------------------------------------------------------%
!           | COMPUTE 1ST COLUMN OF (H - SHIFT*I)*(H - CONJ(SHIFT)*I) |
!           %---------------------------------------------------------%
!
            s = deux*sigmar
            t = dlapy2 ( sigmar, sigmai )
            u(1) = ( h11 * (h11 - s) + t * t ) / h21 + h12
            u(2) = h11 + h22 - s
            u(3) = h32
!
            do i = istart, iend-1
!
                nr = min ( 3, iend-i+1 )
!
!              %-----------------------------------------------------%
!              | CONSTRUCT HOUSEHOLDER REFLECTOR G TO ZERO OUT U(1). |
!              | G IS OF THE FORM I - TAU*( 1 U )' * ( 1 U' ).       |
!              %-----------------------------------------------------%
!
                call ar_dlarfg(nr, u(1), u(2), 1, tau)
!
                if (i .gt. istart) then
                    h(i,i-1) = u(1)
                    h(i+1,i-1) = zero
                    if (i .lt. iend-1) h(i+2,i-1) = zero
                endif
                u(1) = one
!
!              %--------------------------------------%
!              | APPLY THE REFLECTOR TO THE LEFT OF H |
!              %--------------------------------------%
! DUE TO CRP_102 CALL DLARF ('LEFT', NR, KPLUSP-I+1, U, 1, TAU,
                call dlarf('L', nr, kplusp-i+1, u, 1,&
                           tau, h(i, i), ldh, workl)
!
!              %---------------------------------------%
!              | APPLY THE REFLECTOR TO THE RIGHT OF H |
!              %---------------------------------------%
!
                ir = min ( i+3, iend )
! DUE TO CRP_102 CALL DLARF ('RIGHT', IR, NR, U, 1, TAU,
                call dlarf('R', ir, nr, u, 1,&
                           tau, h(1, i), ldh, workl)
!
!              %-----------------------------------------------------%
!              | ACCUMULATE THE REFLECTOR IN THE MATRIX Q,  Q <- Q*G |
!              %-----------------------------------------------------%
!
! DUE TO CRP_102 CALL DLARF ('RIGHT', KPLUSP, NR, U, 1, TAU,
                call dlarf('R', kplusp, nr, u, 1,&
                           tau, q(1, i), ldq, workl)
!
!              %----------------------------%
!              | PREPARE FOR NEXT REFLECTOR |
!              %----------------------------%
!
                if (i .lt. iend-1) then
                    u(1) = h(i+1,i)
                    u(2) = h(i+2,i)
                    if (i .lt. iend-2) u(3) = h(i+3,i)
                endif
!
            end do
!
!           %--------------------------------------------%
!           | FINISHED APPLYING A COMPLEX PAIR OF SHIFTS |
!           | TO THE CURRENT BLOCK                       |
!           %--------------------------------------------%
!
        endif
!
100     continue
!
!        %---------------------------------------------------------%
!        | APPLY THE SAME SHIFT TO THE NEXT BLOCK IF THERE IS ANY. |
!        %---------------------------------------------------------%
!
        istart = iend + 1
        if (iend .lt. kplusp) goto 20
!
!        %---------------------------------------------%
!        | LOOP BACK TO THE TOP TO GET THE NEXT SHIFT. |
!        %---------------------------------------------%
!
110     continue
    end do
!
!     %--------------------------------------------------%
!     | PERFORM A SIMILARITY TRANSFORMATION THAT MAKES   |
!     | SURE THAT H WILL HAVE NON NEGATIVE SUB DIAGONALS |
!     %--------------------------------------------------%
!
    do j = 1, kev
        if (h(j+1,j) .lt. zero) then
            call dscal(kplusp-j+1, -one, h(j+1, j), ldh)
            call dscal(min(j+2, kplusp), -one, h(1, j+1), 1)
            call dscal(min(j+np+1, kplusp), -one, q(1, j+1), 1)
        endif
    end do
!
    do i = 1, kev
!
!        %--------------------------------------------%
!        | FINAL CHECK FOR SPLITTING AND DEFLATION.   |
!        | USE A STANDARD TEST AS IN THE QR ALGORITHM |
!        | REFERENCE: LAPACK SUBROUTINE DLAHQR        |
!        %--------------------------------------------%
!
        tst1 = abs( h( i, i ) ) + abs( h( i+1, i+1 ) )
        if (tst1 .eq. zero) tst1 = dlanhs( '1', kev, h, ldh, workl )
        if (h( i+1,i ) .le. max( ulp*tst1, smlnum )) h(i+1,i) = zero
    end do
!
!     %-------------------------------------------------%
!     | COMPUTE THE (KEV+1)-ST COLUMN OF (V*Q) AND      |
!     | TEMPORARILY STORE THE RESULT IN WORKD(N+1:2*N). |
!     | THIS IS NEEDED IN THE RESIDUAL UPDATE SINCE WE  |
!     | CANNOT GUARANTEE THAT THE CORRESPONDING ENTRY   |
!     | OF H WOULD BE ZERO AS IN EXACT ARITHMETIC.      |
!     %-------------------------------------------------%
!
    if (h(kev+1,kev) .gt. zero) call dgemv('N', n, kplusp, one, v,&
                                           ldv, q(1, kev+1), 1, zero, workd(n+1),&
                                           1)
!
!     %----------------------------------------------------------%
!     | COMPUTE COLUMN 1 TO KEV OF (V*Q) IN BACKWARD ORDER       |
!     | TAKING ADVANTAGE OF THE UPPER HESSENBERG STRUCTURE OF Q. |
!     %----------------------------------------------------------%
!
    do i = 1, kev
        call dgemv('N', n, kplusp-i+1, one, v,&
                   ldv, q(1, kev-i+1), 1, zero, workd,&
                   1)
        call dcopy(n, workd, 1, v(1, kplusp-i+1), 1)
    end do
!
!     %-------------------------------------------------%
!     |  MOVE V(:,KPLUSP-KEV+1:KPLUSP) INTO V(:,1:KEV). |
!     %-------------------------------------------------%
!
    call dlacpy('A', n, kev, v(1, kplusp-kev+1), ldv,&
                v, ldv)
!
!     %--------------------------------------------------------------%
!     | COPY THE (KEV+1)-ST COLUMN OF (V*Q) IN THE APPROPRIATE PLACE |
!     %--------------------------------------------------------------%
!
    if (h(kev+1,kev) .gt. zero) call dcopy(n, workd(n+1), 1, v(1, kev+1), 1)
!
!     %-------------------------------------%
!     | UPDATE THE RESIDUAL VECTOR:         |
!     |    R <- SIGMAK*R + BETAK*V(:,KEV+1) |
!     | WHERE                               |
!     |    SIGMAK = (E_(KPLUSP)'*Q)*E_(KEV) |
!     |    BETAK = E_(KEV+1)'*H*E_(KEV)     |
!     %-------------------------------------%
!
    call dscal(n, q(kplusp, kev), resid, 1)
    if (h(kev+1,kev) .gt. zero) call daxpy(n, h(kev+1, kev), v(1, kev+1), 1, resid,&
                                           1)
!
    if (msglvl .gt. 1) then
        call dvout(logfil, 1, q(kplusp, kev), ndigit, '_NAPPS: SIGMAK = (E_(KEV+P)T*Q)*E_(KEV)')
        call dvout(logfil, 1, h(kev+1, kev), ndigit, '_NAPPS: BETAK = E_(KEV+1)T*H*E_(KEV)')
        call ivout(logfil, 1, [kev], ndigit, '_NAPPS: ORDER OF THE FINAL HESSENBERG MATRIX ')
        if (msglvl .gt. 2) then
            call dmout(logfil, kev, kev, h, ldh,&
                       ndigit, '_NAPPS: UPDATED HESSENBERG MATRIX H FOR NEXT ITERATION')
        endif
    endif
!
9000 continue
!
    call matfpe(1)
!
!     %---------------%
!     | END OF DNAPPS |
!     %---------------%
!
end subroutine
