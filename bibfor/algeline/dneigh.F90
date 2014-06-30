subroutine dneigh(rnorm, n, h, ldh, ritzr,&
                  ritzi, bounds, q, ldq, workl,&
                  ierr)
!-----------------------------------------------------------------------
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
!     SUBROUTINE ARPACK CALCULANT LES MODES PROPRES DE LA MATRICE DE
!     HESSENBERG AINSI QUE LEURS ERREURS.
!-----------------------------------------------------------------------
! BEGINDOC
!
! DESCRIPTION:
!  COMPUTE THE EIGENVALUES OF THE CURRENT UPPER HESSENBERG MATRIX
!  AND THE CORRESPONDING RITZ ESTIMATES GIVEN THE CURRENT RESIDUAL NORM.
!
! ARGUMENTS
!  RNORM   REAL*8 SCALAR.  (INPUT)
!          RESIDUAL NORM CORRESPONDING TO THE CURRENT UPPER HESSENBERG
!          MATRIX H.
!
!  N       INTEGER.  (INPUT)
!          SIZE OF THE MATRIX H.
!
!  H       REAL*8 N BY N ARRAY.  (INPUT)
!          H CONTAINS THE CURRENT UPPER HESSENBERG MATRIX.
!
!  LDH     INTEGER.  (INPUT)
!          LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  RITZR,  REAL*8 ARRAYS OF LENGTH N.  (OUTPUT)
!  RITZI   ON OUTPUT, RITZR(1:N) (RESP. RITZI(1:N)) CONTAINS THE REAL
!          (RESPECTIVELY IMAGINARY) PARTS OF THE EIGENVALUES OF H.
!
!  BOUNDS  REAL*8 ARRAY OF LENGTH N.  (OUTPUT)
!         ON OUTPUT, BOUNDS CONTAINS THE RITZ ESTIMATES ASSOCIATED WITH
!         THE EIGENVALUES RITZR AND RITZI.  THIS IS EQUAL TO RNORM
!         TIMES THE LAST COMPONENTS OF THE EIGENVECTORS CORRESPONDING
!         TO THE EIGENVALUES IN RITZR AND RITZI.
!
!  Q       REAL*8 N BY N ARRAY.  (WORKSPACE)
!          WORKSPACE NEEDED TO STORE THE EIGENVECTORS OF H.
!
!  LDQ     INTEGER.  (INPUT)
!          LEADING DIMENSION OF Q EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  WORKL   REAL*8 WORK ARRAY OF LENGTH N**2 + 3*N.  (WORKSPACE)
!          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
!          THE FRONT END.  THIS IS NEEDED TO KEEP THE FULL SCHUR FORM
!          OF H AND ALSO IN THE CALCULATION OF THE EIGENVECTORS OF H.
!
!  IERR    INTEGER.  (OUTPUT)
!          ERROR EXIT FLAG FROM DLAQRB OR FTREVC.
!
! ENDDOC
!-----------------------------------------------------------------------
!
! BEGINLIB
!
! ROUTINES CALLED:
!     DLAQRB  ARPACK ROUTINE TO COMPUTE THE REAL SCHUR FORM OF AN
!             UPPER HESSENBERG MATRIX AND LAST ROW OF THE SCHUR VECTORS.
!     DMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
!     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
!     DLACPY  LAPACK MATRIX COPY ROUTINE.
!     DLAPY2  LAPACK ROUTINE TO COMPUTE SQRT(X**2+Y**2) CAREFULLY.
!     FTREVC  LAPACK ROUTINE TO COMPUTE THE EIGENVECTORS OF A MATRIX
!             IN UPPER QUASI-TRIANGULAR FORM
!     DGEMV   LEVEL 2 BLAS ROUTINE FOR MATRIX VECTOR MULTIPLICATION.
!     DCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER .
!     DNRM2   LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
!     DSCAL   LEVEL 1 BLAS THAT SCALES A VECTOR.
!
! INTRINSIC FUNCTIONS
!
!     ABS
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
! FILE: NEIGH.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE SECOND,
!            COMMON TIMING REMPLACE PAR COMMON INFOR,
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
!
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     %-----------------------------%
!     | INCLUDE FILES FOR DEBUGGING |
!     %-----------------------------%
!
#include "asterc/matfpe.h"
#include "asterfort/dlaqrb.h"
#include "asterfort/dmout.h"
#include "asterfort/dvout.h"
#include "asterfort/ftrevc.h"
#include "blas/dgemv.h"
#include "blas/dlacpy.h"
#include "blas/dlapy2.h"
#include "blas/dnrm2.h"
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
    integer :: ierr, n, ldh, ldq
    real(kind=8) :: rnorm
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    real(kind=8) :: bounds(n), h(ldh, n), q(ldq, n), ritzi(n), ritzr(n)
    real(kind=8) :: workl(n*(n+3))
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
    logical(kind=1) :: select(1)
    integer :: i, iconj, msglvl
    real(kind=8) :: temp, vl(1)
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
!     %-------------------------------%
!     | INITIALIZE TIMING STATISTICS  |
!     | & MESSAGE LEVEL FOR DEBUGGING |
!     %-------------------------------%
!
    msglvl = mneigh
!
    if (msglvl .gt. 2) then
        call dmout(logfil, n, n, h, ldh,&
                   ndigit, '_NEIGH: ENTERING UPPER HESSENBERG MATRIX H ')
    endif
!
!     %-----------------------------------------------------------%
!     | 1. COMPUTE THE EIGENVALUES, THE LAST COMPONENTS OF THE    |
!     |    CORRESPONDING SCHUR VECTORS AND THE FULL SCHUR FORM T  |
!     |    OF THE CURRENT UPPER HESSENBERG MATRIX H.              |
!     | DLAQRB RETURNS THE FULL SCHUR FORM OF H IN WORKL(1:N**2)  |
!     | AND THE LAST COMPONENTS OF THE SCHUR VECTORS IN BOUNDS.   |
!     %-----------------------------------------------------------%
! DUE TO CRP_102 CALL DLACPY ('ALL', N, N, H, LDH, WORKL, N)
    call dlacpy('A', n, n, h, ldh,&
                workl, n)
    call dlaqrb(.true._1, n, 1, n, workl,&
                n, ritzr, ritzi, bounds, ierr)
    if (ierr .ne. 0) goto 9000
!
    if (msglvl .gt. 1) then
        call dvout(logfil, n, bounds, ndigit, '_NEIGH: LAST ROW OF THE SCHUR MATRIX FOR H')
    endif
!
!     %-----------------------------------------------------------%
!     | 2. COMPUTE THE EIGENVECTORS OF THE FULL SCHUR FORM T AND  |
!     |    APPLY THE LAST COMPONENTS OF THE SCHUR VECTORS TO GET  |
!     |    THE LAST COMPONENTS OF THE CORRESPONDING EIGENVECTORS. |
!     | REMEMBER THAT IF THE I-TH AND (I+1)-ST EIGENVALUES ARE    |
!     | COMPLEX CONJUGATE PAIRS, THEN THE REAL & IMAGINARY PART   |
!     | OF THE EIGENVECTOR COMPONENTS ARE SPLIT ACROSS ADJACENT   |
!     | COLUMNS OF Q.                                             |
!     %-----------------------------------------------------------%
!
    call ftrevc('R', 'A', select, n, workl,&
                n, vl, n, q, ldq,&
                n, n, workl(n*n+1), ierr)
!
    if (ierr .ne. 0) goto 9000
!
!     %------------------------------------------------%
!     | SCALE THE RETURNING EIGENVECTORS SO THAT THEIR |
!     | EUCLIDEAN NORMS ARE ALL ONE. LAPACK SUBROUTINE |
!     | FTREVC RETURNS EACH EIGENVECTOR NORMALIZED SO  |
!     | THAT THE ELEMENT OF LARGEST MAGNITUDE HAS      |
!     | MAGNITUDE 1, HERE THE MAGNITUDE OF A COMPLEX   |
!     | NUMBER (X,Y) IS TAKEN TO BE |X| + |Y|.         |
!     %------------------------------------------------%
!
    iconj = 0
    do 10 i = 1, n
        if (abs( ritzi(i) ) .le. zero) then
!
!           %----------------------%
!           | REAL EIGENVALUE CASE |
!           %----------------------%
            temp = dnrm2( n, q(1,i), 1 )
            call dscal(n, one / temp, q(1, i), 1)
        else
!
!           %-------------------------------------------%
!           | COMPLEX CONJUGATE PAIR CASE. NOTE THAT    |
!           | SINCE THE REAL AND IMAGINARY PART OF      |
!           | THE EIGENVECTOR ARE STORED IN CONSECUTIVE |
!           | COLUMNS, WE FURTHER NORMALIZE BY THE      |
!           | SQUARE ROOT OF TWO.                       |
!           %-------------------------------------------%
!
            if (iconj .eq. 0) then
                temp = dlapy2(dnrm2( n, q(1,i), 1 ), dnrm2( n, q(1,i+ 1), 1 ))
                call dscal(n, one / temp, q(1, i), 1)
                call dscal(n, one / temp, q(1, i+1), 1)
                iconj = 1
            else
                iconj = 0
            endif
        endif
10  end do
!
    call dgemv('T', n, n, one, q,&
               ldq, bounds, 1, zero, workl,&
               1)
!
    if (msglvl .gt. 1) then
        call dvout(logfil, n, workl, ndigit, '_NEIGH: LAST ROW OF THE EIGENVECTOR MATRIX FOR H')
    endif
!
!     %----------------------------%
!     | COMPUTE THE RITZ ESTIMATES |
!     %----------------------------%
!
    iconj = 0
    do 20 i = 1, n
        if (abs( ritzi(i) ) .le. zero) then
!
!           %----------------------%
!           | REAL EIGENVALUE CASE |
!           %----------------------%
!
            bounds(i) = rnorm * abs( workl(i) )
        else
!
!           %-------------------------------------------%
!           | COMPLEX CONJUGATE PAIR CASE. NOTE THAT    |
!           | SINCE THE REAL AND IMAGINARY PART OF      |
!           | THE EIGENVECTOR ARE STORED IN CONSECUTIVE |
!           | COLUMNS, WE NEED TO TAKE THE MAGNITUDE    |
!           | OF THE LAST COMPONENTS OF THE TWO VECTORS |
!           %-------------------------------------------%
!
            if (iconj .eq. 0) then
                bounds(i) = rnorm * dlapy2( workl(i), workl(i+1) )
                bounds(i+1) = bounds(i)
                iconj = 1
            else
                iconj = 0
            endif
        endif
20  end do
!
    if (msglvl .gt. 2) then
        call dvout(logfil, n, ritzr, ndigit, '_NEIGH: REAL PART OF THE EIGENVALUES OF H')
        call dvout(logfil, n, ritzi, ndigit, '_NEIGH: IMAGINARY PART OF THE EIGENVALUES OF H')
        call dvout(logfil, n, bounds, ndigit, '_NEIGH: RITZ ESTIMATES FOR THE EIGENVALUES OF H')
    endif
!
9000  continue
!
    call matfpe(1)
!
!     %---------------%
!     | END OF DNEIGH |
!     %---------------%
!
end subroutine
