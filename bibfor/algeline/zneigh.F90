subroutine zneigh(rnorm, n, h, ldh, ritz,&
                  bounds, q, ldq, workl, rwork,&
                  ierr)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     SUBROUTINE ARPACK CALCULANT LES MODES PROPRES DE LA MATRICE DE
!     HESSENBERG AINSI QUE LEURS ERREURS.
!-----------------------------------------------------------------------
!\BEGINDOC
!
!\NAME: ZNEIGH
!
!\DESCRIPTION:
!  COMPUTE THE EIGENVALUES OF THE CURRENT UPPER HESSENBERG MATRIX
!  AND THE CORRESPONDING RITZ ESTIMATES GIVEN THE CURRENT RESIDUAL NORM.
!
!\USAGE:
!  CALL ZNEIGH
!     ( RNORM, N, H, LDH, RITZ, BOUNDS, Q, LDQ, WORKL, RWORK, IERR )
!
!\ARGUMENTS
!  RNORM   DOUBLE PRECISION SCALAR.  (INPUT)
!          RESIDUAL NORM CORRESPONDING TO THE CURRENT UPPER HESSENBERG
!          MATRIX H.
!
!  N       INTEGER.  (INPUT)
!          SIZE OF THE MATRIX H.
!
!  H       COMPLEX*16 N BY N ARRAY.  (INPUT)
!          H CONTAINS THE CURRENT UPPER HESSENBERG MATRIX.
!
!  LDH     INTEGER.  (INPUT)
!          LEADING DIMENSION OF H EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  RITZ    COMPLEX*16 ARRAY OF LENGTH N.  (OUTPUT)
!          ON OUTPUT, RITZ(1:N) CONTAINS THE EIGENVALUES OF H.
!
!  BOUNDS  COMPLEX*16 ARRAY OF LENGTH N.  (OUTPUT)
!          ON OUTPUT, BOUNDS CONTAINS THE RITZ ESTIMATES ASSOCIATED WITH
!          THE EIGENVALUES HELD IN RITZ.  THIS IS EQUAL TO RNORM
!          TIMES THE LAST COMPONENTS OF THE EIGENVECTORS CORRESPONDING
!          TO THE EIGENVALUES IN RITZ.
!
!  Q       COMPLEX*16 N BY N ARRAY.  (WORKSPACE)
!          WORKSPACE NEEDED TO STORE THE EIGENVECTORS OF H.
!
!  LDQ     INTEGER.  (INPUT)
!          LEADING DIMENSION OF Q EXACTLY AS DECLARED IN THE CALLING
!          PROGRAM.
!
!  WORKL   COMPLEX*16 WORK ARRAY OF LENGTH N**2 + 3*N.  (WORKSPACE)
!          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
!          THE FRONT END.  THIS IS NEEDED TO KEEP THE FULL SCHUR FORM
!          OF H AND ALSO IN THE CALCULATION OF THE EIGENVECTORS OF H.
!
!  RWORK   DOUBLE PRECISION  WORK ARRAY OF LENGTH N (WORKSPACE)
!          PRIVATE (REPLICATED) ARRAY ON EACH PE OR ARRAY ALLOCATED ON
!          THE FRONT END.
!
!  IERR    INTEGER.  (OUTPUT)
!          ERROR EXIT FLAG FROM ZLAHQR OR GTREVC.
!
!\ENDDOC
!
!-----------------------------------------------------------------------
!
!\BEGINLIB
!
!\LOCAL VARIABLES:
!     XXXXXX  COMPLEX*16
!
!\ROUTINES CALLED:
!     IVOUT   ARPACK UTILITY ROUTINE THAT PRINTS INTEGERS.
!     ZMOUT   ARPACK UTILITY ROUTINE THAT PRINTS MATRICES
!     ZVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
!     DVOUT   ARPACK UTILITY ROUTINE THAT PRINTS VECTORS.
!     ZLACPY  LAPACK MATRIX COPY ROUTINE.
!     ZLAHQR  LAPACK ROUTINE TO COMPUTE THE SCHUR FORM OF AN
!             UPPER HESSENBERG MATRIX.
!     ZLASET  LAPACK MATRIX INITIALIZATION ROUTINE.
!     GTREVC  LAPACK ROUTINE TO COMPUTE THE EIGENVECTORS OF A MATRIX
!             IN UPPER TRIANGULAR FORM
!     ZCOPY   LEVEL 1 BLAS THAT COPIES ONE VECTOR TO ANOTHER.
!     ZDSCAL  LEVEL 1 BLAS THAT SCALES A COMPLEX VECTOR BY A REAL
!             NUMBER.
!     DZNRM2  LEVEL 1 BLAS THAT COMPUTES THE NORM OF A VECTOR.
!
!
!\AUTHOR
!     DANNY SORENSEN               PHUONG VU
!     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
!     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
!     APPLIED MATHEMATICS
!     RICE UNIVERSITY
!     HOUSTON, TEXAS
!
!\SCCS INFORMATION: @(#)
! FILE: NEIGH.F   SID: 2.2   DATE OF SID: 4/20/96   RELEASE: 2
!
!\REMARKS
!     NONE
!
!\ENDLIB
!
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
#include "asterfort/gtrevc.h"
#include "asterfort/zmout.h"
#include "asterfort/zvout.h"
#include "blas/dznrm2.h"
#include "blas/zcopy.h"
#include "blas/zdscal.h"
#include "blas/zlacpy.h"
#include "blas/zlahqr.h"
#include "blas/zlaset.h"
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
    integer(kind=4) :: ierr4
    real(kind=8) :: rnorm
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    complex(kind=8) :: bounds(n), h(ldh, n), q(ldq, n), ritz(n), workl(n*(n+3))
    real(kind=8) :: rwork(n)
!
!     %------------%
!     | PARAMETERS |
!     %------------%
!
    complex(kind=8) :: one, zero
    real(kind=8) :: rone
    parameter  (one = (1.0d+0, 0.0d+0), zero = (0.0d+0, 0.0d+0),&
     &           rone = 1.0d+0)
!
!     %------------------------%
!     | LOCAL SCALARS & ARRAYS |
!     %------------------------%
!
    aster_logical :: select(1)
    integer :: j, msglvl
    complex(kind=8) :: vl(1)
    real(kind=8) :: temp
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
!     %-------------------------------%
!     | INITIALIZE TIMING STATISTICS  |
!     | & MESSAGE LEVEL FOR DEBUGGING |
!     %-------------------------------%
!
    msglvl = mneigh
!
    if (msglvl .gt. 2) then
        call zmout(logfil, n, n, h, ldh,&
                   ndigit, '_NEIGH: ENTERING UPPER HESSENBERG MATRIX H ')
    endif
!
!     %----------------------------------------------------------%
!     | 1. COMPUTE THE EIGENVALUES, THE LAST COMPONENTS OF THE   |
!     |    CORRESPONDING SCHUR VECTORS AND THE FULL SCHUR FORM T |
!     |    OF THE CURRENT UPPER HESSENBERG MATRIX H.             |
!     |    ZLAHQR RETURNS THE FULL SCHUR FORM OF H               |
!     |    IN WORKL(1:N**2), AND THE SCHUR VECTORS IN Q.         |
!     %----------------------------------------------------------%
!
    call zlacpy('A', n, n, h, ldh,&
                workl, n)
    call zlaset('A', n, n, zero, one,&
                q, ldq)
!
    call zlahqr(.true._1, .true._1, n, 1, n,&
                workl, ldh, ritz, 1, n,&
                q, ldq, ierr4)
    if (ierr4 .ne. 0) goto 9000
!
    call zcopy(n, q(n-1, 1), ldq, bounds, 1)
    if (msglvl .gt. 1) then
        call zvout(logfil, n, bounds, ndigit, '_NEIGH: LAST ROW OF THE SCHUR MATRIX FOR H')
    endif
!
!     %----------------------------------------------------------%
!     | 2. COMPUTE THE EIGENVECTORS OF THE FULL SCHUR FORM T AND |
!     |    APPLY THE SCHUR VECTORS TO GET THE CORRESPONDING      |
!     |    EIGENVECTORS.                                         |
!     %----------------------------------------------------------%
!
    call gtrevc('R', 'B', select, n, workl,&
                n, vl, n, q, ldq,&
                n, n, workl(n*n+1), rwork, ierr)
!
    if (ierr .ne. 0) goto 9000
!
!     %------------------------------------------------%
!     | SCALE THE RETURNING EIGENVECTORS SO THAT THEIR |
!     | EUCLIDEAN NORMS ARE ALL ONE. LAPACK SUBROUTINE |
!     | GTREVC RETURNS EACH EIGENVECTOR NORMALIZED SO  |
!     | THAT THE ELEMENT OF LARGEST MAGNITUDE HAS      |
!     | MAGNITUDE 1; HERE THE MAGNITUDE OF A COMPLEX   |
!     | NUMBER (X,Y) IS TAKEN TO BE |X| + |Y|.         |
!     %------------------------------------------------%
!
    do 10 j = 1, n
        temp = dznrm2( n, q(1,j), 1 )
        call zdscal(n, rone / temp, q(1, j), 1)
 10 end do
!
    if (msglvl .gt. 1) then
        call zcopy(n, q(n, 1), ldq, workl, 1)
        call zvout(logfil, n, workl, ndigit, '_NEIGH: LAST ROW OF THE EIGENVECTOR MATRIX FOR H')
    endif
!
!     %----------------------------%
!     | COMPUTE THE RITZ ESTIMATES |
!     %----------------------------%
!
    call zcopy(n, q(n, 1), n, bounds, 1)
    call zdscal(n, rnorm, bounds, 1)
!
    if (msglvl .gt. 2) then
        call zvout(logfil, n, ritz, ndigit, '_NEIGH: THE EIGENVALUES OF H')
        call zvout(logfil, n, bounds, ndigit, '_NEIGH: RITZ ESTIMATES FOR THE EIGENVALUES OF H')
    endif
!
!
9000 continue
    call matfpe(1)
!
!     %---------------%
!     | END OF ZNEIGH |
!     %---------------%
!
end subroutine
