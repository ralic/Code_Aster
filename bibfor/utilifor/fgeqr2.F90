subroutine fgeqr2(m, n, a, lda, tau,&
                  work, info)
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
!     SUBROUTINE LAPACK CALCULANT UNE FACTORISATION QR.
!-----------------------------------------------------------------------
!  -- LAPACK ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     FEBRUARY 29, 1992
!
!  PURPOSE
!  =======
!
!  FGEQR2 COMPUTES A QR FACTORIZATION OF A REAL M BY N MATRIX A:
!  A = Q * R.
!
!  ARGUMENTS
!  =========
!
!  M       (INPUT) INTEGER
!          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
!
!  N       (INPUT) INTEGER
!          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
!
!  A       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDA,N)
!          ON ENTRY, THE M BY N MATRIX A.
!          ON EXIT, THE ELEMENTS ON AND ABOVE THE DIAGONAL OF THE ARRAY
!          CONTAIN THE MIN(M,N) BY N UPPER TRAPEZOIDAL MATRIX R (R IS
!          UPPER TRIANGULAR IF M >= N), THE ELEMENTS BELOW THE DIAGONAL,
!          WITH THE ARRAY TAU, REPRESENT THE ORTHOGONAL MATRIX Q AS A
!          PRODUCT OF ELEMENTARY REFLECTORS (SEE FURTHER DETAILS).
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
!
!  TAU     (OUTPUT) REAL*8 ARRAY, DIMENSION (MIN(M,N))
!          THE SCALAR FACTORS OF THE ELEMENTARY REFLECTORS (SEE FURTHER
!          DETAILS).
!
!  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION (N)
!
!  INFO    (OUTPUT) INTEGER
!          = 0: SUCCESSFUL EXIT
!          < 0: IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
!
!  FURTHER DETAILS
!  ===============
!
!  THE MATRIX Q IS REPRESENTED AS A PRODUCT OF ELEMENTARY REFLECTORS
!
!     Q = H(1) H(2) . . . H(K), WHERE K = MIN(M,N).
!
!  EACH H(I) HAS THE FORM
!
!     H(I) = I - TAU * V * V'
!
!  WHERE TAU IS A REAL SCALAR, AND V IS A REAL VECTOR WITH
!  V(1:I-1) = 0 AND V(I) = 1, V(I+1:M) IS STORED ON EXIT IN A(I+1:M,I),
!  AND TAU IN TAU(I).
!
!-----------------------------------------------------------------------
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
!            IMPLICIT NONE.
! INTRINSIC FUNCTIONS
!            MAX, MIN.
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
#include "asterc/matfpe.h"
#include "asterfort/flarfg.h"
#include "asterfort/xerbla.h"
#include "blas/dlarf.h"
    integer :: info, lda, m, n
!     ..
!     .. ARRAY ARGUMENTS ..
    real(kind=8) :: a( lda, * ), tau( * ), work( * )
!     ..
!     .. PARAMETERS ..
    real(kind=8) :: one
    parameter          ( one = 1.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    integer :: i, k
    real(kind=8) :: aii
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
!     TEST THE INPUT ARGUMENTS
!
    info = 0
    if (m .lt. 0) then
        info = -1
    else if (n.lt.0) then
        info = -2
    else if (lda.lt.max( 1, m )) then
        info = -4
    endif
    if (info .ne. 0) then
        call xerbla('FGEQR2', -info)
        goto 1000
    endif
!
    k = min( m, n )
!
    do 10 i = 1, k
!
!        GENERATE ELEMENTARY REFLECTOR H(I) TO ANNIHILATE A(I+1:M,I)
!
        call flarfg(m-i+1, a( i, i ), a( min( i+1, m ), i ), 1, tau( i ))
        if (i .lt. n) then
!
!           APPLY H(I) TO A(I:M,I+1:N) FROM THE LEFT
!
            aii = a( i, i )
            a( i, i ) = one
            call dlarf('L', m-i+1, n-i, a( i, i ), 1,&
                       tau( i ), a( i, i+1 ), lda, work)
            a( i, i ) = aii
        endif
10  end do
1000  continue
!
    call matfpe(1)
!
!     END OF FGEQR2
!
end subroutine
