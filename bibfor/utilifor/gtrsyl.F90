subroutine gtrsyl(trana, tranb, isgn, m, n,&
                  a, lda, b, ldb, c,&
                  ldc, scale, info)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!  -- LAPACK ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     MARCH 31, 1993
!
!
!  PURPOSE
!  =======
!
!  GTRSYL SOLVES THE COMPLEX SYLVESTER MATRIX EQUATION:
!
!     OP(A)*X + X*OP(B) = SCALE*C OR
!     OP(A)*X - X*OP(B) = SCALE*C,
!
!  WHERE OP(A) = A OR A**H, AND A AND B ARE BOTH UPPER TRIANGULAR. A IS
!  M-BY-M AND B IS N-BY-N; THE RIGHT HAND SIDE C AND THE SOLUTION X ARE
!  M-BY-N; AND SCALE IS AN OUTPUT SCALE FACTOR, SET <= 1 TO AVOID
!  OVERFLOW IN X.
!
!  ARGUMENTS
!  =========
!
!  TRANA   (INPUT) CHARACTER*1
!          SPECIFIES THE OPTION OP(A):
!          = 'N': OP(A) = A    (NO TRANSPOSE)
!          = 'C': OP(A) = A**H (CONJUGATE TRANSPOSE)
!
!  TRANB   (INPUT) CHARACTER*1
!          SPECIFIES THE OPTION OP(B):
!          = 'N': OP(B) = B    (NO TRANSPOSE)
!          = 'C': OP(B) = B**H (CONJUGATE TRANSPOSE)
!
!  ISGN    (INPUT) INTEGER
!          SPECIFIES THE SIGN IN THE EQUATION:
!          = +1: SOLVE OP(A)*X + X*OP(B) = SCALE*C
!          = -1: SOLVE OP(A)*X - X*OP(B) = SCALE*C
!
!  M       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX A, AND THE NUMBER OF ROWS IN THE
!          MATRICES X AND C. M >= 0.
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX B, AND THE NUMBER OF COLUMNS IN THE
!          MATRICES X AND C. N >= 0.
!
!  A       (INPUT) COMPLEX*16 ARRAY, DIMENSION (LDA,M)
!          THE UPPER TRIANGULAR MATRIX A.
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY A. LDA >= MAX(1,M).
!
!  B       (INPUT) COMPLEX*16 ARRAY, DIMENSION (LDB,N)
!          THE UPPER TRIANGULAR MATRIX B.
!
!  LDB     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY B. LDB >= MAX(1,N).
!
!  C       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDC,N)
!          ON ENTRY, THE M-BY-N RIGHT HAND SIDE MATRIX C.
!          ON EXIT, C IS OVERWRITTEN BY THE SOLUTION MATRIX X.
!
!  LDC     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY C. LDC >= MAX(1,M)
!
!  SCALE   (OUTPUT) DOUBLE PRECISION
!          THE SCALE FACTOR, SCALE, SET <= 1 TO AVOID OVERFLOW IN X.
!
!  INFO    (OUTPUT) INTEGER
!          = 0: SUCCESSFUL EXIT
!          < 0: IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
!          = 1: A AND B HAVE COMMON OR VERY CLOSE EIGENVALUES; PERTURBED
!               VALUES WERE USED TO SOLVE THE EQUATION (BUT THE MATRICES
!               A AND B ARE UNCHANGED).
!
!  =====================================================================
!-----------------------------------------------------------------------
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
!            IMPLICIT NONE.
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
    include 'asterc/isbaem.h'
    include 'asterc/matfpe.h'
    include 'asterc/r8prem.h'
    include 'asterfort/xerbla.h'
    include 'blas/lsame.h'
    include 'blas/zdotc.h'
    include 'blas/zdotu.h'
    include 'blas/zdscal.h'
    include 'blas/zlange.h'
    character(len=1) :: trana, tranb
    integer :: info, isgn, lda, ldb, ldc, m, n
    real(kind=8) :: scale
!     ..
!     .. ARRAY ARGUMENTS ..
    complex(kind=8) :: a( lda, * ), b( ldb, * ), c( ldc, * )
!     ..
!     .. PARAMETERS ..
    real(kind=8) :: one
    parameter          ( one = 1.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    logical :: notrna, notrnb
    integer :: j, k, l
    real(kind=8) :: bignum, da11, db, eps, scaloc, sgn, smin, smlnum, ulp
    complex(kind=8) :: a11, suml, sumr, vec, x11
!     ..
!     .. LOCAL ARRAYS ..
    real(kind=8) :: dum
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
!     DECODE AND TEST INPUT PARAMETERS
!
    notrna = lsame( trana, 'N' )
    notrnb = lsame( tranb, 'N' )
!
    info = 0
    dum = 0.0d0
    if (.not.notrna .and. .not.lsame( trana, 'T' ) .and. .not. lsame( trana, 'C' )) then
        info = -1
        else if( .not.notrnb .and. .not.lsame( tranb, 'T' ) .and. .not.&
    lsame( tranb, 'C' ) ) then
        info = -2
    else if (isgn.ne.1 .and. isgn.ne.-1) then
        info = -3
    else if (m.lt.0) then
        info = -4
    else if (n.lt.0) then
        info = -5
    else if (lda.lt.max( 1, m )) then
        info = -7
    else if (ldb.lt.max( 1, n )) then
        info = -9
    else if (ldc.lt.max( 1, m )) then
        info = -11
    endif
    if (info .ne. 0) then
        call xerbla('GTRSYL', -info)
        goto 1000
    endif
!
!     QUICK RETURN IF POSSIBLE
!
    if (m .eq. 0 .or. n .eq. 0) goto 1000
!
!     SET CONSTANTS TO CONTROL OVERFLOW
!
    eps = r8prem()
! DUE TO CRS512      OVFL = ONE / UNFL
    ulp = r8prem() * 0.5d0 * isbaem()
    smlnum = eps*( n / ulp )
    bignum = ( one-ulp ) / smlnum
    smin = max(smlnum, eps*zlange( 'M', m, m, a, lda, dum ), eps*zlange( 'M', n, n, b, ldb, dum )&
           )
    scale = one
    sgn = isgn
!
    if (notrna .and. notrnb) then
!
!        SOLVE    A*X + ISGN*X*B = SCALE*C.
!
!        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
!        BOTTOM-LEFT CORNER COLUMN BY COLUMN BY
!
!            A(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L)
!
!        WHERE
!                    M                        L-1
!          R(K,L) = SUM [A(K,I)*X(I,L)] +ISGN*SUM [X(K,J)*B(J,L)].
!                  I=K+1                      J=1
!
        do 30 l = 1, n
            do 20 k = m, 1, -1
!
                suml = zdotu( m-k, a( k, min( k+1, m ) ), lda, c( min( k+1, m ), l ), 1 )
                sumr = zdotu( l-1, c( k, 1 ), ldc, b( 1, l ), 1 )
                vec = c( k, l ) - ( suml+sgn*sumr )
!
                scaloc = one
                a11 = a( k, k ) + sgn*b( l, l )
                da11 = abs( dble( a11 ) ) + abs( dimag( a11 ) )
                if (da11 .le. smin) then
                    a11 = smin
                    da11 = smin
                    info = 1
                endif
                db = abs( dble( vec ) ) + abs( dimag( vec ) )
                if (da11 .lt. one .and. db .gt. one) then
                    if (db .gt. bignum*da11) scaloc = one / db
                endif
                x11 = ( vec*dcmplx( scaloc ) ) / a11
!
                if (scaloc .ne. one) then
                    do 10 j = 1, n
                        call zdscal(m, scaloc, c( 1, j ), 1)
10                  continue
                    scale = scale*scaloc
                endif
                c( k, l ) = x11
!
20          continue
30      continue
!
    else if (.not.notrna .and. notrnb) then
!
!        SOLVE    A' *X + ISGN*X*B = SCALE*C.
!
!        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
!        UPPER-LEFT CORNER COLUMN BY COLUMN BY
!
!            A'(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L)
!
!        WHERE
!                   K-1                         L-1
!          R(K,L) = SUM [A'(I,K)*X(I,L)] + ISGN*SUM [X(K,J)*B(J,L)]
!                   I=1                         J=1
!
        do 60 l = 1, n
            do 50 k = 1, m
!
                suml = zdotc( k-1, a( 1, k ), 1, c( 1, l ), 1 )
                sumr = zdotu( l-1, c( k, 1 ), ldc, b( 1, l ), 1 )
                vec = c( k, l ) - ( suml+sgn*sumr )
!
                scaloc = one
                a11 = dconjg( a( k, k ) ) + sgn*b( l, l )
                da11 = abs( dble( a11 ) ) + abs( dimag( a11 ) )
                if (da11 .le. smin) then
                    a11 = smin
                    da11 = smin
                    info = 1
                endif
                db = abs( dble( vec ) ) + abs( dimag( vec ) )
                if (da11 .lt. one .and. db .gt. one) then
                    if (db .gt. bignum*da11) scaloc = one / db
                endif
!
                x11 = ( vec*dcmplx( scaloc ) ) / a11
!
                if (scaloc .ne. one) then
                    do 40 j = 1, n
                        call zdscal(m, scaloc, c( 1, j ), 1)
40                  continue
                    scale = scale*scaloc
                endif
                c( k, l ) = x11
!
50          continue
60      continue
!
    else if (.not.notrna .and. .not.notrnb) then
!
!        SOLVE    A'*X + ISGN*X*B' = C.
!
!        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
!        UPPER-RIGHT CORNER COLUMN BY COLUMN BY
!
!            A'(K,K)*X(K,L) + ISGN*X(K,L)*B'(L,L) = C(K,L) - R(K,L)
!
!        WHERE
!                    K-1
!           R(K,L) = SUM [A'(I,K)*X(I,L)] +
!                    I=1
!                           N
!                     ISGN*SUM [X(K,J)*B'(L,J)].
!                          J=L+1
!
        do 90 l = n, 1, -1
            do 80 k = 1, m
!
                suml = zdotc( k-1, a( 1, k ), 1, c( 1, l ), 1 )
                sumr = zdotc( n-l, c( k, min( l+1, n ) ), ldc, b( l, min( l+1, n ) ), ldb )
                vec = c( k, l ) - ( suml+sgn*dconjg( sumr ) )
!
                scaloc = one
                a11 = dconjg( a( k, k )+sgn*b( l, l ) )
                da11 = abs( dble( a11 ) ) + abs( dimag( a11 ) )
                if (da11 .le. smin) then
                    a11 = smin
                    da11 = smin
                    info = 1
                endif
                db = abs( dble( vec ) ) + abs( dimag( vec ) )
                if (da11 .lt. one .and. db .gt. one) then
                    if (db .gt. bignum*da11) scaloc = one / db
                endif
!
                x11 = ( vec*dcmplx( scaloc ) ) / a11
!
                if (scaloc .ne. one) then
                    do 70 j = 1, n
                        call zdscal(m, scaloc, c( 1, j ), 1)
70                  continue
                    scale = scale*scaloc
                endif
                c( k, l ) = x11
!
80          continue
90      continue
!
    else if (notrna .and. .not.notrnb) then
!
!        SOLVE    A*X + ISGN*X*B' = C.
!
!        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
!        BOTTOM-LEFT CORNER COLUMN BY COLUMN BY
!
!           A(K,K)*X(K,L) + ISGN*X(K,L)*B'(L,L) = C(K,L) - R(K,L)
!
!        WHERE
!                    M                          N
!          R(K,L) = SUM [A(K,I)*X(I,L)] + ISGN*SUM [X(K,J)*B'(L,J)]
!                  I=K+1                      J=L+1
!
        do 120 l = n, 1, -1
            do 110 k = m, 1, -1
!
                suml = zdotu( m-k, a( k, min( k+1, m ) ), lda, c( min( k+1, m ), l ), 1 )
                sumr = zdotc( n-l, c( k, min( l+1, n ) ), ldc, b( l, min( l+1, n ) ), ldb )
                vec = c( k, l ) - ( suml+sgn*dconjg( sumr ) )
!
                scaloc = one
                a11 = a( k, k ) + sgn*dconjg( b( l, l ) )
                da11 = abs( dble( a11 ) ) + abs( dimag( a11 ) )
                if (da11 .le. smin) then
                    a11 = smin
                    da11 = smin
                    info = 1
                endif
                db = abs( dble( vec ) ) + abs( dimag( vec ) )
                if (da11 .lt. one .and. db .gt. one) then
                    if (db .gt. bignum*da11) scaloc = one / db
                endif
!
                x11 = ( vec*dcmplx( scaloc ) ) / a11
!
                if (scaloc .ne. one) then
                    do 100 j = 1, n
                        call zdscal(m, scaloc, c( 1, j ), 1)
100                  continue
                    scale = scale*scaloc
                endif
                c( k, l ) = x11
!
110          continue
120      continue
!
    endif
!
1000  continue
    call matfpe(1)
!
!     END OF GTRSYL
!
end subroutine
