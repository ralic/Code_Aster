subroutine flrsyl(trana, tranb, isgn, m, n,&
                  a, lda, b, ldb, c,&
                  ldc, scale, info)
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
!     SUBROUTINE LAPACK RESOLVANT L'EQUATION DE SYLVESTER.
!-----------------------------------------------------------------------
!  -- LAPACK ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     MARCH 31, 1993
!
!  PURPOSE
!  =======
!
!  FLRSYL SOLVES THE REAL SYLVESTER MATRIX EQUATION:
!
!     OP(A)*X + X*OP(B) = SCALE*C OR
!     OP(A)*X - X*OP(B) = SCALE*C,
!
!  WHERE OP(A) = A OR A**T, AND  A AND B ARE BOTH UPPER QUASI-
!  TRIANGULAR. A IS M-BY-M AND B IS N-BY-N, THE RIGHT HAND SIDE C AND
!  THE SOLUTION X ARE M-BY-N, AND SCALE IS AN OUTPUT SCALE FACTOR, SET
!  <= 1 TO AVOID OVERFLOW IN X.
!
!  A AND B MUST BE IN SCHUR CANONICAL FORM (AS RETURNED BY DHSEQR), THAT
!  IS, BLOCK UPPER TRIANGULAR WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS,
!  EACH 2-BY-2 DIAGONAL BLOCK HAS ITS DIAGONAL ELEMENTS EQUAL AND ITS
!  OFF-DIAGONAL ELEMENTS OF OPPOSITE SIGN.
!
!  ARGUMENTS
!  =========
!
!  TRANA   (INPUT) CHARACTER*1
!          SPECIFIES THE OPTION OP(A):
!          = 'N': OP(A) = A    (NO TRANSPOSE)
!          = 'T': OP(A) = A**T (TRANSPOSE)
!          = 'C': OP(A) = A**H (CONJUGATE TRANSPOSE = TRANSPOSE)
!
!  TRANB   (INPUT) CHARACTER*1
!          SPECIFIES THE OPTION OP(B):
!          = 'N': OP(B) = B    (NO TRANSPOSE)
!          = 'T': OP(B) = B**T (TRANSPOSE)
!          = 'C': OP(B) = B**H (CONJUGATE TRANSPOSE = TRANSPOSE)
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
!  A       (INPUT) REAL*8 ARRAY, DIMENSION (LDA,M)
!          THE UPPER QUASI-TRIANGULAR MATRIX A, IN SCHUR CANONICAL FORM.
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY A. LDA >= MAX(1,M).
!
!  B       (INPUT) REAL*8 ARRAY, DIMENSION (LDB,N)
!          THE UPPER QUASI-TRIANGULAR MATRIX B, IN SCHUR CANONICAL FORM.
!
!  LDB     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY B. LDB >= MAX(1,N).
!
!  C       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDC,N)
!          ON ENTRY, THE M-BY-N RIGHT HAND SIDE MATRIX C.
!          ON EXIT, C IS OVERWRITTEN BY THE SOLUTION MATRIX X.
!
!  LDC     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY C. LDC >= MAX(1,M)
!
!  SCALE   (OUTPUT) REAL*8
!          THE SCALE FACTOR, SCALE, SET <= 1 TO AVOID OVERFLOW IN X.
!
!  INFO    (OUTPUT) INTEGER
!          = 0: SUCCESSFUL EXIT
!          < 0: IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
!          = 1: A AND B HAVE COMMON OR VERY CLOSE EIGENVALUES, PERTURBED
!               VALUES WERE USED TO SOLVE THE EQUATION (BUT THE MATRICES
!               A AND B ARE UNCHANGED).
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!         REMPLACEMENT DE DLAMCH ET DLABAD PAR R8PREM,R8MIEM ET ISBAEM,
!            REMPLACEMENT DE 2 RETURN PAR UN GOTO 1000,
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! INTRINSIC FUNCTION
!   MAX, MIN, DBLE, ABS
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1501
    implicit none
!
!     .. SCALAR ARGUMENTS ..
    include 'asterc/isbaem.h'
    include 'asterc/matfpe.h'
    include 'asterc/r8miem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/flaln2.h'
    include 'asterfort/flasy2.h'
    include 'asterfort/xerbla.h'
    include 'blas/ddot.h'
    include 'blas/dlange.h'
    include 'blas/dscal.h'
    include 'blas/lsame.h'
    character(len=1) :: trana, tranb
    integer :: info, isgn, lda, ldb, ldc, m, n
    real(kind=8) :: scale
!     ..
!     .. ARRAY ARGUMENTS ..
    real(kind=8) :: a( lda, * ), b( ldb, * ), c( ldc, * )
!     ..
!     .. PARAMETERS ..
    real(kind=8) :: zero, one
    parameter          ( zero = 0.0d+0, one = 1.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    logical :: notrna, notrnb
    integer :: ierr, j, k, k1, k2, knext, l, l1, l2, lnext
    real(kind=8) :: a11, bignum, da11, db, eps, scaloc, sgn, smin, smlnum, suml
    real(kind=8) :: sumr, xnorm
!     ..
!     .. LOCAL ARRAYS ..
    real(kind=8) :: dum( 1 ), vec( 2, 2 ), x( 2, 2 )
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
! DUE TO CRS513
    dum(1) = 0.d0
!
!     DECODE AND TEST INPUT PARAMETERS
!
    notrna = lsame( trana, 'N' )
    notrnb = lsame( tranb, 'N' )
!
    info = 0
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
        call xerbla('FLRSYL', -info)
        goto 1000
    endif
!
!     QUICK RETURN IF POSSIBLE
!
    if (m .eq. 0 .or. n .eq. 0) goto 1000
!
!     SET CONSTANTS TO CONTROL OVERFLOW
!
    eps = r8prem() * 0.5d0 * isbaem()
    smlnum = r8miem()
    bignum = one / smlnum
    smlnum = smlnum*dble( m*n ) / eps
    bignum = one / smlnum
!
    smin = max(smlnum, eps*dlange( 'M', m, m, a, lda, dum ), eps*dlange( 'M', n, n, b, ldb, dum )&
           )
!
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
!         A(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L)
!
!        WHERE
!                  M                         L-1
!        R(K,L) = SUM (A(K,I)*X(I,L)) + ISGN*SUM (X(K,J)*B(J,L)).
!                I=K+1                       J=1
!
!        START COLUMN LOOP (INDEX = L)
!        L1 (L2) : COLUMN INDEX OF THE FIRST (FIRST) ROW OF X(K,L).
!
        lnext = 1
        do 60 l = 1, n
            if (l .lt. lnext) goto 60
            if (l .eq. n) then
                l1 = l
                l2 = l
            else
                if (b( l+1, l ) .ne. zero) then
                    l1 = l
                    l2 = l + 1
                    lnext = l + 2
                else
                    l1 = l
                    l2 = l
                    lnext = l + 1
                endif
            endif
!
!           START ROW LOOP (INDEX = K)
!           K1 (K2): ROW INDEX OF THE FIRST (LAST) ROW OF X(K,L).
!
            knext = m
            do 50 k = m, 1, -1
                if (k .gt. knext) goto 50
                if (k .eq. 1) then
                    k1 = k
                    k2 = k
                else
                    if (a( k, k-1 ) .ne. zero) then
                        k1 = k - 1
                        k2 = k
                        knext = k - 2
                    else
                        k1 = k
                        k2 = k
                        knext = k - 1
                    endif
                endif
!
                if (l1 .eq. l2 .and. k1 .eq. k2) then
                    suml = ddot( m-k1, a( k1, min( k1+1, m ) ), lda, c( min( k1+1, m ), l1 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
                    scaloc = one
!
                    a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                    da11 = abs( a11 )
                    if (da11 .le. smin) then
                        a11 = smin
                        da11 = smin
                        info = 1
                    endif
                    db = abs( vec( 1, 1 ) )
                    if (da11 .lt. one .and. db .gt. one) then
                        if (db .gt. bignum*da11) scaloc = one / db
                    endif
                    x( 1, 1 ) = ( vec( 1, 1 )*scaloc ) / a11
!
                    if (scaloc .ne. one) then
                        do 10 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
10                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
!
                else if (l1.eq.l2 .and. k1.ne.k2) then
!
                    suml = ddot( m-k2, a( k1, min( k2+1, m ) ), lda, c( min( k2+1, m ), l1 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( m-k2, a( k2, min( k2+1, m ) ), lda, c( min( k2+1, m ), l1 ), 1 )
                    sumr = ddot( l1-1, c( k2, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 2, 1 ) = c( k2, l1 ) - ( suml+sgn*sumr )
!
                    call flaln2(.false., 2, 1, smin, one,&
                                a( k1, k1 ), lda, one, one, vec,&
                                2, -sgn*b( l1, l1 ), zero, x, 2,&
                                scaloc, xnorm, ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 20 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
20                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k2, l1 ) = x( 2, 1 )
!
                else if (l1.ne.l2 .and. k1.eq.k2) then
!
                    suml = ddot( m-k1, a( k1, min( k1+1, m ) ), lda, c( min( k1+1, m ), l1 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 1, 1 ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
!
                    suml = ddot( m-k1, a( k1, min( k1+1, m ) ), lda, c( min( k1+1, m ), l2 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l2 ), 1 )
                    vec( 2, 1 ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
!
                    call flaln2(.true., 2, 1, smin, one,&
                                b( l1, l1 ), ldb, one, one, vec,&
                                2, -sgn*a( k1, k1 ), zero, x, 2,&
                                scaloc, xnorm, ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 30 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
30                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k1, l2 ) = x( 2, 1 )
!
                else if (l1.ne.l2 .and. k1.ne.k2) then
!
                    suml = ddot( m-k2, a( k1, min( k2+1, m ) ), lda, c( min( k2+1, m ), l1 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( m-k2, a( k1, min( k2+1, m ) ), lda, c( min( k2+1, m ), l2 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l2 ), 1 )
                    vec( 1, 2 ) = c( k1, l2 ) - ( suml+sgn*sumr )
!
                    suml = ddot( m-k2, a( k2, min( k2+1, m ) ), lda, c( min( k2+1, m ), l1 ), 1 )
                    sumr = ddot( l1-1, c( k2, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 2, 1 ) = c( k2, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( m-k2, a( k2, min( k2+1, m ) ), lda, c( min( k2+1, m ), l2 ), 1 )
                    sumr = ddot( l1-1, c( k2, 1 ), ldc, b( 1, l2 ), 1 )
                    vec( 2, 2 ) = c( k2, l2 ) - ( suml+sgn*sumr )
!
                    call flasy2(.false., .false., isgn, 2, 2,&
                                a( k1, k1 ), lda, b( l1, l1 ), ldb, vec,&
                                2, scaloc, x, 2, xnorm,&
                                ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 40 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
40                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k1, l2 ) = x( 1, 2 )
                    c( k2, l1 ) = x( 2, 1 )
                    c( k2, l2 ) = x( 2, 2 )
                endif
!
50          continue
!
60      continue
!
    else if (.not.notrna .and. notrnb) then
!
!        SOLVE    A' *X + ISGN*X*B = SCALE*C.
!
!        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
!        UPPER-LEFT CORNER COLUMN BY COLUMN BY
!
!          A(K,K)'*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L)
!
!        WHERE
!                   K-1                        L-1
!          R(K,L) = SUM (A(I,K)'*X(I,L)) +ISGN*SUM (X(K,J)*B(J,L))
!                   I=1                        J=1
!
!        START COLUMN LOOP (INDEX = L)
!        L1 (L2): COLUMN INDEX OF THE FIRST (LAST) ROW OF X(K,L)
!
        lnext = 1
        do 120 l = 1, n
            if (l .lt. lnext) goto 120
            if (l .eq. n) then
                l1 = l
                l2 = l
            else
                if (b( l+1, l ) .ne. zero) then
                    l1 = l
                    l2 = l + 1
                    lnext = l + 2
                else
                    l1 = l
                    l2 = l
                    lnext = l + 1
                endif
            endif
!
!           START ROW LOOP (INDEX = K)
!           K1 (K2): ROW INDEX OF THE FIRST (LAST) ROW OF X(K,L)
!
            knext = 1
            do 110 k = 1, m
                if (k .lt. knext) goto 110
                if (k .eq. m) then
                    k1 = k
                    k2 = k
                else
                    if (a( k+1, k ) .ne. zero) then
                        k1 = k
                        k2 = k + 1
                        knext = k + 2
                    else
                        k1 = k
                        k2 = k
                        knext = k + 1
                    endif
                endif
!
                if (l1 .eq. l2 .and. k1 .eq. k2) then
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
                    scaloc = one
!
                    a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                    da11 = abs( a11 )
                    if (da11 .le. smin) then
                        a11 = smin
                        da11 = smin
                        info = 1
                    endif
                    db = abs( vec( 1, 1 ) )
                    if (da11 .lt. one .and. db .gt. one) then
                        if (db .gt. bignum*da11) scaloc = one / db
                    endif
                    x( 1, 1 ) = ( vec( 1, 1 )*scaloc ) / a11
!
                    if (scaloc .ne. one) then
                        do 70 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
70                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
!
                else if (l1.eq.l2 .and. k1.ne.k2) then
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( k1-1, a( 1, k2 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot( l1-1, c( k2, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 2, 1 ) = c( k2, l1 ) - ( suml+sgn*sumr )
!
                    call flaln2(.true., 2, 1, smin, one,&
                                a( k1, k1 ), lda, one, one, vec,&
                                2, -sgn*b( l1, l1 ), zero, x, 2,&
                                scaloc, xnorm, ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 80 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
80                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k2, l1 ) = x( 2, 1 )
!
                else if (l1.ne.l2 .and. k1.eq.k2) then
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 1, 1 ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l2 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l2 ), 1 )
                    vec( 2, 1 ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
!
                    call flaln2(.true., 2, 1, smin, one,&
                                b( l1, l1 ), ldb, one, one, vec,&
                                2, -sgn*a( k1, k1 ), zero, x, 2,&
                                scaloc, xnorm, ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 90 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
90                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k1, l2 ) = x( 2, 1 )
!
                else if (l1.ne.l2 .and. k1.ne.k2) then
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l2 ), 1 )
                    sumr = ddot( l1-1, c( k1, 1 ), ldc, b( 1, l2 ), 1 )
                    vec( 1, 2 ) = c( k1, l2 ) - ( suml+sgn*sumr )
!
                    suml = ddot( k1-1, a( 1, k2 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot( l1-1, c( k2, 1 ), ldc, b( 1, l1 ), 1 )
                    vec( 2, 1 ) = c( k2, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( k1-1, a( 1, k2 ), 1, c( 1, l2 ), 1 )
                    sumr = ddot( l1-1, c( k2, 1 ), ldc, b( 1, l2 ), 1 )
                    vec( 2, 2 ) = c( k2, l2 ) - ( suml+sgn*sumr )
!
                    call flasy2(.true., .false., isgn, 2, 2,&
                                a( k1, k1 ), lda, b( l1, l1 ), ldb, vec,&
                                2, scaloc, x, 2, xnorm,&
                                ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 100 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
100                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k1, l2 ) = x( 1, 2 )
                    c( k2, l1 ) = x( 2, 1 )
                    c( k2, l2 ) = x( 2, 2 )
                endif
!
110          continue
120      continue
!
    else if (.not.notrna .and. .not.notrnb) then
!
!        SOLVE    A'*X + ISGN*X*B' = SCALE*C.
!
!        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
!        TOP-RIGHT CORNER COLUMN BY COLUMN BY
!
!           A(K,K)'*X(K,L) + ISGN*X(K,L)*B(L,L)' = C(K,L) - R(K,L)
!
!        WHERE
!                     K-1                          N
!            R(K,L) = SUM (A(I,K)'*X(I,L)) + ISGN*SUM (X(K,J)*B(L,J)').
!                     I=1                        J=L+1
!
!        START COLUMN LOOP (INDEX = L)
!        L1 (L2): COLUMN INDEX OF THE FIRST (LAST) ROW OF X(K,L)
!
        lnext = n
        do 180 l = n, 1, -1
            if (l .gt. lnext) goto 180
            if (l .eq. 1) then
                l1 = l
                l2 = l
            else
                if (b( l, l-1 ) .ne. zero) then
                    l1 = l - 1
                    l2 = l
                    lnext = l - 2
                else
                    l1 = l
                    l2 = l
                    lnext = l - 1
                endif
            endif
!
!           START ROW LOOP (INDEX = K)
!           K1 (K2): ROW INDEX OF THE FIRST (LAST) ROW OF X(K,L)
!
            knext = 1
            do 170 k = 1, m
                if (k .lt. knext) goto 170
                if (k .eq. m) then
                    k1 = k
                    k2 = k
                else
                    if (a( k+1, k ) .ne. zero) then
                        k1 = k
                        k2 = k + 1
                        knext = k + 2
                    else
                        k1 = k
                        k2 = k
                        knext = k + 1
                    endif
                endif
!
                if (l1 .eq. l2 .and. k1 .eq. k2) then
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot(n-l1, c( k1, min( l1+1, n ) ), ldc, b( l1, min( l1+1, n ) ), ldb)
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
                    scaloc = one
!
                    a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                    da11 = abs( a11 )
                    if (da11 .le. smin) then
                        a11 = smin
                        da11 = smin
                        info = 1
                    endif
                    db = abs( vec( 1, 1 ) )
                    if (da11 .lt. one .and. db .gt. one) then
                        if (db .gt. bignum*da11) scaloc = one / db
                    endif
                    x( 1, 1 ) = ( vec( 1, 1 )*scaloc ) / a11
!
                    if (scaloc .ne. one) then
                        do 130 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
130                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
!
                else if (l1.eq.l2 .and. k1.ne.k2) then
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( k1-1, a( 1, k2 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot(n-l2, c( k2, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 2, 1 ) = c( k2, l1 ) - ( suml+sgn*sumr )
!
                    call flaln2(.true., 2, 1, smin, one,&
                                a( k1, k1 ), lda, one, one, vec,&
                                2, -sgn*b( l1, l1 ), zero, x, 2,&
                                scaloc, xnorm, ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 140 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
140                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k2, l1 ) = x( 2, 1 )
!
                else if (l1.ne.l2 .and. k1.eq.k2) then
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 1, 1 ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l2 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l2, min( l2+1, n ) ), ldb)
                    vec( 2, 1 ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
!
                    call flaln2(.false., 2, 1, smin, one,&
                                b( l1, l1 ), ldb, one, one, vec,&
                                2, -sgn*a( k1, k1 ), zero, x, 2,&
                                scaloc, xnorm, ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 150 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
150                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k1, l2 ) = x( 2, 1 )
!
                else if (l1.ne.l2 .and. k1.ne.k2) then
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( k1-1, a( 1, k1 ), 1, c( 1, l2 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l2, min( l2+1, n ) ), ldb)
                    vec( 1, 2 ) = c( k1, l2 ) - ( suml+sgn*sumr )
!
                    suml = ddot( k1-1, a( 1, k2 ), 1, c( 1, l1 ), 1 )
                    sumr = ddot(n-l2, c( k2, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 2, 1 ) = c( k2, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( k1-1, a( 1, k2 ), 1, c( 1, l2 ), 1 )
                    sumr = ddot(n-l2, c( k2, min( l2+1, n ) ), ldc, b( l2, min( l2+1, n ) ), ldb)
                    vec( 2, 2 ) = c( k2, l2 ) - ( suml+sgn*sumr )
!
                    call flasy2(.true., .true., isgn, 2, 2,&
                                a( k1, k1 ), lda, b( l1, l1 ), ldb, vec,&
                                2, scaloc, x, 2, xnorm,&
                                ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 160 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
160                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k1, l2 ) = x( 1, 2 )
                    c( k2, l1 ) = x( 2, 1 )
                    c( k2, l2 ) = x( 2, 2 )
                endif
!
170          continue
180      continue
!
    else if (notrna .and. .not.notrnb) then
!
!        SOLVE    A*X + ISGN*X*B' = SCALE*C.
!
!        THE (K,L)TH BLOCK OF X IS DETERMINED STARTING FROM
!        BOTTOM-RIGHT CORNER COLUMN BY COLUMN BY
!
!            A(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L)' = C(K,L) - R(K,L)
!
!        WHERE
!                      M                          N
!            R(K,L) = SUM (A(K,I)*X(I,L)) + ISGN*SUM (X(K,J)*B(L,J)').
!                    I=K+1                      J=L+1
!
!        START COLUMN LOOP (INDEX = L)
!        L1 (L2): COLUMN INDEX OF THE FIRST (LAST) ROW OF X(K,L)
!
        lnext = n
        do 240 l = n, 1, -1
            if (l .gt. lnext) goto 240
            if (l .eq. 1) then
                l1 = l
                l2 = l
            else
                if (b( l, l-1 ) .ne. zero) then
                    l1 = l - 1
                    l2 = l
                    lnext = l - 2
                else
                    l1 = l
                    l2 = l
                    lnext = l - 1
                endif
            endif
!
!           START ROW LOOP (INDEX = K)
!           K1 (K2): ROW INDEX OF THE FIRST (LAST) ROW OF X(K,L)
!
            knext = m
            do 230 k = m, 1, -1
                if (k .gt. knext) goto 230
                if (k .eq. 1) then
                    k1 = k
                    k2 = k
                else
                    if (a( k, k-1 ) .ne. zero) then
                        k1 = k - 1
                        k2 = k
                        knext = k - 2
                    else
                        k1 = k
                        k2 = k
                        knext = k - 1
                    endif
                endif
!
                if (l1 .eq. l2 .and. k1 .eq. k2) then
                    suml = ddot( m-k1, a( k1, min( k1+1, m ) ), lda, c( min( k1+1, m ), l1 ), 1 )
                    sumr = ddot(n-l1, c( k1, min( l1+1, n ) ), ldc, b( l1, min( l1+1, n ) ), ldb)
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
                    scaloc = one
!
                    a11 = a( k1, k1 ) + sgn*b( l1, l1 )
                    da11 = abs( a11 )
                    if (da11 .le. smin) then
                        a11 = smin
                        da11 = smin
                        info = 1
                    endif
                    db = abs( vec( 1, 1 ) )
                    if (da11 .lt. one .and. db .gt. one) then
                        if (db .gt. bignum*da11) scaloc = one / db
                    endif
                    x( 1, 1 ) = ( vec( 1, 1 )*scaloc ) / a11
!
                    if (scaloc .ne. one) then
                        do 190 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
190                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
!
                else if (l1.eq.l2 .and. k1.ne.k2) then
!
                    suml = ddot( m-k2, a( k1, min( k2+1, m ) ), lda, c( min( k2+1, m ), l1 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( m-k2, a( k2, min( k2+1, m ) ), lda, c( min( k2+1, m ), l1 ), 1 )
                    sumr = ddot(n-l2, c( k2, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 2, 1 ) = c( k2, l1 ) - ( suml+sgn*sumr )
!
                    call flaln2(.false., 2, 1, smin, one,&
                                a( k1, k1 ), lda, one, one, vec,&
                                2, -sgn*b( l1, l1 ), zero, x, 2,&
                                scaloc, xnorm, ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 200 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
200                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k2, l1 ) = x( 2, 1 )
!
                else if (l1.ne.l2 .and. k1.eq.k2) then
!
                    suml = ddot( m-k1, a( k1, min( k1+1, m ) ), lda, c( min( k1+1, m ), l1 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 1, 1 ) = sgn*( c( k1, l1 )-( suml+sgn*sumr ) )
!
                    suml = ddot( m-k1, a( k1, min( k1+1, m ) ), lda, c( min( k1+1, m ), l2 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l2, min( l2+1, n ) ), ldb)
                    vec( 2, 1 ) = sgn*( c( k1, l2 )-( suml+sgn*sumr ) )
!
                    call flaln2(.false., 2, 1, smin, one,&
                                b( l1, l1 ), ldb, one, one, vec,&
                                2, -sgn*a( k1, k1 ), zero, x, 2,&
                                scaloc, xnorm, ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 210 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
210                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k1, l2 ) = x( 2, 1 )
!
                else if (l1.ne.l2 .and. k1.ne.k2) then
!
                    suml = ddot( m-k2, a( k1, min( k2+1, m ) ), lda, c( min( k2+1, m ), l1 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 1, 1 ) = c( k1, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( m-k2, a( k1, min( k2+1, m ) ), lda, c( min( k2+1, m ), l2 ), 1 )
                    sumr = ddot(n-l2, c( k1, min( l2+1, n ) ), ldc, b( l2, min( l2+1, n ) ), ldb)
                    vec( 1, 2 ) = c( k1, l2 ) - ( suml+sgn*sumr )
!
                    suml = ddot( m-k2, a( k2, min( k2+1, m ) ), lda, c( min( k2+1, m ), l1 ), 1 )
                    sumr = ddot(n-l2, c( k2, min( l2+1, n ) ), ldc, b( l1, min( l2+1, n ) ), ldb)
                    vec( 2, 1 ) = c( k2, l1 ) - ( suml+sgn*sumr )
!
                    suml = ddot( m-k2, a( k2, min( k2+1, m ) ), lda, c( min( k2+1, m ), l2 ), 1 )
                    sumr = ddot(n-l2, c( k2, min( l2+1, n ) ), ldc, b( l2, min( l2+1, n ) ), ldb)
                    vec( 2, 2 ) = c( k2, l2 ) - ( suml+sgn*sumr )
!
                    call flasy2(.false., .true., isgn, 2, 2,&
                                a( k1, k1 ), lda, b( l1, l1 ), ldb, vec,&
                                2, scaloc, x, 2, xnorm,&
                                ierr)
                    if (ierr .ne. 0) info = 1
!
                    if (scaloc .ne. one) then
                        do 220 j = 1, n
                            call dscal(m, scaloc, c( 1, j ), 1)
220                      continue
                        scale = scale*scaloc
                    endif
                    c( k1, l1 ) = x( 1, 1 )
                    c( k1, l2 ) = x( 1, 2 )
                    c( k2, l1 ) = x( 2, 1 )
                    c( k2, l2 ) = x( 2, 2 )
                endif
!
230          continue
240      continue
!
    endif
!
1000  continue
    call matfpe(1)
!
!     END OF FLRSYL
!
end subroutine
