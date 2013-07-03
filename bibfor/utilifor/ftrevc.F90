subroutine ftrevc(side, howmny, select, n, t,&
                  ldt, vl, ldvl, vr, ldvr,&
                  mm, m, work, info)
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
!     SUBROUTINE LAPACK CALCULANT DES VECTEUR PROPRES.
!-----------------------------------------------------------------------
!  -- LAPACK ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     SEPTEMBER 30, 1994
!
!  PURPOSE
!  =======
!
!  FTREVC COMPUTES SOME OR ALL OF THE RIGHT AND/OR LEFT EIGENVECTORS OF
!  A REAL UPPER QUASI-TRIANGULAR MATRIX T.
!
!  THE RIGHT EIGENVECTOR X AND THE LEFT EIGENVECTOR Y OF T CORRESPONDING
!  TO AN EIGENVALUE W ARE DEFINED BY:
!
!               T*X = W*X,     Y'*T = W*Y'
!
!  WHERE Y' DENOTES THE CONJUGATE TRANSPOSE OF THE VECTOR Y.
!
!  IF ALL EIGENVECTORS ARE REQUESTED, THE ROUTINE MAY EITHER RETURN THE
!  MATRICES X AND/OR Y OF RIGHT OR LEFT EIGENVECTORS OF T, OR THE
!  PRODUCTS Q*X AND/OR Q*Y, WHERE Q IS AN INPUT ORTHOGONAL
!  MATRIX. IF T WAS OBTAINED FROM THE REAL-SCHUR FACTORIZATION OF AN
!  ORIGINAL MATRIX A = Q*T*Q', THEN Q*X AND Q*Y ARE THE MATRICES OF
!  RIGHT OR LEFT EIGENVECTORS OF A.
!
!  T MUST BE IN SCHUR CANONICAL FORM (AS RETURNED BY DHSEQR), THAT IS,
!  BLOCK UPPER TRIANGULAR WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS, EACH
!  2-BY-2 DIAGONAL BLOCK HAS ITS DIAGONAL ELEMENTS EQUAL AND ITS
!  OFF-DIAGONAL ELEMENTS OF OPPOSITE SIGN.  CORRESPONDING TO EACH 2-BY-2
!  DIAGONAL BLOCK IS A COMPLEX CONJUGATE PAIR OF EIGENVALUES AND
!  EIGENVECTORS, ONLY ONE EIGENVECTOR OF THE PAIR IS COMPUTED, NAMELY
!  THE ONE CORRESPONDING TO THE EIGENVALUE WITH POSITIVE IMAGINARY PART.
!
!
!  ARGUMENTS
!  =========
!
!  SIDE    (INPUT) CHARACTER*1
!          = 'R':  COMPUTE RIGHT EIGENVECTORS ONLY,
!          = 'L':  COMPUTE LEFT EIGENVECTORS ONLY,
!          = 'B':  COMPUTE BOTH RIGHT AND LEFT EIGENVECTORS.
!
!  HOWMNY  (INPUT) CHARACTER*1
!          = 'A':  COMPUTE ALL RIGHT AND/OR LEFT EIGENVECTORS,
!          = 'B':  COMPUTE ALL RIGHT AND/OR LEFT EIGENVECTORS,
!                  AND BACKTRANSFORM THEM USING THE INPUT MATRICES
!                  SUPPLIED IN VR AND/OR VL,
!          = 'S':  COMPUTE SELECTED RIGHT AND/OR LEFT EIGENVECTORS,
!                  SPECIFIED BY THE LOGICAL ARRAY SELECT.
!
!  SELECT  (INPUT/OUTPUT) LOGICAL ARRAY, DIMENSION (N)
!          IF HOWMNY = 'S', SELECT SPECIFIES THE EIGENVECTORS TO BE
!          COMPUTED.
!          IF HOWMNY = 'A' OR 'B', SELECT IS NOT REFERENCED.
!          TO SELECT THE REAL EIGENVECTOR CORRESPONDING TO A REAL
!          EIGENVALUE W(J), SELECT(J) MUST BE SET TO .TRUE..  TO SELECT
!          THE COMPLEX EIGENVECTOR CORRESPONDING TO A COMPLEX CONJUGATE
!          PAIR W(J) AND W(J+1), EITHER SELECT(J) OR SELECT(J+1) MUST BE
!          SET TO .TRUE., THEN ON EXIT SELECT(J) IS .TRUE. AND
!          SELECT(J+1) IS .FALSE..
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX T. N >= 0.
!
!  T       (INPUT) REAL*8 ARRAY, DIMENSION (LDT,N)
!          THE UPPER QUASI-TRIANGULAR MATRIX T IN SCHUR CANONICAL FORM.
!
!  LDT     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
!
!  VL      (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDVL,MM)
!          ON ENTRY, IF SIDE = 'L' OR 'B' AND HOWMNY = 'B', VL MUST
!          CONTAIN AN N-BY-N MATRIX Q (USUALLY THE ORTHOGONAL MATRIX Q
!          OF SCHUR VECTORS RETURNED BY DHSEQR).
!          ON EXIT, IF SIDE = 'L' OR 'B', VL CONTAINS:
!          IF HOWMNY = 'A', THE MATRIX Y OF LEFT EIGENVECTORS OF T,
!          IF HOWMNY = 'B', THE MATRIX Q*Y,
!          IF HOWMNY = 'S', THE LEFT EIGENVECTORS OF T SPECIFIED BY
!                           SELECT, STORED CONSECUTIVELY IN THE COLUMNS
!                           OF VL, IN THE SAME ORDER AS THEIR
!                           EIGENVALUES.
!          A COMPLEX EIGENVECTOR CORRESPONDING TO A COMPLEX EIGENVALUE
!          IS STORED IN TWO CONSECUTIVE COLUMNS, THE FIRST HOLDING THE
!          REAL PART, AND THE SECOND THE IMAGINARY PART.
!          IF SIDE = 'R', VL IS NOT REFERENCED.
!
!  LDVL    (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY VL.  LDVL >= MAX(1,N) IF
!          SIDE = 'L' OR 'B', LDVL >= 1 OTHERWISE.
!
!  VR      (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDVR,MM)
!          ON ENTRY, IF SIDE = 'R' OR 'B' AND HOWMNY = 'B', VR MUST
!          CONTAIN AN N-BY-N MATRIX Q (USUALLY THE ORTHOGONAL MATRIX Q
!          OF SCHUR VECTORS RETURNED BY DHSEQR).
!          ON EXIT, IF SIDE = 'R' OR 'B', VR CONTAINS:
!          IF HOWMNY = 'A', THE MATRIX X OF RIGHT EIGENVECTORS OF T,
!          IF HOWMNY = 'B', THE MATRIX Q*X,
!          IF HOWMNY = 'S', THE RIGHT EIGENVECTORS OF T SPECIFIED BY
!                           SELECT, STORED CONSECUTIVELY IN THE COLUMNS
!                           OF VR, IN THE SAME ORDER AS THEIR
!                           EIGENVALUES.
!          A COMPLEX EIGENVECTOR CORRESPONDING TO A COMPLEX EIGENVALUE
!          IS STORED IN TWO CONSECUTIVE COLUMNS, THE FIRST HOLDING THE
!          REAL PART AND THE SECOND THE IMAGINARY PART.
!          IF SIDE = 'L', VR IS NOT REFERENCED.
!
!  LDVR    (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY VR.  LDVR >= MAX(1,N) IF
!          SIDE = 'R' OR 'B', LDVR >= 1 OTHERWISE.
!
!  MM      (INPUT) INTEGER
!          THE NUMBER OF COLUMNS IN THE ARRAYS VL AND/OR VR. MM >= M.
!
!  M       (OUTPUT) INTEGER
!          THE NUMBER OF COLUMNS IN THE ARRAYS VL AND/OR VR ACTUALLY
!          USED TO STORE THE EIGENVECTORS.
!          IF HOWMNY = 'A' OR 'B', M IS SET TO N.
!          EACH SELECTED REAL EIGENVECTOR OCCUPIES ONE COLUMN AND EACH
!          SELECTED COMPLEX EIGENVECTOR OCCUPIES TWO COLUMNS.
!
!  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION (3*N)
!
!  INFO    (OUTPUT) INTEGER
!          = 0:  SUCCESSFUL EXIT
!          < 0:  IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
!
!  FURTHER DETAILS
!  ===============
!
!  THE ALGORITHM USED IN THIS PROGRAM IS BASICALLY BACKWARD (FORWARD)
!  SUBSTITUTION, WITH SCALING TO MAKE THE THE CODE ROBUST AGAINST
!  POSSIBLE OVERFLOW.
!
!  EACH EIGENVECTOR IS NORMALIZED SO THAT THE ELEMENT OF LARGEST
!  MAGNITUDE HAS MAGNITUDE 1, HERE THE MAGNITUDE OF A COMPLEX NUMBER
!  (X,Y) IS TAKEN TO BE |X| + |Y|.
!-----------------------------------------------------------------------
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE DLAMCH ET DLABAD,
!            UTILISATION DE R8PREM(), R8MIEM() ET ISBAEM(),
!            REMPLACEMENT DE 2 RETURN PAR GOTO 1000,
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! INTRINSIC FUNCTIONS
!            ABS, MAX, SQRT.
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1501
    implicit none
!
!     .. SCALAR ARGUMENTS ..
#include "asterc/isbaem.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/flaln2.h"
#include "asterfort/xerbla.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dgemv.h"
#include "blas/dscal.h"
#include "blas/idamax.h"
#include "blas/lsame.h"
    character(len=1) :: howmny, side
    integer :: info, ldt, ldvl, ldvr, m, mm, n
!     ..
!     .. ARRAY ARGUMENTS ..
    logical :: select( * )
    real(kind=8) :: t( ldt, * ), vl( ldvl, * ), vr( ldvr, * ), work( * )
!     .. PARAMETERS ..
    real(kind=8) :: zero, one
    parameter          ( zero = 0.0d+0, one = 1.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    logical :: allv, bothv, leftv, over, pair, rightv, somev
    integer :: i, ierr, ii, ip, is, j, j1, j2, jnxt, k, ki, n2
    real(kind=8) :: beta, bignum, emax, rec, remax, scale, smin, smlnum, ulp
    real(kind=8) :: unfl, vcrit, vmax, wi, wr, xnorm
! DUE TO CRS512       REAL*8 OVFL
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. LOCAL ARRAYS ..
    real(kind=8) :: x( 2, 2 )
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     DECODE AND TEST THE INPUT PARAMETERS
!
    bothv = lsame( side, 'B' )
    rightv = lsame( side, 'R' ) .or. bothv
    leftv = lsame( side, 'L' ) .or. bothv
!
    allv = lsame( howmny, 'A' )
    over = lsame( howmny, 'B' ) .or. lsame( howmny, 'O' )
    somev = lsame( howmny, 'S' )
!
    info = 0
    if (.not.rightv .and. .not.leftv) then
        info = -1
    else if (.not.allv .and. .not.over .and. .not.somev) then
        info = -2
    else if (n.lt.0) then
        info = -4
    else if (ldt.lt.max( 1, n )) then
        info = -6
    else if (ldvl.lt.1 .or. ( leftv .and. ldvl.lt.n )) then
        info = -8
    else if (ldvr.lt.1 .or. ( rightv .and. ldvr.lt.n )) then
        info = -10
    else
!
!        SET M TO THE NUMBER OF COLUMNS REQUIRED TO STORE THE SELECTED
!        EIGENVECTORS, STANDARDIZE THE ARRAY SELECT IF NECESSARY, AND
!        TEST MM.
!
        if (somev) then
            m = 0
            pair = .false.
            do 10 j = 1, n
                if (pair) then
                    pair = .false.
                    select( j ) = .false.
                else
                    if (j .lt. n) then
                        if (t( j+1, j ) .eq. zero) then
                            if (select( j )) m = m + 1
                        else
                            pair = .true.
                            if (select( j ) .or. select( j+1 )) then
                                select( j ) = .true.
                                m = m + 2
                            endif
                        endif
                    else
                        if (select( n )) m = m + 1
                    endif
                endif
10          continue
        else
            m = n
        endif
!
        if (mm .lt. m) then
            info = -11
        endif
    endif
    if (info .ne. 0) then
        call xerbla('FTREVC', -info)
        goto 1000
    endif
!
!     QUICK RETURN IF POSSIBLE.
!
    if (n .eq. 0) goto 1000
!
!     SET THE CONSTANTS TO CONTROL OVERFLOW.
!
    unfl = r8miem()
! DUE TO CRS512      OVFL = ONE / UNFL
    ulp = r8prem() * 0.5d0 * isbaem()
    smlnum = unfl*( n / ulp )
    bignum = ( one-ulp ) / smlnum
!
!     COMPUTE 1-NORM OF EACH COLUMN OF STRICTLY UPPER TRIANGULAR
!     PART OF T TO CONTROL OVERFLOW IN TRIANGULAR SOLVER.
!
    work( 1 ) = zero
    do 30 j = 2, n
        work( j ) = zero
        do 20 i = 1, j - 1
            work( j ) = work( j ) + abs( t( i, j ) )
20      continue
30  end do
!
!     INDEX IP IS USED TO SPECIFY THE REAL OR COMPLEX EIGENVALUE:
!       IP = 0, REAL EIGENVALUE,
!            1, FIRST OF CONJUGATE COMPLEX PAIR: (WR,WI)
!           -1, SECOND OF CONJUGATE COMPLEX PAIR: (WR,WI)
!
    n2 = 2*n
!
    if (rightv) then
!
!        COMPUTE RIGHT EIGENVECTORS.
!
        ip = 0
        is = m
        do 140 ki = n, 1, -1
!
            if (ip .eq. 1) goto 130
            if (ki .eq. 1) goto 40
            if (t( ki, ki-1 ) .eq. zero) goto 40
            ip = -1
!
40          continue
            if (somev) then
                if (ip .eq. 0) then
                    if (.not.select( ki )) goto 130
                else
                    if (.not.select( ki-1 )) goto 130
                endif
            endif
!
!           COMPUTE THE KI-TH EIGENVALUE (WR,WI).
!
            wr = t( ki, ki )
            wi = zero
            if (ip .ne. 0) wi = sqrt( abs(t( ki, ki-1 ) ) )* sqrt( abs( t( ki-1, ki ) ) )
            smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
!
            if (ip .eq. 0) then
!
!              REAL RIGHT EIGENVECTOR
!
                work( ki+n ) = one
!
!              FORM RIGHT-HAND SIDE
!
                do 50 k = 1, ki - 1
                    work( k+n ) = -t( k, ki )
50              continue
!
!              SOLVE THE UPPER QUASI-TRIANGULAR SYSTEM:
!                 (T(1:KI-1,1:KI-1) - WR)*X = SCALE*WORK.
!
                jnxt = ki - 1
                do 60 j = ki - 1, 1, -1
                    if (j .gt. jnxt) goto 60
                    j1 = j
                    j2 = j
                    jnxt = j - 1
                    if (j .gt. 1) then
                        if (t( j, j-1 ) .ne. zero) then
                            j1 = j - 1
                            jnxt = j - 2
                        endif
                    endif
!
                    if (j1 .eq. j2) then
!
!                    1-BY-1 DIAGONAL BLOCK
!
                        call flaln2(.false., 1, 1, smin, one,&
                                    t( j, j ), ldt, one, one, work( j+n ),&
                                    n, wr, zero, x, 2,&
                                    scale, xnorm, ierr)
!
!                    SCALE X(1,1) TO AVOID OVERFLOW WHEN UPDATING
!                    THE RIGHT-HAND SIDE.
!
                        if (xnorm .gt. one) then
                            if (work( j ) .gt. bignum / xnorm) then
                                x( 1, 1 ) = x( 1, 1 ) / xnorm
                                scale = scale / xnorm
                            endif
                        endif
!
!                    SCALE IF NECESSARY
!
                        if (scale .ne. one) call dscal(ki, scale, work( 1+n ), 1)
                        work( j+n ) = x( 1, 1 )
!
!                    UPDATE RIGHT-HAND SIDE
!
                        call daxpy(j-1, -x( 1, 1 ), t( 1, j ), 1, work( 1+n ),&
                                   1)
!
                    else
!
!                    2-BY-2 DIAGONAL BLOCK
!
                        call flaln2(.false., 2, 1, smin, one,&
                                    t( j-1, j-1 ), ldt, one, one, work( j-1+n ),&
                                    n, wr, zero, x, 2,&
                                    scale, xnorm, ierr)
!
!                    SCALE X(1,1) AND X(2,1) TO AVOID OVERFLOW WHEN
!                    UPDATING THE RIGHT-HAND SIDE.
!
                        if (xnorm .gt. one) then
                            beta = max( work( j-1 ), work( j ) )
                            if (beta .gt. bignum / xnorm) then
                                x( 1, 1 ) = x( 1, 1 ) / xnorm
                                x( 2, 1 ) = x( 2, 1 ) / xnorm
                                scale = scale / xnorm
                            endif
                        endif
!
!                    SCALE IF NECESSARY
!
                        if (scale .ne. one) call dscal(ki, scale, work( 1+n ), 1)
                        work( j-1+n ) = x( 1, 1 )
                        work( j+n ) = x( 2, 1 )
!
!                    UPDATE RIGHT-HAND SIDE
!
                        call daxpy(j-2, -x( 1, 1 ), t( 1, j-1 ), 1, work( 1+n ),&
                                   1)
                        call daxpy(j-2, -x( 2, 1 ), t( 1, j ), 1, work( 1+n ),&
                                   1)
                    endif
60              continue
!
!              COPY THE VECTOR X OR Q*X TO VR AND NORMALIZE.
!
                if (.not.over) then
                    call dcopy(ki, work( 1+n ), 1, vr( 1, is ), 1)
!
                    ii = idamax( ki, vr( 1, is ), 1 )
                    remax = one / abs( vr( ii, is ) )
                    call dscal(ki, remax, vr( 1, is ), 1)
!
                    do 70 k = ki + 1, n
                        vr( k, is ) = zero
70                  continue
                else
                    if (ki .gt. 1) call dgemv('N', n, ki-1, one, vr,&
                                              ldvr, work( 1+n ), 1, work( ki+n ), vr( 1, ki ),&
                                              1)
!
                    ii = idamax( n, vr( 1, ki ), 1 )
                    remax = one / abs( vr( ii, ki ) )
                    call dscal(n, remax, vr( 1, ki ), 1)
                endif
!
            else
!
!              COMPLEX RIGHT EIGENVECTOR.
!
!              INITIAL SOLVE
!                ( (T(KI-1,KI-1) T(KI-1,KI) ) - (WR + I* WI))*X = 0.
!                ( (T(KI,KI-1)   T(KI,KI)   )               )
!
                if (abs( t( ki-1, ki ) ) .ge. abs( t( ki, ki-1 ) )) then
                    work( ki-1+n ) = one
                    work( ki+n2 ) = wi / t( ki-1, ki )
                else
                    work( ki-1+n ) = -wi / t( ki, ki-1 )
                    work( ki+n2 ) = one
                endif
                work( ki+n ) = zero
                work( ki-1+n2 ) = zero
!
!              FORM RIGHT-HAND SIDE
!
                do 80 k = 1, ki - 2
                    work( k+n ) = -work( ki-1+n )*t( k, ki-1 )
                    work( k+n2 ) = -work( ki+n2 )*t( k, ki )
80              continue
!
!              SOLVE UPPER QUASI-TRIANGULAR SYSTEM:
!              (T(1:KI-2,1:KI-2) - (WR+I*WI))*X = SCALE*(WORK+I*WORK2)
!
                jnxt = ki - 2
                do 90 j = ki - 2, 1, -1
                    if (j .gt. jnxt) goto 90
                    j1 = j
                    j2 = j
                    jnxt = j - 1
                    if (j .gt. 1) then
                        if (t( j, j-1 ) .ne. zero) then
                            j1 = j - 1
                            jnxt = j - 2
                        endif
                    endif
!
                    if (j1 .eq. j2) then
!
!                    1-BY-1 DIAGONAL BLOCK
!
                        call flaln2(.false., 1, 2, smin, one,&
                                    t( j, j ), ldt, one, one, work( j+n ),&
                                    n, wr, wi, x, 2,&
                                    scale, xnorm, ierr)
!
!                    SCALE X(1,1) AND X(1,2) TO AVOID OVERFLOW WHEN
!                    UPDATING THE RIGHT-HAND SIDE.
!
                        if (xnorm .gt. one) then
                            if (work( j ) .gt. bignum / xnorm) then
                                x( 1, 1 ) = x( 1, 1 ) / xnorm
                                x( 1, 2 ) = x( 1, 2 ) / xnorm
                                scale = scale / xnorm
                            endif
                        endif
!
!                    SCALE IF NECESSARY
!
                        if (scale .ne. one) then
                            call dscal(ki, scale, work( 1+n ), 1)
                            call dscal(ki, scale, work( 1+n2 ), 1)
                        endif
                        work( j+n ) = x( 1, 1 )
                        work( j+n2 ) = x( 1, 2 )
!
!                    UPDATE THE RIGHT-HAND SIDE
!
                        call daxpy(j-1, -x( 1, 1 ), t( 1, j ), 1, work( 1+n ),&
                                   1)
                        call daxpy(j-1, -x( 1, 2 ), t( 1, j ), 1, work( 1+n2 ),&
                                   1)
!
                    else
!
!                    2-BY-2 DIAGONAL BLOCK
!
                        call flaln2(.false., 2, 2, smin, one,&
                                    t( j-1, j-1 ), ldt, one, one, work( j-1+n ),&
                                    n, wr, wi, x, 2,&
                                    scale, xnorm, ierr)
!
!                    SCALE X TO AVOID OVERFLOW WHEN UPDATING
!                    THE RIGHT-HAND SIDE.
!
                        if (xnorm .gt. one) then
                            beta = max( work( j-1 ), work( j ) )
                            if (beta .gt. bignum / xnorm) then
                                rec = one / xnorm
                                x( 1, 1 ) = x( 1, 1 )*rec
                                x( 1, 2 ) = x( 1, 2 )*rec
                                x( 2, 1 ) = x( 2, 1 )*rec
                                x( 2, 2 ) = x( 2, 2 )*rec
                                scale = scale*rec
                            endif
                        endif
!
!                    SCALE IF NECESSARY
!
                        if (scale .ne. one) then
                            call dscal(ki, scale, work( 1+n ), 1)
                            call dscal(ki, scale, work( 1+n2 ), 1)
                        endif
                        work( j-1+n ) = x( 1, 1 )
                        work( j+n ) = x( 2, 1 )
                        work( j-1+n2 ) = x( 1, 2 )
                        work( j+n2 ) = x( 2, 2 )
!
!                    UPDATE THE RIGHT-HAND SIDE
!
                        call daxpy(j-2, -x( 1, 1 ), t( 1, j-1 ), 1, work( 1+n ),&
                                   1)
                        call daxpy(j-2, -x( 2, 1 ), t( 1, j ), 1, work( 1+n ),&
                                   1)
                        call daxpy(j-2, -x( 1, 2 ), t( 1, j-1 ), 1, work( 1+n2 ),&
                                   1)
                        call daxpy(j-2, -x( 2, 2 ), t( 1, j ), 1, work( 1+n2 ),&
                                   1)
                    endif
90              continue
!
!              COPY THE VECTOR X OR Q*X TO VR AND NORMALIZE.
!
                if (.not.over) then
                    call dcopy(ki, work( 1+n ), 1, vr( 1, is-1 ), 1)
                    call dcopy(ki, work( 1+n2 ), 1, vr( 1, is ), 1)
!
                    emax = zero
                    do 100 k = 1, ki
                        emax = max( emax, abs( vr( k, is-1 ) )+ abs( vr( k, is ) ))
100                  continue
!
                    remax = one / emax
                    call dscal(ki, remax, vr( 1, is-1 ), 1)
                    call dscal(ki, remax, vr( 1, is ), 1)
!
                    do 110 k = ki + 1, n
                        vr( k, is-1 ) = zero
                        vr( k, is ) = zero
110                  continue
!
                else
!
                    if (ki .gt. 2) then
                        call dgemv('N', n, ki-2, one, vr,&
                                   ldvr, work( 1+n ), 1, work( ki-1+n ), vr( 1, ki-1 ),&
                                   1)
                        call dgemv('N', n, ki-2, one, vr,&
                                   ldvr, work( 1+n2 ), 1, work( ki+n2 ), vr( 1, ki ),&
                                   1)
                    else
                        call dscal(n, work( ki-1+n ), vr( 1, ki-1 ), 1)
                        call dscal(n, work( ki+n2 ), vr( 1, ki ), 1)
                    endif
!
                    emax = zero
                    do 120 k = 1, n
                        emax = max( emax, abs( vr( k, ki-1 ) )+ abs( vr( k, ki ) ))
120                  continue
                    remax = one / emax
                    call dscal(n, remax, vr( 1, ki-1 ), 1)
                    call dscal(n, remax, vr( 1, ki ), 1)
                endif
            endif
!
            is = is - 1
            if (ip .ne. 0) is = is - 1
130          continue
            if (ip .eq. 1) ip = 0
            if (ip .eq. -1) ip = 1
140      continue
    endif
!
    if (leftv) then
!
!        COMPUTE LEFT EIGENVECTORS.
!
        ip = 0
        is = 1
        do 260 ki = 1, n
!
            if (ip .eq. -1) goto 250
            if (ki .eq. n) goto 150
            if (t( ki+1, ki ) .eq. zero) goto 150
            ip = 1
!
150          continue
            if (somev) then
                if (.not.select( ki )) goto 250
            endif
!
!           COMPUTE THE KI-TH EIGENVALUE (WR,WI).
!
            wr = t( ki, ki )
            wi = zero
            if (ip .ne. 0) wi = sqrt( abs(t( ki, ki+1 ) ) )* sqrt( abs( t( ki+1, ki ) ) )
            smin = max( ulp*( abs( wr )+abs( wi ) ), smlnum )
!
            if (ip .eq. 0) then
!
!              REAL LEFT EIGENVECTOR.
!
                work( ki+n ) = one
!
!              FORM RIGHT-HAND SIDE
!
                do 160 k = ki + 1, n
                    work( k+n ) = -t( ki, k )
160              continue
!
!              SOLVE THE QUASI-TRIANGULAR SYSTEM:
!                 (T(KI+1:N,KI+1:N) - WR)'*X = SCALE*WORK
!
                vmax = one
                vcrit = bignum
!
                jnxt = ki + 1
                do 170 j = ki + 1, n
                    if (j .lt. jnxt) goto 170
                    j1 = j
                    j2 = j
                    jnxt = j + 1
                    if (j .lt. n) then
                        if (t( j+1, j ) .ne. zero) then
                            j2 = j + 1
                            jnxt = j + 2
                        endif
                    endif
!
                    if (j1 .eq. j2) then
!
!                    1-BY-1 DIAGONAL BLOCK
!
!                    SCALE IF NECESSARY TO AVOID OVERFLOW WHEN FORMING
!                    THE RIGHT-HAND SIDE.
!
                        if (work( j ) .gt. vcrit) then
                            rec = one / vmax
                            call dscal(n-ki+1, rec, work( ki+n ), 1)
                            vmax = one
                            vcrit = bignum
                        endif
!
                        work( j+n ) = work( j+n ) - ddot( j-ki-1, t( ki+1, j ), 1, work( ki+1+n )&
                                      &, 1 )
!
!                    SOLVE (T(J,J)-WR)'*X = WORK
!
                        call flaln2(.false., 1, 1, smin, one,&
                                    t( j, j ), ldt, one, one, work( j+n ),&
                                    n, wr, zero, x, 2,&
                                    scale, xnorm, ierr)
!
!                    SCALE IF NECESSARY
!
                        if (scale .ne. one) call dscal(n-ki+1, scale, work( ki+n ), 1)
                        work( j+n ) = x( 1, 1 )
                        vmax = max( abs( work( j+n ) ), vmax )
                        vcrit = bignum / vmax
!
                    else
!
!                    2-BY-2 DIAGONAL BLOCK
!
!                    SCALE IF NECESSARY TO AVOID OVERFLOW WHEN FORMING
!                    THE RIGHT-HAND SIDE.
!
                        beta = max( work( j ), work( j+1 ) )
                        if (beta .gt. vcrit) then
                            rec = one / vmax
                            call dscal(n-ki+1, rec, work( ki+n ), 1)
                            vmax = one
                            vcrit = bignum
                        endif
!
                        work( j+n ) = work( j+n ) - ddot( j-ki-1, t( ki+1, j ), 1, work( ki+1+n )&
                                      &, 1 )
!
                        work( j+1+n ) = work( j+1+n ) - ddot( j-ki-1, t( ki+1, j+1 ), 1, work( ki&
                                        &+1+n ), 1 )
!
!                    SOLVE
!                      (T(J,J)-WR   T(J,J+1)     )'* X = SCALE*( WORK1 )
!                      (T(J+1,J)    T(J+1,J+1)-WR)             ( WORK2 )
!
                        call flaln2(.true., 2, 1, smin, one,&
                                    t( j, j ), ldt, one, one, work( j+n ),&
                                    n, wr, zero, x, 2,&
                                    scale, xnorm, ierr)
!
!                    SCALE IF NECESSARY
!
                        if (scale .ne. one) call dscal(n-ki+1, scale, work( ki+n ), 1)
                        work( j+n ) = x( 1, 1 )
                        work( j+1+n ) = x( 2, 1 )
!
                        vmax = max( abs( work( j+n ) ), abs( work( j+ 1+n ) ), vmax)
                        vcrit = bignum / vmax
!
                    endif
170              continue
!
!              COPY THE VECTOR X OR Q*X TO VL AND NORMALIZE.
!
                if (.not.over) then
                    call dcopy(n-ki+1, work( ki+n ), 1, vl( ki, is ), 1)
!
                    ii = idamax( n-ki+1, vl( ki, is ), 1 ) + ki - 1
                    remax = one / abs( vl( ii, is ) )
                    call dscal(n-ki+1, remax, vl( ki, is ), 1)
!
                    do 180 k = 1, ki - 1
                        vl( k, is ) = zero
180                  continue
!
                else
!
                    if (ki .lt. n) call dgemv('N', n, n-ki, one, vl( 1, ki+1 ),&
                                              ldvl, work( ki+1+n ), 1, work( ki+n ), vl( 1, ki ),&
                                              1)
!
                    ii = idamax( n, vl( 1, ki ), 1 )
                    remax = one / abs( vl( ii, ki ) )
                    call dscal(n, remax, vl( 1, ki ), 1)
!
                endif
!
            else
!
!              COMPLEX LEFT EIGENVECTOR.
!
!               INITIAL SOLVE:
!                 ((T(KI,KI)    T(KI,KI+1) )' - (WR - I* WI))*X = 0.
!                 ((T(KI+1,KI) T(KI+1,KI+1))                )
!
                if (abs( t( ki, ki+1 ) ) .ge. abs( t( ki+1, ki ) )) then
                    work( ki+n ) = wi / t( ki, ki+1 )
                    work( ki+1+n2 ) = one
                else
                    work( ki+n ) = one
                    work( ki+1+n2 ) = -wi / t( ki+1, ki )
                endif
                work( ki+1+n ) = zero
                work( ki+n2 ) = zero
!
!              FORM RIGHT-HAND SIDE
!
                do 190 k = ki + 2, n
                    work( k+n ) = -work( ki+n )*t( ki, k )
                    work( k+n2 ) = -work( ki+1+n2 )*t( ki+1, k )
190              continue
!
!              SOLVE COMPLEX QUASI-TRIANGULAR SYSTEM:
!              ( T(KI+2,N:KI+2,N) - (WR-I*WI) )*X = WORK1+I*WORK2
!
                vmax = one
                vcrit = bignum
!
                jnxt = ki + 2
                do 200 j = ki + 2, n
                    if (j .lt. jnxt) goto 200
                    j1 = j
                    j2 = j
                    jnxt = j + 1
                    if (j .lt. n) then
                        if (t( j+1, j ) .ne. zero) then
                            j2 = j + 1
                            jnxt = j + 2
                        endif
                    endif
!
                    if (j1 .eq. j2) then
!
!                    1-BY-1 DIAGONAL BLOCK
!
!                    SCALE IF NECESSARY TO AVOID OVERFLOW WHEN
!                    FORMING THE RIGHT-HAND SIDE ELEMENTS.
!
                        if (work( j ) .gt. vcrit) then
                            rec = one / vmax
                            call dscal(n-ki+1, rec, work( ki+n ), 1)
                            call dscal(n-ki+1, rec, work( ki+n2 ), 1)
                            vmax = one
                            vcrit = bignum
                        endif
!
                        work( j+n ) = work( j+n ) - ddot( j-ki-2, t( ki+2, j ), 1, work( ki+2+n )&
                                      &, 1 )
                        work( j+n2 ) = work( j+n2 ) - ddot( j-ki-2, t( ki+2, j ), 1, work( ki+2+n&
                                       &2 ), 1 )
!
!                    SOLVE (T(J,J)-(WR-I*WI))*(X11+I*X12)= WK+I*WK2
!
                        call flaln2(.false., 1, 2, smin, one,&
                                    t( j, j ), ldt, one, one, work( j+n ),&
                                    n, wr, -wi, x, 2,&
                                    scale, xnorm, ierr)
!
!                    SCALE IF NECESSARY
!
                        if (scale .ne. one) then
                            call dscal(n-ki+1, scale, work( ki+n ), 1)
                            call dscal(n-ki+1, scale, work( ki+n2 ), 1)
                        endif
                        work( j+n ) = x( 1, 1 )
                        work( j+n2 ) = x( 1, 2 )
                        vmax = max( abs( work( j+n ) ), abs( work( j+ n2 ) ), vmax)
                        vcrit = bignum / vmax
!
                    else
!
!                    2-BY-2 DIAGONAL BLOCK
!
!                    SCALE IF NECESSARY TO AVOID OVERFLOW WHEN FORMING
!                    THE RIGHT-HAND SIDE ELEMENTS.
!
                        beta = max( work( j ), work( j+1 ) )
                        if (beta .gt. vcrit) then
                            rec = one / vmax
                            call dscal(n-ki+1, rec, work( ki+n ), 1)
                            call dscal(n-ki+1, rec, work( ki+n2 ), 1)
                            vmax = one
                            vcrit = bignum
                        endif
!
                        work( j+n ) = work( j+n ) - ddot( j-ki-2, t( ki+2, j ), 1, work( ki+2+n )&
                                      &, 1 )
!
                        work( j+n2 ) = work( j+n2 ) - ddot( j-ki-2, t( ki+2, j ), 1, work( ki+2+n&
                                       &2 ), 1 )
!
                        work( j+1+n ) = work( j+1+n ) - ddot( j-ki-2, t( ki+2, j+1 ), 1, work( ki&
                                        &+2+n ), 1 )
!
                        work( j+1+n2 ) = work( j+1+n2 ) - ddot( j-ki- 2, t( ki+2, j+1 ), 1, work(&
                                         & ki+2+n2 ), 1 )
!
!                    SOLVE 2-BY-2 COMPLEX LINEAR EQUATION
!                      ((T(J,J)   T(J,J+1)  )'-(WR-I*WI)*I)*X = SCALE*B
!                      ((T(J+1,J) T(J+1,J+1))             )
!
                        call flaln2(.true., 2, 2, smin, one,&
                                    t( j, j ), ldt, one, one, work( j+n ),&
                                    n, wr, -wi, x, 2,&
                                    scale, xnorm, ierr)
!
!                    SCALE IF NECESSARY
!
                        if (scale .ne. one) then
                            call dscal(n-ki+1, scale, work( ki+n ), 1)
                            call dscal(n-ki+1, scale, work( ki+n2 ), 1)
                        endif
                        work( j+n ) = x( 1, 1 )
                        work( j+n2 ) = x( 1, 2 )
                        work( j+1+n ) = x( 2, 1 )
                        work( j+1+n2 ) = x( 2, 2 )
                        vmax = max(&
                               abs( x( 1, 1 ) ), abs( x( 1, 2 ) ), abs( x( 2, 1 ) ),&
                               abs( x( 2, 2 ) ), vmax&
                               )
                        vcrit = bignum / vmax
!
                    endif
200              continue
!
!              COPY THE VECTOR X OR Q*X TO VL AND NORMALIZE.
!
                if (.not.over) then
                    call dcopy(n-ki+1, work( ki+n ), 1, vl( ki, is ), 1)
                    call dcopy(n-ki+1, work( ki+n2 ), 1, vl( ki, is+ 1 ), 1)
!
                    emax = zero
                    do 220 k = ki, n
                        emax = max( emax, abs( vl( k, is ) )+ abs( vl( k, is+1 ) ))
220                  continue
                    remax = one / emax
                    call dscal(n-ki+1, remax, vl( ki, is ), 1)
                    call dscal(n-ki+1, remax, vl( ki, is+1 ), 1)
!
                    do 230 k = 1, ki - 1
                        vl( k, is ) = zero
                        vl( k, is+1 ) = zero
230                  continue
                else
                    if (ki .lt. n-1) then
                        call dgemv('N', n, n-ki-1, one, vl( 1, ki+2 ),&
                                   ldvl, work( ki+2+n ), 1, work( ki+n ), vl( 1, ki ),&
                                   1)
                        call dgemv('N', n, n-ki-1, one, vl( 1, ki+2 ),&
                                   ldvl, work( ki+2+n2 ), 1, work( ki+1+n2 ), vl( 1, ki+1 ),&
                                   1)
                    else
                        call dscal(n, work( ki+n ), vl( 1, ki ), 1)
                        call dscal(n, work( ki+1+n2 ), vl( 1, ki+1 ), 1)
                    endif
!
                    emax = zero
                    do 240 k = 1, n
                        emax = max( emax, abs( vl( k, ki ) )+ abs( vl( k, ki+1 ) ))
240                  continue
                    remax = one / emax
                    call dscal(n, remax, vl( 1, ki ), 1)
                    call dscal(n, remax, vl( 1, ki+1 ), 1)
!
                endif
!
            endif
!
            is = is + 1
            if (ip .ne. 0) is = is + 1
250          continue
            if (ip .eq. -1) ip = 0
            if (ip .eq. 1) ip = -1
!
260      continue
!
    endif
!
1000  continue
!
!     END OF FTREVC
!
end subroutine
