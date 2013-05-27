subroutine gtrevc(side, howmny, select, n, t,&
                  ldt, vl, ldvl, vr, ldvr,&
                  mm, m, work, rwork, info)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) LAPACK
! ======================================================================
!  -- LAPACK ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     SEPTEMBER 30, 1994
!
!  PURPOSE
!  =======
!
!  GTREVC COMPUTES SOME OR ALL OF THE RIGHT AND/OR LEFT EIGENVECTORS OF
!  A COMPLEX UPPER TRIANGULAR MATRIX T.
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
!  PRODUCTS Q*X AND/OR Q*Y, WHERE Q IS AN INPUT UNITARY
!  MATRIX. IF T WAS OBTAINED FROM THE SCHUR FACTORIZATION OF AN
!  ORIGINAL MATRIX A = Q*T*Q', THEN Q*X AND Q*Y ARE THE MATRICES OF
!  RIGHT OR LEFT EIGENVECTORS OF A.
!
!  ARGUMENTS
!  =========
!
!  SIDE    (INPUT) CHARACTER*1
!          = 'R':  COMPUTE RIGHT EIGENVECTORS ONLY;
!          = 'L':  COMPUTE LEFT EIGENVECTORS ONLY;
!          = 'B':  COMPUTE BOTH RIGHT AND LEFT EIGENVECTORS.
!
!  HOWMNY  (INPUT) CHARACTER*1
!          = 'A':  COMPUTE ALL RIGHT AND/OR LEFT EIGENVECTORS;
!          = 'B':  COMPUTE ALL RIGHT AND/OR LEFT EIGENVECTORS,
!                  AND BACKTRANSFORM THEM USING THE INPUT MATRICES
!                  SUPPLIED IN VR AND/OR VL;
!          = 'S':  COMPUTE SELECTED RIGHT AND/OR LEFT EIGENVECTORS,
!                  SPECIFIED BY THE LOGICAL ARRAY SELECT.
!
!  SELECT  (INPUT) LOGICAL ARRAY, DIMENSION (N)
!          IF HOWMNY = 'S', SELECT SPECIFIES THE EIGENVECTORS TO BE
!          COMPUTED.
!          IF HOWMNY = 'A' OR 'B', SELECT IS NOT REFERENCED.
!          TO SELECT THE EIGENVECTOR CORRESPONDING TO THE J-TH
!          EIGENVALUE, SELECT(J) MUST BE SET TO .TRUE..
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX T. N >= 0.
!
!  T       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDT,N)
!          THE UPPER TRIANGULAR MATRIX T.  T IS MODIFIED, BUT RESTORED
!          ON EXIT.
!
!  LDT     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
!
!  VL      (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDVL,MM)
!          ON ENTRY, IF SIDE = 'L' OR 'B' AND HOWMNY = 'B', VL MUST
!          CONTAIN AN N-BY-N MATRIX Q (USUALLY THE UNITARY MATRIX Q OF
!          SCHUR VECTORS RETURNED BY ZHSEQR).
!          ON EXIT, IF SIDE = 'L' OR 'B', VL CONTAINS:
!          IF HOWMNY = 'A', THE MATRIX Y OF LEFT EIGENVECTORS OF T;
!          IF HOWMNY = 'B', THE MATRIX Q*Y;
!          IF HOWMNY = 'S', THE LEFT EIGENVECTORS OF T SPECIFIED BY
!                           SELECT, STORED CONSECUTIVELY IN THE COLUMNS
!                           OF VL, IN THE SAME ORDER AS THEIR
!                           EIGENVALUES.
!          IF SIDE = 'R', VL IS NOT REFERENCED.
!
!  LDVL    (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY VL.  LDVL >= MAX(1,N) IF
!          SIDE = 'L' OR 'B'; LDVL >= 1 OTHERWISE.
!
!  VR      (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDVR,MM)
!          ON ENTRY, IF SIDE = 'R' OR 'B' AND HOWMNY = 'B', VR MUST
!          CONTAIN AN N-BY-N MATRIX Q (USUALLY THE UNITARY MATRIX Q OF
!          SCHUR VECTORS RETURNED BY ZHSEQR).
!          ON EXIT, IF SIDE = 'R' OR 'B', VR CONTAINS:
!          IF HOWMNY = 'A', THE MATRIX X OF RIGHT EIGENVECTORS OF T;
!          IF HOWMNY = 'B', THE MATRIX Q*X;
!          IF HOWMNY = 'S', THE RIGHT EIGENVECTORS OF T SPECIFIED BY
!                           SELECT, STORED CONSECUTIVELY IN THE COLUMNS
!                           OF VR, IN THE SAME ORDER AS THEIR
!                           EIGENVALUES.
!          IF SIDE = 'L', VR IS NOT REFERENCED.
!
!  LDVR    (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY VR.  LDVR >= MAX(1,N) IF
!           SIDE = 'R' OR 'B'; LDVR >= 1 OTHERWISE.
!
!  MM      (INPUT) INTEGER
!          THE NUMBER OF COLUMNS IN THE ARRAYS VL AND/OR VR. MM >= M.
!
!  M       (OUTPUT) INTEGER
!          THE NUMBER OF COLUMNS IN THE ARRAYS VL AND/OR VR ACTUALLY
!          USED TO STORE THE EIGENVECTORS.  IF HOWMNY = 'A' OR 'B', M
!          IS SET TO N.  EACH SELECTED EIGENVECTOR OCCUPIES ONE
!          COLUMN.
!
!  WORK    (WORKSPACE) COMPLEX*16 ARRAY, DIMENSION (2*N)
!
!  RWORK   (WORKSPACE) DOUBLE PRECISION ARRAY, DIMENSION (N)
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
!  MAGNITUDE HAS MAGNITUDE 1; HERE THE MAGNITUDE OF A COMPLEX NUMBER
!  (X,Y) IS TAKEN TO BE |X| + |Y|.
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
!
!     .. SCALAR ARGUMENTS ..
    include 'asterc/isbaem.h'
    include 'asterc/matfpe.h'
    include 'asterc/r8miem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/xerbla.h'
    include 'blas/dzasum.h'
    include 'blas/izamax.h'
    include 'blas/lsame.h'
    include 'blas/zcopy.h'
    include 'blas/zdscal.h'
    include 'blas/zgemv.h'
    include 'blas/zlatrs.h'
    character(len=1) :: howmny, side
    integer :: info, ldt, ldvl, ldvr, m, mm, n
!     ..
!     .. ARRAY ARGUMENTS ..
    logical :: select( * )
    real(kind=8) :: rwork( * )
    complex(kind=8) :: t( ldt, * ), vl( ldvl, * ), vr( ldvr, * ), work( * )
!     ..
!     .. PARAMETERS ..
    real(kind=8) :: zero, one
    parameter          ( zero = 0.0d+0, one = 1.0d+0 )
    complex(kind=8) :: cmzero, cmone
    parameter          ( cmzero = ( 0.0d+0, 0.0d+0 ),&
     &                   cmone = ( 1.0d+0, 0.0d+0 ) )
!     ..
!     .. LOCAL SCALARS ..
    logical :: allv, bothv, leftv, over, rightv, somev
    integer :: i, ii, is, j, k, ki
    real(kind=8) :: remax, scale, smin, smlnum, ulp, unfl
    complex(kind=8) :: cdum
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. STATEMENT FUNCTIONS ..
    real(kind=8) :: cabs1
!     ..
!     .. STATEMENT FUNCTION DEFINITIONS ..
    cabs1( cdum ) = abs( dble( cdum ) ) + abs( dimag( cdum ) )
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
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
!     SET M TO THE NUMBER OF COLUMNS REQUIRED TO STORE THE SELECTED
!     EIGENVECTORS.
!
    if (somev) then
        m = 0
        do 10 j = 1, n
            if (select( j )) m = m + 1
10      continue
    else
        m = n
    endif
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
    else if (mm.lt.m) then
        info = -11
    endif
    if (info .ne. 0) then
        call xerbla('GTREVC', -info)
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
!
!     STORE THE DIAGONAL ELEMENTS OF T IN WORKING ARRAY WORK.
!
    do 20 i = 1, n
        work( i+n ) = t( i, i )
20  end do
!
!     COMPUTE 1-NORM OF EACH COLUMN OF STRICTLY UPPER TRIANGULAR
!     PART OF T TO CONTROL OVERFLOW IN TRIANGULAR SOLVER.
!
    rwork( 1 ) = zero
    do 30 j = 2, n
        rwork( j ) = dzasum( j-1, t( 1, j ), 1 )
30  end do
!
    if (rightv) then
!
!        COMPUTE RIGHT EIGENVECTORS.
!
        is = m
        do 80 ki = n, 1, -1
!
            if (somev) then
                if (.not.select( ki )) goto 80
            endif
            smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
!
            work( 1 ) = cmone
!
!           FORM RIGHT-HAND SIDE.
!
            do 40 k = 1, ki - 1
                work( k ) = -t( k, ki )
40          continue
!
!           SOLVE THE TRIANGULAR SYSTEM:
!              (T(1:KI-1,1:KI-1) - T(KI,KI))*X = SCALE*WORK.
!
            do 50 k = 1, ki - 1
                t( k, k ) = t( k, k ) - t( ki, ki )
                if (cabs1( t( k, k ) ) .lt. smin) t( k, k ) = smin
50          continue
!
            if (ki .gt. 1) then
                call zlatrs('U', 'N', 'N', 'Y', ki-1,&
                            t, ldt, work( 1 ), scale, rwork,&
                            info)
                work( ki ) = scale
            endif
!
!           COPY THE VECTOR X OR Q*X TO VR AND NORMALIZE.
!
            if (.not.over) then
                call zcopy(ki, work( 1 ), 1, vr( 1, is ), 1)
!
                ii = izamax( ki, vr( 1, is ), 1 )
                remax = one / cabs1( vr( ii, is ) )
                call zdscal(ki, remax, vr( 1, is ), 1)
!
                do 60 k = ki + 1, n
                    vr( k, is ) = cmzero
60              continue
            else
                if (ki .gt. 1) call zgemv('N', n, ki-1, cmone, vr,&
                                          ldvr, work( 1 ), 1, dcmplx( scale ), vr( 1, ki ),&
                                          1)
!
                ii = izamax( n, vr( 1, ki ), 1 )
                remax = one / cabs1( vr( ii, ki ) )
                call zdscal(n, remax, vr( 1, ki ), 1)
            endif
!
!           SET BACK THE ORIGINAL DIAGONAL ELEMENTS OF T.
!
            do 70 k = 1, ki - 1
                t( k, k ) = work( k+n )
70          continue
!
            is = is - 1
80      continue
    endif
!
    if (leftv) then
!
!        COMPUTE LEFT EIGENVECTORS.
!
        is = 1
        do 130 ki = 1, n
!
            if (somev) then
                if (.not.select( ki )) goto 130
            endif
            smin = max( ulp*( cabs1( t( ki, ki ) ) ), smlnum )
!
            work( n ) = cmone
!
!           FORM RIGHT-HAND SIDE.
!
            do 90 k = ki + 1, n
                work( k ) = -dconjg( t( ki, k ) )
90          continue
!
!           SOLVE THE TRIANGULAR SYSTEM:
!              (T(KI+1:N,KI+1:N) - T(KI,KI))'*X = SCALE*WORK.
!
            do 100 k = ki + 1, n
                t( k, k ) = t( k, k ) - t( ki, ki )
                if (cabs1( t( k, k ) ) .lt. smin) t( k, k ) = smin
100          continue
!
            if (ki .lt. n) then
                call zlatrs('U', 'C', 'N', 'Y', n-ki,&
                            t( ki+1, ki+1 ), ldt, work( ki+1 ), scale, rwork,&
                            info)
                work( ki ) = scale
            endif
!
!           COPY THE VECTOR X OR Q*X TO VL AND NORMALIZE.
!
            if (.not.over) then
                call zcopy(n-ki+1, work( ki ), 1, vl( ki, is ), 1)
!
                ii = izamax( n-ki+1, vl( ki, is ), 1 ) + ki - 1
                remax = one / cabs1( vl( ii, is ) )
                call zdscal(n-ki+1, remax, vl( ki, is ), 1)
!
                do 110 k = 1, ki - 1
                    vl( k, is ) = cmzero
110              continue
            else
                if (ki .lt. n) call zgemv('N', n, n-ki, cmone, vl( 1, ki+1 ),&
                                          ldvl, work( ki+1 ), 1, dcmplx( scale ), vl( 1, ki ),&
                                          1)
!
                ii = izamax( n, vl( 1, ki ), 1 )
                remax = one / cabs1( vl( ii, ki ) )
                call zdscal(n, remax, vl( 1, ki ), 1)
            endif
!
!           SET BACK THE ORIGINAL DIAGONAL ELEMENTS OF T.
!
            do 120 k = ki + 1, n
                t( k, k ) = work( k+n )
120          continue
!
            is = is + 1
130      continue
    endif
!
1000  continue
    call matfpe(1)
!
!     END OF GTREVC
!
end subroutine
