subroutine gtrsen(job, compq, select, n, t,&
                  ldt, q, ldq, w, m,&
                  s, sep, work, lwork, info)
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
!  GTRSEN REORDERS THE SCHUR FACTORIZATION OF A COMPLEX MATRIX
!  A = Q*T*Q**H, SO THAT A SELECTED CLUSTER OF EIGENVALUES APPEARS IN
!  THE LEADING POSITIONS ON THE DIAGONAL OF THE UPPER TRIANGULAR MATRIX
!  T, AND THE LEADING COLUMNS OF Q FORM AN ORTHONORMAL BASIS OF THE
!  CORRESPONDING RIGHT INVARIANT SUBSPACE.
!
!  OPTIONALLY THE ROUTINE COMPUTES THE RECIPROCAL CONDITION NUMBERS OF
!  THE CLUSTER OF EIGENVALUES AND/OR THE INVARIANT SUBSPACE.
!
!  ARGUMENTS
!  =========
!
!  JOB     (INPUT) CHARACTER*1
!          SPECIFIES WHETHER CONDITION NUMBERS ARE REQUIRED FOR THE
!          CLUSTER OF EIGENVALUES (S) OR THE INVARIANT SUBSPACE (SEP):
!          = 'N': NONE;
!          = 'E': FOR EIGENVALUES ONLY (S);
!          = 'V': FOR INVARIANT SUBSPACE ONLY (SEP);
!          = 'B': FOR BOTH EIGENVALUES AND INVARIANT SUBSPACE (S AND
!                 SEP).
!
!  COMPQ   (INPUT) CHARACTER*1
!          = 'V': UPDATE THE MATRIX Q OF SCHUR VECTORS;
!          = 'N': DO NOT UPDATE Q.
!
!  SELECT  (INPUT) LOGICAL ARRAY, DIMENSION (N)
!          SELECT SPECIFIES THE EIGENVALUES IN THE SELECTED CLUSTER. TO
!          SELECT THE J-TH EIGENVALUE, SELECT(J) MUST BE SET TO .TRUE..
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX T. N >= 0.
!
!  T       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDT,N)
!          ON ENTRY, THE UPPER TRIANGULAR MATRIX T.
!          ON EXIT, T IS OVERWRITTEN BY THE REORDERED MATRIX T, WITH THE
!          SELECTED EIGENVALUES AS THE LEADING DIAGONAL ELEMENTS.
!
!  LDT     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
!
!  Q       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDQ,N)
!          ON ENTRY, IF COMPQ = 'V', THE MATRIX Q OF SCHUR VECTORS.
!          ON EXIT, IF COMPQ = 'V', Q HAS BEEN POSTMULTIPLIED BY THE
!          UNITARY TRANSFORMATION MATRIX WHICH REORDERS T; THE LEADING M
!          COLUMNS OF Q FORM AN ORTHONORMAL BASIS FOR THE SPECIFIED
!          INVARIANT SUBSPACE.
!          IF COMPQ = 'N', Q IS NOT REFERENCED.
!
!  LDQ     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY Q.
!          LDQ >= 1; AND IF COMPQ = 'V', LDQ >= N.
!
!  W       (OUTPUT) COMPLEX*16
!          THE REORDERED EIGENVALUES OF T, IN THE SAME ORDER AS THEY
!          APPEAR ON THE DIAGONAL OF T.
!
!  M       (OUTPUT) INTEGER
!          THE DIMENSION OF THE SPECIFIED INVARIANT SUBSPACE.
!          0 <= M <= N.
!
!  S       (OUTPUT) DOUBLE PRECISION
!          IF JOB = 'E' OR 'B', S IS A LOWER BOUND ON THE RECIPROCAL
!          CONDITION NUMBER FOR THE SELECTED CLUSTER OF EIGENVALUES.
!          S CANNOT UNDERESTIMATE THE TRUE RECIPROCAL CONDITION NUMBER
!          BY MORE THAN A FACTOR OF SQRT(N). IF M = 0 OR N, S = 1.
!          IF JOB = 'N' OR 'V', S IS NOT REFERENCED.
!
!  SEP     (OUTPUT) DOUBLE PRECISION
!          IF JOB = 'V' OR 'B', SEP IS THE ESTIMATED RECIPROCAL
!          CONDITION NUMBER OF THE SPECIFIED INVARIANT SUBSPACE. IF
!          M = 0 OR N, SEP = NORM(T).
!          IF JOB = 'N' OR 'E', SEP IS NOT REFERENCED.
!
!  WORK    (WORKSPACE) COMPLEX*16 ARRAY, DIMENSION (LWORK)
!          IF JOB = 'N', WORK IS NOT REFERENCED.
!
!  LWORK   (INPUT) INTEGER
!          THE DIMENSION OF THE ARRAY WORK.
!          IF JOB = 'N', LWORK >= 1;
!          IF JOB = 'E', LWORK = M*(N-M);
!          IF JOB = 'V' OR 'B', LWORK >= 2*M*(N-M).
!
!  INFO    (OUTPUT) INTEGER
!          = 0:  SUCCESSFUL EXIT
!          < 0:  IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
!
!  FURTHER DETAILS
!  ===============
!
!  GTRSEN FIRST COLLECTS THE SELECTED EIGENVALUES BY COMPUTING A UNITARY
!  TRANSFORMATION Z TO MOVE THEM TO THE TOP LEFT CORNER OF T. IN OTHER
!  WORDS, THE SELECTED EIGENVALUES ARE THE EIGENVALUES OF T11 IN:
!
!                Z'*T*Z = ( T11 T12 ) N1
!                         (  0  T22 ) N2
!                            N1  N2
!
!  WHERE N = N1+N2 AND Z' MEANS THE CONJUGATE TRANSPOSE OF Z. THE FIRST
!  N1 COLUMNS OF Z SPAN THE SPECIFIED INVARIANT SUBSPACE OF T.
!
!  IF T HAS BEEN OBTAINED FROM THE SCHUR FACTORIZATION OF A MATRIX
!  A = Q*T*Q', THEN THE REORDERED SCHUR FACTORIZATION OF A IS GIVEN BY
!  A = (Q*Z)*(Z'*T*Z)*(Q*Z)', AND THE FIRST N1 COLUMNS OF Q*Z SPAN THE
!  CORRESPONDING INVARIANT SUBSPACE OF A.
!
!  THE RECIPROCAL CONDITION NUMBER OF THE AVERAGE OF THE EIGENVALUES OF
!  T11 MAY BE RETURNED IN S. S LIES BETWEEN 0 (VERY BADLY CONDITIONED)
!  AND 1 (VERY WELL CONDITIONED). IT IS COMPUTED AS FOLLOWS. FIRST WE
!  COMPUTE R SO THAT
!
!                         P = ( I  R ) N1
!                             ( 0  0 ) N2
!                               N1 N2
!
!  IS THE PROJECTOR ON THE INVARIANT SUBSPACE ASSOCIATED WITH T11.
!  R IS THE SOLUTION OF THE SYLVESTER EQUATION:
!
!                        T11*R - R*T22 = T12.
!
!  LET F-NORM(M) DENOTE THE FROBENIUS-NORM OF M AND 2-NORM(M) DENOTE
!  THE TWO-NORM OF M. THEN S IS COMPUTED AS THE LOWER BOUND
!
!                      (1 + F-NORM(R)**2)**(-1/2)
!
!  ON THE RECIPROCAL OF 2-NORM(P), THE TRUE RECIPROCAL CONDITION NUMBER.
!  S CANNOT UNDERESTIMATE 1 / 2-NORM(P) BY MORE THAN A FACTOR OF
!  SQRT(N).
!
!  AN APPROXIMATE ERROR BOUND FOR THE COMPUTED AVERAGE OF THE
!  EIGENVALUES OF T11 IS
!
!                         EPS * NORM(T) / S
!
!  WHERE EPS IS THE MACHINE PRECISION.
!
!  THE RECIPROCAL CONDITION NUMBER OF THE RIGHT INVARIANT SUBSPACE
!  SPANNED BY THE FIRST N1 COLUMNS OF Z (OR OF Q*Z) IS RETURNED IN SEP.
!  SEP IS DEFINED AS THE SEPARATION OF T11 AND T22:
!
!                     SEP( T11, T22 ) = SIGMA-MIN( C )
!
!  WHERE SIGMA-MIN(C) IS THE SMALLEST SINGULAR VALUE OF THE
!  N1*N2-BY-N1*N2 MATRIX
!
!     C  = KPROD( I(N2), T11 ) - KPROD( TRANSPOSE(T22), I(N1) )
!
!  I(M) IS AN M BY M IDENTITY MATRIX, AND KPROD DENOTES THE KRONECKER
!  PRODUCT. WE ESTIMATE SIGMA-MIN(C) BY THE RECIPROCAL OF AN ESTIMATE OF
!  THE 1-NORM OF INVERSE(C). THE TRUE RECIPROCAL 1-NORM OF INVERSE(C)
!  CANNOT DIFFER FROM SIGMA-MIN(C) BY MORE THAN A FACTOR OF SQRT(N1*N2).
!
!  WHEN SEP IS SMALL, SMALL CHANGES IN T CAN CAUSE LARGE CHANGES IN
!  THE INVARIANT SUBSPACE. AN APPROXIMATE BOUND ON THE MAXIMUM ANGULAR
!  ERROR IN THE COMPUTED RIGHT INVARIANT SUBSPACE IS
!
!                      EPS * NORM(T) / SEP
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
!     .. SCALAR ARGUMENTS ..
    include 'asterc/matfpe.h'
    include 'asterfort/gtrexc.h'
    include 'asterfort/gtrsyl.h'
    include 'asterfort/xerbla.h'
    include 'blas/lsame.h'
    include 'blas/zlacon.h'
    include 'blas/zlacpy.h'
    include 'blas/zlange.h'
    character(len=1) :: compq, job
    integer :: info, ldq, ldt, lwork, m, n
    real(kind=8) :: s, sep
!     ..
!     .. ARRAY ARGUMENTS ..
    logical :: select( * )
    complex(kind=8) :: q( ldq, * ), t( ldt, * ), w( * ), work( * )
!     ..
!
!     .. PARAMETERS ..
    real(kind=8) :: zero, one
    parameter          ( zero = 0.0d+0, one = 1.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    logical :: wantbh, wantq, wants, wantsp
    integer :: ierr, k, kase, ks, n1, n2, nn
    real(kind=8) :: est, rnorm, scale
!     ..
!     .. LOCAL ARRAYS ..
    real(kind=8) :: rwork(1)
!     ..
!
!
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
!     DECODE AND TEST THE INPUT PARAMETERS.
!
    wantbh = lsame( job, 'B' )
    wants = lsame( job, 'E' ) .or. wantbh
    wantsp = lsame( job, 'V' ) .or. wantbh
    wantq = lsame( compq, 'V' )
!
!     SET M TO THE NUMBER OF SELECTED EIGENVALUES.
!
    rwork(1)=0.d0
!
    m = 0
    do 10 k = 1, n
        if (select( k )) m = m + 1
10  end do
!
    n1 = m
    n2 = n - m
    nn = n1*n2
!
    info = 0
    if (.not.lsame( job, 'N' ) .and. .not.wants .and. .not.wantsp) then
        info = -1
    else if (.not.lsame( compq, 'N' ) .and. .not.wantq) then
        info = -2
    else if (n.lt.0) then
        info = -4
    else if (ldt.lt.max( 1, n )) then
        info = -6
    else if (ldq.lt.1 .or. ( wantq .and. ldq.lt.n )) then
        info = -8
        else if( lwork.lt.1 .or. ( ( wants .and. .not.wantsp ) .and.&
    lwork.lt.nn ) .or. ( wantsp .and. lwork.lt.2*nn ) ) then
        info = -14
    endif
    if (info .ne. 0) then
        call xerbla('GTRSEN', -info)
        goto 1000
    endif
!
!     QUICK RETURN IF POSSIBLE
!
    if (m .eq. n .or. m .eq. 0) then
        if (wants) s = one
        if (wantsp) sep = zlange( '1', n, n, t, ldt, rwork )
        goto 40
    endif
!
!     COLLECT THE SELECTED EIGENVALUES AT THE TOP LEFT CORNER OF T.
!
    ks = 0
    do 20 k = 1, n
        if (select( k )) then
            ks = ks + 1
!
!           SWAP THE K-TH EIGENVALUE TO POSITION KS.
!
            if (k .ne. ks) call gtrexc(compq, n, t, ldt, q,&
                                       ldq, k, ks, ierr)
        endif
20  end do
!
    if (wants) then
!
!        SOLVE THE SYLVESTER EQUATION FOR R:
!
!           T11*R - R*T22 = SCALE*T12
!
        call zlacpy('F', n1, n2, t( 1, n1+1 ), ldt,&
                    work, n1)
        call gtrsyl('N', 'N', -1, n1, n2,&
                    t, ldt, t( n1+1, n1+1 ), ldt, work,&
                    n1, scale, ierr)
!
!        ESTIMATE THE RECIPROCAL OF THE CONDITION NUMBER OF THE CLUSTER
!        OF EIGENVALUES.
!
        rnorm = zlange( 'F', n1, n2, work, n1, rwork )
        if (rnorm .eq. zero) then
            s = one
        else
            s = scale / ( sqrt( scale*scale / rnorm+rnorm )* sqrt( rnorm ) )
        endif
    endif
!
    if (wantsp) then
!
!        ESTIMATE SEP(T11,T22).
!
        est = zero
        kase = 0
30      continue
        call zlacon(nn, work( nn+1 ), work, est, kase)
        if (kase .ne. 0) then
            if (kase .eq. 1) then
!
!              SOLVE T11*R - R*T22 = SCALE*X.
!
                call gtrsyl('N', 'N', -1, n1, n2,&
                            t, ldt, t( n1+1, n1+1 ), ldt, work,&
                            n1, scale, ierr)
            else
!
!              SOLVE T11'*R - R*T22' = SCALE*X.
!
                call gtrsyl('C', 'C', -1, n1, n2,&
                            t, ldt, t( n1+1, n1+1 ), ldt, work,&
                            n1, scale, ierr)
            endif
            goto 30
        endif
!
        sep = scale / est
    endif
!
40  continue
!
!     COPY REORDERED EIGENVALUES TO W.
!
    do 50 k = 1, n
        w( k ) = t( k, k )
50  end do
1000  continue
    call matfpe(1)
!
!     END OF GTRSEN
!
end subroutine
