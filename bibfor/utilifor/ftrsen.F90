!----------------------------------------------------------------------
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
!     SUBROUTINE LAPACK REORDONNANT LA FACTORISATION DE SCHUR REELLE
!     DEJA CALCULEES PAR DHSEQR, POUR OBTENIR UN CLUSTER DE VALEURS
!     PROPRES SUR LA DIAGONALE DE LA MATRICE TRIANGULAIRE SUPERIEURE.
!-----------------------------------------------------------------------
!  -- LAPACK ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     SEPTEMBER 30, 1994
!
!  PURPOSE
!  =======
!
!  FTRSEN REORDERS THE REAL SCHUR FACTORIZATION OF A REAL MATRIX
!  A = Q*T*Q**T, SO THAT A SELECTED CLUSTER OF EIGENVALUES APPEARS IN
!  THE LEADING DIAGONAL BLOCKS OF THE UPPER QUASI-TRIANGULAR MATRIX T,
!  AND THE LEADING COLUMNS OF Q FORM AN ORTHONORMAL BASIS OF THE
!  CORRESPONDING RIGHT INVARIANT SUBSPACE.
!
!  OPTIONALLY THE ROUTINE COMPUTES THE RECIPROCAL CONDITION NUMBERS OF
!  THE CLUSTER OF EIGENVALUES AND/OR THE INVARIANT SUBSPACE.
!
!  T MUST BE IN SCHUR CANONICAL FORM (AS RETURNED BY DHSEQR), THAT IS,
!  BLOCK UPPER TRIANGULAR WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS, EACH
!  2-BY-2 DIAGONAL BLOCK HAS ITS DIAGONAL ELEMNTS EQUAL AND ITS
!  OFF-DIAGONAL ELEMENTS OF OPPOSITE SIGN.
!
!  ARGUMENTS
!  =========
!
!  JOB     (INPUT) CHARACTER*1
!          SPECIFIES WHETHER CONDITION NUMBERS ARE REQUIRED FOR THE
!          CLUSTER OF EIGENVALUES (S) OR THE INVARIANT SUBSPACE (SEP):
!          = 'N': NONE,
!          = 'E': FOR EIGENVALUES ONLY (S),
!          = 'V': FOR INVARIANT SUBSPACE ONLY (SEP),
!          = 'B': FOR BOTH EIGENVALUES AND INVARIANT SUBSPACE (S AND
!                 SEP).
!
!  COMPQ   (INPUT) CHARACTER*1
!          = 'V': UPDATE THE MATRIX Q OF SCHUR VECTORS,
!          = 'N': DO NOT UPDATE Q.
!
!  SELECT  (INPUT) LOGICAL ARRAY, DIMENSION (N)
!          SELECT SPECIFIES THE EIGENVALUES IN THE SELECTED CLUSTER. TO
!          SELECT A REAL EIGENVALUE W(J), SELECT(J) MUST BE SET TO
!          .TRUE.. TO SELECT A COMPLEX CONJUGATE PAIR OF EIGENVALUES
!          W(J) AND W(J+1), CORRESPONDING TO A 2-BY-2 DIAGONAL BLOCK,
!          EITHER SELECT(J) OR SELECT(J+1) OR BOTH MUST BE SET TO
!          .TRUE., A COMPLEX CONJUGATE PAIR OF EIGENVALUES MUST BE
!          EITHER BOTH INCLUDED IN THE CLUSTER OR BOTH EXCLUDED.
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX T. N >= 0.
!
!  T       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDT,N)
!          ON ENTRY, THE UPPER QUASI-TRIANGULAR MATRIX T, IN SCHUR
!          CANONICAL FORM.
!          ON EXIT, T IS OVERWRITTEN BY THE REORDERED MATRIX T, AGAIN IN
!          SCHUR CANONICAL FORM, WITH THE SELECTED EIGENVALUES IN THE
!          LEADING DIAGONAL BLOCKS.
!
!  LDT     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
!
!  Q       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDQ,N)
!          ON ENTRY, IF COMPQ = 'V', THE MATRIX Q OF SCHUR VECTORS.
!          ON EXIT, IF COMPQ = 'V', Q HAS BEEN POSTMULTIPLIED BY THE
!          ORTHOGONAL TRANSFORMATION MATRIX WHICH REORDERS T, THE
!          LEADING M COLUMNS OF Q FORM AN ORTHONORMAL BASIS FOR THE
!          SPECIFIED INVARIANT SUBSPACE.
!          IF COMPQ = 'N', Q IS NOT REFERENCED.
!
!  LDQ     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY Q.
!          LDQ >= 1, AND IF COMPQ = 'V', LDQ >= N.
!
!  WR      (OUTPUT) REAL*8 ARRAY, DIMENSION (N)
!  WI      (OUTPUT) REAL*8 ARRAY, DIMENSION (N)
!          THE REAL AND IMAGINARY PARTS, RESPECTIVELY, OF THE REORDERED
!          EIGENVALUES OF T. THE EIGENVALUES ARE STORED IN THE SAME
!          ORDER AS ON THE DIAGONAL OF T, WITH WR(I) = T(I,I) AND, IF
!          T(I:I+1,I:I+1) IS A 2-BY-2 DIAGONAL BLOCK, WI(I) > 0 AND
!          WI(I+1) = -WI(I). NOTE THAT IF A COMPLEX EIGENVALUE IS
!          SUFFICIENTLY ILL-CONDITIONED, THEN ITS VALUE MAY DIFFER
!          SIGNIFICANTLY FROM ITS VALUE BEFORE REORDERING.
!
!  M       (OUTPUT) INTEGER
!          THE DIMENSION OF THE SPECIFIED INVARIANT SUBSPACE.
!          0 < = M <= N.
!
!  S       (OUTPUT) REAL*8
!          IF JOB = 'E' OR 'B', S IS A LOWER BOUND ON THE RECIPROCAL
!          CONDITION NUMBER FOR THE SELECTED CLUSTER OF EIGENVALUES.
!          S CANNOT UNDERESTIMATE THE TRUE RECIPROCAL CONDITION NUMBER
!          BY MORE THAN A FACTOR OF SQRT(N). IF M = 0 OR N, S = 1.
!          IF JOB = 'N' OR 'V', S IS NOT REFERENCED.
!
!  SEP     (OUTPUT) REAL*8
!          IF JOB = 'V' OR 'B', SEP IS THE ESTIMATED RECIPROCAL
!          CONDITION NUMBER OF THE SPECIFIED INVARIANT SUBSPACE. IF
!          M = 0 OR N, SEP = NORM(T).
!          IF JOB = 'N' OR 'E', SEP IS NOT REFERENCED.
!
!  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION (LWORK)
!
!  LWORK   (INPUT) INTEGER
!          THE DIMENSION OF THE ARRAY WORK.
!          IF JOB = 'N', LWORK >= MAX(1,N),
!          IF JOB = 'E', LWORK >= M*(N-M),
!          IF JOB = 'V' OR 'B', LWORK >= 2*M*(N-M).
!
!  IWORK   (WORKSPACE) INTEGER ARRAY, DIMENSION (LIWORK)
!          IF JOB = 'N' OR 'E', IWORK IS NOT REFERENCED.
!
!  LIWORK  (INPUT) INTEGER
!          THE DIMENSION OF THE ARRAY IWORK.
!          IF JOB = 'N' OR 'E', LIWORK >= 1,
!          IF JOB = 'V' OR 'B', LIWORK >= M*(N-M).
!
!  INFO    (OUTPUT) INTEGER
!          = 0: SUCCESSFUL EXIT
!          < 0: IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
!          = 1: REORDERING OF T FAILED BECAUSE SOME EIGENVALUES ARE TOO
!               CLOSE TO SEPARATE (THE PROBLEM IS VERY ILL-CONDITIONED),
!               T MAY HAVE BEEN PARTIALLY REORDERED, AND WR AND WI
!               CONTAIN THE EIGENVALUES IN THE SAME ORDER AS IN T, S AND
!               SEP (IF REQUESTED) ARE SET TO ZERO.
!
!  FURTHER DETAILS
!  ===============
!
!  FTRSEN FIRST COLLECTS THE SELECTED EIGENVALUES BY COMPUTING AN
!  ORTHOGONAL TRANSFORMATION Z TO MOVE THEM TO THE TOP LEFT CORNER OF T.
!  IN OTHER WORDS, THE SELECTED EIGENVALUES ARE THE EIGENVALUES OF T11
!  IN:
!
!                Z'*T*Z = ( T11 T12 ) N1
!                         (  0  T22 ) N2
!                            N1  N2
!
!  WHERE N = N1+N2 AND Z' MEANS THE TRANSPOSE OF Z. THE FIRST N1 COLUMNS
!  OF Z SPAN THE SPECIFIED INVARIANT SUBSPACE OF T.
!
!  IF T HAS BEEN OBTAINED FROM THE REAL SCHUR FACTORIZATION OF A MATRIX
!  A = Q*T*Q', THEN THE REORDERED REAL SCHUR FACTORIZATION OF A IS GIVEN
!  BY A = (Q*Z)*(Z'*T*Z)*(Q*Z)', AND THE FIRST N1 COLUMNS OF Q*Z SPAN
!  THE CORRESPONDING INVARIANT SUBSPACE OF A.
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
subroutine ftrsen(job, compq, select, n, t,&
                  ldt, q, ldq, wr, wi,&
                  m, s, sep, work, lwork,&
                  iwork, liwork, info)
!  THE INVARIANT SUBSPACE. AN APPROXIMATE BOUND ON THE MAXIMUM ANGULAR
!  ERROR IN THE COMPUTED RIGHT INVARIANT SUBSPACE IS
!
!                      EPS * NORM(T) / SEP
!
!-----------------------------------------------------------------------
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE 1 RETURN PAR GOTO 1000,
!            IMPLICIT NONE.
! INTRINSIC FUNCTIONS
!            ABS, MAX, SQRT.
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
    include 'asterc/matfpe.h'
    include 'asterfort/flacon.h'
    include 'asterfort/flrsyl.h'
    include 'asterfort/ftrexc.h'
    include 'asterfort/xerbla.h'
    include 'blas/dlacpy.h'
    include 'blas/dlange.h'
    include 'blas/lsame.h'
    character(len=1) :: compq, job
    integer :: info, ldq, ldt, liwork, lwork, m, n
    real(kind=8) :: s, sep
!     ..
!     .. ARRAY ARGUMENTS ..
    logical :: select( * )
    integer :: iwork( * )
    real(kind=8) :: q( ldq, * ), t( ldt, * ), wi( * ), work( * ), wr( * )
!     .. PARAMETERS ..
    real(kind=8) :: zero, one
    parameter          ( zero = 0.0d+0, one = 1.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    logical :: pair, swap, wantbh, wantq, wants, wantsp
    integer :: ierr, k, kase, kk, ks, n1, n2, nn
    real(kind=8) :: est, rnorm, scale
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
!     DECODE AND TEST THE INPUT PARAMETERS
!
    wantbh = lsame( job, 'B' )
    wants = lsame( job, 'E' ) .or. wantbh
    wantsp = lsame( job, 'V' ) .or. wantbh
    wantq = lsame( compq, 'V' )
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
    else
!
!        SET M TO THE DIMENSION OF THE SPECIFIED INVARIANT SUBSPACE,
!        AND TEST LWORK AND LIWORK.
!
        m = 0
        pair = .false.
        do 10 k = 1, n
            if (pair) then
                pair = .false.
            else
                if (k .lt. n) then
                    if (t( k+1, k ) .eq. zero) then
                        if (select( k )) m = m + 1
                    else
                        pair = .true.
                        if (select( k ) .or. select( k+1 )) m = m + 2
                    endif
                else
                    if (select( n )) m = m + 1
                endif
            endif
10      continue
!
        n1 = m
        n2 = n - m
        nn = n1*n2
!
        if (lwork .lt. 1 .or. ( ( wants .and. .not.wantsp ) .and. lwork.lt.nn ) .or.&
            ( wantsp .and. lwork.lt.2*nn )) then
            info = -15
            else if( liwork.lt.1 .or. ( wantsp .and. liwork.lt.nn ) )&
        then
            info = -17
        endif
    endif
    if (info .ne. 0) then
        call xerbla('FTRSEN', -info)
        goto 1000
    endif
!
!     QUICK RETURN IF POSSIBLE.
!
    if (m .eq. n .or. m .eq. 0) then
        if (wants) s = one
        if (wantsp) sep = dlange( '1', n, n, t, ldt, work )
        goto 40
    endif
!
!     COLLECT THE SELECTED BLOCKS AT THE TOP-LEFT CORNER OF T.
!
    ks = 0
    pair = .false.
    do 20 k = 1, n
        if (pair) then
            pair = .false.
        else
            swap = select( k )
            if (k .lt. n) then
                if (t( k+1, k ) .ne. zero) then
                    pair = .true.
                    swap = swap .or. select( k+1 )
                endif
            endif
            if (swap) then
                ks = ks + 1
!
!              SWAP THE K-TH BLOCK TO POSITION KS.
!
                ierr = 0
                kk = k
                if (k .ne. ks) call ftrexc(compq, n, t, ldt, q,&
                                           ldq, kk, ks, work, ierr)
                if (ierr .eq. 1 .or. ierr .eq. 2) then
!
!                 BLOCKS TOO CLOSE TO SWAP: EXIT.
!
                    info = 1
                    if (wants) s = zero
                    if (wantsp) sep = zero
                    goto 40
                endif
                if (pair) ks = ks + 1
            endif
        endif
20  end do
!
    if (wants) then
!
!        SOLVE SYLVESTER EQUATION FOR R:
!
!           T11*R - R*T22 = SCALE*T12
!
        call dlacpy('F', n1, n2, t( 1, n1+1 ), ldt,&
                    work, n1)
        call flrsyl('N', 'N', -1, n1, n2,&
                    t, ldt, t( n1+1, n1+1 ), ldt, work,&
                    n1, scale, ierr)
!
!        ESTIMATE THE RECIPROCAL OF THE CONDITION NUMBER OF THE CLUSTER
!        OF EIGENVALUES.
!
        rnorm = dlange( 'F', n1, n2, work, n1, work )
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
        call flacon(nn, work( nn+1 ), work, iwork, est,&
                    kase)
        if (kase .ne. 0) then
            if (kase .eq. 1) then
!
!              SOLVE  T11*R - R*T22 = SCALE*X.
!
                call flrsyl('N', 'N', -1, n1, n2,&
                            t, ldt, t( n1+1, n1+1 ), ldt, work,&
                            n1, scale, ierr)
            else
!
!              SOLVE  T11'*R - R*T22' = SCALE*X.
!
                call flrsyl('T', 'T', -1, n1, n2,&
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
!     STORE THE OUTPUT EIGENVALUES IN WR AND WI.
!
    do 50 k = 1, n
        wr( k ) = t( k, k )
        wi( k ) = zero
50  end do
    do 60 k = 1, n - 1
        if (t( k+1, k ) .ne. zero) then
            wi( k ) = sqrt( abs( t( k, k+1 ) ) )* sqrt( abs( t( k+1, k ) ) )
            wi( k+1 ) = -wi( k )
        endif
60  end do
1000  continue
    call matfpe(1)
!
!     END OF FTRSEN
!
end subroutine
