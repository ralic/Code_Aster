subroutine gtrexc(compq, n, t, ldt, q,&
                  ldq, ifst, ilst, info)
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
!  PURPOSE
!  =======
!
!  GTREXC REORDERS THE SCHUR FACTORIZATION OF A COMPLEX MATRIX
!  A = Q*T*Q**H, SO THAT THE DIAGONAL ELEMENT OF T WITH ROW INDEX IFST
!  IS MOVED TO ROW ILST.
!
!  THE SCHUR FORM T IS REORDERED BY A UNITARY SIMILARITY TRANSFORMATION
!  Z**H*T*Z, AND OPTIONALLY THE MATRIX Q OF SCHUR VECTORS IS UPDATED BY
!  POSTMULTPLYING IT WITH Z.
!
!  ARGUMENTS
!  =========
!
!  COMPQ   (INPUT) CHARACTER*1
!          = 'V':  UPDATE THE MATRIX Q OF SCHUR VECTORS;
!          = 'N':  DO NOT UPDATE Q.
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX T. N >= 0.
!
!  T       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDT,N)
!          ON ENTRY, THE UPPER TRIANGULAR MATRIX T.
!          ON EXIT, THE REORDERED UPPER TRIANGULAR MATRIX.
!
!  LDT     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
!
!  Q       (INPUT/OUTPUT) COMPLEX*16 ARRAY, DIMENSION (LDQ,N)
!          ON ENTRY, IF COMPQ = 'V', THE MATRIX Q OF SCHUR VECTORS.
!          ON EXIT, IF COMPQ = 'V', Q HAS BEEN POSTMULTIPLIED BY THE
!          UNITARY TRANSFORMATION MATRIX Z WHICH REORDERS T.
!          IF COMPQ = 'N', Q IS NOT REFERENCED.
!
!  LDQ     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY Q.  LDQ >= MAX(1,N).
!
!  IFST    (INPUT) INTEGER
!  ILST    (INPUT) INTEGER
!          SPECIFY THE REORDERING OF THE DIAGONAL ELEMENTS OF T:
!          THE ELEMENT WITH ROW INDEX IFST IS MOVED TO ROW ILST BY A
!          SEQUENCE OF TRANSPOSITIONS BETWEEN ADJACENT ELEMENTS.
!          1 <= IFST <= N; 1 <= ILST <= N.
!
!  INFO    (OUTPUT) INTEGER
!          = 0:  SUCCESSFUL EXIT
!          < 0:  IF INFO = -I, THE I-TH ARGUMENT HAD AN ILLEGAL VALUE
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
    include 'asterc/matfpe.h'
    include 'asterfort/glartg.h'
    include 'asterfort/xerbla.h'
    include 'blas/lsame.h'
    include 'blas/zrot.h'
    character(len=1) :: compq
    integer :: ifst, ilst, info, ldq, ldt, n
!     ..
!     .. ARRAY ARGUMENTS ..
    complex(kind=8) :: q( ldq, * ), t( ldt, * )
!     ..
!
!     .. LOCAL SCALARS ..
    logical :: wantq
    integer :: k, m1, m2, m3
    real(kind=8) :: cs
    complex(kind=8) :: sn, t11, t22, temp
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
!     DECODE AND TEST THE INPUT PARAMETERS.
!
    info = 0
    wantq = lsame( compq, 'V' )
    if (.not.lsame( compq, 'N' ) .and. .not.wantq) then
        info = -1
    else if (n.lt.0) then
        info = -2
    else if (ldt.lt.max( 1, n )) then
        info = -4
    else if (ldq.lt.1 .or. ( wantq .and. ldq.lt.max( 1, n ) )) then
        info = -6
    else if (ifst.lt.1 .or. ifst.gt.n) then
        info = -7
    else if (ilst.lt.1 .or. ilst.gt.n) then
        info = -8
    endif
    if (info .ne. 0) then
        call xerbla('GTREXC', -info)
        goto 1000
    endif
!
!     QUICK RETURN IF POSSIBLE
!
    if (n .eq. 1 .or. ifst .eq. ilst) goto 1000
!
    if (ifst .lt. ilst) then
!
!        MOVE THE IFST-TH DIAGONAL ELEMENT FORWARD DOWN THE DIAGONAL.
!
        m1 = 0
        m2 = -1
        m3 = 1
    else
!
!        MOVE THE IFST-TH DIAGONAL ELEMENT BACKWARD UP THE DIAGONAL.
!
        m1 = -1
        m2 = 0
        m3 = -1
    endif
!
    do 10 k = ifst + m1, ilst + m2, m3
!
!        INTERCHANGE THE K-TH AND (K+1)-TH DIAGONAL ELEMENTS.
!
        t11 = t( k, k )
        t22 = t( k+1, k+1 )
!
!        DETERMINE THE TRANSFORMATION TO PERFORM THE INTERCHANGE.
!
        call glartg(t( k, k+1 ), t22-t11, cs, sn, temp)
!
!        APPLY TRANSFORMATION TO THE MATRIX T.
!
        if (k+2 .le. n) call zrot(n-k-1, t( k, k+2 ), ldt, t( k+1, k+ 2 ), ldt,&
                                  cs, sn)
        call zrot(k-1, t( 1, k ), 1, t( 1, k+1 ), 1,&
                  cs, dconjg( sn ))
!
        t( k, k ) = t22
        t( k+1, k+1 ) = t11
!
        if (wantq) then
!
!           ACCUMULATE TRANSFORMATION IN THE MATRIX Q.
!
            call zrot(n, q( 1, k ), 1, q( 1, k+1 ), 1,&
                      cs, dconjg( sn ))
        endif
!
10  end do
!
1000  continue
    call matfpe(1)
!
!     END OF GTREXC
!
end subroutine
