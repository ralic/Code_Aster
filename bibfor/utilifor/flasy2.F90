subroutine flasy2(ltranl, ltranr, isgn, n1, n2,&
                  tl, ldtl, tr, ldtr, b,&
                  ldb, scale, x, ldx, xnorm,&
                  info)
! ----------------------------------------------------------------------
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
!     SUBROUTINE LAPACK RESOLVANT L'EQUATION MATRICIELLE CI-DESSOUS.
!-----------------------------------------------------------------------
!  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     OCTOBER 31, 1992
!
!
!  PURPOSE
!  =======
!
!  FLASY2 SOLVES FOR THE N1 BY N2 MATRIX X, 1 <= N1,N2 <= 2, IN
!
!         OP(TL)*X + ISGN*X*OP(TR) = SCALE*B,
!
!  WHERE TL IS N1 BY N1, TR IS N2 BY N2, B IS N1 BY N2, AND ISGN = 1 OR
!  -1.  OP(T) = T OR T', WHERE T' DENOTES THE TRANSPOSE OF T.
!
!  ARGUMENTS
!  =========
!
!  LTRANL  (INPUT) LOGICAL
!          ON ENTRY, LTRANL SPECIFIES THE OP(TL):
!             = .FALSE., OP(TL) = TL,
!             = .TRUE., OP(TL) = TL'.
!
!  LTRANR  (INPUT) LOGICAL
!          ON ENTRY, LTRANR SPECIFIES THE OP(TR):
!            = .FALSE., OP(TR) = TR,
!            = .TRUE., OP(TR) = TR'.
!
!  ISGN    (INPUT) INTEGER
!          ON ENTRY, ISGN SPECIFIES THE SIGN OF THE EQUATION
!          AS DESCRIBED BEFORE. ISGN MAY ONLY BE 1 OR -1.
!
!  N1      (INPUT) INTEGER
!          ON ENTRY, N1 SPECIFIES THE ORDER OF MATRIX TL.
!          N1 MAY ONLY BE 0, 1 OR 2.
!
!  N2      (INPUT) INTEGER
!          ON ENTRY, N2 SPECIFIES THE ORDER OF MATRIX TR.
!          N2 MAY ONLY BE 0, 1 OR 2.
!
!  TL      (INPUT) REAL*8 ARRAY, DIMENSION (LDTL,2)
!          ON ENTRY, TL CONTAINS AN N1 BY N1 MATRIX.
!
!  LDTL    (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE MATRIX TL. LDTL >= MAX(1,N1).
!
!  TR      (INPUT) REAL*8 ARRAY, DIMENSION (LDTR,2)
!          ON ENTRY, TR CONTAINS AN N2 BY N2 MATRIX.
!
!  LDTR    (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE MATRIX TR. LDTR >= MAX(1,N2).
!
!  B       (INPUT) REAL*8 ARRAY, DIMENSION (LDB,2)
!          ON ENTRY, THE N1 BY N2 MATRIX B CONTAINS THE RIGHT-HAND
!          SIDE OF THE EQUATION.
!
!  LDB     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE MATRIX B. LDB >= MAX(1,N1).
!
!  SCALE   (OUTPUT) REAL*8
!          ON EXIT, SCALE CONTAINS THE SCALE FACTOR. SCALE IS CHOSEN
!          LESS THAN OR EQUAL TO 1 TO PREVENT THE SOLUTION OVERFLOWING.
!
!  X       (OUTPUT) REAL*8 ARRAY, DIMENSION (LDX,2)
!          ON EXIT, X CONTAINS THE N1 BY N2 SOLUTION.
!
!  LDX     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE MATRIX X. LDX >= MAX(1,N1).
!
!  XNORM   (OUTPUT) REAL*8
!          ON EXIT, XNORM IS THE INFINITY-NORM OF THE SOLUTION.
!
!  INFO    (OUTPUT) INTEGER
!          ON EXIT, INFO IS SET TO
!             0: SUCCESSFUL EXIT.
!             1: TL AND TR HAVE TOO CLOSE EIGENVALUES, SO TL OR
!                TR IS PERTURBED TO GET A NONSINGULAR EQUATION.
!          NOTE: IN THE INTERESTS OF SPEED, THIS ROUTINE DOES NOT
!                CHECK THE INPUTS FOR ERRORS.
!
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE 3 RETURN PAR GOTO 1000,
!            REMPLACEMENT DE DLAMCH PAR R8PREM, R8MIEM ET ISBAEM,
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! INTRINSIC FUNCTIONS
!            ABS, MAX.
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
    include 'asterc/isbaem.h'
    include 'asterc/r8miem.h'
    include 'asterc/r8prem.h'
    include 'blas/dcopy.h'
    include 'blas/dswap.h'
    include 'blas/idamax.h'
    logical :: ltranl, ltranr
    integer :: info, isgn, ldb, ldtl, ldtr, ldx, n1, n2
    real(kind=8) :: scale, xnorm
!     ..
!     .. ARRAY ARGUMENTS ..
    real(kind=8) :: b( ldb, * ), tl( ldtl, * ), tr( ldtr, * ), x( ldx, * )
!     ..
!     .. PARAMETERS ..
    real(kind=8) :: zero, one
    parameter          ( zero = 0.0d+0, one = 1.0d+0 )
    real(kind=8) :: two, half, eight
    parameter          ( two = 2.0d+0, half = 0.5d+0, eight = 8.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    logical :: bswap, xswap
    integer :: i, ip, ipiv, ipsv, j, jp, jpsv, k
    real(kind=8) :: bet, eps, gam, l21, sgn, smin, smlnum, tau1, temp, u11, u12
    real(kind=8) :: u22, xmax
!     ..
!     .. LOCAL ARRAYS ..
    logical :: bswpiv( 4 ), xswpiv( 4 )
    integer :: jpiv( 4 ), locl21( 4 ), locu12( 4 ), locu22( 4 )
    real(kind=8) :: btmp( 4 ), t16( 4, 4 ), tmp( 4 ), x2( 2 )
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. DATA STATEMENTS ..
    data               locu12 / 3, 4, 1, 2 / , locl21 / 2, 1, 4, 3 / ,&
     &                   locu22 / 4, 3, 2, 1 /
    data               xswpiv / .false., .false., .true., .true. /
    data               bswpiv / .false., .true., .false., .true. /
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     DO NOT CHECK THE INPUT PARAMETERS FOR ERRORS
!
    info = 0
!
!     QUICK RETURN IF POSSIBLE
!
    if (n1 .eq. 0 .or. n2 .eq. 0) goto 1000
!
!     SET CONSTANTS TO CONTROL OVERFLOW
!
    eps = r8prem() * 0.5d0 * isbaem()
    smlnum = r8miem() / eps
    sgn = isgn
!
    k = n1 + n1 + n2 - 2
    goto ( 10, 20, 30, 50 )k
!
!     1 BY 1: TL11*X + SGN*X*TR11 = B11
!
10  continue
    tau1 = tl( 1, 1 ) + sgn*tr( 1, 1 )
    bet = abs( tau1 )
    if (bet .le. smlnum) then
        tau1 = smlnum
        bet = smlnum
        info = 1
    endif
!
    scale = one
    gam = abs( b( 1, 1 ) )
    if (smlnum*gam .gt. bet) scale = one / gam
!
    x( 1, 1 ) = ( b( 1, 1 )*scale ) / tau1
    xnorm = abs( x( 1, 1 ) )
    goto 1000
!
!     1 BY 2:
!     TL11*(X11 X12) + ISGN*(X11 X12)*OP(TR11 TR12)  = (B11 B12)
!                                       (TR21 TR22)
!
20  continue
!
    smin = max(&
           eps*max(&
           abs( tl( 1, 1 ) ), abs( tr( 1, 1 ) ), abs( tr( 1, 2 ) ), abs( tr( 2, 1 ) ),&
           abs( tr( 2, 2 ) )&
           ),&
           smlnum&
           )
    tmp( 1 ) = tl( 1, 1 ) + sgn*tr( 1, 1 )
    tmp( 4 ) = tl( 1, 1 ) + sgn*tr( 2, 2 )
    if (ltranr) then
        tmp( 2 ) = sgn*tr( 2, 1 )
        tmp( 3 ) = sgn*tr( 1, 2 )
    else
        tmp( 2 ) = sgn*tr( 1, 2 )
        tmp( 3 ) = sgn*tr( 2, 1 )
    endif
    btmp( 1 ) = b( 1, 1 )
    btmp( 2 ) = b( 1, 2 )
    goto 40
!
!     2 BY 1:
!          OP(TL11 TL12)*(X11) + ISGN* (X11)*TR11  = (B11)
!            (TL21 TL22) (X21)         (X21)         (B21)
!
30  continue
    smin = max(&
           eps*max(&
           abs( tr( 1, 1 ) ), abs( tl( 1, 1 ) ), abs( tl( 1, 2 ) ), abs( tl( 2, 1 ) ),&
           abs( tl( 2, 2 ) )&
           ),&
           smlnum&
           )
    tmp( 1 ) = tl( 1, 1 ) + sgn*tr( 1, 1 )
    tmp( 4 ) = tl( 2, 2 ) + sgn*tr( 1, 1 )
    if (ltranl) then
        tmp( 2 ) = tl( 1, 2 )
        tmp( 3 ) = tl( 2, 1 )
    else
        tmp( 2 ) = tl( 2, 1 )
        tmp( 3 ) = tl( 1, 2 )
    endif
    btmp( 1 ) = b( 1, 1 )
    btmp( 2 ) = b( 2, 1 )
40  continue
!
!     SOLVE 2 BY 2 SYSTEM USING COMPLETE PIVOTING.
!     SET PIVOTS LESS THAN SMIN TO SMIN.
!
    ipiv = idamax( 4, tmp, 1 )
    u11 = tmp( ipiv )
    if (abs( u11 ) .le. smin) then
        info = 1
        u11 = smin
    endif
    u12 = tmp( locu12( ipiv ) )
    l21 = tmp( locl21( ipiv ) ) / u11
    u22 = tmp( locu22( ipiv ) ) - u12*l21
    xswap = xswpiv( ipiv )
    bswap = bswpiv( ipiv )
    if (abs( u22 ) .le. smin) then
        info = 1
        u22 = smin
    endif
    if (bswap) then
        temp = btmp( 2 )
        btmp( 2 ) = btmp( 1 ) - l21*temp
        btmp( 1 ) = temp
    else
        btmp( 2 ) = btmp( 2 ) - l21*btmp( 1 )
    endif
    scale = one
    if (( two*smlnum )*abs( btmp( 2 ) ) .gt. abs( u22 ) .or.&
        ( two*smlnum )*abs( btmp( 1 ) ) .gt. abs( u11 )) then
        scale = half / max( abs( btmp( 1 ) ), abs( btmp( 2 ) ) )
        btmp( 1 ) = btmp( 1 )*scale
        btmp( 2 ) = btmp( 2 )*scale
    endif
    x2( 2 ) = btmp( 2 ) / u22
    x2( 1 ) = btmp( 1 ) / u11 - ( u12 / u11 )*x2( 2 )
    if (xswap) then
        temp = x2( 2 )
        x2( 2 ) = x2( 1 )
        x2( 1 ) = temp
    endif
    x( 1, 1 ) = x2( 1 )
    if (n1 .eq. 1) then
        x( 1, 2 ) = x2( 2 )
        xnorm = abs( x( 1, 1 ) ) + abs( x( 1, 2 ) )
    else
        x( 2, 1 ) = x2( 2 )
        xnorm = max( abs( x( 1, 1 ) ), abs( x( 2, 1 ) ) )
    endif
    goto 1000
!
!     2 BY 2:
!     OP(TL11 TL12)*(X11 X12) +ISGN* (X11 X12)*OP(TR11 TR12) = (B11 B12)
!       (TL21 TL22) (X21 X22)        (X21 X22)   (TR21 TR22)   (B21 B22)
!
!     SOLVE EQUIVALENT 4 BY 4 SYSTEM USING COMPLETE PIVOTING.
!     SET PIVOTS LESS THAN SMIN TO SMIN.
!
50  continue
    smin = max( abs( tr( 1, 1 ) ), abs( tr( 1, 2 ) ), abs( tr( 2, 1 ) ), abs( tr( 2, 2 ) ) )
    smin = max(smin, abs( tl( 1, 1 ) ), abs( tl( 1, 2 ) ), abs( tl( 2, 1 ) ), abs( tl( 2, 2 ) ))
    smin = max( eps*smin, smlnum )
    btmp( 1 ) = zero
    call dcopy(16, btmp, 0, t16, 1)
    t16( 1, 1 ) = tl( 1, 1 ) + sgn*tr( 1, 1 )
    t16( 2, 2 ) = tl( 2, 2 ) + sgn*tr( 1, 1 )
    t16( 3, 3 ) = tl( 1, 1 ) + sgn*tr( 2, 2 )
    t16( 4, 4 ) = tl( 2, 2 ) + sgn*tr( 2, 2 )
    if (ltranl) then
        t16( 1, 2 ) = tl( 2, 1 )
        t16( 2, 1 ) = tl( 1, 2 )
        t16( 3, 4 ) = tl( 2, 1 )
        t16( 4, 3 ) = tl( 1, 2 )
    else
        t16( 1, 2 ) = tl( 1, 2 )
        t16( 2, 1 ) = tl( 2, 1 )
        t16( 3, 4 ) = tl( 1, 2 )
        t16( 4, 3 ) = tl( 2, 1 )
    endif
    if (ltranr) then
        t16( 1, 3 ) = sgn*tr( 1, 2 )
        t16( 2, 4 ) = sgn*tr( 1, 2 )
        t16( 3, 1 ) = sgn*tr( 2, 1 )
        t16( 4, 2 ) = sgn*tr( 2, 1 )
    else
        t16( 1, 3 ) = sgn*tr( 2, 1 )
        t16( 2, 4 ) = sgn*tr( 2, 1 )
        t16( 3, 1 ) = sgn*tr( 1, 2 )
        t16( 4, 2 ) = sgn*tr( 1, 2 )
    endif
    btmp( 1 ) = b( 1, 1 )
    btmp( 2 ) = b( 2, 1 )
    btmp( 3 ) = b( 1, 2 )
    btmp( 4 ) = b( 2, 2 )
!
!     PERFORM ELIMINATION
!
    do 100 i = 1, 3
        xmax = zero
        do 70 ip = i, 4
            do 60 jp = i, 4
                if (abs( t16( ip, jp ) ) .ge. xmax) then
                    xmax = abs( t16( ip, jp ) )
                    ipsv = ip
                    jpsv = jp
                endif
60          continue
70      continue
        if (ipsv .ne. i) then
            call dswap(4, t16( ipsv, 1 ), 4, t16( i, 1 ), 4)
            temp = btmp( i )
            btmp( i ) = btmp( ipsv )
            btmp( ipsv ) = temp
        endif
        if (jpsv .ne. i) call dswap(4, t16( 1, jpsv ), 1, t16( 1, i ), 1)
        jpiv( i ) = jpsv
        if (abs( t16( i, i ) ) .lt. smin) then
            info = 1
            t16( i, i ) = smin
        endif
        do 90 j = i + 1, 4
            t16( j, i ) = t16( j, i ) / t16( i, i )
            btmp( j ) = btmp( j ) - t16( j, i )*btmp( i )
            do 80 k = i + 1, 4
                t16( j, k ) = t16( j, k ) - t16( j, i )*t16( i, k )
80          continue
90      continue
100  end do
    if (abs( t16( 4, 4 ) ) .lt. smin) t16( 4, 4 ) = smin
    scale = one
    if (( eight*smlnum )*abs( btmp( 1 ) ) .gt. abs( t16( 1, 1 ) ) .or.&
        ( eight*smlnum )*abs( btmp( 2 ) ) .gt. abs( t16( 2, 2 ) ) .or.&
        ( eight*smlnum )*abs( btmp( 3 ) ) .gt. abs( t16( 3, 3 ) ) .or.&
        ( eight*smlnum )*abs( btmp( 4 ) ) .gt. abs( t16( 4, 4 ) )) then
        scale = ( one / eight ) / max( abs( btmp( 1 ) ), abs( btmp( 2 ) ), abs( btmp( 3 ) ), abs(&
                & btmp( 4 ) ) )
        btmp( 1 ) = btmp( 1 )*scale
        btmp( 2 ) = btmp( 2 )*scale
        btmp( 3 ) = btmp( 3 )*scale
        btmp( 4 ) = btmp( 4 )*scale
    endif
    do 120 i = 1, 4
        k = 5 - i
        temp = one / t16( k, k )
        tmp( k ) = btmp( k )*temp
        do 110 j = k + 1, 4
            tmp( k ) = tmp( k ) - ( temp*t16( k, j ) )*tmp( j )
110      continue
120  end do
    do 130 i = 1, 3
        if (jpiv( 4-i ) .ne. 4-i) then
            temp = tmp( 4-i )
            tmp( 4-i ) = tmp( jpiv( 4-i ) )
            tmp( jpiv( 4-i ) ) = temp
        endif
130  end do
    x( 1, 1 ) = tmp( 1 )
    x( 2, 1 ) = tmp( 2 )
    x( 1, 2 ) = tmp( 3 )
    x( 2, 2 ) = tmp( 4 )
    xnorm = max( abs( tmp( 1 ) )+abs( tmp( 3 ) ), abs( tmp( 2 ) )+abs( tmp( 4 ) ))
1000  continue
!
!     END OF FLASY2
!
end subroutine
