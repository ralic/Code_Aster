subroutine flaln2(ltrans, na, nw, smin, ca,&
                  a, lda, d1, d2, b,&
                  ldb, wr, wi, x, ldx,&
                  scale, xnorm, info)
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
!     SUBROUTINE LAPACK RESOLVANT UN SYSTEME LINEAIRE PERTURBE
!     PARTICULIER (DU TYPE MENTIONNE CI DESSOUS).
!-----------------------------------------------------------------------
!  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     OCTOBER 31, 1992
!     ..
!
!  PURPOSE
!  =======
!
!  FLALN2 SOLVES A SYSTEM OF THE FORM  (CA A - W D ) X = S B
!  OR (CA A' - W D) X = S B   WITH POSSIBLE SCALING ("S") AND
!  PERTURBATION OF A.  (A' MEANS A-TRANSPOSE.)
!
!  A IS AN NA X NA REAL MATRIX, CA IS A REAL SCALAR, D IS AN NA X NA
!  REAL DIAGONAL MATRIX, W IS A REAL OR COMPLEX VALUE, AND X AND B ARE
!  NA X 1 MATRICES -- REAL IF W IS REAL, COMPLEX IF W IS COMPLEX.  NA
!  MAY BE 1 OR 2.
!
!  IF W IS COMPLEX, X AND B ARE REPRESENTED AS NA X 2 MATRICES,
!  THE FIRST COLUMN OF EACH BEING THE REAL PART AND THE SECOND
!  BEING THE IMAGINARY PART.
!
!  "S" IS A SCALING FACTOR (.LE. 1), COMPUTED BY FLALN2, WHICH IS
!  SO CHOSEN THAT X CAN BE COMPUTED WITHOUT OVERFLOW.  X IS FURTHER
!  SCALED IF NECESSARY TO ASSURE THAT NORM(CA A - W D)*NORM(X) IS LESS
!  THAN OVERFLOW.
!
!  IF BOTH SINGULAR VALUES OF (CA A - W D) ARE LESS THAN SMIN,
!  SMIN*IDENTITY WILL BE USED INSTEAD OF (CA A - W D).  IF ONLY ONE
!  SINGULAR VALUE IS LESS THAN SMIN, ONE ELEMENT OF (CA A - W D) WILL BE
!  PERTURBED ENOUGH TO MAKE THE SMALLEST SINGULAR VALUE ROUGHLY SMIN.
!  IF BOTH SINGULAR VALUES ARE AT LEAST SMIN, (CA A - W D) WILL NOT BE
!  PERTURBED.  IN ANY CASE, THE PERTURBATION WILL BE AT MOST SOME SMALL
!  MULTIPLE OF MAX( SMIN, ULP*NORM(CA A - W D) ).  THE SINGULAR VALUES
!  ARE COMPUTED BY INFINITY-NORM APPROXIMATIONS, AND THUS WILL ONLY BE
!  CORRECT TO A FACTOR OF 2 OR SO.
!
!  NOTE: ALL INPUT QUANTITIES ARE ASSUMED TO BE SMALLER THAN OVERFLOW
!  BY A REASONABLE FACTOR.  (SEE BIGNUM.)
!
!  ARGUMENTS
!  ==========
!
!  LTRANS  (INPUT) LOGICAL
!          =.TRUE.:  A-TRANSPOSE WILL BE USED.
!          =.FALSE.: A WILL BE USED (NOT TRANSPOSED.)
!
!  NA      (INPUT) INTEGER
!          THE SIZE OF THE MATRIX A.  IT MAY (ONLY) BE 1 OR 2.
!
!  NW      (INPUT) INTEGER
!          1 IF "W" IS REAL, 2 IF "W" IS COMPLEX.  IT MAY ONLY BE 1
!          OR 2.
!
!  SMIN    (INPUT) REAL*8
!          THE DESIRED LOWER BOUND ON THE SINGULAR VALUES OF A.  THIS
!          SHOULD BE A SAFE DISTANCE AWAY FROM UNDERFLOW OR OVERFLOW,
!          SAY, BETWEEN (UNDERFLOW/MACHINE PRECISION) AND  (MACHINE
!          PRECISION * OVERFLOW ).  (SEE BIGNUM AND ULP.)
!
!  CA      (INPUT) REAL*8
!          THE COEFFICIENT C, WHICH A IS MULTIPLIED BY.
!
!  A       (INPUT) REAL*8 ARRAY, DIMENSION (LDA,NA)
!          THE NA X NA MATRIX A.
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF A.  IT MUST BE AT LEAST NA.
!
!  D1      (INPUT) REAL*8
!          THE 1,1 ELEMENT IN THE DIAGONAL MATRIX D.
!
!  D2      (INPUT) REAL*8
!          THE 2,2 ELEMENT IN THE DIAGONAL MATRIX D.  NOT USED IF NW=1.
!
!  B       (INPUT) REAL*8 ARRAY, DIMENSION (LDB,NW)
!          THE NA X NW MATRIX B (RIGHT-HAND SIDE).  IF NW=2 ("W" IS
!          COMPLEX), COLUMN 1 CONTAINS THE REAL PART OF B AND COLUMN 2
!          CONTAINS THE IMAGINARY PART.
!
!  LDB     (INPUT) INTEGER
!          THE LEADING DIMENSION OF B.  IT MUST BE AT LEAST NA.
!
!  WR      (INPUT) REAL*8
!          THE REAL PART OF THE SCALAR "W".
!
!  WI      (INPUT) REAL*8
!          THE IMAGINARY PART OF THE SCALAR "W".  NOT USED IF NW=1.
!
!  X       (OUTPUT) REAL*8 ARRAY, DIMENSION (LDX,NW)
!          THE NA X NW MATRIX X (UNKNOWNS), AS COMPUTED BY FLALN2.
!          IF NW=2 ("W" IS COMPLEX), ON EXIT, COLUMN 1 WILL CONTAIN
!          THE REAL PART OF X AND COLUMN 2 WILL CONTAIN THE IMAGINARY
!          PART.
!
!  LDX     (INPUT) INTEGER
!          THE LEADING DIMENSION OF X.  IT MUST BE AT LEAST NA.
!
!  SCALE   (OUTPUT) REAL*8
!          THE SCALE FACTOR THAT B MUST BE MULTIPLIED BY TO INSURE
!          THAT OVERFLOW DOES NOT OCCUR WHEN COMPUTING X.  THUS,
!          (CA A - W D) X  WILL BE SCALE*B, NOT B (IGNORING
!          PERTURBATIONS OF A.)  IT WILL BE AT MOST 1.
!
!  XNORM   (OUTPUT) REAL*8
!          THE INFINITY-NORM OF X, WHEN X IS REGARDED AS AN NA X NW
!          REAL MATRIX.
!
!  INFO    (OUTPUT) INTEGER
!          AN ERROR FLAG.  IT WILL BE SET TO ZERO IF NO ERROR OCCURS,
!          A NEGATIVE NUMBER IF AN ARGUMENT IS IN ERROR, OR A POSITIVE
!          NUMBER IF  CA A - W D  HAD TO BE PERTURBED.
!          THE POSSIBLE VALUES ARE:
!          = 0: NO ERROR OCCURRED, AND (CA A - W D) DID NOT HAVE TO BE
!                 PERTURBED.
!          = 1: (CA A - W D) HAD TO BE PERTURBED TO MAKE ITS SMALLEST
!               (OR ONLY) SINGULAR VALUE GREATER THAN SMIN.
!          NOTE: IN THE INTERESTS OF SPEED, THIS ROUTINE DOES NOT
!                CHECK THE INPUTS FOR ERRORS.
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            DISPARITION DE DLAMCH REMPLACE PAR R8MIEM(),
!            REMPLACEMENT DE 2 RETURN PAR GOTO 1000,
!            DISPARITION DE L'EQUIVALENCE DUE TO CRP18,
!            IMPLICIT NONE.
! INTRINSIC FUNCTIONS
!   ABS, MAX
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
#include "asterc/matfpe.h"
#include "asterc/r8miem.h"
#include "blas/dladiv.h"
    logical :: ltrans
    integer :: info, lda, ldb, ldx, na, nw
    real(kind=8) :: ca, d1, d2, scale, smin, wi, wr, xnorm
!     ..
!     .. ARRAY ARGUMENTS ..
    real(kind=8) :: a( lda, * ), b( ldb, * ), x( ldx, * )
!
!     .. PARAMETERS ..
    real(kind=8) :: zero, one
    parameter          ( zero = 0.0d0, one = 1.0d0 )
    real(kind=8) :: two
    parameter          ( two = 2.0d0 )
!     ..
!     .. LOCAL SCALARS ..
    integer :: icmax, j
    real(kind=8) :: bbnd, bi1, bi2, bignum, bnorm, br1, br2, ci21, ci22, cmax
    real(kind=8) :: cnorm, cr21, cr22, csi, csr, li21, lr21, smini, smlnum, temp
    real(kind=8) :: u22abs, ui11, ui11r, ui12, ui12s, ui22, ur11, ur11r, ur12
    real(kind=8) :: ur12s, ur22, xi1, xi2, xr1, xr2
!     ..
!     .. LOCAL ARRAYS ..
    logical :: rswap( 4 ), zswap( 4 )
    integer :: ipivot( 4, 4 )
    real(kind=8) :: ci( 2, 2 ), civ( 4 ), cr( 2, 2 ), crv( 4 )
!     ..
!     .. EXTERNAL FUNCTIONS ..
!
! DUE TO CRP_18     ..
!     .. EQUIVALENCES ..
!      EQUIVALENCE        ( CI( 1, 1 ), CIV( 1 ) ),
!     &                   ( CR( 1, 1 ), CRV( 1 ) )
!     ..
!     .. DATA STATEMENTS ..
    data               zswap / .false., .false., .true., .true. /
    data               rswap / .false., .true., .false., .true. /
    data               ipivot / 1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4,&
     &                   3, 2, 1 /
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
!     COMPUTE BIGNUM
!
    smlnum = two*r8miem()
    bignum = one / smlnum
    smini = max( smin, smlnum )
!
!     DON'T CHECK FOR INPUT ERRORS
!
    info = 0
!
!     STANDARD INITIALIZATIONS
!
    scale = one
!
    if (na .eq. 1) then
!
!        1 X 1  (I.E., SCALAR) SYSTEM   C X = B
!
        if (nw .eq. 1) then
!
!           REAL 1X1 SYSTEM.
!
!           C = CA A - W D
!
            csr = ca*a( 1, 1 ) - wr*d1
            cnorm = abs( csr )
!
!           IF | C | < SMINI, USE C = SMINI
!
            if (cnorm .lt. smini) then
                csr = smini
                cnorm = smini
                info = 1
            endif
!
!           CHECK SCALING FOR  X = B / C
!
            bnorm = abs( b( 1, 1 ) )
            if (cnorm .lt. one .and. bnorm .gt. one) then
                if (bnorm .gt. bignum*cnorm) scale = one / bnorm
            endif
!
!           COMPUTE X
!
            x( 1, 1 ) = ( b( 1, 1 )*scale ) / csr
            xnorm = abs( x( 1, 1 ) )
        else
!
!           COMPLEX 1X1 SYSTEM (W IS COMPLEX)
!
!           C = CA A - W D
!
            csr = ca*a( 1, 1 ) - wr*d1
            csi = -wi*d1
            cnorm = abs( csr ) + abs( csi )
!
!           IF | C | < SMINI, USE C = SMINI
!
            if (cnorm .lt. smini) then
                csr = smini
                csi = zero
                cnorm = smini
                info = 1
            endif
!
!           CHECK SCALING FOR  X = B / C
!
            bnorm = abs( b( 1, 1 ) ) + abs( b( 1, 2 ) )
            if (cnorm .lt. one .and. bnorm .gt. one) then
                if (bnorm .gt. bignum*cnorm) scale = one / bnorm
            endif
!
!           COMPUTE X
!
            call dladiv(scale*b( 1, 1 ), scale*b( 1, 2 ), csr, csi, x( 1, 1 ),&
                        x( 1, 2 ))
            xnorm = abs( x( 1, 1 ) ) + abs( x( 1, 2 ) )
        endif
!
    else
!
!        2X2 SYSTEM
!
!        COMPUTE THE REAL PART OF  C = CA A - W D  (OR  CA A' - W D )
!
        cr( 1, 1 ) = ca*a( 1, 1 ) - wr*d1
        cr( 2, 2 ) = ca*a( 2, 2 ) - wr*d2
        if (ltrans) then
            cr( 1, 2 ) = ca*a( 2, 1 )
            cr( 2, 1 ) = ca*a( 1, 2 )
        else
            cr( 2, 1 ) = ca*a( 2, 1 )
            cr( 1, 2 ) = ca*a( 1, 2 )
        endif
!
! DUE TO CRP_18
        crv(1) = cr(1,1)
        crv(2) = cr(2,1)
        crv(3) = cr(1,2)
        crv(4) = cr(2,2)
!
        if (nw .eq. 1) then
!
!           REAL 2X2 SYSTEM  (W IS REAL)
!
!           FIND THE LARGEST ELEMENT IN C
!
            cmax = zero
            icmax = 0
!
            do j = 1, 4
                if (abs( crv( j ) ) .gt. cmax) then
                    cmax = abs( crv( j ) )
                    icmax = j
                endif
            end do
!
!           IF NORM(C) < SMINI, USE SMINI*IDENTITY.
!
            if (cmax .lt. smini) then
                bnorm = max( abs( b( 1, 1 ) ), abs( b( 2, 1 ) ) )
                if (smini .lt. one .and. bnorm .gt. one) then
                    if (bnorm .gt. bignum*smini) scale = one / bnorm
                endif
                temp = scale / smini
                x( 1, 1 ) = temp*b( 1, 1 )
                x( 2, 1 ) = temp*b( 2, 1 )
                xnorm = temp*bnorm
                info = 1
                goto 1000
            endif
!
!           GAUSSIAN ELIMINATION WITH COMPLETE PIVOTING.
!
            ur11 = crv( icmax )
            cr21 = crv( ipivot( 2, icmax ) )
            ur12 = crv( ipivot( 3, icmax ) )
            cr22 = crv( ipivot( 4, icmax ) )
            ur11r = one / ur11
            lr21 = ur11r*cr21
            ur22 = cr22 - ur12*lr21
!
!           IF SMALLER PIVOT < SMINI, USE SMINI
!
            if (abs( ur22 ) .lt. smini) then
                ur22 = smini
                info = 1
            endif
            if (rswap( icmax )) then
                br1 = b( 2, 1 )
                br2 = b( 1, 1 )
            else
                br1 = b( 1, 1 )
                br2 = b( 2, 1 )
            endif
            br2 = br2 - lr21*br1
            bbnd = max( abs( br1*( ur22*ur11r ) ), abs( br2 ) )
            if (bbnd .gt. one .and. abs( ur22 ) .lt. one) then
                if (bbnd .ge. bignum*abs( ur22 )) scale = one / bbnd
            endif
!
            xr2 = ( br2*scale ) / ur22
            xr1 = ( scale*br1 )*ur11r - xr2*( ur11r*ur12 )
            if (zswap( icmax )) then
                x( 1, 1 ) = xr2
                x( 2, 1 ) = xr1
            else
                x( 1, 1 ) = xr1
                x( 2, 1 ) = xr2
            endif
            xnorm = max( abs( xr1 ), abs( xr2 ) )
!
!           FURTHER SCALING IF  NORM(A) NORM(X) > OVERFLOW
!
            if (xnorm .gt. one .and. cmax .gt. one) then
                if (xnorm .gt. bignum / cmax) then
                    temp = cmax / bignum
                    x( 1, 1 ) = temp*x( 1, 1 )
                    x( 2, 1 ) = temp*x( 2, 1 )
                    xnorm = temp*xnorm
                    scale = temp*scale
                endif
            endif
        else
!
!           COMPLEX 2X2 SYSTEM  (W IS COMPLEX)
!
!           FIND THE LARGEST ELEMENT IN C
!
            ci( 1, 1 ) = -wi*d1
            ci( 2, 1 ) = zero
            ci( 1, 2 ) = zero
            ci( 2, 2 ) = -wi*d2
            cmax = zero
            icmax = 0
! DUE TO CRP_18
            civ(1) = ci(1,1)
            civ(2) = ci(2,1)
            civ(3) = ci(1,2)
            civ(4) = ci(2,2)
!
            do j = 1, 4
                if (abs( crv( j ) )+abs( civ( j ) ) .gt. cmax) then
                    cmax = abs( crv( j ) ) + abs( civ( j ) )
                    icmax = j
                endif
            end do
!
!           IF NORM(C) < SMINI, USE SMINI*IDENTITY.
!
            if (cmax .lt. smini) then
                bnorm = max(abs( b( 1, 1 ) )+abs( b( 1, 2 ) ), abs( b( 2, 1 ) )+abs( b( 2, 2 ) ))
                if (smini .lt. one .and. bnorm .gt. one) then
                    if (bnorm .gt. bignum*smini) scale = one / bnorm
                endif
                temp = scale / smini
                x( 1, 1 ) = temp*b( 1, 1 )
                x( 2, 1 ) = temp*b( 2, 1 )
                x( 1, 2 ) = temp*b( 1, 2 )
                x( 2, 2 ) = temp*b( 2, 2 )
                xnorm = temp*bnorm
                info = 1
                goto 1000
            endif
!
!           GAUSSIAN ELIMINATION WITH COMPLETE PIVOTING.
!
            ur11 = crv( icmax )
            ui11 = civ( icmax )
            cr21 = crv( ipivot( 2, icmax ) )
            ci21 = civ( ipivot( 2, icmax ) )
            ur12 = crv( ipivot( 3, icmax ) )
            ui12 = civ( ipivot( 3, icmax ) )
            cr22 = crv( ipivot( 4, icmax ) )
            ci22 = civ( ipivot( 4, icmax ) )
            if (icmax .eq. 1 .or. icmax .eq. 4) then
!
!              CODE WHEN OFF-DIAGONALS OF PIVOTED C ARE REAL
!
                if (abs( ur11 ) .gt. abs( ui11 )) then
                    temp = ui11 / ur11
                    ur11r = one / ( ur11*( one+temp**2 ) )
                    ui11r = -temp*ur11r
                else
                    temp = ur11 / ui11
                    ui11r = -one / ( ui11*( one+temp**2 ) )
                    ur11r = -temp*ui11r
                endif
                lr21 = cr21*ur11r
                li21 = cr21*ui11r
                ur12s = ur12*ur11r
                ui12s = ur12*ui11r
                ur22 = cr22 - ur12*lr21
                ui22 = ci22 - ur12*li21
            else
!
!              CODE WHEN DIAGONALS OF PIVOTED C ARE REAL
!
                ur11r = one / ur11
                ui11r = zero
                lr21 = cr21*ur11r
                li21 = ci21*ur11r
                ur12s = ur12*ur11r
                ui12s = ui12*ur11r
                ur22 = cr22 - ur12*lr21 + ui12*li21
                ui22 = -ur12*li21 - ui12*lr21
            endif
            u22abs = abs( ur22 ) + abs( ui22 )
!
!           IF SMALLER PIVOT < SMINI, USE SMINI
!
            if (u22abs .lt. smini) then
                ur22 = smini
                ui22 = zero
                info = 1
            endif
            if (rswap( icmax )) then
                br2 = b( 1, 1 )
                br1 = b( 2, 1 )
                bi2 = b( 1, 2 )
                bi1 = b( 2, 2 )
            else
                br1 = b( 1, 1 )
                br2 = b( 2, 1 )
                bi1 = b( 1, 2 )
                bi2 = b( 2, 2 )
            endif
            br2 = br2 - lr21*br1 + li21*bi1
            bi2 = bi2 - li21*br1 - lr21*bi1
            bbnd = max(&
                   ( abs( br1 )+abs( bi1 ) )* ( u22abs*( abs( ur11r )+abs( ui11r ) ) ),&
                   abs( br2 )+abs( bi2 )&
                   )
            if (bbnd .gt. one .and. u22abs .lt. one) then
                if (bbnd .ge. bignum*u22abs) then
                    scale = one / bbnd
                    br1 = scale*br1
                    bi1 = scale*bi1
                    br2 = scale*br2
                    bi2 = scale*bi2
                endif
            endif
!
            call dladiv(br2, bi2, ur22, ui22, xr2,&
                        xi2)
            xr1 = ur11r*br1 - ui11r*bi1 - ur12s*xr2 + ui12s*xi2
            xi1 = ui11r*br1 + ur11r*bi1 - ui12s*xr2 - ur12s*xi2
            if (zswap( icmax )) then
                x( 1, 1 ) = xr2
                x( 2, 1 ) = xr1
                x( 1, 2 ) = xi2
                x( 2, 2 ) = xi1
            else
                x( 1, 1 ) = xr1
                x( 2, 1 ) = xr2
                x( 1, 2 ) = xi1
                x( 2, 2 ) = xi2
            endif
            xnorm = max( abs( xr1 )+abs( xi1 ), abs( xr2 )+abs( xi2 ) )
!
!           FURTHER SCALING IF  NORM(A) NORM(X) > OVERFLOW
!
            if (xnorm .gt. one .and. cmax .gt. one) then
                if (xnorm .gt. bignum / cmax) then
                    temp = cmax / bignum
                    x( 1, 1 ) = temp*x( 1, 1 )
                    x( 2, 1 ) = temp*x( 2, 1 )
                    x( 1, 2 ) = temp*x( 1, 2 )
                    x( 2, 2 ) = temp*x( 2, 2 )
                    xnorm = temp*xnorm
                    scale = temp*scale
                endif
            endif
        endif
    endif
!
1000 continue
!
    call matfpe(1)
!
!     END OF FLALN2
!
end subroutine
