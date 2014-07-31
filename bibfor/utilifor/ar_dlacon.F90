! ===============================================================
! THIS LAPACK 2.0 ROUTINE IS DEPRECATED  
! DO NOT USE IT : YOU SHOULD PREFER UP-TO-DATE LAPACK ROUTINE
!
! BUT DO NOT REMOVE IT :
! THE PRESENT ROUTINE IS MANDATORY FOR ARPACK LIBRARY
! WHICH STICKS TO LAPACK 2.0 VERSION 
! ==============================================================
subroutine ar_dlacon(n, v, x, isgn, est,&
                  kase)
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
!     SUBROUTINE LAPACK CALCULANT LA NORME L1 D'UNE MATRICE CARREE.
!-----------------------------------------------------------------------
!  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     FEBRUARY 29, 1992
!
!  PURPOSE
!  =======
!
!  DLACON ESTIMATES THE 1-NORM OF A SQUARE, REAL MATRIX A.
!  REVERSE COMMUNICATION IS USED FOR EVALUATING MATRIX-VECTOR PRODUCTS.
!
!  ARGUMENTS
!  =========
!
!  N      (INPUT) INTEGER
!         THE ORDER OF THE MATRIX.  N >= 1.
!
!  V      (WORKSPACE) REAL*8 ARRAY, DIMENSION (N)
!         ON THE FINAL RETURN, V = A*W,  WHERE  EST = NORM(V)/NORM(W)
!         (W IS NOT RETURNED).
!
!  X      (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (N)
!         ON AN INTERMEDIATE RETURN, X SHOULD BE OVERWRITTEN BY
!               A * X,   IF KASE=1,
!               A' * X,  IF KASE=2,
!         AND DLACON MUST BE RE-CALLED WITH ALL THE OTHER PARAMETERS
!         UNCHANGED.
!
!  ISGN   (WORKSPACE) INTEGER ARRAY, DIMENSION (N)
!
!  EST    (OUTPUT) REAL*8
!         AN ESTIMATE (A LOWER BOUND) FOR NORM(A).
!
!  KASE   (INPUT/OUTPUT) INTEGER
!         ON THE INITIAL CALL TO DLACON, KASE SHOULD BE 0.
!         ON AN INTERMEDIATE RETURN, KASE WILL BE 1 OR 2, INDICATING
!         WHETHER X SHOULD BE OVERWRITTEN BY A * X  OR A' * X.
!         ON THE FINAL RETURN FROM DLACON, KASE WILL AGAIN BE 0.
!
!  FURTHER DETAILS
!  ======= =======
!
!  CONTRIBUTED BY NICK HIGHAM, UNIVERSITY OF MANCHESTER.
!  ORIGINALLY NAMED SONEST, DATED MARCH 16, 1988.
!
!  REFERENCE: N.J. HIGHAM, "FORTRAN CODES FOR ESTIMATING THE ONE-NORM OF
!  A REAL OR COMPLEX MATRIX, WITH APPLICATIONS TO CONDITION ESTIMATION",
!  ACM TRANS. MATH. SOFT., VOL. 14, NO. 4, PP. 381-396, DECEMBER 1988.
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE 5 RETURN PAR UN GOTO 1000,
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! INTRINSIC FUNCTION
!   ABS, DBLE, NINT, SIGN
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
#include "asterfort/ldasum.h"
#include "blas/dcopy.h"
#include "blas/idamax.h"
    integer :: kase, n
    real(kind=8) :: est
!     ..
!     .. ARRAY ARGUMENTS ..
    integer :: isgn( * )
    real(kind=8) :: v( * ), x( * )
!     ..
!     .. PARAMETERS ..
    integer :: itmax
    parameter          ( itmax = 5 )
    real(kind=8) :: zero, one, two
    parameter          ( zero = 0.0d+0, one = 1.0d+0, two = 2.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    integer :: i, iter, j, jlast, jump
    real(kind=8) :: altsgn, estold, temp
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. SAVE STATEMENT ..
    save
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    if (kase .eq. 0) then
        do i = 1, n
            x( i ) = one / dble( n )
        end do
        kase = 1
        jump = 1
        goto 1000
    endif
!
    if (jump .eq. 1) then
        goto 20
    else if (jump .eq. 2) then
        goto 40
    else if (jump .eq. 3) then
        goto 70
    else if (jump .eq. 4) then
        goto 110
    else if (jump .eq. 5) then
        goto 140
    endif
!
!     ................ ENTRY   (JUMP = 1)
!     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X.
!
 20 continue
    if (n .eq. 1) then
        v( 1 ) = x( 1 )
        est = abs( v( 1 ) )
!        ... QUIT
        goto 150
    endif
    est = ldasum( n, x, 1 )
!
    do i = 1, n
        x( i ) = sign( one, x( i ) )
        isgn( i ) = nint( x( i ) )
    end do
    kase = 2
    jump = 2
    goto 1000
!
!     ................ ENTRY   (JUMP = 2)
!     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY TRANDPOSE(A)*X.
!
 40 continue
    j = idamax( n, x, 1 )
    iter = 2
!
!     MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
!
 50 continue
    do i = 1, n
        x( i ) = zero
    end do
    x( j ) = one
    kase = 1
    jump = 3
    goto 1000
!
!     ................ ENTRY   (JUMP = 3)
!     X HAS BEEN OVERWRITTEN BY A*X.
!
 70 continue
    call dcopy(n, x, 1, v, 1)
    estold = est
    est = ldasum( n, v, 1 )
    do i = 1, n
        if (nint( sign( one, x( i ) ) ) .ne. isgn( i )) goto 90
    end do
!     REPEATED SIGN VECTOR DETECTED, HENCE ALGORITHM HAS CONVERGED.
    goto 120
!
 90 continue
!     TEST FOR CYCLING.
    if (est .le. estold) goto 120
!
    do i = 1, n
        x( i ) = sign( one, x( i ) )
        isgn( i ) = nint( x( i ) )
    end do
    kase = 2
    jump = 4
    goto 1000
!
!     ................ ENTRY   (JUMP = 4)
!     X HAS BEEN OVERWRITTEN BY TRANDPOSE(A)*X.
!
110 continue
    jlast = j
    j = idamax( n, x, 1 )
    if (( x( jlast ).ne.abs( x( j ) ) ) .and. ( iter.lt.itmax )) then
        iter = iter + 1
        goto 50
    endif
!
!     ITERATION COMPLETE.  FINAL STAGE.
!
120 continue
    altsgn = one
    do i = 1, n
        x( i ) = altsgn*( one+dble( i-1 ) / dble( n-1 ) )
        altsgn = -altsgn
    end do
    kase = 1
    jump = 5
    goto 1000
!
!     ................ ENTRY   (JUMP = 5)
!     X HAS BEEN OVERWRITTEN BY A*X.
!
140 continue
    temp = two*( ldasum( n, x, 1 ) / dble( 3*n ) )
    if (temp .gt. est) then
        call dcopy(n, x, 1, v, 1)
        est = temp
    endif
!
150 continue
    kase = 0
1000 continue
!
!     END OF DLACON
!
end subroutine
