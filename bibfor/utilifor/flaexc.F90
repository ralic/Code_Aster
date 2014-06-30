subroutine flaexc(wantq, n, t, ldt, q,&
                  ldq, j1, n1, n2, work,&
                  info)
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
!     SUBROUTINE LAPACK PERMUTANT DEUX BLOCS DIAGONAUX D'UN MATRICE
!     TRIANGULAIRE SUPERIEURE.
!-----------------------------------------------------------------------
!  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     FEBRUARY 29, 1992
!
!  PURPOSE
!  =======
!
!  FLAEXC SWAPS ADJACENT DIAGONAL BLOCKS T11 AND T22 OF ORDER 1 OR 2 IN
!  AN UPPER QUASI-TRIANGULAR MATRIX T BY AN ORTHOGONAL SIMILARITY
!  TRANSFORMATION.
!
!  T MUST BE IN SCHUR CANONICAL FORM, THAT IS, BLOCK UPPER TRIANGULAR
!  WITH 1-BY-1 AND 2-BY-2 DIAGONAL BLOCKS, EACH 2-BY-2 DIAGONAL BLOCK
!  HAS ITS DIAGONAL ELEMNTS EQUAL AND ITS OFF-DIAGONAL ELEMENTS OF
!  OPPOSITE SIGN.
!
!  ARGUMENTS
!  =========
!
!  WANTQ   (INPUT) LOGICAL
!          = .TRUE. : ACCUMULATE THE TRANSFORMATION IN THE MATRIX Q,
!          = .FALSE.: DO NOT ACCUMULATE THE TRANSFORMATION.
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX T. N >= 0.
!
!  T       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDT,N)
!          ON ENTRY, THE UPPER QUASI-TRIANGULAR MATRIX T, IN SCHUR
!          CANONICAL FORM.
!          ON EXIT, THE UPDATED MATRIX T, AGAIN IN SCHUR CANONICAL FORM.
!
!  LDT     (INPUT)  INTEGER
!          THE LEADING DIMENSION OF THE ARRAY T. LDT >= MAX(1,N).
!
!  Q       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION (LDQ,N)
!          ON ENTRY, IF WANTQ IS .TRUE., THE ORTHOGONAL MATRIX Q.
!          ON EXIT, IF WANTQ IS .TRUE., THE UPDATED MATRIX Q.
!          IF WANTQ IS .FALSE., Q IS NOT REFERENCED.
!
!  LDQ     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY Q.
!          LDQ >= 1, AND IF WANTQ IS .TRUE., LDQ >= N.
!
!  J1      (INPUT) INTEGER
!          THE INDEX OF THE FIRST ROW OF THE FIRST BLOCK T11.
!
!  N1      (INPUT) INTEGER
!          THE ORDER OF THE FIRST BLOCK T11. N1 = 0, 1 OR 2.
!
!  N2      (INPUT) INTEGER
!          THE ORDER OF THE SECOND BLOCK T22. N2 = 0, 1 OR 2.
!
!  WORK    (WORKSPACE) REAL*8 ARRAY, DIMENSION (N)
!
!  INFO    (OUTPUT) INTEGER
!          = 0: SUCCESSFUL EXIT
!          = 1: THE TRANSFORMED MATRIX T WOULD BE TOO FAR FROM SCHUR
!               FORM, THE BLOCKS ARE NOT SWAPPED AND T AND Q ARE
!               UNCHANGED.
!
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE 3 RETURN PAR GOTO 999,
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
#include "asterc/isbaem.h"
#include "asterc/matfpe.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/flanv2.h"
#include "asterfort/flarfg.h"
#include "asterfort/flartg.h"
#include "asterfort/flasy2.h"
#include "blas/dlacpy.h"
#include "blas/dlange.h"
#include "blas/dlarfx.h"
#include "blas/drot.h"
    logical(kind=1) :: wantq
    integer :: info, j1, ldq, ldt, n, n1, n2
!     ..
!     .. ARRAY ARGUMENTS ..
    real(kind=8) :: q( ldq, * ), t( ldt, * ), work( * )
!     ..
!     .. PARAMETERS ..
    real(kind=8) :: zero, one
    parameter          ( zero = 0.0d+0, one = 1.0d+0 )
    real(kind=8) :: ten
    parameter          ( ten = 1.0d+1 )
    integer :: ldd, ldx
    parameter          ( ldd = 4, ldx = 2 )
!     ..
!     .. LOCAL SCALARS ..
    integer :: ierr, j2, j3, j4, k, nd
    real(kind=8) :: cs, dnorm, eps, scale, smlnum, sn, t11, t22, t33, tau, tau1
    real(kind=8) :: tau2, temp, thresh, wi1, wi2, wr1, wr2, xnorm
!     ..
!     .. LOCAL ARRAYS ..
    real(kind=8) :: d( ldd, 4 ), u( 3 ), u1( 3 ), u2( 3 ), x( ldx, 2 )
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
    info = 0
!
!     QUICK RETURN IF POSSIBLE
!
    if (n .eq. 0 .or. n1 .eq. 0 .or. n2 .eq. 0) goto 999
    if (j1+n1 .gt. n) goto 999
!
    j2 = j1 + 1
    j3 = j1 + 2
    j4 = j1 + 3
!
    if (n1 .eq. 1 .and. n2 .eq. 1) then
!
!        SWAP TWO 1-BY-1 BLOCKS.
!
        t11 = t( j1, j1 )
        t22 = t( j2, j2 )
!
!        DETERMINE THE TRANSFORMATION TO PERFORM THE INTERCHANGE.
!
        call flartg(t( j1, j2 ), t22-t11, cs, sn, temp)
!
!        APPLY TRANSFORMATION TO THE MATRIX T.
!
        if (j3 .le. n) call drot(n-j1-1, t( j1, j3 ), ldt, t( j2, j3 ), ldt,&
                                 cs, sn)
        call drot(j1-1, t( 1, j1 ), 1, t( 1, j2 ), 1,&
                  cs, sn)
!
        t( j1, j1 ) = t22
        t( j2, j2 ) = t11
!
        if (wantq) then
!
!           ACCUMULATE TRANSFORMATION IN THE MATRIX Q.
!
            call drot(n, q( 1, j1 ), 1, q( 1, j2 ), 1,&
                      cs, sn)
        endif
!
    else
!
!        SWAPPING INVOLVES AT LEAST ONE 2-BY-2 BLOCK.
!
!        COPY THE DIAGONAL BLOCK OF ORDER N1+N2 TO THE LOCAL ARRAY D
!        AND COMPUTE ITS NORM.
!
        nd = n1 + n2
! DUE TO CRP102 CALL DLACPY( 'FULL', ND, ND, T( J1, J1 ), LDT, D, LDD )
        call dlacpy('F', nd, nd, t( j1, j1 ), ldt,&
                    d, ldd)
! DUE TO CRP102 DNORM = DLANGE( 'MAX', ND, ND, D, LDD, WORK )
        dnorm = dlange( 'M', nd, nd, d, ldd, work )
!
!        COMPUTE MACHINE-DEPENDENT THRESHOLD FOR TEST FOR ACCEPTING
!        SWAP.
!
        eps = r8prem() * 0.5d0 * isbaem()
        smlnum = r8miem() / eps
        thresh = max( ten*eps*dnorm, smlnum )
!
!        SOLVE T11*X - X*T22 = SCALE*T12 FOR X.
!
        call flasy2(.false._1, .false._1, -1, n1, n2,&
                    d, ldd, d( n1+1, n1+1 ), ldd, d( 1, n1+1 ),&
                    ldd, scale, x, ldx, xnorm,&
                    ierr)
!
!        SWAP THE ADJACENT DIAGONAL BLOCKS.
!
        k = n1 + n1 + n2 - 3
!
        select case (k)
        case (1)
!
!           N1 = 1, N2 = 2: GENERATE ELEMENTARY REFLECTOR H SO THAT:
!
!           ( SCALE, X11, X12 ) H = ( 0, 0, * )
!
            u( 1 ) = scale
            u( 2 ) = x( 1, 1 )
            u( 3 ) = x( 1, 2 )
            call flarfg(3, u( 3 ), u, 1, tau)
            u( 3 ) = one
            t11 = t( j1, j1 )
!
!           PERFORM SWAP PROVISIONALLY ON DIAGONAL BLOCK IN D.
!
            call dlarfx('L', 3, 3, u, tau,&
                        d, ldd, work)
            call dlarfx('R', 3, 3, u, tau,&
                        d, ldd, work)
!
!           TEST WHETHER TO REJECT SWAP.
!
            if (max( abs( d( 3, 1 ) ), abs( d( 3, 2 ) ), abs( d( 3, 3 )-t11 ) ) .gt. thresh) &
            goto 50
!
!           ACCEPT SWAP: APPLY TRANSFORMATION TO THE ENTIRE MATRIX T.
!
            call dlarfx('L', 3, n-j1+1, u, tau,&
                        t( j1, j1 ), ldt, work)
            call dlarfx('R', j2, 3, u, tau,&
                        t( 1, j1 ), ldt, work)
!
            t( j3, j1 ) = zero
            t( j3, j2 ) = zero
            t( j3, j3 ) = t11
!
            if (wantq) then
!
!               ACCUMULATE TRANSFORMATION IN THE MATRIX Q.
!
                call dlarfx('R', n, 3, u, tau,&
                            q( 1, j1 ), ldq, work)
            endif
!
        case (2)
!           N1 = 2, N2 = 1: GENERATE ELEMENTARY REFLECTOR H SO THAT:
!
!           H (  -X11 ) = ( * )
!             (  -X21 ) = ( 0 )
!             ( SCALE ) = ( 0 )
!
            u( 1 ) = -x( 1, 1 )
            u( 2 ) = -x( 2, 1 )
            u( 3 ) = scale
            call flarfg(3, u( 1 ), u( 2 ), 1, tau)
            u( 1 ) = one
            t33 = t( j3, j3 )
!
!            PERFORM SWAP PROVISIONALLY ON DIAGONAL BLOCK IN D.
!
            call dlarfx('L', 3, 3, u, tau,&
                        d, ldd, work)
            call dlarfx('R', 3, 3, u, tau,&
                        d, ldd, work)
!
!            TEST WHETHER TO REJECT SWAP.
!
            if (max( abs( d( 2, 1 ) ), abs( d( 3, 1 ) ), abs( d( 1, 1 )-t33 ) ) .gt. thresh) &
            goto 50
!
!            ACCEPT SWAP: APPLY TRANSFORMATION TO THE ENTIRE MATRIX T.
!
            call dlarfx('R', j3, 3, u, tau,&
                        t( 1, j1 ), ldt, work)
            call dlarfx('L', 3, n-j1, u, tau,&
                        t( j1, j2 ), ldt, work)
!
            t( j1, j1 ) = t33
            t( j2, j1 ) = zero
            t( j3, j1 ) = zero
!
            if (wantq) then
!
!               ACCUMULATE TRANSFORMATION IN THE MATRIX Q.
!
                call dlarfx('R', n, 3, u, tau,&
                            q( 1, j1 ), ldq, work)
            endif
!
        case (3)
!
!            N1 = 2, N2 = 2: GENERATE ELEMENTARY REFLECTORS H(1) AND H(2) SO
!            THAT:
!
!            H(2) H(1) (  -X11  -X12 ) = (  *  * )
!                      (  -X21  -X22 )   (  0  * )
!                      ( SCALE    0  )   (  0  0 )
!                      (    0  SCALE )   (  0  0 )
!
            u1( 1 ) = -x( 1, 1 )
            u1( 2 ) = -x( 2, 1 )
            u1( 3 ) = scale
            call flarfg(3, u1( 1 ), u1( 2 ), 1, tau1)
            u1( 1 ) = one
!
            temp = -tau1*( x( 1, 2 )+u1( 2 )*x( 2, 2 ) )
            u2( 1 ) = -temp*u1( 2 ) - x( 2, 2 )
            u2( 2 ) = -temp*u1( 3 )
            u2( 3 ) = scale
            call flarfg(3, u2( 1 ), u2( 2 ), 1, tau2)
            u2( 1 ) = one
!
!            PERFORM SWAP PROVISIONALLY ON DIAGONAL BLOCK IN D.
!
            call dlarfx('L', 3, 4, u1, tau1,&
                        d, ldd, work)
            call dlarfx('R', 4, 3, u1, tau1,&
                        d, ldd, work)
            call dlarfx('L', 3, 4, u2, tau2,&
                        d( 2, 1 ), ldd, work)
            call dlarfx('R', 4, 3, u2, tau2,&
                        d( 1, 2 ), ldd, work)
!
!            TEST WHETHER TO REJECT SWAP.
!
            if (max(abs(d(3,1)), abs(d(3,2)), abs(d(4,1)), abs(d(4,2))) .gt. thresh) goto 50
!
!            ACCEPT SWAP: APPLY TRANSFORMATION TO THE ENTIRE MATRIX T.
!
            call dlarfx('L', 3, n-j1+1, u1, tau1,&
                        t( j1, j1 ), ldt, work)
            call dlarfx('R', j4, 3, u1, tau1,&
                        t( 1, j1 ), ldt, work)
            call dlarfx('L', 3, n-j1+1, u2, tau2,&
                        t( j2, j1 ), ldt, work)
            call dlarfx('R', j4, 3, u2, tau2,&
                        t( 1, j2 ), ldt, work)
!
            t( j3, j1 ) = zero
            t( j3, j2 ) = zero
            t( j4, j1 ) = zero
            t( j4, j2 ) = zero
!
            if (wantq) then
!
!               ACCUMULATE TRANSFORMATION IN THE MATRIX Q.
!
                call dlarfx('R', n, 3, u1, tau1,&
                            q( 1, j1 ), ldq, work)
                call dlarfx('R', n, 3, u2, tau2,&
                            q( 1, j2 ), ldq, work)
            endif
!
        end select
!
        if (n2 .eq. 2) then
!
!           STANDARDIZE NEW 2-BY-2 BLOCK T11
!
            call flanv2(t( j1, j1 ), t( j1, j2 ), t( j2, j1 ), t( j2, j2 ), wr1,&
                        wi1, wr2, wi2, cs, sn)
            call drot(n-j1-1, t( j1, j1+2 ), ldt, t( j2, j1+2 ), ldt,&
                      cs, sn)
            call drot(j1-1, t( 1, j1 ), 1, t( 1, j2 ), 1,&
                      cs, sn)
            if (wantq) call drot(n, q( 1, j1 ), 1, q( 1, j2 ), 1,&
                                 cs, sn)
        endif
!
        if (n1 .eq. 2) then
!
!           STANDARDIZE NEW 2-BY-2 BLOCK T22
!
            j3 = j1 + n2
            j4 = j3 + 1
            call flanv2(t( j3, j3 ), t( j3, j4 ), t( j4, j3 ), t( j4, j4 ), wr1,&
                        wi1, wr2, wi2, cs, sn)
            if (j3+2 .le. n) call drot(n-j3-1, t( j3, j3+2 ), ldt, t( j4, j3+2 ), ldt,&
                                       cs, sn)
            call drot(j3-1, t( 1, j3 ), 1, t( 1, j4 ), 1,&
                      cs, sn)
            if (wantq) call drot(n, q( 1, j3 ), 1, q( 1, j4 ), 1,&
                                 cs, sn)
        endif
!
    endif
    goto 999
!
!     EXIT WITH INFO = 1 IF SWAP WAS REJECTED.
!
 50 continue
    info = 1
999 continue
!
    call matfpe(1)
!
!     END OF FLAEXC
!
end subroutine
