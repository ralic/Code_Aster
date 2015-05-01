! ===============================================================
! THIS LAPACK 2.0 ROUTINE IS DEPRECATED  
! DO NOT USE IT : YOU SHOULD PREFER UP-TO-DATE LAPACK ROUTINE
!
! BUT DO NOT REMOVE IT :
! THE PRESENT ROUTINE IS MANDATORY FOR ARPACK LIBRARY
! WHICH STICKS TO LAPACK 2.0 VERSION 
! ==============================================================
subroutine ar_dlarfg(n, alpha, x, incx, tau)
! ======================================================================
! COPYRIGHT (C) LAPACK
! COPYRIGHT (C) 2007 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
!     SUBROUTINE LAPACK CALCULANT UN REFLECTEUR H TEL QUE DECRIT
!     CI DESSOUS.
!-----------------------------------------------------------------------
!  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     SEPTEMBER 30, 1994
!
!  PURPOSE
!  =======
!
!  DLARFG GENERATES A REAL ELEMENTARY REFLECTOR H OF ORDER N, SUCH
!  THAT
!
!        H * ( ALPHA ) = ( BETA ),   H' * H = I.
!            (   X   )   (   0  )
!
!  WHERE ALPHA AND BETA ARE SCALARS, AND X IS AN (N-1)-ELEMENT REAL
!  VECTOR. H IS REPRESENTED IN THE FORM
!
!        H = I - TAU * ( 1 ) * ( 1 V' ) ,
!                      ( V )
!
!  WHERE TAU IS A REAL SCALAR AND V IS A REAL (N-1)-ELEMENT
!  VECTOR.
!
!  IF THE ELEMENTS OF X ARE ALL ZERO, THEN TAU = 0 AND H IS TAKEN TO BE
!  THE UNIT MATRIX.
!
!  OTHERWISE  1 <= TAU <= 2.
!
!  ARGUMENTS
!  =========
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE ELEMENTARY REFLECTOR.
!
!  ALPHA   (INPUT/OUTPUT) REAL*8
!          ON ENTRY, THE VALUE ALPHA.
!          ON EXIT, IT IS OVERWRITTEN WITH THE VALUE BETA.
!
!  X       (INPUT/OUTPUT) REAL*8 ARRAY, DIMENSION
!                         (1+(N-2)*ABS(INCX))
!          ON ENTRY, THE VECTOR X.
!          ON EXIT, IT IS OVERWRITTEN WITH THE VECTOR V.
!
!  INCX    (INPUT) INTEGER
!          THE INCREMENT BETWEEN ELEMENTS OF X. INCX > 0.
!
!  TAU     (OUTPUT) REAL*8
!          THE VALUE TAU.
!
!-----------------------------------------------------------------------
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE DLAMCH PAR R8PREM ET R8MIEM,
!            REMPLACEMENT DE RETURN PAR GOTO 1000,
!            MODIFICATION DES APPELS BLAS (ROUTINE ASTER BL...),
!            IMPLICIT NONE.
! INTRINSIC FUNCTION
!    ABS, SIGN
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
#include "asterc/matfpe.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "blas/dlapy2.h"
#include "blas/dnrm2.h"
#include "blas/dscal.h"
    integer :: incx, n
    real(kind=8) :: alpha, tau
!     ..
!     .. ARRAY ARGUMENTS ..
    real(kind=8) :: x( * )
!
!     .. PARAMETERS ..
    real(kind=8) :: one, zero
    parameter          ( one = 1.0d+0, zero = 0.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    integer :: j, knt
    real(kind=8) :: beta, rsafmn, safmin, xnorm
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
    if (n .le. 1) then
        tau = zero
        goto 1000
    endif
!
    xnorm = dnrm2( n-1, x, incx )
!
    if (xnorm .eq. zero) then
!
!        H  =  I
!
        tau = zero
    else
!
!        GENERAL CASE
!
        beta = -sign( dlapy2( alpha, xnorm ), alpha )
        safmin = r8miem() / (r8prem()*0.5d0)
        if (abs( beta ) .lt. safmin) then
!
!           XNORM, BETA MAY BE INACCURATE, SCALE X AND RECOMPUTE THEM
!
            rsafmn = one / safmin
            knt = 0
10          continue
            knt = knt + 1
            call dscal(n-1, rsafmn, x, incx)
            beta = beta*rsafmn
            alpha = alpha*rsafmn
            if (abs( beta ) .lt. safmin) goto 10
!
!           NEW BETA IS AT MOST 1, AT LEAST SAFMIN
!
            xnorm = dnrm2( n-1, x, incx )
            beta = -sign( dlapy2( alpha, xnorm ), alpha )
            tau = ( beta-alpha ) / beta
            call dscal(n-1, one / ( alpha-beta ), x, incx)
!
!           IF ALPHA IS SUBNORMAL, IT MAY LOSE RELATIVE ACCURACY
!
            alpha = beta
            do 20 j = 1, knt
                alpha = alpha*safmin
20          continue
        else
            tau = ( beta-alpha ) / beta
            call dscal(n-1, one / ( alpha-beta ), x, incx)
            alpha = beta
        endif
    endif
!
1000  continue
    call matfpe(1)
!
!     END OF DLARFG
!
end subroutine
