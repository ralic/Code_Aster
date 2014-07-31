! ===============================================================
! THIS LAPACK 2.0 ROUTINE IS DEPRECATED  
! DO NOT USE IT : YOU SHOULD PREFER UP-TO-DATE LAPACK ROUTINE
!
! BUT DO NOT REMOVE IT :
! THE PRESENT ROUTINE IS MANDATORY FOR ARPACK LIBRARY
! WHICH STICKS TO LAPACK 2.0 VERSION 
! ==============================================================
subroutine ar_dlanv2(a, b, c, d, rt1r,&
                  rt1i, rt2r, rt2i, cs, sn)
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
!     SUBROUTINE LAPACK CALCULANT LA FACTORISATION DE SCHUR D'UNE
!     MATRICE 2X2.
!-----------------------------------------------------------------------
!  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     SEPTEMBER 30, 1994
!
!  PURPOSE
!  =======
!
!  DLANV2 COMPUTES THE SCHUR FACTORIZATION OF A REAL 2-BY-2 NONSYMMETRIC
!  MATRIX IN STANDARD FORM:
!
!       ( A  B ) = ( CS -SN ) ( AA  BB ) ( CS  SN )
!       ( C  D )   ( SN  CS ) ( CC  DD ) (-SN  CS )
!
!  WHERE EITHER
!  1) CC = 0 SO THAT AA AND DD ARE REAL EIGENVALUES OF THE MATRIX, OR
!  2) AA = DD AND BB*CC < 0, SO THAT AA + OR - SQRT(BB*CC) ARE COMPLEX
!  CONJUGATE EIGENVALUES.
!
!  ARGUMENTS
!  =========
!
!  A       (INPUT/OUTPUT) REAL*8
!  B       (INPUT/OUTPUT) REAL*8
!  C       (INPUT/OUTPUT) REAL*8
!  D       (INPUT/OUTPUT) REAL*8
!          ON ENTRY, THE ELEMENTS OF THE INPUT MATRIX.
!          ON EXIT, THEY ARE OVERWRITTEN BY THE ELEMENTS OF THE
!          STANDARDISED SCHUR FORM.
!
!  RT1R    (OUTPUT) REAL*8
!  RT1I    (OUTPUT) REAL*8
!  RT2R    (OUTPUT) REAL*8
!  RT2I    (OUTPUT) REAL*8
!          THE REAL AND IMAGINARY PARTS OF THE EIGENVALUES. IF THE
!          EIGENVALUES ARE BOTH REAL, ABS(RT1R) >= ABS(RT2R), IF THE
!          EIGENVALUES ARE A COMPLEX CONJUGATE PAIR, RT1I > 0.
!
!  CS      (OUTPUT) REAL*8
!  SN      (OUTPUT) REAL*8
!          PARAMETERS OF THE ROTATION MATRIX.
!-----------------------------------------------------------------------
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            IMPLICIT NONE.
! INTRINSIC FUNCTIONS
!            ABS, SIGN, SQRT.
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
#include "asterc/matfpe.h"
#include "blas/dlapy2.h"
    real(kind=8) :: a, b, c, cs, d, rt1i, rt1r, rt2i, rt2r, sn
!
!     ..
!     .. PARAMETERS ..
    real(kind=8) :: zero, half, one
    parameter          ( zero = 0.0d+0, half = 0.5d+0, one = 1.0d+0 )
!     ..
!     .. LOCAL SCALARS ..
    real(kind=8) :: aa, bb, cc, cs1, dd, p, sab, sac, sigma, sn1, tau, temp
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    call matfpe(-1)
!
!     INITIALIZE CS AND SN
!
    cs = one
    sn = zero
!
    if (c .eq. zero) then
        goto 10
!
    else if (b.eq.zero) then
!
!        SWAP ROWS AND COLUMNS
!
        cs = zero
        sn = one
        temp = d
        d = a
        a = temp
        b = -c
        c = zero
        goto 10
        else if( (a-d).eq.zero .and. sign( one, b ).ne. sign( one, c ) )&
    then
        goto 10
    else
!
!        MAKE DIAGONAL ELEMENTS EQUAL
!
        temp = a - d
        p = half*temp
        sigma = b + c
        tau = dlapy2( sigma, temp )
        cs1 = sqrt( half*( one+abs( sigma ) / tau ) )
        sn1 = -( p / ( tau*cs1 ) )*sign( one, sigma )
!
!        COMPUTE ( AA  BB ) = ( A  B ) ( CS1 -SN1 )
!                ( CC  DD )   ( C  D ) ( SN1  CS1 )
!
        aa = a*cs1 + b*sn1
        bb = -a*sn1 + b*cs1
        cc = c*cs1 + d*sn1
        dd = -c*sn1 + d*cs1
!
!        COMPUTE ( A  B ) = ( CS1  SN1 ) ( AA  BB )
!                ( C  D )   (-SN1  CS1 ) ( CC  DD )
!
        a = aa*cs1 + cc*sn1
        b = bb*cs1 + dd*sn1
        c = -aa*sn1 + cc*cs1
        d = -bb*sn1 + dd*cs1
!
!        ACCUMULATE TRANSFORMATION
!
        temp = cs*cs1 - sn*sn1
        sn = cs*sn1 + sn*cs1
        cs = temp
!
        temp = half*( a+d )
        a = temp
        d = temp
!
        if (c .ne. zero) then
            if (b .ne. zero) then
                if (sign( one, b ) .eq. sign( one, c )) then
!
!                 REAL EIGENVALUES: REDUCE TO UPPER TRIANGULAR FORM
!
                    sab = sqrt( abs( b ) )
                    sac = sqrt( abs( c ) )
                    p = sign( sab*sac, c )
                    tau = one / sqrt( abs( b+c ) )
                    a = temp + p
                    d = temp - p
                    b = b - c
                    c = zero
                    cs1 = sab*tau
                    sn1 = sac*tau
                    temp = cs*cs1 - sn*sn1
                    sn = cs*sn1 + sn*cs1
                    cs = temp
                endif
            else
                b = -c
                c = zero
                temp = cs
                cs = -sn
                sn = temp
            endif
        endif
    endif
!
10  continue
!
!     STORE EIGENVALUES IN (RT1R,RT1I) AND (RT2R,RT2I).
!
    rt1r = a
    rt2r = d
    if (c .eq. zero) then
        rt1i = zero
        rt2i = zero
    else
        rt1i = sqrt( abs( b ) )*sqrt( abs( c ) )
        rt2i = -rt1i
    endif
!
    call matfpe(1)
!
!     END OF DLANV2
!
end subroutine
