! ===============================================================
! THIS LAPACK 2.0 ROUTINE IS DEPRECATED  
! DO NOT USE IT : YOU SHOULD PREFER UP-TO-DATE LAPACK ROUTINE
!
! BUT DO NOT REMOVE IT :
! THE PRESENT ROUTINE IS MANDATORY FOR ARPACK LIBRARY
! WHICH STICKS TO LAPACK 2.0 VERSION 
! ==============================================================
subroutine ar_zlartg(f, g, cs, sn, r)
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
!  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     SEPTEMBER 30, 1994
!
!
!  PURPOSE
!  =======
!
!  ZLARTG GENERATES A PLANE ROTATION SO THAT
!
!     [  CS  SN  ]     [ F ]     [ R ]
!     [  __      ]  .  [   ]  =  [   ]   WHERE CS**2 + |SN|**2 = 1.
!     [ -SN  CS  ]     [ G ]     [ 0 ]
!
!  THIS IS A FASTER VERSION OF THE BLAS1 ROUTINE GROTG, EXCEPT FOR
!  THE FOLLOWING DIFFERENCES:
!     F AND G ARE UNCHANGED ON RETURN.
!     IF G=0, THEN CS=1 AND SN=0.
!     IF F=0 AND (G .NE. 0), THEN CS=0 AND SN=1 WITHOUT DOING ANY
!        FLOATING POINT OPERATIONS.
!
!  ARGUMENTS
!  =========
!
!  F       (INPUT) COMPLEX*16
!          THE FIRST COMPONENT OF VECTOR TO BE ROTATED.
!
!  G       (INPUT) COMPLEX*16
!          THE SECOND COMPONENT OF VECTOR TO BE ROTATED.
!
!  CS      (OUTPUT) DOUBLE PRECISION
!          THE COSINE OF THE ROTATION.
!
!  SN      (OUTPUT) COMPLEX*16
!          THE SINE OF THE ROTATION.
!
!  R       (OUTPUT) COMPLEX*16
!          THE NONZERO COMPONENT OF THE ROTATED VECTOR.
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
    real(kind=8) :: cs
    complex(kind=8) :: f, g, r, sn
!     ..
!     .. PARAMETERS ..
    real(kind=8) :: one, zero
    parameter          ( one = 1.0d+0, zero = 0.0d+0 )
    complex(kind=8) :: czero
    parameter          ( czero = ( 0.0d+0, 0.0d+0 ) )
!     ..
!     .. LOCAL SCALARS ..
    real(kind=8) :: d, di, f1, f2, fa, g1, g2, ga
    complex(kind=8) :: fs, gs, ss
!     ..
!     .. STATEMENT FUNCTIONS ..
!     ..
!     .. STATEMENT FUNCTION DEFINITIONS ..
#define abs1( t )   abs( dble( t ) ) + abs( dimag( t ) )
#define abssq( t )   dble( t )**2 + dimag( t )**2
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     [ 25 OR 38 OPS FOR MAIN PATHS ]
!
    if (g .eq. czero) then
        cs = one
        sn = zero
        r = f
    else if (f.eq.czero) then
        cs = zero
!
        sn = dconjg( g ) / abs( g )
        r = abs( g )
!
!         SN = ONE
!         R = G
!
    else
        f1 = abs1( f )
        g1 = abs1( g )
        if (f1 .ge. g1) then
            gs = g / f1
            g2 = abssq( gs )
            fs = f / f1
            f2 = abssq( fs )
            d = sqrt( one+g2 / f2 )
            cs = one / d
            sn = dconjg( gs )*fs*( cs / f2 )
            r = f*d
        else
            fs = f / g1
            f2 = abssq( fs )
            fa = sqrt( f2 )
            gs = g / g1
            g2 = abssq( gs )
            ga = sqrt( g2 )
            d = sqrt( one+f2 / g2 )
            di = one / d
            cs = ( fa / ga )*di
            ss = ( dconjg( gs )*fs ) / ( fa*ga )
            sn = ss*di
            r = g*ss*d
        endif
    endif
!
!     END OF ZLARTG
!
end subroutine
