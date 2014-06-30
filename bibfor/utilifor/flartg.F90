subroutine flartg(f, g, cs, sn, r)
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
!     SUBROUTINE LAPACK GENERANT UNE ROTATION PLANE QUI EST UNE
!     VERSION PLUS PRECISE QUE LA ROUTINE BLAS1 DROTG.
!---------------------------------------------------------------------
!  -- LAPACK AUXILIARY ROUTINE (VERSION 2.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     SEPTEMBER 30, 1994
!
!  PURPOSE
!  =======
!
!  FLARTG GENERATE A PLANE ROTATION SO THAT
!
!     (  CS  SN  )  .  ( F )  =  ( R )   WHERE CS**2 + SN**2 = 1.
!     ( -SN  CS  )     ( G )     ( 0 )
!
!  THIS IS A SLOWER, MORE ACCURATE VERSION OF THE BLAS1 ROUTINE DROTG,
!  WITH THE FOLLOWING OTHER DIFFERENCES:
!     F AND G ARE UNCHANGED ON RETURN.
!     IF G=0, THEN CS=1 AND SN=0.
!     IF F=0 AND (G .NE. 0), THEN CS=0 AND SN=1 WITHOUT DOING ANY
!        FLOATING POINT OPERATIONS (SAVES WORK IN DBDSQR WHEN
!        THERE ARE ZEROS ON THE DIAGONAL).
!
!  IF F EXCEEDS G IN MAGNITUDE, CS WILL BE POSITIVE.
!
!  ARGUMENTS
!  =========
!
!  F       (INPUT) REAL*8
!          THE FIRST COMPONENT OF VECTOR TO BE ROTATED.
!
!  G       (INPUT) REAL*8
!          THE SECOND COMPONENT OF VECTOR TO BE ROTATED.
!
!  CS      (OUTPUT) REAL*8
!          THE COSINE OF THE ROTATION.
!
!  SN      (OUTPUT) REAL*8
!          THE SINE OF THE ROTATION.
!
!  R       (OUTPUT) REAL*8
!          THE NONZERO COMPONENT OF THE ROTATED VECTOR.
!
! ASTER INFORMATION
! 11/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE DLAMCH PAR R8PREM(), R8MIEM() ET
!            ISBAEM().
! 28/01/2000 RAJOUT DE LA VARIABLE BASE.
! INTRINSIC FUNCTIONS
!            ABS, INT, LOG, MAX, SQRT, DBLE.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
#include "asterc/isbaem.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
    real(kind=8) :: cs, f, g, r, sn
!     ..
!     .. PARAMETERS ..
    real(kind=8) :: zero
    parameter          ( zero = 0.0d0 )
    real(kind=8) :: one
    parameter          ( one = 1.0d0 )
    real(kind=8) :: two
    parameter          ( two = 2.0d0 )
!     ..
!     .. LOCAL SCALARS ..
    logical(kind=1) :: first
    integer :: count, i
    real(kind=8) :: eps, f1, g1, safmin, safmn2, safmx2, scale, base
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. SAVE STATEMENT ..
    save               first, safmx2, safmin, safmn2
!     ..
!     .. DATA STATEMENTS ..
    data               first / .true. /
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
    if (first) then
        first = .false.
        safmin = r8miem()
        eps = r8prem()*0.5d0
        base = dble(isbaem())
        safmn2 = base**int( log( safmin / eps ) / log( base ) / two )
        safmx2 = one / safmn2
    endif
    if (g .eq. zero) then
        cs = one
        sn = zero
        r = f
    else if (f.eq.zero) then
        cs = zero
        sn = one
        r = g
    else
        f1 = f
        g1 = g
        scale = max( abs( f1 ), abs( g1 ) )
        if (scale .ge. safmx2) then
            count = 0
10          continue
            count = count + 1
            f1 = f1*safmn2
            g1 = g1*safmn2
            scale = max( abs( f1 ), abs( g1 ) )
            if (scale .ge. safmx2) goto 10
            r = sqrt( f1**2+g1**2 )
            cs = f1 / r
            sn = g1 / r
            do 20 i = 1, count
                r = r*safmx2
20          continue
        else if (scale.le.safmn2) then
            count = 0
30          continue
            count = count + 1
            f1 = f1*safmx2
            g1 = g1*safmx2
            scale = max( abs( f1 ), abs( g1 ) )
            if (scale .le. safmn2) goto 30
            r = sqrt( f1**2+g1**2 )
            cs = f1 / r
            sn = g1 / r
            do 40 i = 1, count
                r = r*safmn2
40          continue
        else
            r = sqrt( f1**2+g1**2 )
            cs = f1 / r
            sn = g1 / r
        endif
        if (abs( f ) .gt. abs( g ) .and. cs .lt. zero) then
            cs = -cs
            sn = -sn
            r = -r
        endif
    endif
!
!     END OF FLARTG
!
end subroutine
