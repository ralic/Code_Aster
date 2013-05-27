function ldasum(n, dx, incx)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) LINPACK
! ======================================================================
!     SUBROUTINE LINPACK CALCULANT UNE SOMME DE VALEUR ABSOLUE.
!-----------------------------------------------------------------------
!     TAKES THE SUM OF THE ABSOLUTE VALUES.
!     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!     MODIFIED TO CORRECT PROBLEM WITH NEGATIVE INCREMENT, 8/21/90.
!
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            REMPLACEMENT DE 2 RETURN PAR GOTO 1000,
!            REMPLACEMENT DE DABS PAR ABS,
!            IMPLICIT NONE.
! INTRINSIC FUNCTIONS
!            ABS.
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
    real(kind=8) :: ldasum
!
    real(kind=8) :: dx(1), dtemp
    integer :: i, incx, ix, m, mp1, n
!
    ldasum = 0.0d0
    dtemp = 0.0d0
    if (n .le. 0) goto 1000
    if (incx .eq. 1) goto 20
!
!        CODE FOR INCREMENT NOT EQUAL TO 1
!
    ix = 1
    if(incx.lt.0)ix = (-n+1)*incx + 1
    do 10 i = 1, n
        dtemp = dtemp + abs(dx(ix))
        ix = ix + incx
10  end do
    ldasum = dtemp
    goto 1000
!
!        CODE FOR INCREMENT EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
! DUE TO CRP_11
20  continue
    m = mod(n,6)
    if (m .eq. 0) goto 40
    do 30 i = 1, m
        dtemp = dtemp + abs(dx(i))
30  end do
    if (n .lt. 6) goto 60
! DUE TO CRP_11
40  continue
    mp1 = m + 1
    do 50 i = mp1, n, 6
        dtemp = dtemp + abs(&
                dx(i)) + abs(dx(i + 1)) + abs(dx(i + 2)) + abs(dx(i + 3)) + abs(dx(i + 4)) + abs(&
                &dx(i + 5)&
                )
50  end do
! DUE TO CRP_11
60  continue
    ldasum = dtemp
!
1000  continue
end function
