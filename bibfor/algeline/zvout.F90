subroutine zvout(lout, n, cx, idigit, ifmt)
!----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!
!     SUBROUTINE ARPACK ECRIVANT DES VECTEURS DE COMPLEXE.
!-----------------------------------------------------------------------
!  Routine:    ZVOUT
!
!  Purpose:    Complex*16 vector output routine.
!
!  Usage:      CALL ZVOUT (LOUT, N, CX, IDIGIT, IFMT)
!
!  Arguments
!     N      - Length of array CX.  (Input)
!     CX     - Complex*16 array to be printed.  (Input)
!     IFMT   - Format to be used in printing array CX.  (Input)
!     IDIGIT - Print up to IABS(IDIGIT) decimal digits per number.  (In)
!              If IDIGIT .LT. 0, printing is done with 72 columns.
!              If IDIGIT .GT. 0, printing is done with 132 columns.
!
!-----------------------------------------------------------------------
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER.
!            REMPLACEMENT DE 1 RETURN PAR 1 GOTO 1005.
!            IMPLICIT NONE.
!-----------------------------------------------------------------------
    implicit none
!     ... SPECIFICATIONS FOR ARGUMENTS
    integer :: n, idigit, lout
    complex(kind=8) :: cx(*)
    character(len=*) :: ifmt
!
!     ... SPECIFICATIONS FOR LOCAL VARIABLES
    integer :: i, ndigit, k1, k2, lll
    character(len=80) :: line
!
!     ... FIRST EXECUTABLE STATEMENT
!
!
    lll = min( len( ifmt ), 80 )
    do 1 i = 1, lll
        line( i: i ) = '-'
 1  end do
!
    do 2 i = lll + 1, 80
        line( i: i ) = ' '
 2  end do
!
    write( lout, 2000 )ifmt, line( 1: lll )
    2000 format ( /1x, a  /1x, a )
!
    if (n .le. 0) goto 1005
    ndigit = idigit
    if (idigit .eq. 0) ndigit = 4
!
!=======================================================================
!             CODE FOR OUTPUT USING 72 COLUMNS FORMAT
!=======================================================================
!
    if (idigit .lt. 0) then
        ndigit = -idigit
        if (ndigit .le. 4) then
            do 30 k1 = 1, n, 2
                k2 = min( n, k1+1 )
                if (k1 .ne. n) then
                    write( lout, 9998 )k1, k2, ( cx( i ), i = k1, k2 )
                else
                    write( lout, 9997 )k1, k2, ( cx( i ), i = k1, k2 )
                endif
30          continue
        else if (ndigit.le.6) then
            do 40 k1 = 1, n, 2
                k2 = min( n, k1+1 )
                if (k1 .ne. n) then
                    write( lout, 9988 )k1, k2, ( cx( i ), i = k1, k2 )
                else
                    write( lout, 9987 )k1, k2, ( cx( i ), i = k1, k2 )
                endif
40          continue
        else if (ndigit.le.8) then
            do 50 k1 = 1, n, 2
                k2 = min( n, k1+1 )
                if (k1 .ne. n) then
                    write( lout, 9978 )k1, k2, ( cx( i ), i = k1, k2 )
                else
                    write( lout, 9977 )k1, k2, ( cx( i ), i = k1, k2 )
                endif
50          continue
        else
            do 60 k1 = 1, n
                write( lout, 9968 )k1, k1, cx( i )
60          continue
        endif
!
!=======================================================================
!             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
!=======================================================================
!
    else
        if (ndigit .le. 4) then
            do 70 k1 = 1, n, 4
                k2 = min( n, k1+3 )
                if ((k1+3) .le. n) then
                    write( lout, 9958 )k1, k2, ( cx( i ), i = k1, k2 )
                else if ((k1+3-n) .eq. 1) then
                    write( lout, 9957 )k1, k2, ( cx( i ), i = k1, k2 )
                else if ((k1+3-n) .eq. 2) then
                    write( lout, 9956 )k1, k2, ( cx( i ), i = k1, k2 )
                else if ((k1+3-n) .eq. 1) then
                    write( lout, 9955 )k1, k2, ( cx( i ), i = k1, k2 )
                endif
70          continue
        else if (ndigit.le.6) then
            do 80 k1 = 1, n, 3
                k2 = min( n, k1+2 )
                if ((k1+2) .le. n) then
                    write( lout, 9948 )k1, k2, ( cx( i ), i = k1, k2 )
                else if ((k1+2-n) .eq. 1) then
                    write( lout, 9947 )k1, k2, ( cx( i ), i = k1, k2 )
                else if ((k1+2-n) .eq. 2) then
                    write( lout, 9946 )k1, k2, ( cx( i ), i = k1, k2 )
                endif
80          continue
        else if (ndigit.le.8) then
            do 90 k1 = 1, n, 3
                k2 = min( n, k1+2 )
                if ((k1+2) .le. n) then
                    write( lout, 9938 )k1, k2, ( cx( i ), i = k1, k2 )
                else if ((k1+2-n) .eq. 1) then
                    write( lout, 9937 )k1, k2, ( cx( i ), i = k1, k2 )
                else if ((k1+2-n) .eq. 2) then
                    write( lout, 9936 )k1, k2, ( cx( i ), i = k1, k2 )
                endif
90          continue
        else
            do 100 k1 = 1, n, 2
                k2 = min( n, k1+1 )
                if ((k1+2) .le. n) then
                    write( lout, 9928 )k1, k2, ( cx( i ), i = k1, k2 )
                else if ((k1+2-n) .eq. 1) then
                    write( lout, 9927 )k1, k2, ( cx( i ), i = k1, k2 )
                endif
100          continue
        endif
    endif
    write( lout, 9994 )
    goto 1005
!
!=======================================================================
!                   FORMAT FOR 72 COLUMNS
!=======================================================================
!
!                 DISPLAY 4 SIGNIFICANT DIGITS
!
    9998 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,2('(',d10.3,',',d10.3,')  ') )
    9997 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,1('(',d10.3,',',d10.3,')  ') )
!
!                 DISPLAY 6 SIGNIFICANT DIGITS
!
    9988 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,2('(',d12.5,',',d12.5,')  ') )
    9987 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,1('(',d12.5,',',d12.5,')  ') )
!
!                 DISPLAY 8 SIGNIFICANT DIGITS
!
    9978 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,2('(',d14.7,',',d14.7,')  ') )
    9977 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,1('(',d14.7,',',d14.7,')  ') )
!
!                 DISPLAY 13 SIGNIFICANT DIGITS
!
    9968 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,1('(',d20.13,',',d20.13,')  ') )
!
!=====================================================================
!                   FORMAT FOR 132 COLUMNS
!=====================================================================
!
!                 DISPLAY 4 SIGNIFICANT DIGITS
!
    9958 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,4('(',d10.3,',',d10.3,')  ') )
    9957 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,3('(',d10.3,',',d10.3,')  ') )
    9956 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,2('(',d10.3,',',d10.3,')  ') )
    9955 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,1('(',d10.3,',',d10.3,')  ') )
!
!                 DISPLAY 6 SIGNIFICANT DIGITS
!
    9948 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,3('(',d12.5,',',d12.5,')  ') )
    9947 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,2('(',d12.5,',',d12.5,')  ') )
    9946 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,1('(',d12.5,',',d12.5,')  ') )
!
!                 DISPLAY 8 SIGNIFICANT DIGITS
!
    9938 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,3('(',d14.7,',',d14.7,')  ') )
    9937 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,2('(',d14.7,',',d14.7,')  ') )
    9936 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,1('(',d14.7,',',d14.7,')  ') )
!
!                 DISPLAY 13 SIGNIFICANT DIGITS
!
    9928 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,2('(',d20.13,',',d20.13,')  ') )
    9927 format( 1x, i4, ' - ', i4, ':', 1x,&
     &        1p,1('(',d20.13,',',d20.13,')  ') )
!
!
    9994 format( 1x, ' ' )
1005  continue
end subroutine
