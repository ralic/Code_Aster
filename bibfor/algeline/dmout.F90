subroutine dmout(lout, m, n, a, lda,&
                 idigit, ifmt)
!----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     SUBROUTINE ARPACK ECRIVANT DES MATRICES.
!-----------------------------------------------------------------------
!  PURPOSE:    REAL MATRIX OUTPUT ROUTINE.
!
!  USAGE:      CALL DMOUT (LOUT, M, N, A, LDA, IDIGIT, IFMT)
!
!  ARGUMENTS
!     M      - NUMBER OF ROWS OF A.  (INPUT)
!     N      - NUMBER OF COLUMNS OF A.  (INPUT)
!     A      - REAL M BY N MATRIX TO BE PRINTED.  (INPUT)
!     LDA    - LEADING DIMENSION OF A EXACTLY AS SPECIFIED IN THE
!              DIMENSION STATEMENT OF THE CALLING PROGRAM.  (INPUT)
!     IFMT   - FORMAT TO BE USED IN PRINTING MATRIX A.  (INPUT)
!     IDIGIT - PRINT UP TO IABS(IDIGIT) DECIMAL DIGITS PER NUMBER.  (IN)
!              IF IDIGIT .LT. 0, PRINTING IS DONE WITH 72 COLUMNS.
!              IF IDIGIT .GT. 0, PRINTING IS DONE WITH 132 COLUMNS.
!  INTRINSIC FUNCTIONS
!     MIN, LEN.
!-----------------------------------------------------------------------
! ASTER INFORMATION
! 14/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER.
!            REMPLACEMENT DE 1 RETURN PAR 1 GOTO 1000.
!            IMPLICIT NONE.
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     .. SCALAR ARGUMENTS ..
    character(len=*) :: ifmt
    integer :: idigit, lda, lout, m, n
!
!     .. ARRAY ARGUMENTS ..
    real(kind=8) :: a( lda, * )
!
!     .. LOCAL SCALARS ..
    character(len=80) :: line
    integer :: i, j, k1, k2, lll, ndigit
!
!     .. LOCAL ARRAYS ..
    character(len=1) :: icol( 3 )
!
!     .. DATA STATEMENTS ..
    data               icol( 1 ), icol( 2 ), icol( 3 ) / 'C', 'O',&
     &                   'L' /
!
!     .. EXECUTABLE STATEMENTS ..
!
!     ... FIRST EXECUTABLE STATEMENT
!
    lll = min( len( ifmt ), 80 )
    do 10 i = 1, lll
        line( i: i ) = '-'
10  end do
!
    do 20 i = lll + 1, 80
        line( i: i ) = ' '
20  end do
!
    write( lout, fmt = 9999 )ifmt, line( 1: lll )
    9999 format( / 1x, a, / 1x, a )
!
    if (m .le. 0 .or. n .le. 0 .or. lda .le. 0) goto 1000
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
            do 40 k1 = 1, n, 5
                k2 = min( n, k1+4 )
                write( lout, fmt = 9998 )( icol, i, i = k1, k2 )
                do 30 i = 1, m
                    write( lout, fmt = 9994 )i, ( a( i, j ), j = k1,&
                    k2 )
30              continue
40          continue
!
        else if (ndigit.le.6) then
            do 60 k1 = 1, n, 4
                k2 = min( n, k1+3 )
                write( lout, fmt = 9997 )( icol, i, i = k1, k2 )
                do 50 i = 1, m
                    write( lout, fmt = 9993 )i, ( a( i, j ), j = k1,&
                    k2 )
50              continue
60          continue
!
        else if (ndigit.le.10) then
            do 80 k1 = 1, n, 3
                k2 = min( n, k1+2 )
                write( lout, fmt = 9996 )( icol, i, i = k1, k2 )
                do 70 i = 1, m
                    write( lout, fmt = 9992 )i, ( a( i, j ), j = k1,&
                    k2 )
70              continue
80          continue
!
        else
            do 100 k1 = 1, n, 2
                k2 = min( n, k1+1 )
                write( lout, fmt = 9995 )( icol, i, i = k1, k2 )
                do 90 i = 1, m
                    write( lout, fmt = 9991 )i, ( a( i, j ), j = k1,&
                    k2 )
90              continue
100          continue
        endif
!
!=======================================================================
!             CODE FOR OUTPUT USING 132 COLUMNS FORMAT
!=======================================================================
!
    else
        if (ndigit .le. 4) then
            do 120 k1 = 1, n, 10
                k2 = min( n, k1+9 )
                write( lout, fmt = 9998 )( icol, i, i = k1, k2 )
                do 110 i = 1, m
                    write( lout, fmt = 9994 )i, ( a( i, j ), j = k1,&
                    k2 )
110              continue
120          continue
!
        else if (ndigit.le.6) then
            do 140 k1 = 1, n, 8
                k2 = min( n, k1+7 )
                write( lout, fmt = 9997 )( icol, i, i = k1, k2 )
                do 130 i = 1, m
                    write( lout, fmt = 9993 )i, ( a( i, j ), j = k1,&
                    k2 )
130              continue
140          continue
!
        else if (ndigit.le.10) then
            do 160 k1 = 1, n, 6
                k2 = min( n, k1+5 )
                write( lout, fmt = 9996 )( icol, i, i = k1, k2 )
                do 150 i = 1, m
                    write( lout, fmt = 9992 )i, ( a( i, j ), j = k1,&
                    k2 )
150              continue
160          continue
!
        else
            do 180 k1 = 1, n, 5
                k2 = min( n, k1+4 )
                write( lout, fmt = 9995 )( icol, i, i = k1, k2 )
                do 170 i = 1, m
                    write( lout, fmt = 9991 )i, ( a( i, j ), j = k1,&
                    k2 )
170              continue
180          continue
        endif
    endif
    write( lout, fmt = 9990 )
!
    9998 format( 10x, 10( 4x, 3a1, i4, 1x ) )
    9997 format( 10x, 8( 5x, 3a1, i4, 2x ) )
    9996 format( 10x, 6( 7x, 3a1, i4, 4x ) )
    9995 format( 10x, 5( 9x, 3a1, i4, 6x ) )
    9994 format( 1x, ' ROW', i4, ':', 1x, 1p, 10d12.3 )
    9993 format( 1x, ' ROW', i4, ':', 1x, 1p, 8d14.5 )
    9992 format( 1x, ' ROW', i4, ':', 1x, 1p, 6d18.9 )
    9991 format( 1x, ' ROW', i4, ':', 1x, 1p, 5d22.13 )
    9990 format( 1x, ' ' )
!
1000  continue
end subroutine
