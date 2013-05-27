subroutine zmout(lout, m, n, a, lda,&
                 idigit, ifmt)
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
!     SUBROUTINE ARPACK ECRIVANT DES MATRICES COMPLEXES.
!-----------------------------------------------------------------------
!
!  ROUTINE:    ZMOUT
!
!  PURPOSE:    COMPLEX*16 MATRIX OUTPUT ROUTINE.
!
!  USAGE:      CALL ZMOUT (LOUT, M, N, A, LDA, IDIGIT, IFMT)
!
!  ARGUMENTS
!     M      - NUMBER OF ROWS OF A.  (INPUT)
!     N      - NUMBER OF COLUMNS OF A.  (INPUT)
!     A      - COMPLEX*16 M BY N MATRIX TO BE PRINTED.  (INPUT)
!     LDA    - LEADING DIMENSION OF A EXACTLY AS SPECIFIED IN THE
!              DIMENSION STATEMENT OF THE CALLING PROGRAM.  (INPUT)
!     IFMT   - FORMAT TO BE USED IN PRINTING MATRIX A.  (INPUT)
!     IDIGIT - PRINT UP TO IABS(IDIGIT) DECIMAL DIGITS PER NUMBER.  (IN)
!              IF IDIGIT .LT. 0, PRINTING IS DONE WITH 72 COLUMNS.
!              IF IDIGIT .GT. 0, PRINTING IS DONE WITH 132 COLUMNS.
!
!\SCCS INFORMATION: @(#)
! FILE: ZMOUT.F   SID: 2.1   DATE OF SID: 11/16/95   RELEASE: 2
!
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     ... SPECIFICATIONS FOR ARGUMENTS
    integer :: m, n, idigit, lda, lout
    complex(kind=8) :: a( lda, * )
    character(len=*) :: ifmt
!
!     ... SPECIFICATIONS FOR LOCAL VARIABLES
    integer :: i, j, ndigit, k1, k2, lll
    character(len=1) :: icol( 3 )
    character(len=80) :: line
!     ...
!
    data               icol( 1 ), icol( 2 ), icol( 3 ) / 'C', 'O',&
     &                   'L' /
!     ...
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
    write( lout, 9999 )ifmt, line( 1: lll )
    9999 format( / 1x, a / 1x, a )
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
            do 40 k1 = 1, n, 2
                k2 = min( n, k1+1 )
                write( lout, 9998 )( icol, i, i = k1, k2 )
                do 30 i = 1, m
                    if (k1 .ne. n) then
                        write( lout, 9994 )i, ( a( i, j ), j = k1, k2&
                        )
                    else
                        write( lout, 9984 )i, ( a( i, j ), j = k1, k2&
                        )
                    endif
30              continue
40          continue
!
        else if (ndigit.le.6) then
            do 60 k1 = 1, n, 2
                k2 = min( n, k1+1 )
                write( lout, 9997 )( icol, i, i = k1, k2 )
                do 50 i = 1, m
                    if (k1 .ne. n) then
                        write( lout, 9993 )i, ( a( i, j ), j = k1, k2&
                        )
                    else
                        write( lout, 9983 )i, ( a( i, j ), j = k1, k2&
                        )
                    endif
50              continue
60          continue
!
        else if (ndigit.le.8) then
            do 80 k1 = 1, n, 2
                k2 = min( n, k1+1 )
                write( lout, 9996 )( icol, i, i = k1, k2 )
                do 70 i = 1, m
                    if (k1 .ne. n) then
                        write( lout, 9992 )i, ( a( i, j ), j = k1, k2&
                        )
                    else
                        write( lout, 9982 )i, ( a( i, j ), j = k1, k2&
                        )
                    endif
70              continue
80          continue
!
        else
            do 100 k1 = 1, n
                write( lout, 9995 ) icol, k1
                do 90 i = 1, m
                    write( lout, 9991 )i, a( i, k1 )
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
            do 120 k1 = 1, n, 4
                k2 = min( n, k1+3 )
                write( lout, 9998 )( icol, i, i = k1, k2 )
                do 110 i = 1, m
                    if ((k1+3) .le. n) then
                        write( lout, 9974 )i, ( a( i, j ), j = k1, k2&
                        )
                    else if ((k1+3-n).eq.1) then
                        write( lout, 9964 )i, ( a( i, j ), j = k1, k2&
                        )
                    else if ((k1+3-n).eq.2) then
                        write( lout, 9954 )i, ( a( i, j ), j = k1, k2&
                        )
                    else if ((k1+3-n).eq.3) then
                        write( lout, 9944 )i, ( a( i, j ), j = k1, k2&
                        )
                    endif
110              continue
120          continue
!
        else if (ndigit.le.6) then
            do 140 k1 = 1, n, 3
                k2 = min( n, k1+ 2)
                write( lout, 9997 )( icol, i, i = k1, k2 )
                do 130 i = 1, m
                    if ((k1+2) .le. n) then
                        write( lout, 9973 )i, ( a( i, j ), j = k1, k2&
                        )
                    else if ((k1+2-n).eq.1) then
                        write( lout, 9963 )i, ( a( i, j ), j = k1, k2&
                        )
                    else if ((k1+2-n).eq.2) then
                        write( lout, 9953 )i, ( a( i, j ), j = k1, k2&
                        )
                    endif
130              continue
140          continue
!
        else if (ndigit.le.8) then
            do 160 k1 = 1, n, 3
                k2 = min( n, k1+2 )
                write( lout, 9996 )( icol, i, i = k1, k2 )
                do 150 i = 1, m
                    if ((k1+2) .le. n) then
                        write( lout, 9972 )i, ( a( i, j ), j = k1, k2&
                        )
                    else if ((k1+2-n).eq.1) then
                        write( lout, 9962 )i, ( a( i, j ), j = k1, k2&
                        )
                    else if ((k1+2-n).eq.2) then
                        write( lout, 9952 )i, ( a( i, j ), j = k1, k2&
                        )
                    endif
150              continue
160          continue
!
        else
            do 180 k1 = 1, n, 2
                k2 = min( n, k1+1 )
                write( lout, 9995 )( icol, i, i = k1, k2 )
                do 170 i = 1, m
                    if ((k1+1) .le. n) then
                        write( lout, 9971 )i, ( a( i, j ), j = k1, k2&
                        )
                    else
                        write( lout, 9961 )i, ( a( i, j ), j = k1, k2&
                        )
                    endif
170              continue
180          continue
        endif
    endif
    write( lout, 9990 )
!
    9998 format( 11x, 4( 9x, 3a1, i4, 9x ) )
    9997 format( 10x, 4( 11x, 3a1, i4, 11x ) )
    9996 format( 10x, 3( 13x, 3a1, i4, 13x ) )
    9995 format( 12x, 2( 18x, 3a1, i4, 18x ) )
!
!========================================================
!              FORMAT FOR 72 COLUMN
!========================================================
!
!            DISPLAY 4 SIGNIFICANT DIGITS
!
    9994 format( 1x, ' ROW', i4, ':', 1x, 1p,2('(',d10.3,',',d10.3,')  ') )
    9984 format( 1x, ' ROW', i4, ':', 1x, 1p,1('(',d10.3,',',d10.3,')  ') )
!
!            DISPLAY 6 SIGNIFICANT DIGITS
!
    9993 format( 1x, ' ROW', i4, ':', 1x, 1p,2('(',d12.5,',',d12.5,')  ') )
    9983 format( 1x, ' ROW', i4, ':', 1x, 1p,1('(',d12.5,',',d12.5,')  ') )
!
!            DISPLAY 8 SIGNIFICANT DIGITS
!
    9992 format( 1x, ' ROW', i4, ':', 1x, 1p,2('(',d14.7,',',d14.7,')  ') )
    9982 format( 1x, ' ROW', i4, ':', 1x, 1p,1('(',d14.7,',',d14.7,')  ') )
!
!            DISPLAY 13 SIGNIFICANT DIGITS
!
    9991 format( 1x, ' ROW', i4, ':', 1x, 1p,1('(',d20.13,',',d20.13,')') )
    9990 format( 1x, ' ' )
!
!
!========================================================
!              FORMAT FOR 132 COLUMN
!========================================================
!
!            DISPLAY 4 SIGNIFICANT DIGIT
!
    9974 format( 1x, ' ROW', i4, ':', 1x, 1p,4('(',d10.3,',',d10.3,')  ') )
    9964 format( 1x, ' ROW', i4, ':', 1x, 1p,3('(',d10.3,',',d10.3,')  ') )
    9954 format( 1x, ' ROW', i4, ':', 1x, 1p,2('(',d10.3,',',d10.3,')  ') )
    9944 format( 1x, ' ROW', i4, ':', 1x, 1p,1('(',d10.3,',',d10.3,')  ') )
!
!            DISPLAY 6 SIGNIFICANT DIGIT
!
    9973 format( 1x, ' ROW', i4, ':', 1x, 1p,3('(',d12.5,',',d12.5,')  ') )
    9963 format( 1x, ' ROW', i4, ':', 1x, 1p,2('(',d12.5,',',d12.5,')  ') )
    9953 format( 1x, ' ROW', i4, ':', 1x, 1p,1('(',d12.5,',',d12.5,')  ') )
!
!            DISPLAY 8 SIGNIFICANT DIGIT
!
    9972 format( 1x, ' ROW', i4, ':', 1x, 1p,3('(',d14.7,',',d14.7,')  ') )
    9962 format( 1x, ' ROW', i4, ':', 1x, 1p,2('(',d14.7,',',d14.7,')  ') )
    9952 format( 1x, ' ROW', i4, ':', 1x, 1p,1('(',d14.7,',',d14.7,')  ') )
!
!            DISPLAY 13 SIGNIFICANT DIGIT
!
    9971 format( 1x, ' ROW', i4, ':', 1x, 1p,2('(',d20.13,',',d20.13,&
     &        ')  '))
    9961 format( 1x, ' ROW', i4, ':', 1x, 1p,1('(',d20.13,',',d20.13,&
     &        ')  '))
!
!
!
1000  continue
end subroutine
