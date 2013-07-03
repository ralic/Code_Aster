subroutine dsortc(which, apply, n, xreal, ximag,&
                  y)
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
!     SUBROUTINE ARPACK TRIANT DES VECTEURS.
!-----------------------------------------------------------------------
! BEGINDOC
!
! DESCRIPTION:
!  SORTS THE COMPLEX ARRAY IN XREAL AND XIMAG INTO THE ORDER
!  SPECIFIED BY WHICH AND OPTIONALLY APPLIES THE PERMUTATION TO THE
!  REAL ARRAY Y. IT IS ASSUMED THAT IF AN ELEMENT OF XIMAG IS
!  NONZERO, THEN ITS NEGATIVE IS ALSO AN ELEMENT. IN OTHER WORDS,
!  BOTH MEMBERS OF A COMPLEX CONJUGATE PAIR ARE TO BE SORTED AND THE
!  PAIRS ARE KEPT ADJACENT TO EACH OTHER.
!
! ARGUMENTS
!  WHICH   CHARACTER*2.  (INPUT)
!          'LM' -> SORT XREAL,XIMAG INTO INCREASING ORDER OF MAGNITUDE.
!          'SM' -> SORT XREAL,XIMAG INTO DECREASING ORDER OF MAGNITUDE.
!          'LR' -> SORT XREAL INTO INCREASING ORDER OF ALGEBRAIC.
!          'SR' -> SORT XREAL INTO DECREASING ORDER OF ALGEBRAIC.
!          'LI' -> SORT XIMAG INTO INCREASING ORDER OF MAGNITUDE.
!          'SI' -> SORT XIMAG INTO DECREASING ORDER OF MAGNITUDE.
!          NOTE: IF AN ELEMENT OF XIMAG IS NON-ZERO, THEN ITS NEGATIVE
!                IS ALSO AN ELEMENT.
!
!  APPLY   LOGICAL.  (INPUT)
!          APPLY = .TRUE.  -> APPLY THE SORTED ORDER TO ARRAY Y.
!          APPLY = .FALSE. -> DO NOT APPLY THE SORTED ORDER TO ARRAY Y.
!
!  N       INTEGER.  (INPUT)
!          SIZE OF THE ARRAYS.
!
!  XREAL,  REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
!  XIMAG   REAL AND IMAGINARY PART OF THE ARRAY TO BE SORTED.
!
!  Y       REAL*8 ARRAY OF LENGTH N.  (INPUT/OUTPUT)
!
! ENDDOC
!-----------------------------------------------------------------------
! BEGINLIB
!
! AUTHOR
!     DANNY SORENSEN               PHUONG VU
!     RICHARD LEHOUCQ              CRPC / RICE UNIVERSITY
!     DEPT. OF COMPUTATIONAL &     HOUSTON, TEXAS
!     APPLIED MATHEMATICS
!     RICE UNIVERSITY
!     HOUSTON, TEXAS
!
! REVISION HISTORY:
!     XX/XX/92: VERSION ' 2.1'
!               ADAPTED FROM THE SORT ROUTINE IN LANSO.
!
! FILE: SORTC.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2
!
! ASTER INFORMATION
! 07/01/2000 TOILETTAGE DU FORTRAN SUIVANT LES REGLES ASTER,
!            IMPLICIT NONE.
! ENDLIB
!-----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
!     %------------------%
!     | SCALAR ARGUMENTS |
!     %------------------%
!
#include "asterc/matfpe.h"
#include "blas/dlapy2.h"
    character(len=2) :: which
    logical :: apply
    integer :: n
!
!     %-----------------%
!     | ARRAY ARGUMENTS |
!     %-----------------%
!
    real(kind=8) :: xreal(0:n-1), ximag(0:n-1), y(0:n-1)
!
!     %---------------%
!     | LOCAL SCALARS |
!     %---------------%C
    integer :: i, igap, j
    real(kind=8) :: temp, temp1, temp2
!
!     %--------------------%
!     | EXTERNAL FUNCTIONS |
!     %--------------------%
!
!
!     %-----------------------%
!     | EXECUTABLE STATEMENTS |
!     %-----------------------%
!
    call matfpe(-1)
!
    igap = n / 2
!
    if (which .eq. 'LM') then
!
!        %------------------------------------------------------%
!        | SORT XREAL,XIMAG INTO INCREASING ORDER OF MAGNITUDE. |
!        %------------------------------------------------------%
!
10      continue
        if (igap .eq. 0) goto 9000
!
        do 30 i = igap, n-1
            j = i-igap
20          continue
!
            if (j .lt. 0) goto 30
!
            temp1 = dlapy2(xreal(j),ximag(j))
            temp2 = dlapy2(xreal(j+igap),ximag(j+igap))
!
            if (temp1 .gt. temp2) then
                temp = xreal(j)
                xreal(j) = xreal(j+igap)
                xreal(j+igap) = temp
!
                temp = ximag(j)
                ximag(j) = ximag(j+igap)
                ximag(j+igap) = temp
!
                if (apply) then
                    temp = y(j)
                    y(j) = y(j+igap)
                    y(j+igap) = temp
                endif
            else
                goto 30
            endif
            j = j-igap
            goto 20
30      continue
        igap = igap / 2
        goto 10
!
    else if (which .eq. 'SM') then
!
!        %------------------------------------------------------%
!        | SORT XREAL,XIMAG INTO DECREASING ORDER OF MAGNITUDE. |
!        %------------------------------------------------------%
!
40      continue
        if (igap .eq. 0) goto 9000
!
        do 60 i = igap, n-1
            j = i-igap
50          continue
!
            if (j .lt. 0) goto 60
!
            temp1 = dlapy2(xreal(j),ximag(j))
            temp2 = dlapy2(xreal(j+igap),ximag(j+igap))
!
            if (temp1 .lt. temp2) then
                temp = xreal(j)
                xreal(j) = xreal(j+igap)
                xreal(j+igap) = temp
!
                temp = ximag(j)
                ximag(j) = ximag(j+igap)
                ximag(j+igap) = temp
!
                if (apply) then
                    temp = y(j)
                    y(j) = y(j+igap)
                    y(j+igap) = temp
                endif
            else
                goto 60
            endif
            j = j-igap
            goto 50
60      continue
        igap = igap / 2
        goto 40
!
    else if (which .eq. 'LR') then
!
!        %------------------------------------------------%
!        | SORT XREAL INTO INCREASING ORDER OF ALGEBRAIC. |
!        %------------------------------------------------%
!
70      continue
        if (igap .eq. 0) goto 9000
!
        do 90 i = igap, n-1
            j = i-igap
80          continue
!
            if (j .lt. 0) goto 90
!
            if (xreal(j) .gt. xreal(j+igap)) then
                temp = xreal(j)
                xreal(j) = xreal(j+igap)
                xreal(j+igap) = temp
!
                temp = ximag(j)
                ximag(j) = ximag(j+igap)
                ximag(j+igap) = temp
!
                if (apply) then
                    temp = y(j)
                    y(j) = y(j+igap)
                    y(j+igap) = temp
                endif
            else
                goto 90
            endif
            j = j-igap
            goto 80
90      continue
        igap = igap / 2
        goto 70
!
    else if (which .eq. 'SR') then
!
!        %------------------------------------------------%
!        | SORT XREAL INTO DECREASING ORDER OF ALGEBRAIC. |
!        %------------------------------------------------%
!
100      continue
        if (igap .eq. 0) goto 9000
        do 120 i = igap, n-1
            j = i-igap
110          continue
!
            if (j .lt. 0) goto 120
!
            if (xreal(j) .lt. xreal(j+igap)) then
                temp = xreal(j)
                xreal(j) = xreal(j+igap)
                xreal(j+igap) = temp
!
                temp = ximag(j)
                ximag(j) = ximag(j+igap)
                ximag(j+igap) = temp
!
                if (apply) then
                    temp = y(j)
                    y(j) = y(j+igap)
                    y(j+igap) = temp
                endif
            else
                goto 120
            endif
            j = j-igap
            goto 110
120      continue
        igap = igap / 2
        goto 100
!
    else if (which .eq. 'LI') then
!
!        %------------------------------------------------%
!        | SORT XIMAG INTO INCREASING ORDER OF MAGNITUDE. |
!        %------------------------------------------------%
!
130      continue
        if (igap .eq. 0) goto 9000
        do 150 i = igap, n-1
            j = i-igap
140          continue
!
            if (j .lt. 0) goto 150
!
            if (abs(ximag(j)) .gt. abs(ximag(j+igap))) then
                temp = xreal(j)
                xreal(j) = xreal(j+igap)
                xreal(j+igap) = temp
!
                temp = ximag(j)
                ximag(j) = ximag(j+igap)
                ximag(j+igap) = temp
!
                if (apply) then
                    temp = y(j)
                    y(j) = y(j+igap)
                    y(j+igap) = temp
                endif
            else
                goto 150
            endif
            j = j-igap
            goto 140
150      continue
        igap = igap / 2
        goto 130
!
    else if (which .eq. 'SI') then
!
!        %------------------------------------------------%
!        | SORT XIMAG INTO DECREASING ORDER OF MAGNITUDE. |
!        %------------------------------------------------%
!
160      continue
        if (igap .eq. 0) goto 9000
        do 180 i = igap, n-1
            j = i-igap
170          continue
!
            if (j .lt. 0) goto 180
!
            if (abs(ximag(j)) .lt. abs(ximag(j+igap))) then
                temp = xreal(j)
                xreal(j) = xreal(j+igap)
                xreal(j+igap) = temp
!
                temp = ximag(j)
                ximag(j) = ximag(j+igap)
                ximag(j+igap) = temp
!
                if (apply) then
                    temp = y(j)
                    y(j) = y(j+igap)
                    y(j+igap) = temp
                endif
            else
                goto 180
            endif
            j = j-igap
            goto 170
180      continue
        igap = igap / 2
        goto 160
    endif
!
9000  continue
!
    call matfpe(1)
!
!     %---------------%
!     | END OF DSORTC |
!     %---------------%
!
end subroutine
