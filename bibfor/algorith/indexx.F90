subroutine indexx(n, arr, indx)
!
!----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!----------------------------------------------------------------------
! ****************** DECLARATION DES VARIABLES ************************
!
    implicit none
!
    integer :: n, m, nstack
    parameter ( m = 7, nstack = 50)
!
    integer :: i, j, indx(n), ir, l, jstack, istack(nstack), k, itemp
    integer :: indxt
    real(kind=8) :: arr(n), a
!
! ******************* DEBUT DU CODE EXECUTABLE ************************
!
!
    do 11 j = 1, n
        indx(j) = j
11  end do
!
    jstack = 0
!
    l = 1
    ir = n
 1  continue
    if (ir-l .lt. m) then
        do 13 j = l+1, ir
            indxt = indx(j)
            a = arr(indxt)
            do 12 i = j-1, 1, -1
                if (arr(indx(i)) .le. a) goto 2
                indx(i+1) = indx(i)
12          continue
            i = 0
 2          continue
            indx(i+1) = indxt
13      continue
        if (jstack .eq. 0) goto 9999
        ir = istack(jstack)
        l = istack(jstack-1)
        jstack = jstack - 2
    else
        k = (l+ir)/2
        itemp = indx(k)
        indx(k) = indx(l+1)
        indx(l+1) = itemp
        if (arr(indx(l+1)) .gt. arr(indx(ir))) then
            itemp = indx(l+1)
            indx(l+1) = indx(ir)
            indx(ir) = itemp
        endif
        if (arr(indx(l)) .gt. arr(indx(ir))) then
            itemp = indx(l)
            indx(l) = indx(ir)
            indx(ir) = itemp
        endif
        if (arr(indx(l+1)) .gt. arr(indx(l))) then
            itemp = indx(l+1)
            indx(l+1) = indx(l)
            indx(l) = itemp
        endif
        i = l+1
        j = ir
        indxt = indx(l)
        a = arr(indxt)
 3      continue
        i = i+ 1
        if (arr(indx(i)) .lt. a) goto 3
 4      continue
        j = j-1
        if (arr(indx(j)) .gt. a) goto 4
        if (j .lt. i) goto 5
        itemp = indx(i)
        indx(i) = indx(j)
        indx(j) = itemp
        goto 3
 5      continue
        indx(l) = indx(j)
        indx(j) = indxt
        jstack = jstack + 2
        if (ir-i+1 .ge. j-l) then
            istack(jstack) = ir
            istack(jstack-1) = i
            ir = j-1
        else
            istack(jstack) = j-1
            istack(jstack-1) = l
        endif
    endif
    goto 1
9999  continue
end subroutine
