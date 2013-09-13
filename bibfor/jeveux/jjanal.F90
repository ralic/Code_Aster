subroutine jjanal(condlu, nval, nvalo, lval, cval)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "asterfort/utmess.h"
    character(len=*) :: condlu, cval(*)
    integer :: nval, nvalo, lval(*)
!
    integer :: long, i, j, nbsc
!
    do 20 i = 1, nval
        cval (i) = ' '
        lval (i) = 0
20  end do
    long = len(condlu)
    if (long .eq. 0 .and. nvalo .gt. 0) then
        call utmess('F', 'JEVEUX1_31')
    endif
    nbsc = 0
    i = 1
!
 1  continue
    if (i .gt. long) then
        if (nbsc .lt. nvalo) then
            call utmess('F', 'JEVEUX1_31')
        else
            goto 100
        endif
    endif
    if (condlu(i:i) .eq. ' ') then
        i = i + 1
        goto 1
    endif
    j = i + 1
 2  continue
    if (j .gt. long) goto 3
    if (condlu(j:j) .ne. ' ') then
        j = j + 1
        goto 2
    endif
!
 3  continue
    nbsc = nbsc + 1
    cval( nbsc ) = condlu(i:j-1)
    lval( nbsc ) = j - i
    if (nbsc .lt. nval .and. j .le. long) then
        i = j + 1
        goto 1
    else if (nbsc .lt. nvalo .and. j.eq. long+1) then
        call utmess('F', 'JEVEUX1_31')
    endif
100  continue
    do 10 i = j, long
        if (condlu(i:i) .ne. ' ') then
            call utmess('F', 'JEVEUX1_32')
        endif
10  end do
!
end subroutine
