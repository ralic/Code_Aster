function blimax(n, dx, incx)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! REMPLACE LA FONCTION INTMAX SUR LES MACHINES OU ELLE N'EST PAS
! DISPONIBLE DANS LES LIBRAIRIES SYSTEME
!
!
    implicit none
    integer :: blimax
    integer :: n, dx(1), incx
    integer :: i, ix, imax, max
!
    blimax = 0
    if (n .le. 0) goto 9999
    if (incx .ne. 1) then
!
!        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!          NOT EQUAL TO 1
!
        ix = 1
        if(incx.lt.0)ix = (-n+1)*incx + 1
        imax = ix
        max = dx(ix)
        do 10 i = 1, n
            if (dx(ix) .gt. max) then
                imax = ix
                max = dx(ix)
            endif
            ix = ix + incx
10      continue
        blimax = imax
    else
!
!        CODE FOR INCREMENT EQUAL TO 1
!
        imax = 1
        max = dx(1)
        do 20 i = 1, n
            if (dx(i) .gt. max) then
                imax = i
                max = dx(i)
            endif
20      continue
        blimax = imax
    endif
9999  continue
end function
