subroutine i2rdl2(n, t, k, nom, adr)
    implicit none
    integer :: n, t(*), adr
    character(len=8) :: k, nom(*)
!
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
!       COPIE DE I2RDLI
!
    integer :: i, j
    logical(kind=1) :: fini, trouve
!
    i = 1
    j = 0
    trouve = .false.
    fini = .false.
!
10  continue
    if ((.not. fini) .and. (i .lt. adr)) then
!
        if (t(i) .lt. n) then
!
            i = i + 1
!
        else if (t(i) .eq. n) then
!
            trouve = .true.
            fini = .true.
!
        else
!
            fini = .true.
!
        endif
!
        goto 10
!
    endif
!
    if (.not. trouve) then
!
        do 20, j = adr-1, i, -1
!
        t(j+1) = t(j)
        nom(j+1) = nom(j)
!
20      continue
!
        t(i) = n
        nom(i) = k
        adr = adr + 1
!
    endif
!
end subroutine
