subroutine numek8(tglok8, tlock8, nbgk8, nblk8, tind)
    implicit none
!
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
!
    character(len=8) :: tlock8(*), tglok8(*)
    integer :: tind(*), nblk8, nbgk8
!
!***********************************************************************
!
!     TIND(I) <-- INDICE DANS LE TABLEAU TGLOK8 DE L' ELEMEMT
!                 NUMERO I DE TLOCK8
!                 (NBLK8 : DIMENSION DE TLOCK8)
!                 (NBGK8 : DIMENSION DE TGLOK8)
!
!***********************************************************************
!
    character(len=8) :: nlk8
    logical(kind=1) :: trouve
    integer :: i, j
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    trouve = .false.
!
    i = 0
    j = 0
!
    do 10, i =1, nblk8, 1
!
    tind (i) = 0
!
    10 end do
!
    do 100, i = 1, nblk8, 1
!
    nlk8 = tlock8(i)
!
    j = 0
!
    trouve = .false.
!
110  continue
    if ((.not. trouve) .and. (j .lt. nbgk8)) then
!
        j = j + 1
!
        if (nlk8 .eq. tglok8(j)) then
!
            trouve = .true.
!
        endif
!
        goto 110
!
    endif
!
    if (trouve) then
!
        tind(i) = j
!
    endif
!
    100 end do
!
end subroutine
