subroutine impfor(unit, length, prec, valr, string)
!
implicit none
!
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    integer, intent(in) :: unit
    integer, intent(in) :: length
    integer, intent(in) :: prec
    real(kind=8), intent(in) :: valr
    character(len=*), intent(out) :: string
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Create string to print real in logical unit
!
! --------------------------------------------------------------------------------------------------
!
! In  unit             : logical unit
! In  length           : length of real
! In  prec             : precision of real
! In  valr             : real to print
! Out string           : string created
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: for8
    character(len=9) :: for9
    character(len=1) :: for1
    integer :: form_length, prec_local
!
! --------------------------------------------------------------------------------------------------
!
    prec_local = prec
    if ((prec .lt. 1) .or. (prec .gt. 9)) then
        prec_local = 5
    endif
!
    if (valr .eq. r8vide()) then
        if (unit .ne. 0) then
            write(unit,'(A)') ' '
        else
            write(string,'(A)') ' '
        endif
        goto 99
    endif
!
    if (length .le. 9) then
        form_length = 8
        for8(1:4) = '(1PE'
        write(for1,'(I1)') length
        for8(5:5) = for1
        for8(6:6) = '.'
        write(for1,'(I1)') prec_local
        for8(7:7) = for1
        for8(8:8) = ')'
    else if (length .le. 19) then
        form_length = 9
        for9(1:4) = '(1PE'
        for9(5:5) = '1'
        write(for1,'(I1)') length-10
        for9(6:6) = for1
        for9(7:7) = '.'
        write(for1,'(I1)') prec_local
        for9(8:8) = for1
        for9(9:9) = ')'
    else
        ASSERT(.false.)
    endif
!
    if (unit .ne. 0) then
        if (form_length .eq. 8) then
            write(unit,for8) valr
        else if (form_length.eq.9) then
            write(unit,for9) valr
        else
            ASSERT(.false.)
        endif
    else
        if (form_length .eq. 8) then
            write(string,for8) valr
        else if (form_length.eq.9) then
            write(string,for9) valr
        else
            ASSERT(.false.)
        endif
    endif
!
99  continue
!
end subroutine
