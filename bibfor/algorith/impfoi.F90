subroutine impfoi(unit, length, vali, string)
!
implicit none
!
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
    integer, intent(in) :: vali
    character(len=*), intent(out) :: string
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Create string to print integer in logical unit
!
! --------------------------------------------------------------------------------------------------
!
! In  unit             : logical unit
! In  length           : length of integer
! In  vali             : integer to print
! Out string           : string created
!
! --------------------------------------------------------------------------------------------------
!
    character(len=4) :: for4
    character(len=5) :: for5
    character(len=1) :: for1
    integer :: form_length
!
! --------------------------------------------------------------------------------------------------
!
    if (length .le. 9) then
        form_length = 4
        for4(1:2) = '(I'
        write(for1,'(I1)') length
        for4(3:3) = for1
        for4(4:4) = ')'
    else if (length.le.19) then
        form_length = 5
        for5(1:2) = '(I'
        for5(3:3) = '1'
        write(for1,'(I1)') length-10
        for5(4:4) = for1
        for5(5:5) = ')'
    else
        ASSERT(.false.)
    endif
!
    if (unit .ne. 0) then
        if (form_length .eq. 4) then
            write(unit,for4) vali
        else if (form_length.eq.5) then
            write(unit,for5) vali
        else
            ASSERT(.false.)
        endif
    else
        if (form_length .eq. 4) then
            write(string,for4) vali
        else if (form_length.eq.5) then
            write(string,for5) vali
        else
            ASSERT(.false.)
        endif
    endif
!
end subroutine
