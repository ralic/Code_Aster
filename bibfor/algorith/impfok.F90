subroutine impfok(mesg, length, unit)
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
    character(len=*), intent(in) :: mesg
    integer, intent(in) :: length
    integer, intent(in) :: unit
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Create string to print text in logical unit
!
! --------------------------------------------------------------------------------------------------
!
! In  mesg             : message to write
! In  length           : length of message
! In  unit             : logical unit
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: zlig  = 512
    character(len=6) :: forma
!
! --------------------------------------------------------------------------------------------------
!
    if (length .le. 0) then
        forma = '(A)'
    else if (length .gt. zlig) then
        ASSERT(.false.)
    else
        write(forma,10) length
    endif
    if (unit .le. 0) then
        ASSERT(.false.)
    else
        write(unit,forma) mesg(1:length)
    endif
!
10  format ('(A',i3,')')
!
end subroutine
