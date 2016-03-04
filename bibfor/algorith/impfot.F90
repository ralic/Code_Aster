subroutine impfot(time, string)
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
    real(kind=8), intent(in) :: time
    character(len=24), intent(out) :: string
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Convert time in string
!
! --------------------------------------------------------------------------------------------------
!
! In  time             : time to convert
! Out string           : string created
!
! --------------------------------------------------------------------------------------------------
!
    integer :: minut, heure, second
!
! --------------------------------------------------------------------------------------------------
!
    if (time .lt. 60.0d0) then
        write(string,10) time
    else
        if (time .le. 3600.d0) then
            minut = int(time/60)
            second = int(time - (minut*60))
            write(string,20) minut,second
        else
            heure = int(time/3600)
            minut = int((time - (heure*3600))/60)
            second = int(time - (heure*3600) - (minut*60))
            write(string,30) heure,minut,second
        endif
    endif
!
10  format (16x               ,f6.3,' s')
20  format (13x      ,i2,' min ',i2,' s')
30  format (i10,' h ',i2,' min ',i2,' s')
!
end subroutine
