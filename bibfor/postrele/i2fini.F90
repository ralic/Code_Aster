subroutine i2fini(epsi, binf, bsup, tsor, tsex,&
                  tm2, adrgt, fini)
    implicit none
#include "asterf_types.h"
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
    integer :: adrgt, tm2(*)
    real(kind=8) :: tsor(*), tsex(*), epsi, binf, bsup
    aster_logical :: fini
!
    integer :: i
    aster_logical :: stop
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    fini = .false.
    stop = .false.
    i = 2
!
    if (adrgt .ge. 2) then
!
        if ((abs(tsor(1) - binf) .lt. epsi) .and. (abs(tsex(adrgt-1) - bsup) .lt. epsi)&
            .and. (tm2(1) .ne. -1)) then
!
 10         continue
            if ((.not. stop) .and. (i .lt. adrgt)) then
!
                if ((tm2(i) .ne. -1) .and. (abs(tsor(i)-tsex(i-1)) .lt. epsi)) then
!
                    i = i+1
!
                else
!
                    stop = .true.
!
                endif
!
                goto 10
!
            endif
!
            fini = .not. stop
!
        endif
!
    endif
!
end subroutine
