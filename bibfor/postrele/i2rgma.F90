subroutine i2rgma(epsi, sor, sex, ror, rex,&
                  m1, m2, for, fex, tsor,&
                  tsex, tror, trex, tfor, tfex,&
                  tm1, tm2, adr)
    implicit none
#include "asterf_types.h"
!
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
!
    integer :: adr, for, fex, tfor(*), tfex(*), tm1(*), tm2(*)
    real(kind=8) :: sor, sex, ror, rex, epsi
    real(kind=8) :: tsor(*), tsex(*), tror(*), trex(*)
!
    integer :: i, j
    aster_logical :: trouve, dejala
    real(kind=8) :: s
!
!-----------------------------------------------------------------------
    integer :: m1, m2
!-----------------------------------------------------------------------
    trouve = .false.
    dejala = .false.
    s = -1.0d0
    i = 1
    j = 0
!
 10 continue
    if ((.not. trouve) .and. (i .lt. adr)) then
!
        s = tsor(i)
!
        if (abs(s-sor) .lt. epsi) then
!
            trouve = .true.
            dejala = .true.
!
        else if (s .lt. sor) then
!
            i = i + 1
!
        else
!
            trouve = .true.
!
        endif
!
        goto 10
!
    endif
!
    if (dejala) then
!
        tm2(i) = m1
!
    else
!
        if (trouve) then
!
            do 20 j = adr, i+1, -1
!
                tsor(j) = tsor(j-1)
                tsex(j) = tsex(j-1)
                tror(j) = tror(j-1)
                trex(j) = trex(j-1)
                tfor(j) = tfor(j-1)
                tfex(j) = tfex(j-1)
                tm1 (j) = tm1 (j-1)
                tm2 (j) = tm2 (j-1)
!
 20         continue
!
            tsor(i) = sor
            tsex(i) = sex
            tror(i) = ror
            trex(i) = rex
            tfor(i) = for
            tfex(i) = fex
            tm1 (i) = m1
            tm2 (i) = m2
!
        else
!
            tsor(adr) = sor
            tsex(adr) = sex
            tror(adr) = ror
            trex(adr) = rex
            tfor(adr) = for
            tfex(adr) = fex
            tm1 (adr) = m1
            tm2 (adr) = m2
!
        endif
!
        adr = adr + 1
!
    endif
!
end subroutine
