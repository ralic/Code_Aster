subroutine usubis(type, para, crit, epsi, x1,&
                  x2, x, iret)
    implicit none
!-----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
#include "asterfort/usufon.h"
    real(kind=8) :: para(*)
    character(len=*) :: type, crit
!-----------------------------------------------------------------------
    integer :: i, iret, maxit
    real(kind=8) :: df, df1, df2, epsi, f, f1, f2
    real(kind=8) :: resu, x, x1, x2, xd, xg
!-----------------------------------------------------------------------
    parameter     ( maxit = 100 )
!     ------------------------------------------------------------------
!
    iret = 0
!
    resu = para(5)
    call usufon(type, para, x1, f1, df1)
    call usufon(type, para, x2, f2, df2)
    if (crit(1:4) .eq. 'RELA') then
        if (abs(f1-resu) .le. epsi * abs(resu)) goto 9999
    else
        if (abs(f1 - resu) .le. epsi) goto 9999
    endif
    if (crit(1:4) .eq. 'RELA') then
        if (abs(f2-resu) .le. epsi * abs(resu)) goto 9999
    else
        if (abs(f2 - resu) .le. epsi) goto 9999
    endif
    xg = x1
    xd = x2
    do 10 i = 1, maxit
        x = ( xg + xd ) * 0.5d0
        call usufon(type, para, x, f, df)
        if (crit(1:4) .eq. 'RELA') then
            if (abs(f-resu) .le. epsi * abs(resu)) goto 9999
        else
            if (abs(f-resu) .le. epsi) goto 9999
        endif
        if (f .lt. resu) then
            xg = x
            xd = xd
        else
            xg = xg
            xd = x
        endif
10  end do
    iret = 10
    goto 9999
!
9999  continue
end subroutine
