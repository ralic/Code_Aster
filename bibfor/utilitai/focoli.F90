subroutine focoli(ipt, coli, interp, x, y,&
                  rvar, resu, ier)
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/utmess.h"
    integer :: ipt, ier
    real(kind=8) :: x(*), y(*), rvar, resu
    character(len=1) :: coli
    character(len=24) :: interp
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    real(kind=8) :: epsi, tole
    real(kind=8) :: valr(3)
! ----------------------------------------------------------------------
#define linlin(x0,x1,y1,x2,y2) y1+(x0-x1)*(y2-y1)/(x2-x1)
#define linlog(x0,x1,y1,x2,y2) exp(log(y1)+(x0-x1)*(log(y2)-log(y1)) \
    /(x2-x1))
#define loglog(x0,x1,y1,x2,y2) exp(log(y1)+(log(x0)-log(x1))*(log(y2) \
    -log(y1))/(log(x2)-log(x1)))
#define loglin(x0,x1,y1,x2,y2) y1+(log(x0)-log(x1))*(y2-y1) \
    /(log(x2)-log(x1))
! ----------------------------------------------------------------------
!
!     --- PAS D'INTERPOLATION ---
!
    ier = 0
    if (coli .eq. 'C') then
        resu = y(ipt)
!
!     --- INTERPOLATION ---
!
    else if (coli.eq.'I') then
        if (interp .eq. 'LIN LIN ') then
            resu = linlin(rvar,x(ipt),y(ipt),x(ipt+1),y(ipt+1))
!
        else if (interp.eq.'LIN LOG ') then
            resu = linlog(rvar,x(ipt),y(ipt),x(ipt+1),y(ipt+1))
!
        else if (interp.eq.'LOG LOG ') then
            resu = loglog(rvar,x(ipt),y(ipt),x(ipt+1),y(ipt+1))
!
        else if (interp.eq.'LOG LIN ') then
            resu = loglin(rvar,x(ipt),y(ipt),x(ipt+1),y(ipt+1))
!
        else if (interp(1:3).eq.'NON') then
            epsi = sqrt ( r8prem() )
            tole = epsi * abs( x(ipt) - x(ipt+1) )
            if (abs(x(ipt)-rvar) .le. tole) then
                resu = y(ipt)
            else if (abs(x(ipt+1)-rvar) .le. tole) then
                resu = y(ipt+1)
            else
                ier = 200
                valr (1) = rvar
                valr (2) = x(ipt)
                valr (3) = x(ipt+1)
                call utmess('A', 'UTILITAI6_14', nr=3, valr=valr)
            endif
!
        else
            ier = 230
            call utmess('A', 'UTILITAI_84', sk=interp)
        endif
!
!     --- EXTRAPOLATION ---
!
    else if (coli.eq.'E') then
        resu = linlin(rvar,x(ipt),y(ipt),x(ipt+1),y(ipt+1))
!
    else
        ier = 240
        call utmess('A', 'UTILITAI_85', sk=coli)
    endif
!
end subroutine
