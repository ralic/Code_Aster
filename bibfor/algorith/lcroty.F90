function lcroty(t, prec, itemax)
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
    implicit none
#include "asterfort/utmess.h"
    real(kind=8) :: lcroty
    integer :: itemax
    real(kind=8) :: t, prec
!
! *****************************************************
!       INTEGRATION DE LA LOI DE ROUSSELIER LOCAL     *
!           RESOLUTION Y.EXP(Y) = T                   *
! *****************************************************
!
! IN  T       : SECOND MEMBRE CONSTANT
! IN  PREC    : PRECISION RELATIVE SOUHAITEE
! IN  ITEMAX  : NOMBRE MAX D'ITERATIONS
!
    integer :: iter
    real(kind=8) :: y, u, h, dh
!
! 1 - NEWTON SANS CHANGEMENT DE VARIABLE
!
    if (t .le. exp(1.d0)) then
!
        y = 0.d0
        do 10 iter = 1, itemax
            h = y*exp(y) - t
            dh = (1+y) * exp(y)
            if (abs(h) .le. prec*t) goto 100
            y = y - h/dh
10      continue
!
!
! 2 - NEWTON AVEC CHANGEMENT DE VARIABLE U=EXP(Y)
!
    else
        u = t
        do 20 iter = 1, itemax
            h = u*log(u) - t
            dh = 1 + log(u)
            if (abs(h) .le. prec*t) then
                y = log(u)
                goto 100
            endif
            u = u - h/dh
20      continue
    endif
!
    call utmess('F', 'ALGORITH3_55')
!
!
100  continue
    lcroty = y
end function
