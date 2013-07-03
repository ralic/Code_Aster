subroutine canor2(coor, a, b)
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
    implicit none
#include "asterfort/u2mesg.h"
    real(kind=8) :: coor(3, *), a, b, x1, x2, y1, y2, x12, y12, norme
    real(kind=8) :: valr(5)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    x1=coor(1,1)
    x2=coor(1,2)
    y1=coor(2,1)
    y2=coor(2,2)
    x12=x2-x1
    y12=y2-y1
    a=y12
    b=-x12
    norme  =sqrt(a*a+b*b)
    if (norme .gt. 0) then
        a=a/norme
        b=b/norme
    else
        valr (1) = x1
        valr (2) = y1
        valr (3) = x2
        valr (4) = y2
        valr (5) = norme
        call u2mesg('F', 'MODELISA8_52', 0, ' ', 0,&
                    0, 5, valr)
    endif
end subroutine
