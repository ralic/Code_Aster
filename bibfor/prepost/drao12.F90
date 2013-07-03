subroutine drao12(coord1, coord2, xo1o2, yo1o2, zo1o2,&
                  do1o2, coorda, ray)
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
! DECLARATION GLOBALE
!
#include "asterfort/assert.h"
    real(kind=8) :: coord1(3)
    real(kind=8) :: coord2(3)
    real(kind=8) :: xo1o2, yo1o2, zo1o2, do1o2
    real(kind=8) :: coorda(3)
    real(kind=8) :: ray(2)
!
! DECLARATION LOCALE
!
    real(kind=8) :: xao1, yao1, zao1, xao2, yao2, zao2
    real(kind=8) :: dao1, dao2, pao1o2, pao2o1, n2, n3, n4
    real(kind=8) :: r, cos1, cos2
!
    xao1 = coorda(1)-coord1(1)
    yao1 = coorda(2)-coord1(2)
    zao1 = coorda(3)-coord1(3)
    xao2 = coorda(1)-coord2(1)
    yao2 = coorda(2)-coord2(2)
    zao2 = coorda(3)-coord2(3)
!
    dao1 = sqrt( xao1**2 + yao1**2 + zao1**2 )
    dao2 = sqrt( xao2**2 + yao2**2 + zao2**2 )
!
    r = sqrt(&
        ( yao1*zo1o2 - zao1*yo1o2 )**2 + ( zao1*xo1o2 - xao1*zo1o2 )**2 + ( xao1*yo1o2 - yao1*xo1&
        &o2 )**2&
        ) / do1o2
!
    ray(1) = r
    ray(2) = 0.0d0
!
    pao1o2 =( xao1*xo1o2 ) +( yao1*yo1o2 ) +( zao1*zo1o2 )
    if (dao1 .eq. 0.d0) then
        cos1 = 1.0d0
    else
        cos1 = pao1o2 /(do1o2*dao1)
    endif
    pao2o1 =( -xao2*xo1o2 ) +( -yao2*yo1o2 ) +( -zao2*zo1o2 )
    if (dao2 .eq. 0.d0) then
        cos2 = 1.0d0
    else
        cos2 = pao2o1 /(do1o2*dao2)
    endif
    n2 =( dao1*cos1 ) / do1o2
    n3 =( dao2*cos2 ) / do1o2
    n4 = n2 + n3
    if (abs(1.d0-n2) .le. 1.0d-10) n2 = 1.0d0
    if (abs(1.d0-n3) .le. 1.0d-10) n3 = 1.0d0
    if (abs(1.d0-n4) .le. 1.0d-10) n4 = 1.0d0
    call assert(n4.eq.1.0d0)
!
    if (n2 .gt. 1.0d0 .or. n3 .gt. 1.0d0) then
        if (n2 .gt. 1.0d0) ray(2) = -1.0d0
        if (n3 .gt. 1.0d0) ray(2) = 1.0d0
    endif
!
end subroutine
