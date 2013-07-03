subroutine lunule(r1, r2, angdeb, angfin, angmax,&
                  ansini, ansfin, profon, volume, epais)
! aslint: disable=
    implicit   none
#include "asterc/r8rddg.h"
#include "asterfort/usubis.h"
    real(kind=8) :: r1, r2, angdeb, angfin, angmax, ansini, ansfin, profon
    real(kind=8) :: volume, epais
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
    integer :: iret
    real(kind=8) :: deno1, deg, para(7), epsi, x1, x2, resu
    character(len=4) :: crit
    character(len=24) :: type
!-----------------------------------------------------------------------
!
    deg = r8rddg( )
    crit = 'RELA'
    epsi = 1.d-06
!
    angmax = ( ansini + ansfin ) / 2.d0
!
    x1 = 0.d0
    x2 = r2
    para(1) = r1
    para(2) = r2
    para(3) = epais
    para(4) = 0.d0
    para(5) = 2*volume
    para(6) = 2*volume
    type(1:12) = 'TUBE_ALESAGE'
    call usubis(type, para, crit, epsi, x1,&
                x2, resu, iret)
    deno1 = ( r2 - r1 + resu )
    x1 = r1*r1 - ( ( r2*r2 - r1*r1 - deno1*deno1 )**2 / ( 4*deno1*deno1) )
    x1 = sqrt ( x1 )
    x2 = asin ( x1 / r1 ) * deg
!
    profon = resu
    angdeb = angmax - x2
    angfin = angmax + x2
!
end subroutine
