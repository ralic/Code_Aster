subroutine vetube(r1, r2, angdeb, angfin, angare,&
                  angmax, angva, profon, volume, epais)
    implicit   none
    include 'asterc/r8dgrd.h'
    include 'asterc/r8pi.h'
    include 'asterc/r8rddg.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: r1, r2, angdeb, angfin, angare, angmax, angva, profon
    real(kind=8) :: volume, epais
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_6
!-----------------------------------------------------------------------
    real(kind=8) :: delta, tau, deno1, xval, xnume1, xnume2, deltag
    real(kind=8) :: pi, rad, deg
!-----------------------------------------------------------------------
!
    tau = 0.8d0
    pi = r8pi( )
    rad = r8dgrd( )
    deg = r8rddg( )
!
    angmax = angare
    profon = volume*(1.d0+tau)/epais*angva/7.d-4*rad
!
!     ATTENTION PB EQUATION DANS  DELTA  VOIR  NOTE  HP52/99-01
!     ---------------------------------------------------------
!
    xnume1 = profon * ( 2*r2 + profon )
    deno1 = 2*( r2 - r1 + profon )
    xval = 1.d0 - ( (1.d0/r1)*(xnume1/deno1) )
    xnume2 = 1.d0 - xval
    if (xnume2 .eq. 0.d0) then
        delta = 0.d0
    else
        if ((xval.gt.0.d0) .and. (xnume2.gt.0.d0)) then
            deltag = atan(xnume2/xval)
            delta = deltag*deg
        endif
        if ((xval.lt.0.d0) .and. (xnume2.gt.0.d0)) then
            deltag = pi - abs(atan(xnume2/xval))
            delta = deltag*deg
        endif
        if ((xval.lt.0.d0) .and. (xnume2.lt.0.d0)) then
            deltag = pi + abs(atan(xnume2/xval))
            delta = deltag*deg
        endif
        if ((xval.gt.0.d0) .and. (xnume2.lt.0.d0)) then
            deltag = 2*pi - abs(atan(xnume2/xval))
            delta = deltag*deg
        endif
    endif
!
    if (angmax .lt. 90.d0) then
        angfin = angmax + delta
        angdeb = angmax - delta*(1.d0-tau)/(1.d0+tau)
    endif
!
    if (angmax .gt. 270.d0) then
        angdeb=angmax-delta
        angfin = angmax + delta*(1.d0-tau)/(1.d0+tau)
    endif
!
    if ((angmax.gt.90.d0) .and. (angmax.lt.180.d0)) then
        angdeb = angmax - delta
        angfin = angmax + delta*(1.d0-tau)/(1.d0+tau)
    endif
!
    if ((angmax.gt.180.d0) .and. (angmax.lt.270.d0)) then
        angfin = angmax + delta
        angdeb = angmax - delta*(1.d0-tau)/(1.d0+tau)
    endif
!
    if (2*delta .gt. 360.d0) then
        call u2mess('F', 'PREPOST4_93')
    endif
!
end subroutine
