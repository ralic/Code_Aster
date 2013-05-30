function phase(h)
    implicit none
!     ------------------------------------------------------------------
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
!     RENVOIT LA PHASE DE H COMPRISE ENTRE 0. ET 2PI
!     ------------------------------------------------------------------
    include 'asterc/r8pi.h'
    complex(kind=8) :: h
    real(kind=8) :: x, y, phase, pi
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    pi=r8pi()
    x=dble(h)
    y=dimag(h)
    phase=0.d0
    if ((x.gt.0.d0) .and. (y.ge.0.d0)) phase=atan2(y,x)
    if ((x.lt.0.d0) .and. (y.ge.0.d0)) phase=atan2(y,x)+pi
    if ((x.lt.0.d0) .and. (y.le.0.d0)) phase=atan2(y,x)+pi
    if ((x.gt.0.d0) .and. (y.le.0.d0)) phase=atan2(y,x)+2.d0*pi
    if ((x.eq.0.d0) .and. (y.lt.0.d0)) phase=3.d0*pi/2.d0
    if ((x.eq.0.d0) .and. (y.gt.0.d0)) phase=pi/2.d0
    if ((x.eq.0.d0) .and. (y.eq.0.d0)) phase=0.d0
end function
