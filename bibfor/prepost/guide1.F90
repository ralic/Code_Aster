subroutine guide1(rcarte, d, thet, rayo)
    implicit none
    include 'asterc/r8dgrd.h'
    include 'asterc/r8pi.h'
    include 'asterfort/trigom.h'
    real(kind=8) :: theta, pas, alpha1, alpha2, rcarte, d, pi, rho, rad
    real(kind=8) :: thet(801), rayo(801)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!-----------------------------------------------------------------------
!  CALCUL D'UN OBSTACLE GUIDE A 1 ENCOCHE
!
!**********************************************************
! NOMBRE DE PAS DE DISCRETISATION
!-----------------------------------------------------------------------
    integer :: i, n
!-----------------------------------------------------------------------
    n=800
!**********************************************************
    pi = r8pi( )
    rad = r8dgrd()
    pas = 360.d0/n
    theta = 0.d0
    alpha1 = 20.d0
    alpha2 = trigom('ASIN',d/rcarte)*180.d0/pi
    do 1 i = 1, (n+1)
        if ((theta.lt.alpha1) .or. (theta.gt.(360.d0-alpha1))) then
            rho=d/tan(alpha1*pi/180.d0)/cos(theta*pi/180.d0)
        endif
        if ((theta.ge.alpha1) .and. (theta.lt.alpha2)) then
            rho=d/sin(theta*pi/180.d0)
        endif
        if ((theta.gt.(360.d0-alpha2)) .and. (theta.le.(360.d0-alpha1))) then
            rho=-d/sin(theta*pi/180.d0)
        endif
        if ((theta.ge.alpha2) .and. (theta.le.(360.d0-alpha2))) then
            rho=rcarte
        endif
        thet(i) = theta * rad
        rayo(i) = rho
        theta=theta+pas
 1  end do
end subroutine
