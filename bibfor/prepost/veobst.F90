subroutine veobst(arete, rcarte, angdeb, angfin, angva,&
                  angare, angmax, profon, volume, epais)
    implicit   none
    include 'asterc/r8dgrd.h'
    include 'asterc/r8rddg.h'
    real(kind=8) :: arete, rcarte, angdeb, angfin, angva, angare, angmax, profon
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
!-----------------------------------------------------------------------
    real(kind=8) :: delta, tau, ang1, ang2
    real(kind=8) :: theta1, theta2, theta3, fonc1, fonc2, fonc3
    real(kind=8) :: a1, a2, a3, a, d
    real(kind=8) :: rad, deg
!-----------------------------------------------------------------------
!
    rad = r8dgrd( )
    deg = r8rddg( )
!
    d = rcarte * sin(arete*rad)
    tau = 0.5d0
    theta1 = 25.d0
    theta2 = arete-1.d-5
    angmax = angare
    profon = volume*(1.d0+tau)/epais*angva/0.7d-3*rad
    delta = profon / ( rcarte*tan(profon*angva/0.7d-3*rad) )
    ang2 = arete + delta*deg
!*********************************************************
!      RESOLUTION DE L'EQUATION S*E=V_USE
!      ON PROCEDE PAR DICHOTOMIE
!
!      L'EQUATION DE LA DROITE EST R(THETA)=A*(THETA-ANG2)+RCARTE
!*********************************************************
    a1 = 1.d0 / ( (theta1-ang2)*rad)*(d/sin(theta1*rad)-rcarte)
    fonc1 = 1.d0 / (6*a1)*rcarte**3-((ang2-arete)*rad)/2 *rcarte**2 + d**2/2 *( 1.d0/tan(arete*ra&
            &d)-1.d0/tan(theta1*rad) ) -1.d0/(6*a1)*(a1*(theta1-ang2)*rad+rcarte)**3 -volume/epai&
            &s
!
    a2 = 1.d0 / ((theta2-ang2)*rad)*(d/sin(theta2*rad)-rcarte)
    fonc2 = 1.d0 / (6*a2)*rcarte**3-((ang2-arete)*rad)/2 *rcarte**2 - d**2/2 *( 1.d0/tan(arete*ra&
            &d)-1.d0/tan(theta2*rad) ) -1.d0/(6*a2)*(a2*(theta2-ang2)*rad+rcarte)**3 -volume/epai&
            &s
!
    if (fonc1 .eq. 0.d0) then
        ang1 = theta1
        goto 20
    endif
!
    if (fonc2 .eq. 0.d0) then
        ang1 = theta2
        goto 20
    endif
!
    if (fonc2*fonc1 .gt. 0.d0) then
        ang1 = arete
        goto 20
    endif
!
    if (fonc2*fonc1 .lt. 0.d0) then
10      continue
        if ((theta2-theta1) .lt. 1.d-5) then
            ang1 = theta2
            goto 20
        endif
        theta3 = ( theta1 + theta2 ) / 2
        a3 = 1.d0 / ( (theta3-ang2)*rad) * ( d / sin(theta3*rad) - rcarte )
        fonc3 = 1.d0 / (6.d0*a3)*rcarte**3-((ang2-arete)*rad)/2 *rcarte**2 - d**2/2 *( 1.d0/tan(a&
                &rete*rad)-1.d0/tan(theta3* rad) ) -1.d0/(6.d0*a3)*(a3*(theta3-ang2)*rad+rcarte)*&
                &*3 -volume/epais
!
        if (fonc3 .eq. 0.d0) then
            ang1 = theta3
            goto 20
        endif
        if (fonc1*fonc3 .lt. 0.d0) then
            theta2 = theta3
            fonc2 = fonc3
            goto 10
        endif
        if (fonc2*fonc3 .lt. 0.d0) then
            theta1 = theta3
            fonc1 = fonc3
            goto 10
        endif
    endif
20  continue
!
    if (angmax .lt. 90.d0) then
        angfin = ang2
        angdeb = ang1
    endif
    if (angmax .gt. 270.d0) then
        angdeb = 360.d0 - ang2
        angfin = 360.d0 - ang1
    endif
    if ((angmax.gt.90.d0) .and. (angmax.lt.180.d0)) then
        angdeb = 180.d0 - ang2
        angfin = 180.d0 - ang1
    endif
    if ((angmax.gt.180.d0) .and. (angmax.lt.270.d0)) then
        angfin = 180.d0 + ang2
        angdeb = 180.d0 + ang1
    endif
!
    a = 1.d0 / ((ang1-ang2)*rad)*(d/sin(ang1*rad)-rcarte)
    profon = abs( a*(ang2-arete)*rad )
!
end subroutine
