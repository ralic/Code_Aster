subroutine mouveo(arete, rcarte, angdeb, angfin, angare,&
                  angmax, profon, volume, epais)
! aslint: disable=
    implicit   none
    include 'asterc/r8dgrd.h'
    include 'asterc/r8rddg.h'
    include 'asterfort/infniv.h'
    include 'asterfort/u2mesg.h'
    real(kind=8) :: arete, rcarte, angdeb, angfin, angare, angmax, profon
    real(kind=8) :: volume, epais
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
    real(kind=8) :: ang1, ang2, auxi, ancien, theta0, coteta
    real(kind=8) :: bangf, xandeb, yandeb, tateta, xhaut, yhaut, dishx, dishy
    real(kind=8) :: haut, basex, basey, base, bangm, r, surfac, surf
    real(kind=8) :: a, d, epsi, xprof, yprof, xangm, yangm
    real(kind=8) :: rad, deg
    real(kind=8) :: valr(2)
    integer :: i, k, ifm, niv
    integer :: vali
!-----------------------------------------------------------------------
    call infniv(ifm, niv)
!
    rad = r8dgrd( )
    deg = r8rddg( )
    auxi=0.d0
    r= rcarte
    d = rcarte * sin(arete*rad)
    angmax = angare*rad
    ancien= 80.d0*rad
    theta0= 70.d0*rad
    if (angare .gt. 270.d0) then
        angmax=(360.d0-angare)*rad
    else if (angare.gt.180.d0) then
        angmax=(angare-180.d0)*rad
    else if (angare.gt.90.d0) then
        angmax=(180.d0-angare)*rad
    endif
    ang2=theta0
    coteta=cos(theta0)/(sin(theta0))
    surfac=volume/epais
    k=0
    epsi=1.d-06
    i=1
!
!*********************************************************
!      RESOLUTION DE L'EQUATION S*E=V_USE
!      ON PROCEDE PAR DICHOTOMIE
!      ON PREND LA TANGENTE A L OBSTACLE EN THETA0
!      ON CALCULE LA SURFACE USEE ENTRE THETA0 ET L ANGLE
!      FORME PAR L INTERSECTION DE LA TGTE ET DE L ENCOCHE
!      ON MODIFIE L ANGLE FIN EN FONCTION DES RESULTATS
!*********************************************************
!
50  continue
!
    bangf = rcarte*(sin(ang2)+coteta*cos(ang2))
!
! --- CALCUL DE L ANGLE DEBUT
    xandeb = sin(theta0)*(bangf-d)/cos(theta0)
    yandeb = d
    ang1 = atan(yandeb/xandeb)*deg
!
! --- CALCUL DE LA HAUTEUR
    tateta = sin(theta0)/cos(theta0)
    bangm = r*(sin(angmax)-tateta*cos(angmax))
!
    xhaut = (bangf-bangm)*cos(theta0)*sin(theta0)
    yhaut = tateta*xhaut+bangm
!
    dishx = (r*cos(angmax)-xhaut)**2
    dishy = (r*sin(angmax)-yhaut)**2
    haut = sqrt(dishx+dishy)
!
! --- CALCUL DE LA BASE
    basex = (r*cos(ang2)-xandeb)**2
    basey = (r*sin(ang2)-yandeb)**2
!
    base = sqrt(basex+basey)
!
! --- CALCUL DE LA SURFACE USEE
    surf = (base*haut)/2
!
! --- ON TESTE LA SURFACE CALCULEE AVEC LA SURFACE USEE DONNEE
    if ((abs(surf-surfac)/surfac) .lt. epsi) goto 70
!
! --- DICHOTOMIE ET ON REPART
    if (surf .lt. surfac) then
        k=k+1
        auxi=ang2
        ang2=(ang2+ancien)/2
        if (k .eq. i) then
            theta0=ang2
            coteta=cos(theta0)/(sin(theta0))
        endif
    else
        if (auxi .ne. 0.d0) then
            ancien=ang2
            ang2=(ang2+auxi)/2
        else
            ancien=ang2
            ang2=(ang2+angmax)/2
        endif
    endif
    i=i+1
!
    if (i .eq. 1000) then
        vali = i
        valr (1) = surfac*epais
        valr (2) = surf*epais
        call u2mesg('A', 'PREPOST6_3', 0, ' ', 1,&
                    vali, 2, valr)
        goto 70
    endif
    goto 50
!
! --- C'EST TROUVE
70  continue
    ang2=ang2*deg
    if (angare .lt. 90.d0) then
        angdeb = ang1
        angfin = ang2
    endif
    if (angare .gt. 270.d0) then
        angdeb = 360.d0 - ang2
        angfin = 360.d0 - ang1
    endif
    if ((angare.gt.90.d0) .and. (angare.lt.180.d0)) then
        angdeb = 180.d0 - ang2
        angfin = 180.d0 - ang1
    endif
    if ((angare.gt.180.d0) .and. (angare.lt.270.d0)) then
        angdeb = 180.d0 + ang1
        angfin = 180.d0 + ang2
    endif
!
! --- CALCUL DE LA PROFONDEUR
!
    a = (sin(angmax)/cos(angmax))+(cos(theta0)/sin(theta0))
    xprof =bangf/a
    yprof =(sin(angmax)/cos(angmax))*xprof
    xangm =r*cos(angmax)
    yangm =r*sin(angmax)
    profon=sqrt(((xprof-xangm)**2)+((yprof-yangm)**2))
    angmax = angare
!
end subroutine
