subroutine piqpiq(xp, yp, zp, x, y,&
                  z, rep, ret, rit, bet,&
                  eso, hso, h2, h3, l4,&
                  l5, zone1, zone2, zone3, zone4,&
                  zone5, zone6, zone7, zone8, typsou)
    implicit   none
    include 'asterc/r8pi.h'
    include 'asterfort/piqsou.h'
    real(kind=8) :: xp, yp, zp, x, y, z
    real(kind=8) :: rep, ret, rit, bet, hso, eso, h2, h3, l4, l5
    character(len=8) :: typsou
    logical :: zone1, zone2, zone3, zone4, zone5, zone6, zone7, zone8
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
! TOLE  CRP_21
! TOLE  CRP_6
!     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
!     AUTEUR Y. WADIER
!
!     REALISE LA TRANSFORMATION : GEOMETRIE DE REFERENCE --> PIQUAGE
!     POUR LES DIFFERENTES ZONES : ZONE1 ... ZONE8
! IN  : XP, YP, ZP : COORD. DU POINT DANS LA GEOMETRIE DE REFERENCE
! OUT : X, Y, Z    : COORD. DU POINT DANS LA GEOMETRIE DU PIQUAGE
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: rmp, alp, alp0, alp1, pi, gam, eta
    real(kind=8) :: ksi, lan, fla, lm0, ra, rm, rc, xs, ys, zs, zs0, zm0, yp0
    real(kind=8) :: yp1, z1m1, z1m2, zm1, zm2, xc, yc, zc, zd, xc0, yc0, zc0
    real(kind=8) :: zd0
!     ------------------------------------------------------------------
!
    pi = r8pi()
    rmp = sqrt( xp**2 + yp**2 )
    alp = atan2( yp , xp )
!
    if (typsou .eq. 'TYPE_1') then
        call piqsou(alp, xc, yc, zc, zd,&
                    rep, ret, rit, bet, eso,&
                    typsou)
    else if (typsou .eq. 'TYPE_2') then
        call piqsou(alp, xc, yc, zc, zd,&
                    rep, ret, rit, bet, hso,&
                    typsou)
    endif
!
    if (zone1) then
        x = xp
        y = yp
        z = zp
        goto 9999
    endif
!
    if (zone2) then
        x = xp
        y = yp
        gam = asin( yp / zp )
        z = zp*(cos(gam)*(h2-zp)/(h2-h3)+(zp-h3)/(h2-h3))
        goto 9999
    endif
!
    if (zone3 .or. zone4) then
        x = xp
        y = yp
        gam = asin( yp / zp )
        z = zp * cos(gam)
        goto 9999
    endif
!
    if (zone5) then
        if (typsou .eq. 'TYPE_1') then
            eta = ( rmp - rep ) / eso
        else if (typsou .eq. 'TYPE_2') then
            eta = ( rmp - rep ) / hso
        endif
        ra = rep
        rc = sqrt( xc**2 + yc**2 )
        rm = ra + eta*(rc-ra)
!
        x = rm * cos(alp)
        y = rm * sin(alp)
!
        z1m1 = ret
        if (typsou .eq. 'TYPE_1') then
            z1m2 = ret + (1.d0-eta)*hso
        else if (typsou .eq. 'TYPE_2') then
            z1m2 = ret + (1.d0-eta)*eso
        endif
        gam = asin( y / z1m1 )
        zm1 = z1m1 * cos(gam)
        gam = asin( y / z1m2 )
        zm2 = z1m2 * cos(gam)
        if ((1.d0-eta) .ge. 1.d-14) then
            ksi = (zp-ret) / (z1m2-z1m1)
            z = zm1 + ksi*(zm2-zm1)
        else
            z = zm1
        endif
        goto 9999
    endif
!
    if (zone6) then
        if (typsou .eq. 'TYPE_1') then
            eta = ( rmp - rep ) / eso
        else if (typsou .eq. 'TYPE_2') then
            eta = ( rmp - rep ) / hso
        endif
        ra = rep
        rc = sqrt( xc**2 + yc**2 )
        rm = ra + eta*(rc-ra)
!
        x = rm*cos(alp)
        y = rm*sin(alp)
!
        gam = asin( y / rit )
        zm1 = rit * cos(gam)
        gam = asin( y / ret )
        zm2 = ret * cos(gam)
        ksi = (zp-rit) / (ret-rit)
!
        z = zm1 + ksi*(zm2-zm1)
!
        goto 9999
    endif
!
    if (zone7) then
!
        if (xp .lt. l4) then
!
            alp0 = acos( xp / l4 )
            alp1 = acos( xp / l5 )
!
            if (typsou .eq. 'TYPE_1') then
                call piqsou(alp0, xc0, yc0, zc0, zd0,&
                            rep, ret, rit, bet, eso,&
                            typsou)
            else if (typsou .eq. 'TYPE_2') then
                call piqsou(alp0, xc0, yc0, zc0, zd0,&
                            rep, ret, rit, bet, hso,&
                            typsou)
            endif
            zm0 = ( (zp-rit)*zd0+(ret-zp)*zc0 ) / (ret-rit)
            if (zm0/zp .gt. 1.0d0) zm0 = zp
            gam = acos( zm0 / zp )
            lm0 = gam*zp
!
            yp0 = l4*sin(alp0)
            yp1 = l5*sin(alp1)
            lan = (yp-yp0)/(yp1-yp0)
            fla = ( 1.0d0 + cos(lan*pi) ) / 2.0d0
!
            xs = xp
            ys = yp + fla*(lm0-yp0)
            zs = zp
!
        endif
!
        if (xp .ge. l4) then
            xs = xp
            ys = yp
            zs = zp
        endif
!
        zm0 = (rit+ret) / 2.0d0
        zs0 = ( (rmp-l4)*zm0+(l5-rmp)*zs ) / (l5-l4)
        gam = ys / zs0
!
        x = xs
        y = zs * sin( gam )
        z = zs * cos( gam )
!
        goto 9999
    endif
!
    if (zone8) then
!
        zm0 = ( rit + ret ) / 2.0d0
        gam = yp / zm0
!
        x = xp
        y = zp * sin( gam )
        z = zp * cos( gam )
!
        goto 9999
    endif
!
9999  continue
!
end subroutine
