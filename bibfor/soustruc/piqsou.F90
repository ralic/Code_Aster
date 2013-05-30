subroutine piqsou(alp, xc, yc, zc, zd,&
                  rep, ret, rit, bet, iso,&
                  typsou)
    implicit   none
    include 'asterc/r8pi.h'
    real(kind=8) :: alp, xc, yc, zc, zd, rep, ret, rit, bet, iso
    character(len=8) :: typsou
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
! TOLE  CRP_6
!     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
!     AUTEUR Y. WADIER
!
!     CALCULE LES COORDONNEES DES POINTS DEFINISSANT LA SOUDURE
!
! IN  : ALP         : ANGLE ALPHA (CORRESPOND A L'ANGLE THETA DU
!                     CR MMN/97/136 ECRIT PAR Y. WADIER)
! IN  : ISO         : ISO = ESO POUR LES SOUDURES DE TYPE_1
!                     ISO = HSO POUR LES SOUDURES DE TYPE_2
! OUT : XC,YC,ZC,ZD
!-----------------------------------------------------------------------
!
    real(kind=8) :: ya, za, gam, eps, fi, del, xd, yd, pis2, yi, zi, jeu
!     ------------------------------------------------------------------
!
    pis2 = r8pi() / 2.0d0
!
    if (typsou .eq. 'TYPE_1') then
        jeu = iso - ( ret - rit ) * tan(bet)
!
!        XI  = ( REP + JEU ) * COS(ALP)
        yi = ( rep + jeu ) * sin(alp)
        gam = asin( yi / rit )
        zi = rit * cos(gam)
!
!        XA  = REP * COS(ALP)
        ya = rep * sin(alp)
        gam = asin( ya / rit )
!        ZA  = RIT * COS(GAM)
!
        eps = sin(alp)*tan(bet)
        fi = eps*zi - yi
    else if (typsou .eq. 'TYPE_2') then
        ya = rep * sin(alp)
        za = rit * sqrt(1 - (ya/rit)**2)
        eps = sin(alp) * (iso/(ret-rit))
        fi = eps*za - ya
    endif
    del = fi**2 - (1+eps**2)*(fi**2-(ret*eps)**2)
!
    yd = (-fi+sqrt(del))/(1+eps**2)
!
    if (alp .lt. 1.d-10) then
        xd = rep + iso
    else if (abs(alp-pis2) .lt. 1.d-10) then
        xd = 0.0d0
    else
        xd = yd / tan(alp)
    endif
!
    zd = sqrt( ret**2 - yd**2 )
!
    xc = xd
    yc = yd
    gam = asin( yd / rit )
    zc = rit * cos(gam)
!
end subroutine
