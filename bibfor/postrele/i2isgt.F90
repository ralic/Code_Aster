subroutine i2isgt(epsi, xa, ya, xb, yb,&
                  xc, yc, xd, yd, npi,&
                  s1, s2, r1, r2)
    implicit  none
!
#include "asterfort/rvdet2.h"
    integer :: npi
    real(kind=8) :: epsi, xa, ya, xb, yb, xc, yc, xd, yd, s1, s2, r1, r2
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!**********************************************************************
!
!           CALCUL DE L' INTERSECTION DES SGMENTS (AB) ET (CD)
!
!           3 CAS DE FIGURE :
!
!                1. INTERSECTION VIDE
!                2. INTERSECTION REDUITE A UN UNIQUE POINT
!                3. INTERSECTION EGALE A UN SEGMENT CONTENU
!                   DANS UNE FACE
!
!           ENTREES :
!
!               COORDONNEES DES POINTS DEFINISSANT LES SEGMENTS
!
!           SORTIES :
!
!               NPI : NOMBRE DE POINTS DEFINISSANT L' INTERSECTION
!                     NPI VAUT 0 OU 1 OU 2
!
!               S1, S2 : ABSCISSES CURVILIGNES DES POINTS PRECEDANTS
!                        SUIVANT AB (S(A)=0 ET S(B)=1)
!
!               R1, R2 : ABSCISSES CURVILIGNES DE CES POINTS
!                        SUIVANT LA FACE (CD)
!
!*********************************************************************
!
    real(kind=8) :: d1, d2, d3, d4, ab2, cd2, a, b, c, d, aux
    real(kind=8) :: xba, xcd, xca, yba, ycd, yca, cond
!
    npi = 0
    s1 = 0.0d0
    s2 = 0.0d0
    r1 = 0.0d0
    r2 = 0.0d0
!
    xba = xb - xa
    xca = xc - xa
    xcd = xc - xd
    yba = yb - ya
    yca = yc - ya
    ycd = yc - yd
    cond = abs(xba) + abs(yba) + abs(xcd) + abs(ycd)
    xba = xba / cond
    xca = xca / cond
    xcd = xcd / cond
    yba = yba / cond
    yca = yca / cond
    ycd = ycd / cond
!
    call rvdet2(xba, yba, xcd, ycd, d1)
!
    if (abs( d1 ) .gt. epsi) then
!
!----------LES DROITES AB ET CD SE COUPENT---------------
!
        call rvdet2(xca, yca, xcd, ycd, d2)
        call rvdet2(xba, yba, xca, yca, d3)
        a = d2 / d1
        b = d3 / d1
!
        if (abs(a) .lt. epsi) a = 0.0d0
!
        if (abs(a-1.0d0) .lt. epsi) a = 1.0d0
!
        if (abs(b) .lt. epsi) b = 0.0d0
!
        if (abs(b-1.0d0) .lt. epsi) b = 1.0d0
!
        if ((a .le. 1.0d0) .and. (a .ge. 0.0d0) .and. (b .le. 1.0d0) .and. (b .ge. 0.0d0)) then
!
!----------LEUR POINT D' INTERSECTION SE SITUE ENTRE A ET B -----
!----------ET ENTRE C ET D                                  -----
!
            npi = 1
            s1 = a
            r1 = b
!
        endif
!
    else
!
!----------LES DROITES AB ET CD SONT PARALLELES-----------------
!
        call rvdet2(xca, yca, xba, yba, d4)
!
        if (abs( d4 ) .le. epsi) then
!
!----------LES DROITES AB ET CD SONT, DE PLUS, CONFONDUES----------
!
            call rvdet2(xb-xa, yb-ya, ya-yb, xb-xa, ab2)
            call rvdet2(xd-xc, yd-yc, yc-yd, xd-xc, cd2)
            call rvdet2(xb-xa, yb-ya, ya-yc, xc-xa, c)
            call rvdet2(xb-xa, yb-ya, ya-yd, xd-xa, d)
            call rvdet2(xd-xc, yd-yc, yc-ya, xa-xc, a)
            call rvdet2(xd-xc, yd-yc, yc-yb, xb-xc, b)
!
            s1 = c / ab2
            s2 = d / ab2
            r1 = a / cd2
            r2 = b / cd2
!
            if (abs(s1) .lt. epsi) s1 = 0.0d0
!
            if (abs(s1-1.0d0) .lt. epsi) s1 = 1.0d0
!
            if (abs(s2) .lt. epsi) s2 = 0.0d0
!
            if (abs(s2-1.0d0) .lt. epsi) s2 = 1.0d0
!
            if (abs(r1) .lt. epsi) r1 = 0.0d0
!
            if (abs(r1-1.0d0) .lt. epsi) r1 = 1.0d0
!
            if (abs(r2) .lt. epsi) r2 = 0.0d0
!
            if (abs(r2-1.0d0) .lt. epsi) r2 = 1.0d0
!
            if (s1 .lt. 0.0d0) then
!
                if (s2 .lt. 0.0d0) then
!
                    npi = 0
!
                else if (s2 .le. 1.0d0) then
!
                    npi = 2
                    s1 = 0.0d0
                    r2 = 1.0d0
!
                else
!
                    npi = 2
                    s1 = 0.0d0
                    s2 = 1.0d0
!
                endif
!
            else if (s1 .le. 1.0d0) then
!
                if (s2 .lt. 0.0d0) then
!
                    npi = 2
                    s2 = s1
                    s1 = 0.0d0
                    r2 = 0.0d0
!
                else if (s2 .le. 1.0d0) then
!
                    npi = 2
!
                    if (s1 .lt. s2) then
!
                        r1 = 0.0d0
                        r2 = 1.0d0
!
                    else
!
                        aux = s1
                        s1 = s2
                        s2 = aux
                        r1 = 1.0d0
                        r2 = 0.0d0
!
                    endif
!
                else
!
                    npi = 2
                    s2 = 1.0d0
                    r1 = 0.0d0
!
                endif
!
            else
!
                if (s2 .lt. 0.0d0) then
!
                    npi = 2
                    s1 = 0.0d0
                    s2 = 1.0d0
!
                else if (s2 .le. 1.0d0) then
!
                    npi = 2
                    s1 = s2
                    s2 = 1.0d0
                    r1 = 1.0d0
!
                else
!
                    npi = 0
!
                endif
!
            endif
!
!---------LES DROITES AB ET CD SONT PARALLELES NON CONFONDUES----
!
        endif
!
    endif
!
end subroutine
