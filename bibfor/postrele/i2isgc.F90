subroutine i2isgc(epsi, xa, ya, xb, yb,&
                  x1, y1, x2, y2, x3,&
                  y3, npi, s1, s2, r1,&
                  r2, eli)
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
    implicit none
!
! AJPUT POSTRELEVE
!
!*******************************************************************
!
!             CALCUL DE L' INTERSECTION DE LA PARABOLE
!             PASSANT PAR LES POINTS (XI,YI) AVEC LE
!             SEGMENT (AB)
!
!             NPI = 0  ==> INTERSECTION VIDE
!
!             NPI = 1  ==> INTERSECTION REDUITE AU POINT
!                          D' ABSCISSE S1 SUIVANT (AB)
!                          ALORS LE BOOLEEN ELI INDIQUE SI CE
!                          POINT DOIT ETRE PRIS EN COMPTE OU NON
!                          (LE POINT EST ELIMINE QUAND LE SEGMENT
!                           EST TANGENT A LA PARABOLE EN CE POINT)
!
!             NPI = 2  ==> INTERSECTION REDUITE AUX POINTS
!                          D' ABSCISSES S1 ET S2 SUIVANT (AB)
!
!*******************************************************************
!
#include "asterfort/i2req2.h"
    integer :: npi
    real(kind=8) :: epsi, xa, ya, xb, yb, x1, y1, x2, y2, x3, y3, s1, s2, r1, r2
    logical :: eli
!
    integer :: nbrac, ord1, ord2
    real(kind=8) :: deltax, deltay, coef0, coef1, coef2, n1, n2, n3, n4
    real(kind=8) :: xr, yr, norm2, rac1, rac2, aux
    logical :: lsr10, ls1r1, le0r11
    logical :: lsr20, ls1r2, le0r21
    logical :: lss10, ls1s1, le0s11
    logical :: lss20, ls1s2, le0s21
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    npi = 0
    s1 = 0.0d0
    s2 = 0.0d0
    r1 = 0.0d0
    r2 = 0.0d0
    xr = 0.0d0
    yr = 0.0d0
    aux = 0.0d0
    rac1 = 0.0d0
    rac2 = 0.0d0
    nbrac = 0
    ord1 = 0
    ord2 = 0
!
!-----------COORDONNEES ET CARRE DE LA NORME DU VECTEUR AB-----------
!
    deltax = xb - xa
    deltay = yb - ya
    norm2 = deltax*deltax + deltay*deltay
!
!----------COEFFICIENTS DE LA REPRESENTATION PARAMETRIQUE------------
!----------DE L' ARC DE PARABOLE                         ------------
!
    n1 = x1 - 2*x2 + x3
    n1 = 2*n1
    n3 = y1 - 2*y2 + y3
    n3 = 2*n3
    n2 = -3*x1 + 4*x2 - x3
    n4 = -3*y1 + 4*y2 - y3
!
!------------COEFFICIENTS DE L 'EQUATION DEFFINISSANT L' INTERSECTION---
!
    coef0 = deltay*(x1-xa) - deltax*(y1-ya)
    coef1 = deltay*n2 - deltax*n4
    coef2 = deltay*n1 - deltax*n3
!
!------------BOOLEENS DE POSITIONEMENT DES RACINES-------------------
!
    lsr10 = .false.
    ls1r1 = .false.
    le0r11 = .false.
    lsr20 = .false.
    ls1r2 = .false.
    le0r21 = .false.
    lss10 = .false.
    ls1s1 = .false.
    le0s11 = .false.
    lss20 = .false.
    ls1s2 = .false.
    le0s21 = .false.
!
!------------BOOLEEN D' ELIMINATION DES POINTS TANGENTS-------------
!
    eli = .false.
!
!------------CALCUL DES PARAMETRES SUR LA PARABOLE DES--------------
!------------POINTS D' INTERSECTION                   --------------
!
    call i2req2(epsi, coef2, coef1, coef0, nbrac,&
                rac1, rac2, ord1, ord2)
!
!------------REAJUSTEMENT DES PARAMETRES RACINES-------------------
!
    if (abs(rac1) .lt. epsi) then
!
        rac1 = 0.0d0
!
    endif
!
    if (abs(rac2) .lt. epsi) then
!
        rac2 = 0.0d0
!
    endif
!
    if (abs(rac1-1.0d0) .lt. epsi) then
!
        rac1 = 1.0d0
!
    endif
!
    if (abs(rac2-1.0d0) .lt. epsi) then
!
        rac2 = 1.0d0
!
    endif
!
!------------DEPOUILLEMENT DES RESULTATS----------------------------
!
    if (nbrac .eq. 1) then
!
        if ((rac1 .le. 1.0d0) .and. (rac1 .ge. 0.0d0)) then
!
            xr = (n1*rac1 + n2)*rac1 + x1 - xa
            yr = (n3*rac1 + n4)*rac1 + y1 - ya
            s1 = deltax*xr + deltay*yr
!
            if (abs(s1) .lt. epsi) then
!
                s1 = 0.0d0
!
            endif
!
            if (abs(s1-norm2) .lt. epsi) then
!
                s1 = norm2
!
            endif
!
            if ((s1 .le. norm2) .and. (s1 .ge. 0.0d0)) then
!
                npi = 1
                s1 = s1/norm2
                r1 = rac1
!
                if ((ord1 .ge. 2) .and. (r1 .gt. epsi) .and. (s1 .gt. epsi) .and.&
                    ( abs(r1-1.0d0) .gt. epsi) .and. ( abs(s1-1.0d0) .gt. epsi)) then
!
                    eli = .true.
!
                endif
!
            endif
!
        endif
!
    endif
!
    if (nbrac .eq. 2) then
!
        lsr10 = (rac1 .lt. 0.0d0)
        ls1r1 = (rac1 .gt. 1.0d0)
        le0r11 = ( (.not. lsr10) .and. (.not. ls1r1) )
        lsr20 = (rac2 .lt. 0.0d0)
        ls1r2 = (rac2 .gt. 1.0d0)
        le0r21 = ( (.not. lsr20) .and. (.not. ls1r2) )
!
        if ((lsr10 .or. ls1r1) .and. (le0r21)) then
!
            xr = (n1*rac2 + n2)*rac2 + x1 - xa
            yr = (n3*rac2 + n4)*rac2 + y1 - ya
            s1 = deltax*xr + deltay*yr
!
            if (abs(s1) .lt. epsi) then
!
                s1 = 0.0d0
!
            endif
!
            if (abs(s1-norm2) .lt. epsi) then
!
                s1 = norm2
!
            endif
!
            if ((s1 .le. norm2) .and. (s1 .ge. 0.0d0)) then
!
                npi = 1
                s1 = s1/norm2
                r1 = rac2
!
            endif
!
        endif
!
        if ((le0r11) .and. (lsr20 .or. ls1r2)) then
!
            xr = (n1*rac1 + n2)*rac1 + x1 - xa
            yr = (n3*rac1 + n4)*rac1 + y1 - ya
            s1 = deltax*xr + deltay*yr
!
            if (abs(s1) .lt. epsi) then
!
                s1 = 0.0d0
!
            endif
!
            if (abs(s1-norm2) .lt. epsi) then
!
                s1 = norm2
!
            endif
!
            if ((s1 .le. norm2) .and. (s1 .ge. 0.0d0)) then
!
                npi = 1
                s1 = s1/norm2
                r1 = rac1
!
            endif
!
        endif
!
        if (le0r11 .and. le0r21) then
!
            xr = (n1*rac1 + n2)*rac1 + x1 - xa
            yr = (n3*rac1 + n4)*rac1 + y1 - ya
            s1 = deltax*xr + deltay*yr
            xr = (n1*rac2 + n2)*rac2 + x1 - xa
            yr = (n3*rac2 + n4)*rac2 + y1 - ya
            s2 = deltax*xr + deltay*yr
!
            if (abs(s1) .lt. epsi) then
!
                s1 = 0.0d0
!
            endif
!
            if (abs(s1-norm2) .lt. epsi) then
!
                s1 = norm2
!
            endif
!
            if (abs(s2) .lt. epsi) then
!
                s2 = 0.0d0
!
            endif
!
            if (abs(s2-norm2) .lt. epsi) then
!
                s2 = norm2
!
            endif
!
            lss10 = (s1 .lt. 0.0d0)
            ls1s1 = (s1 .gt. norm2)
            le0s11 = ( (.not. lss10) .and. (.not. ls1s1) )
            lss20 = (s2 .lt. 0.0d0)
            ls1s2 = (s2 .gt. norm2)
            le0s21 = ( (.not. lss20) .and. (.not. ls1s2) )
!
            if ((lss10 .or. ls1s1) .and. (le0s21)) then
!
                npi = 1
                s1 = s2/norm2
                r1 = rac2
!
            endif
!
            if ((le0s11) .and. (lss20 .or. ls1s2)) then
!
                npi = 1
                s1 = s1/norm2
                r1 = rac1
!
            endif
!
            if (le0s11 .and. le0s21) then
!
                npi = 2
                s1 = s1/norm2
                s2 = s2/norm2
                r1 = rac1
                r2 = rac2
!
                if (s1 .gt. s2) then
!
                    aux = s1
                    s1 = s2
                    s2 = aux
                    aux = r1
                    r1 = r2
                    r2 = aux
!
                endif
!
            endif
!
        endif
!
    endif
!
end subroutine
