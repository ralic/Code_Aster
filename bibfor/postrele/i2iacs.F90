subroutine i2iacs(epsi, xc, yc, r, alfinf,&
                  alfsup, x1, y1, x2, y2,&
                  npi, a1, a2, r1, r2,&
                  eli)
    implicit none
!
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
!
!**********************************************************************
!
!          CALCUL DE L' INTERSECTION DU CERCLE DE CENTRE
!          (XC,YC), DE RAYON R ET DE SECTEUR ANGULAIRE
!          (ALFINF,ALFSUP) AVEC LE SEGMENT DEFINI PAR
!          LES POINTS (X1,Y1) ET (X2,Y2)
!
!*********************************************************************
!
#include "asterfort/i2req2.h"
#include "asterfort/trigom.h"
    integer :: npi
    real(kind=8) :: epsi, xc, yc, x1, y1, x2, y2, r, alfinf, alfsup, a1, a2, r1
    real(kind=8) :: r2
    logical :: eli
!
    integer :: nbrac, ord1, ord2
    real(kind=8) :: coef2, coef1, coef0, rac1, rac2, deltax, deltay
    real(kind=8) :: invr, xs, ys, aux
!
    logical :: lsr10, ls1r1, lsr20, ls1r2, le0r11, le0r21
    logical :: lsa1i, lssa1, lsa2i, lssa2, leia1s, leia2s
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    lsa1i = .false.
    lssa1 = .false.
    lsa2i = .false.
    lssa2 = .false.
    leia1s = .false.
    leia2s = .false.
!
    lsr10 = .false.
    ls1r1 = .false.
    lsr20 = .false.
    ls1r2 = .false.
    le0r11 = .false.
    le0r21 = .false.
!
    npi = 0
    a1 = 0.0d0
    a2 = 0.0d0
    r1 = 0.0d0
    r2 = 0.0d0
    eli = .false.
!
    xs = 0.0d0
    ys = 0.0d0
    invr = 1.0d0/r
    aux = 0.0d0
!
    deltax = x2-x1
    deltay = y2-y1
    coef2 = deltax*deltax + deltay*deltay
    coef1 = 2*(deltax*(x1-xc) + deltay*(y1-yc))
    coef0 = (x1-xc)*(x1-xc) + (y1-yc)*(y1-yc) - r*r
!
    rac1 = 0.0d0
    rac2 = 0.0d0
    nbrac = 0
    ord1 = 0
    ord2 = 0
!
!----------CALCUL DES PARAMETRES DE REPERAGE SUR LE SEGMENT---------
!
    call i2req2(epsi, coef2, coef1, coef0, nbrac,&
                rac1, rac2, ord1, ord2)
!
!-------------REAJUSTEMENT DES RACINES--------------------------------
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
!---------------DEPOUILLEMENT DES RESULTATS------------------------
!
    if (nbrac .eq. 1) then
!
        if ((rac1 .le. 1.0d0) .and. (rac1 .ge. 0.0d0)) then
!
            xs = x1 + rac1*deltax
            ys = y1 + rac1*deltay
!
            aux = invr*(xs-xc)
!
            if (abs(aux - 1.0d0) .lt. epsi) then
!
                aux = 1.0d0
!
            endif
!
            if (abs(aux + 1.0d0) .lt. epsi) then
!
                aux = -1.0d0
!
            endif
!
            a1 = trigom('ACOS', aux)
!
            if (abs(ys-yc) .lt. epsi) then
!
                ys = yc
!
            endif
!
            if ((ys-yc) .lt. 0.0d0) then
!
                a1 = -a1
!
            endif
!
            if (abs(a1-alfinf) .lt. epsi) then
!
                a1 = alfinf
!
            endif
!
            if (abs(a1-alfsup) .lt. epsi) then
!
                a1 = alfsup
!
            endif
!
            if ((alfinf .le. a1) .and. (a1 .le. alfsup)) then
!
                npi = 1
                r1 = rac1
!
                if ((ord1 .ge. 2 ) .and.&
                    (&
                    (abs(rac1) .gt. epsi) .and. (abs(rac1- 1.0d0) .gt. epsi) .and.&
                    (abs(a1 - alfinf) .gt. epsi) .and. (abs(a1 - alfsup) .gt. epsi)&
                    )) then
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
            xs = x1 + rac2*deltax
            ys = y1 + rac2*deltay
!
            aux = invr*(xs-xc)
!
            if (abs(aux - 1.0d0) .lt. epsi) then
!
                aux = 1.0d0
!
            endif
!
            if (abs(aux + 1.0d0) .lt. epsi) then
!
                aux = -1.0d0
!
            endif
!
            a1 = trigom('ACOS', aux)
!
            if (abs(ys-yc) .lt. epsi) then
!
                ys = yc
!
            endif
!
            if ((ys-yc) .lt. 0.0d0) then
!
                a1 = -a1
!
            endif
!
            if (abs(a1-alfinf) .lt. epsi) then
!
                a1 = alfinf
!
            endif
!
            if (abs(a1-alfsup) .lt. epsi) then
!
                a1 = alfsup
!
            endif
!
            if ((a1 .le. alfsup) .and. (a1 .ge. alfinf)) then
!
                npi = 1
                r1 = rac2
!
            endif
!
        endif
!
        if ((le0r11) .and. (lsr20 .or. ls1r2)) then
!
            xs = x1 + rac1*deltax
            ys = y1 + rac1*deltay
!
            aux = invr*(xs-xc)
!
            if (abs(aux - 1.0d0) .lt. epsi) then
!
                aux = 1.0d0
!
            endif
!
            if (abs(aux + 1.0d0) .lt. epsi) then
!
                aux = -1.0d0
!
            endif
!
            a1 = trigom('ACOS', aux)
!
            if (abs(ys-yc) .lt. epsi) then
!
                ys = yc
!
            endif
!
            if ((ys-yc) .lt. 0.0d0) then
!
                a1 = -a1
!
            endif
!
            if (abs(a1-alfinf) .lt. epsi) then
!
                a1 = alfinf
!
            endif
!
            if (abs(a1-alfsup) .lt. epsi) then
!
                a1 = alfsup
!
            endif
!
            if ((a1 .le. alfsup) .and. (a1 .ge. alfinf)) then
!
                npi = 1
                r1 = rac1
!
            endif
!
        endif
!
        if (le0r11 .and. le0r21) then
!
            xs = x1 + rac1*deltax
            ys = y1 + rac1*deltay
!
            aux = invr*(xs-xc)
!
            if (abs(aux - 1.0d0) .lt. epsi) then
!
                aux = 1.0d0
!
            endif
!
            if (abs(aux + 1.0d0) .lt. epsi) then
!
                aux = -1.0d0
!
            endif
!
            a1 = trigom('ACOS', aux)
!
            if (abs(ys-yc) .lt. epsi) then
!
                ys = yc
!
            endif
!
            if ((ys-yc) .lt. 0.0d0) then
!
                a1 = -a1
!
            endif
!
            xs = x1 + rac2*deltax
            ys = y1 + rac2*deltay
!
            aux = invr*(xs-xc)
!
            if (abs(aux - 1.0d0) .lt. epsi) then
!
                aux = 1.0d0
!
            endif
!
            if (abs(aux + 1.0d0) .lt. epsi) then
!
                aux = -1.0d0
!
            endif
!
            a2 = trigom('ACOS', aux)
!
            if (abs(ys-yc) .lt. epsi) then
!
                ys = yc
!
            endif
!
            if ((ys-yc) .lt. 0.0d0) then
!
                a2 = -a2
!
            endif
!
            if (abs(a1-alfinf) .lt. epsi) then
!
                a1 = alfinf
!
            endif
!
            if (abs(a1-alfsup) .lt. epsi) then
!
                a1 = alfsup
!
            endif
!
            if (abs(a2-alfinf) .lt. epsi) then
!
                a2 = alfinf
!
            endif
!
            if (abs(a2-alfsup) .lt. epsi) then
!
                a2 = alfsup
!
            endif
!
            lsa1i = (a1 .lt. alfinf)
            lssa1 = (a1 .gt. alfsup)
            leia1s = ( (.not. lsa1i) .and. (.not. lssa1) )
            lsa2i = (a2 .lt. alfinf)
            lssa2 = (a2 .gt. alfsup)
            leia2s = ( (.not. lsa2i) .and. (.not. lssa2) )
!
            if ((lsa1i .or. lssa1) .and. (leia2s)) then
!
                npi = 1
                a1 = a2
                r1 = rac2
!
            endif
!
            if ((lsa2i .or. lssa2) .and. (leia1s)) then
!
                npi = 1
                r1 = rac1
!
            endif
!
            if (leia1s .and. leia2s) then
!
                npi = 2
                r1 = rac1
                r2 = rac2
!
                if (a1 .gt. a2) then
!
                    aux = a1
                    a1 = a2
                    a2 = aux
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
