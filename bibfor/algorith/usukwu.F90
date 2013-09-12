subroutine usukwu(nbpt, fn, vg, para, w,&
                  iret)
    implicit none
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!
#include "asterfort/getvr8.h"
#include "asterfort/iunifi.h"
#include "asterfort/u2mess.h"
    real(kind=8) :: fn(*), vg(*), para(*)
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: i, if, ifn0, iret, n1, n2
    integer :: nbpt
    real(kind=8) :: de, fn1, fn2, fn3, fn4, fn5, fnm
    real(kind=8) :: sphi, spwi, un, vg0, vgm, w, xc
    real(kind=8) :: xfn, xi11, xi12, xi13, xi14, xi15, xi21
    real(kind=8) :: xi22, xi23, xi24, xi25, xi31, xi32, xi33
    real(kind=8) :: xi34, xi35, xi41, xi42, xi43, xi44, xi45
    real(kind=8) :: xi51, xi52, xi53, xi54, xi55, xit1, xit2
    real(kind=8) :: xit3, xit4, xit5, xitt, xk, xk1, xk2
    real(kind=8) :: xvg, xvg1, xvg2, xvg3, xvg4, xvg5, xxi11
    real(kind=8) :: xxi12, xxi13, xxi14, xxi15, xxi21, xxi22, xxi23
    real(kind=8) :: xxi24, xxi25, xxi31, xxi32, xxi33, xxi34, xxi35
    real(kind=8) :: xxi41, xxi42, xxi43, xxi44, xxi45, xxi51, xxi52
    real(kind=8) :: xxi53, xxi54, xxi55, yy, yyi11, yyi12, yyi13
    real(kind=8) :: yyi14, yyi15, yyi20, yyi21, yyi22, yyi23, yyi24
    real(kind=8) :: yyi25, yyi31, yyi32, yyi33, yyi34, yyi35, yyi41
    real(kind=8) :: yyi42, yyi43, yyi44, yyi45, yyi51, yyi52, yyi53
    real(kind=8) :: yyi54, yyi55, zero
!-----------------------------------------------------------------------
    if = iunifi('RESULTAT')
    iret = 0
    zero = 0.d0
    un = 1.d0
    de = 2.d0
    xk1 = para(2)
    xk2 = para(3)
    xk = para(4)
    xc = para(5)
!
!     --- RECHERCHE DES MAX ---
!
    call getvr8(' ', 'FNOR_MAXI', scal=fnm, nbret=n1)
    call getvr8(' ', 'VTAN_MAXI', scal=vgm, nbret=n2)
    if ((n1+n2) .eq. 0) then
        fnm = zero
        vgm = zero
        do 10 i = 1, nbpt
            fnm = max(fnm,fn(i))
            vgm = max(vgm,vg(i))
10      continue
    else if (n1 .eq. 0) then
        fnm = zero
        do 12 i = 1, nbpt
            fnm = max(fnm,fn(i))
12      continue
    else if (n2 .eq. 0) then
        vgm = zero
        do 14 i = 1, nbpt
            vgm = max(vgm,vg(i))
14      continue
    endif
    write(if,2000) fnm
    write(if,2010) vgm
!
!     --- DETERMINATION DES BORNES DES INTERVALLES ---
!
    ifn0 = nint ( fnm )
    if (ifn0 .eq. 0) then
        call u2mess('A', 'ALGORITH11_5')
        iret = 10
        goto 9999
    endif
    ifn0 = ifn0 / 5
    if (ifn0 .eq. 0) then
        call u2mess('A', 'ALGORITH11_5')
        iret = 10
        goto 9999
    endif
    vg0 = vgm / 5.d0
    fn1 = dble ( ifn0 )
    fn2 = fn1 + ifn0
    fn3 = fn2 + ifn0
    fn4 = fn3 + ifn0
    fn5 = fn4 + ifn0
    xvg1 = vg0
    xvg2 = xvg1 + vg0
    xvg3 = xvg2 + vg0
    xvg4 = xvg3 + vg0
    xvg5 = xvg4 + vg0
!
!     --- REMPLISSAGE DES CLASSES ---
!
    xi11 = zero
    xi12 = zero
    xi13 = zero
    xi14 = zero
    xi15 = zero
    xi21 = zero
    xi22 = zero
    xi23 = zero
    xi24 = zero
    xi25 = zero
    xi31 = zero
    xi32 = zero
    xi33 = zero
    xi34 = zero
    xi35 = zero
    xi41 = zero
    xi42 = zero
    xi43 = zero
    xi44 = zero
    xi45 = zero
    xi51 = zero
    xi52 = zero
    xi53 = zero
    xi54 = zero
    xi55 = zero
    do 30 i = 1, nbpt
        xvg = vg(i)
        xfn = fn(i)
        if (xfn .gt. zero) then
            if (xfn .gt. fn4) then
                if (xvg .gt. xvg4) then
                    xi55 = xi55 + un
                else if (xvg.gt.xvg3) then
                    xi45 = xi45 + un
                else if (xvg.gt.xvg2) then
                    xi35 = xi35 + un
                else if (xvg.gt.xvg1) then
                    xi25 = xi25 + un
                else
                    xi15 = xi15 + un
                endif
            else if (xfn.gt.fn3) then
                if (xvg .gt. xvg4) then
                    xi54 = xi54 + un
                else if (xvg.gt.xvg3) then
                    xi44 = xi44 + un
                else if (xvg.gt.xvg2) then
                    xi34 = xi34 + un
                else if (xvg.gt.xvg1) then
                    xi24 = xi24 + un
                else
                    xi14 = xi14 + un
                endif
            else if (xfn.gt.fn2) then
                if (xvg .gt. xvg4) then
                    xi53 = xi53 + un
                else if (xvg.gt.xvg3) then
                    xi43 = xi43 + un
                else if (xvg.gt.xvg2) then
                    xi33 = xi33 + un
                else if (xvg.gt.xvg1) then
                    xi23 = xi23 + un
                else
                    xi13 = xi13 + un
                endif
            else if (xfn.gt.fn1) then
                if (xvg .gt. xvg4) then
                    xi52 = xi52 + un
                else if (xvg.gt.xvg3) then
                    xi42 = xi42 + un
                else if (xvg.gt.xvg2) then
                    xi32 = xi32 + un
                else if (xvg.gt.xvg1) then
                    xi22 = xi22 + un
                else
                    xi12 = xi12 + un
                endif
            else
                if (xvg .gt. xvg4) then
                    xi51 = xi51 + un
                else if (xvg.gt.xvg3) then
                    xi41 = xi41 + un
                else if (xvg.gt.xvg2) then
                    xi31 = xi31 + un
                else if (xvg.gt.xvg1) then
                    xi21 = xi21 + un
                else
                    xi11 = xi11 + un
                endif
            endif
        endif
30  end do
    xit1 = xi11 + xi12 + xi13 + xi14 + xi15
    xit2 = xi21 + xi22 + xi23 + xi24 + xi25
    xit3 = xi31 + xi32 + xi33 + xi34 + xi35
    xit4 = xi41 + xi42 + xi43 + xi44 + xi45
    xit5 = xi51 + xi52 + xi53 + xi54 + xi55
    xitt = xit1 + xit2 + xit3 + xit4 + xit5
    write(if,2020) nint(xitt)
    xxi11 = xi11 / xitt
    xxi12 = xi12 / xitt
    xxi13 = xi13 / xitt
    xxi14 = xi14 / xitt
    xxi15 = xi15 / xitt
    xxi21 = xi21 / xitt
    xxi22 = xi22 / xitt
    xxi23 = xi23 / xitt
    xxi24 = xi24 / xitt
    xxi25 = xi25 / xitt
    xxi31 = xi31 / xitt
    xxi32 = xi32 / xitt
    xxi33 = xi33 / xitt
    xxi34 = xi34 / xitt
    xxi35 = xi35 / xitt
    xxi41 = xi41 / xitt
    xxi42 = xi42 / xitt
    xxi43 = xi43 / xitt
    xxi44 = xi44 / xitt
    xxi45 = xi45 / xitt
    xxi51 = xi51 / xitt
    xxi52 = xi52 / xitt
    xxi53 = xi53 / xitt
    xxi54 = xi54 / xitt
    xxi55 = xi55 / xitt
    write(if,2030)
    write(if,1020) zero,fn1,fn2,fn3,fn4
    write(if,1022)  fn1,fn2,fn3,fn4,fn5
    write(if,1024)
    write(if,1000) zero,xxi11,xxi12,xxi13,xxi14,xxi15
    write(if,1010) xvg1
    write(if,1026)
    write(if,1000) xvg1,xxi21,xxi22,xxi23,xxi24,xxi25
    write(if,1010) xvg2
    write(if,1026)
    write(if,1000) xvg2,xxi31,xxi32,xxi33,xxi34,xxi35
    write(if,1010) xvg3
    write(if,1026)
    write(if,1000) xvg3,xxi41,xxi42,xxi43,xxi44,xxi45
    write(if,1010) xvg4
    write(if,1026)
    write(if,1000) xvg4,xxi51,xxi52,xxi53,xxi54,xxi55
    write(if,1010) xvg5
    write(if,1026)
!
!     --- CALCUL DES FONCTEURS DE PONDERATION ---
!
    yyi11 = xk1 * xk * ( ( fn1 /de ) / xc ) ** 3
    yyi12 = xk1 * xk * ( ( (fn2+fn1) /de ) / xc ) ** 3
    yyi13 = xk1 * xk * ( ( (fn3+fn2) /de ) / xc ) ** 3
    yyi14 = xk1 * xk * ( ( (fn4+fn3) /de ) / xc ) ** 3
    yyi15 = xk1 * xk * ( ( (fn5+fn4) /de ) / xc ) ** 3
    yy = xvg1 / de
    yyi20 = xk2 * ( fn1 /de) * yy * yy
    yy = ( xvg2 + xvg1 ) / de
    yyi21 = xk2 * ( fn1 /de) * yy * yy
    yyi22 = xk2 * ((fn2+fn1)/de) * yy * yy
    yyi23 = xk2 * ((fn3+fn2)/de) * yy * yy
    yyi24 = xk2 * ((fn4+fn3)/de) * yy * yy
    yyi25 = xk2 * ((fn5+fn4)/de) * yy * yy
    yy = ( xvg3 + xvg2 ) / de
    yyi31 = xk2 * ( fn1 /de) * yy * yy
    yyi32 = xk2 * ((fn2+fn1)/de) * yy * yy
    yyi33 = xk2 * ((fn3+fn2)/de) * yy * yy
    yyi34 = xk2 * ((fn4+fn3)/de) * yy * yy
    yyi35 = xk2 * ((fn5+fn4)/de) * yy * yy
    yy = ( xvg4 + xvg3 ) / de
    yyi41 = xk2 * ( fn1 /de) * yy * yy
    yyi42 = xk2 * ((fn2+fn1)/de) * yy * yy
    yyi43 = xk2 * ((fn3+fn2)/de) * yy * yy
    yyi44 = xk2 * ((fn4+fn3)/de) * yy * yy
    yyi45 = xk2 * ((fn5+fn4)/de) * yy * yy
    yy = ( xvg5 + xvg4 ) / de
    yyi51 = xk2 * ( fn1 /de) * yy * yy
    yyi52 = xk2 * ((fn2+fn1)/de) * yy * yy
    yyi53 = xk2 * ((fn3+fn2)/de) * yy * yy
    yyi54 = xk2 * ((fn4+fn3)/de) * yy * yy
    yyi55 = xk2 * ((fn5+fn4)/de) * yy * yy
    write(if,2040)
    write(if,1020) zero,fn1,fn2,fn3,fn4
    write(if,1022)  fn1,fn2,fn3,fn4,fn5
    write(if,1024)
    write(if,1000) zero,yyi11,yyi12,yyi13,yyi14,yyi15
    write(if,1030) xvg1,yyi20
    write(if,1026)
    write(if,1000) xvg1,yyi21,yyi22,yyi23,yyi24,yyi25
    write(if,1010) xvg2
    write(if,1026)
    write(if,1000) xvg2,yyi31,yyi32,yyi33,yyi34,yyi35
    write(if,1010) xvg3
    write(if,1026)
    write(if,1000) xvg3,yyi41,yyi42,yyi43,yyi44,yyi45
    write(if,1010) xvg4
    write(if,1026)
    write(if,1000) xvg4,yyi51,yyi52,yyi53,yyi54,yyi55
    write(if,1010) xvg5
    write(if,1026)
!
!     --- CALCUL DU FACTEUR GLOBAL D'INTENSITE ---
!
    sphi = xxi15*yyi15 + xxi14*yyi14 + xxi13*yyi13 + xxi12*yyi12 + xxi11*yyi11
    spwi = xxi25*yyi25 + xxi24*yyi24 + xxi23*yyi23 + xxi22*yyi22 + xxi21*yyi21 + xxi11*yyi20 + xx&
           &i35*yyi35 + xxi34*yyi34 + xxi33*yyi33 + xxi32*yyi32 + xxi31*yyi31 + xxi45*yyi45 + xxi&
           &44*yyi44 + xxi43*yyi43 + xxi42*yyi42 + xxi41*yyi41 + xxi55*yyi55 + xxi54*yyi54 + xxi5&
           &3*yyi53 + xxi52*yyi52 + xxi51*yyi51
    w = spwi * spwi / ( spwi + sphi )
    write(if,2050) sphi
    write(if,2060) spwi
    write(if,2070) w
    if (sphi .lt. spwi) then
        iret = 10
        call u2mess('A', 'ALGORITH11_6')
    endif
!
    1000 format(1p,1x,e12.5,' !',5(1x,e12.5))
    1010 format(1p,1x,e12.5,' !')
    1020 format(1p,'   FORCE_NORM  ',5(1x,e12.5))
    1022 format(1p,15x,5(1x,e12.5))
    1024 format('   VITE_GLIS  ',66('-'))
    1026 format(14x,66('-'))
    1030 format(1p,1x,e12.5,' !',1x,e12.5)
    2000 format(/,1p,'===> FORCE NORMALE MAXIMUM      : ',e12.5)
    2010 format(  1p,'     VITESSE GLISSEMENT MAXIMUM : ',e12.5)
    2020 format(/,'===> NOMBRE TOTAL DE COUPLES : ',i8)
    2030 format(/,'===> POURCENTAGES OBTENUS POUR CHAQUE CLASSE :')
    2040 format(/,'===> FACTEUR DE PONDERATION :')
    2050 format(/,1p,'===> SOMME DE LA CATEGORIE "IMPACTS-ECROUISSAGE" : '&
     &           ,e12.5)
    2060 format(1p,5x,'SOMME DE LA CATEGORIE "GLISSEMENT"          : '&
     &         ,e12.5)
    2070 format(1p,5x,'FACTEUR GLOBAL INTENSITE D''USURE            : '&
     &         ,e12.5)
!
9999  continue
end subroutine
