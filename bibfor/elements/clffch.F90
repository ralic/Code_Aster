subroutine clffch(alias, type, nno, xi, yi,&
                  zi, xin, yin, zin, tn,&
                  ajx, ajy, ajz, bjxx, bjyy,&
                  bjzz, bjxy, bjxz, bjyz, ider)
! ----------------------------------------------------------------------
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
!
!......................................................................C
!......................................................................C
!                                                                      C
! BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES           C
!        AU POINT DE COORDONNEES XI,YI,ZI                              C
!                                                                      C
! ATTENTION: CETTE ROUTINE EST SPECIFIQUE DES ELEMENTS HOMOGENEISE     C
!                            INI100                                    C
!                                                                      C
! ENTREES                                                              C
!      NNO         : NOMBRE DE NOEUDS                                  C
!      ALIAS       : NOM D'ALIAS DE L'ELEMENT :HEXA20 OU HEXA8         C
!      TYPE        : TYPE DE LA FONCTION DE FORME : POUTRE OU FLUIDE   C
!      XI,YI,ZI    : POINT DE CALCUL DES F FORMES ET DERIVEES          C
!      XIN,YIN,ZIN : COORDONNEES INTRINSEQUES                          C
!      IDER        : INDICATEUR DE CALCUL DES DERIVEES                 C
!                    IDER = 0 CALCUL DE TN                             C
!                    IDER = 1 CALCUL DE TN, AJ*                        C
!                    IDER = 2 CALCUL DE TN AJ* BJ*                     C
!                                                                      C
! SORTIES                                                              C
!      TN  : FONCTIONS DE FORMES EN XI,YI,ZI                           C
!      AJ (AJX, AJY, AJZ) : DERIVEES DES F FORMES EN XI,YI,ZI          C
!      BJ (BJXX ... BJYZ) : DERIVEES SECONDES DES F FORMES EN XI,YI,ZI C
!......................................................................C
!......................................................................C
!
    implicit     none
#include "asterfort/u2mess.h"
    character(len=6) :: alias, type
    real(kind=8) :: tn(1), ajx(1), ajy(1), ajz(1), xin(1), yin(1), zin(1)
    real(kind=8) :: bjxx(1), bjyy(1), bjzz(1), bjxy(1), bjxz(1), bjyz(1), xi, yi
    real(kind=8) :: zi
    integer :: ider, nno
!----------------------------------------------------------------------
    real(kind=8) :: x0, y0, z0, fxy, fxydx, fxydy, fxydxy, fz, fzdz, fzd2z, f
    real(kind=8) :: fdx, fdy, fdz
    integer :: i
!----------------------------------------------------------------------
!
    if (type .eq. 'POUTRE') then
!
!     -------------------------------------------------------------
!     --- F. DE FORME ASSOCIEES AUX DDLS DE FLEXION DES POUTRES ---
!     -------------------------------------------------------------
!
        do 10 i = 1, nno
!
            x0 = xi*xin(i)
            y0 = yi*yin(i)
            z0 = zi*zin(i)
!
            fxy = (1.d0+x0) * (1.d0+y0) * 0.25d0
            fxydx = xin(i) * (1.d0+y0) * 0.25d0
            fxydy = (1.d0+x0) * yin(i) * 0.25d0
            fxydxy = xin(i) * yin(i) * 0.25d0
!
!          FONCTIONS DE FORME A DERIVEE NULLE AU BORD
!
            fz = - 0.25d0 * z0*z0*z0 + 0.75d0 * z0 + 0.5d0
            fzdz = - 0.75d0 * z0*z0*zin(i) + 0.75d0 * zin(i)
            fzd2z = - 1.5d0 * z0*zin(i)*zin(i)
!
            tn(i) = fxy * fz
!
            if (ider .gt. 0) then
                ajx(i) = fxydx * fz
                ajy(i) = fxydy * fz
                ajz(i) = fxy * fzdz
            endif
!
            if (ider .gt. 1) then
                bjxx(i) = 0.d0
                bjyy(i) = 0.d0
                bjzz(i) = fxy * fzd2z
                bjxy(i) = fxydxy * fz
                bjxz(i) = fxydx * fzdz
                bjyz(i) = fxydy * fzdz
            endif
!
!          FONCTIONS DE FORME A VALEUR NULLE AU BORD
!
            fz = 0.25d0*( -zin(i) - zi + zin(i)*zi*zi + zi*zi*zi )
            fzdz = 0.25d0*( -1.d0 + 2.d0*zin(i)*zi + 3.d0*zi*zi )
            fzd2z = 0.25d0*( 2.d0*zin(i) + 6.d0*zi )
!
            tn(i+8) = fxy * fz
!
            if (ider .gt. 0) then
                ajx(i+8) = fxydx * fz
                ajy(i+8) = fxydy * fz
                ajz(i+8) = fxy * fzdz
            endif
!
            if (ider .gt. 1) then
                bjxx(i+8) = 0.d0
                bjyy(i+8) = 0.d0
                bjzz(i+8) = fxy * fzd2z
                bjxy(i+8) = fxydxy * fz
                bjxz(i+8) = fxydx * fzdz
                bjyz(i+8) = fxydy * fzdz
            endif
10      continue
!
    else if (type .eq. 'FLUIDE') then
!     --------------------------------------------------------------
        if (alias .eq. 'HEXA20') then
!
!     --------------------------------------------------------------
!     --- FONCTIONS DE FORMES AUX DDL FLUIDE DE LA MAILLE HEXA20 ---
!     --------------------------------------------------------------
!
            do 20 i = 1, nno
                x0 = xi*xin(i)
                y0 = yi*yin(i)
                z0 = zi*zin(i)
!
                if (i .le. 8) then
                    f = 0.125d0 * (1.d0+x0) * (1.d0+y0) * (1.d0+z0) * (x0+y0+z0-2.d0)
                    fdx = 0.125d0 * (1.d0+y0) * (1.d0+z0) * (2.d0*xi + xin(i)*(z0+y0-1.d0))
                    fdy = 0.125d0 * (1.d0+x0) * (1.d0+z0) * (2.d0*yi + yin(i)*(x0+z0-1.d0))
                    fdz = 0.125d0 * (1.d0+x0) * (1.d0+y0) * (2.d0*zi + zin(i)*(x0+y0-1.d0))
!
                    else if ((i.eq.9).or.(i.eq.11).or. (i.eq.17).or.(&
                i.eq.19)) then
                    f = 0.25d0 * (1.d0-xi*xi) * (1.d0+y0) * (1.d0+z0)
                    fdx = -0.5d0 * xi * (1.d0+y0) * (1.d0+z0)
                    fdy = 0.25d0 * (1.d0-xi*xi) * yin(i) * (1.d0+z0)
                    fdz = 0.25d0 * (1.d0-xi*xi) * (1.d0+y0) * zin(i)
!
                    else if ((i.eq.10).or.(i.eq.12).or. (i.eq.18).or.(&
                i.eq.20)) then
                    f = 0.25d0 * (1.d0-yi*yi) * (1.d0+x0) * (1.d0+z0)
                    fdy = -0.5d0 * yi * (1.d0+x0) * (1.d0+z0)
                    fdx = 0.25d0 * (1.d0-yi*yi) * xin(i) * (1.d0+z0)
                    fdz = 0.25d0 * (1.d0-yi*yi) * (1.d0+x0) * zin(i)
!
                    else if ((i.eq.13).or.(i.eq.14).or. (i.eq.15).or.(&
                i.eq.16)) then
                    f = 0.25d0 * (1.d0-zi*zi) * (1.d0+x0) * (1.d0+y0)
                    fdz = -0.5d0 * zi * (1.d0+x0) * (1.d0+y0)
                    fdx = 0.25d0 * (1.d0-zi*zi) * xin(i) * (1.d0+y0)
                    fdy = 0.25d0 * (1.d0-zi*zi) * (1.d0+x0) * yin(i)
                endif
!
                tn(i) = f
                if (ider .gt. 0) then
                    ajx(i) = fdx
                    ajy(i) = fdy
                    ajz(i) = fdz
                endif
20          continue
!
        else if (alias .eq. 'HEXA8 ') then
!
!     ----------------------------------------------------
!     --- FONCTIONS DE FORMES ASSOCIEES A LA GEOMETRIE ---
!     ---    ET AUX DDL FLUIDE DE LA MAILLE HEXA8      ---
!     ----------------------------------------------------
!
            do 30 i = 1, nno
                x0 = xi*xin(i)
                y0 = yi*yin(i)
                z0 = zi*zin(i)
!
                tn(i) = (1.d0+x0) * (1.d0+y0) * (1.d0+z0) * 0.125d0
!
                if (ider .gt. 0) then
                    ajx(i) = xin(i) * (1.d0+y0) * (1.d0+z0) * 0.125d0
                    ajy(i) = yin(i) * (1.d0+x0) * (1.d0+z0) * 0.125d0
                    ajz(i) = zin(i) * (1.d0+x0) * (1.d0+y0) * 0.125d0
                endif
                if (ider .gt. 1) then
                    bjxx(i) = 0.d0
                    bjyy(i) = 0.d0
                    bjzz(i) = 0.d0
                    bjxy(i) = xin(i) * yin(i) * (1.d0+z0) * 0.125d0
                    bjxz(i) = xin(i) * zin(i) * (1.d0+y0) * 0.125d0
                    bjyz(i) = yin(i) * zin(i) * (1.d0+x0) * 0.125d0
                endif
30          end do
        endif
!     ---------------------------------------------------------
    else
        call u2mess('F', 'ELEMENTS_20')
    endif
!
end subroutine
