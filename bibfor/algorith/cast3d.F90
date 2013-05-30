subroutine cast3d(proj, gamma, dh, def, nno,&
                  kpg, nub, nu, dsidep, calbn,&
                  bn, jac, matuu)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!     CALCUL DES TERMES DE STABILISATION POUR LE HEXA8 SOUS INTEGRE
!     STABILISE PAR LA METHODE ASSUMED STRAIN => HEXAS8
!-----------------------------------------------------------------------
!
    implicit none
!
    include 'asterfort/caatdb.h'
    logical :: calbn
    integer :: kpg, i, j, k, nno, proj, ic, iadpg
    real(kind=8) :: dsidep(6, 6), bn(6, 3, 8)
    real(kind=8) :: gamma(4, 8), dh(4, 24)
    real(kind=8) :: jac, xf(8), yf(8), zf(8)
    real(kind=8) :: matuu(*)
    real(kind=8) :: nub, nu, unt, deut, def(6, 3, 8)
    real(kind=8) :: x12(8), y12(8), x13(8), z13(8), y23(8), z23(8)
    real(kind=8) :: x14(8), y24(8), z34(8)
    real(kind=8) :: x2(8), y1(8), x3(8), z1(8), y3(8), z2(8)
!
!    PROJ : INDICATEUR DE LA PROJECTION
!           0 AUCUNE
!           1 ADS
!           2 ASBQI
!
    iadpg = 3*(kpg-1)
!
!   CALCUL DE TERMES INTERMEDIAIRES
!
    do 1 i = 1, nno
        xf(i) = 0.d0
        yf(i) = 0.d0
        zf(i) = 0.d0
 1  end do
!
    do 2 ic = 1, 4
        do 3 i = 1, nno
            xf(i) = xf(i) + dh(ic,iadpg+1) * gamma(ic,i)
            yf(i) = yf(i) + dh(ic,iadpg+2) * gamma(ic,i)
            zf(i) = zf(i) + dh(ic,iadpg+3) * gamma(ic,i)
 3      continue
 2  end do
!
    do 4 i = 1, 6
        do 4 j = 1, 3
            do 4 k = 1, nno
                bn(i,j,k) =0.d0
 4          continue
!
!         HEXAS8 SANS PROJECTION
!         ----------------------
!
    if (proj .eq. 0) then
        do 5 i = 1, nno
            bn(1,1,i) = xf(i)
            bn(2,2,i) = yf(i)
            bn(3,3,i) = zf(i)
            bn(4,1,i) = yf(i)
            bn(5,1,i) = zf(i)
            bn(4,2,i) = xf(i)
            bn(6,2,i) = zf(i)
            bn(5,3,i) = xf(i)
            bn(6,3,i) = yf(i)
 5      continue
!
    else if (proj.eq.1.or.proj.eq.2) then
        do 100 i = 1, 8
            x12(i) = 0.d0
            y12(i) = 0.d0
            x13(i) = 0.d0
            z13(i) = 0.d0
            y23(i) = 0.d0
            z23(i) = 0.d0
100      end do
!
!   CALCUL DE X12 Y12 Y13 Z13 Y23 Z23
!
        do 6 ic = 1, 2
            do 7 i = 1, nno
                x12(i) = x12(i) + dh(ic,iadpg+1) * gamma(ic,i)
                y12(i) = y12(i) + dh(ic,iadpg+2) * gamma(ic,i)
 7          continue
 6      end do
!
        do 8 ic = 1, 3, 2
            do 9 i = 1, nno
                x13(i) = x13(i) + dh(ic,iadpg+1) * gamma(ic,i)
                z13(i) = z13(i) + dh(ic,iadpg+3) * gamma(ic,i)
 9          continue
 8      end do
!
        do 10 ic = 2, 3
            do 11 i = 1, nno
                y23(i) = y23(i) + dh(ic,iadpg+2) * gamma(ic,i)
                z23(i) = z23(i) + dh(ic,iadpg+3) * gamma(ic,i)
11          continue
10      end do
!
!    ADS
!
        if (proj .eq. 1) then
            unt = 1.d0/3.d0
            deut = 2.d0/3.d0
            do 12 i = 1, nno
                bn(1,1,i) = deut * xf(i)
                bn(2,2,i) = deut * yf(i)
                bn(3,3,i) = deut * zf(i)
                bn(2,1,i) = -unt * xf(i)
                bn(3,1,i) = bn(2,1,i)
                bn(1,2,i) = -unt * yf(i)
                bn(3,2,i) = bn(1,2,i)
                bn(1,3,i) = -unt * zf(i)
                bn(2,3,i) = bn(1,3,i)
                bn(4,1,i) = y12(i)
                bn(4,2,i) = x12(i)
                bn(5,1,i) = z13(i)
                bn(5,3,i) = x13(i)
                bn(6,2,i) = z23(i)
                bn(6,3,i) = y23(i)
12          continue
!
!   ASQBI
!
        else if (proj.eq.2) then
            do 200 i = 1, 8
                x14(i)= 0.d0
                y24(i)= 0.d0
                z34(i)= 0.d0
200          end do
!
            do 13 ic = 1, 4, 3
                do 14 i = 1, nno
                    x14(i) = x14(i) + dh(ic,iadpg+1) * gamma(ic,i)
14              continue
13          continue
!
            do 15 ic = 2, 4, 2
                do 16 i = 1, nno
                    y24(i) = y24(i) + dh(ic,iadpg+2) * gamma(ic,i)
16              continue
15          continue
!
            do 17 ic = 3, 4
                do 18 i = 1, nno
                    z34(i) = z34(i) + dh(ic,iadpg+3) * gamma(ic,i)
18              continue
17          end do
!
            do 19 i = 1, nno
                x2(i) = dh(2,iadpg+1) * gamma(2,i)
                x3(i) = dh(3,iadpg+1) * gamma(3,i)
                y1(i) = dh(1,iadpg+2) * gamma(1,i)
                y3(i) = dh(3,iadpg+2) * gamma(3,i)
                z1(i) = dh(1,iadpg+3) * gamma(1,i)
                z2(i) = dh(2,iadpg+3) * gamma(2,i)
19          continue
!
            do 30 i = 1, nno
                bn(1,1,i) = xf(i)
                bn(2,1,i) = -nub * x3(i) - nu * x14(i)
                bn(3,1,i) = -nub * x2(i) - nu * x14(i)
                bn(4,1,i) = y12(i)
                bn(5,1,i) = z13(i)
                bn(6,1,i) = 0.0d0
                bn(1,2,i) = -nub * y3(i) - nu * y24(i)
                bn(2,2,i) = yf(i)
                bn(3,2,i) = -nub * y1(i) - nu * y24(i)
                bn(4,2,i) = x12(i)
                bn(5,2,i) = 0.0d0
                bn(6,2,i) = z23(i)
                bn(1,3,i) = -nub * z2(i) - nu * z34(i)
                bn(2,3,i) = -nub * z1(i) - nu * z34(i)
                bn(3,3,i) = zf(i)
                bn(4,3,i) = 0.0d0
                bn(5,3,i) = x13(i)
                bn(6,3,i) = y23(i)
30          continue
        endif
    endif
    if (.not.calbn) then
        call caatdb(nno, bn, dsidep, bn, jac,&
                    matuu)
        call caatdb(nno, bn, dsidep, def, jac,&
                    matuu)
        call caatdb(nno, def, dsidep, bn, jac,&
                    matuu)
    endif
end subroutine
