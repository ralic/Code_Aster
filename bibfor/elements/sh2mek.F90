subroutine sh2mek(xetemp, sigma, re)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!               ELEMENT SHB20
!
    implicit none
#include "jeveux.h"
#include "asterfort/chrp3d.h"
#include "asterfort/r8inir.h"
#include "asterfort/rloshb.h"
#include "asterfort/s2calb.h"
#include "asterfort/sh2ksi.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: iprope
    real(kind=8) :: xe(60), re(60, 60)
    real(kind=8) :: xcoq(3, 4), bksip(3, 20, 20), b(3, 20)
    real(kind=8) :: xcent(3), ppp(3, 3), tmpke2(60, 60)
    real(kind=8) :: xl(3, 4), xxx(3), yyy(3), tmpke(60, 60)
    real(kind=8) :: xxg5(20), xyg5(20), xzg5(20), pxg5(20)
    real(kind=8) :: sigloc(6), sigma(*)
!
    real(kind=8) :: sigmag(6)
!
    real(kind=8) :: sitmp1(20, 20), sitmp2(20, 20)
    real(kind=8) :: xetemp(*)
!
!
! ON DEFINIT LES POINTS DE GAUSS ET LES POIDS
!
!-----------------------------------------------------------------------
    integer :: i, ip, iz, j, kk
    real(kind=8) :: ajac, rbid, zeta, zlamb
!-----------------------------------------------------------------------
    xzg5(1) = -0.906179845938664d0
    xzg5(2) = -0.538469310105683d0
    xzg5(3) = 0.d0
    xzg5(4) = 0.538469310105683d0
    xzg5(5) = 0.906179845938664d0
!
    pxg5(1) = 0.236926885056189d0
    pxg5(2) = 0.478628670499366d0
    pxg5(3) = 0.568888888888889d0
    pxg5(4) = 0.478628670499366d0
    pxg5(5) = 0.236926885056189d0
!
    do 10 iz = 1, 5
        xxg5(iz) = -0.577350269189625d0
        xxg5(iz+5) = 0.577350269189625d0
        xxg5(iz+10) = 0.577350269189625d0
        xxg5(iz+15) = -0.577350269189625d0
        xyg5(iz) = -0.577350269189625d0
        xyg5(iz+5) = -0.577350269189625d0
        xyg5(iz+10) = 0.577350269189625d0
        xyg5(iz+15) = 0.577350269189625d0
        xzg5(iz+5) = xzg5(iz)
        pxg5(iz+5) = pxg5(iz)
        xzg5(iz+10) = xzg5(iz)
        pxg5(iz+10) = pxg5(iz)
        xzg5(iz+15) = xzg5(iz)
        pxg5(iz+15) = pxg5(iz)
10  end do
!
    do 20 i = 1, 60
        xe(i) = xetemp(i)
20  end do
!C
!C CALCUL DE B (1 2 3) AUX 5 POINTS DE GAUSS
!C
    call sh2ksi(20, xxg5, xyg5, xzg5, bksip)
    do 40 j = 1, 20
        do 30 i = 1, 20
            sitmp2(i,j) = 0.d0
30      continue
40  continue
!C
!C DEBUT DE LA BOUCLE SUR LES 20 PTS GAUSS
!C
    do 140 ip = 1, 20
!C
!C CALCUL DE B
!C
        call s2calb(bksip(1, 1, ip), xe, b, ajac)
!C
!C CALCUL DE MATRICE DE PASSAGE POUR POUVOIR CALCULER LES CONTRAINTES
!C DANS LE REPERE GLOBAL
!C
        do 50 i = 1, 6
!C LES CONTRAINTES LOCALES POUR POUVOIR TRAITER LA PLASTICITE AVANT
            sigloc(i)=sigma((ip-1)*6+i)
50      continue
        zeta = xzg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 70 i = 1, 4
            do 60 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(3*i+ 9+j)
60          continue
70      continue
!
        call rloshb(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!C
!C PASSAGE DES CONTRAINTES AU REPERE GLOBAL
!C
        call chrp3d(ppp, sigloc, sigmag, 1)
        do 90 j = 1, 20
            do 80 i = 1, 20
                sitmp1(i,j) = 0.d0
80          continue
90      continue
!
        do 110 j = 1, 20
            do 100 i = 1, 20
                sitmp1(i,j) = sigmag(1)*b(1,i)*b(1,j) + sigmag(2)*b(2, i)*b(2,j) + sigmag(3)*b(3,&
                              &i)*b(3,j) + sigmag(4)*(b(1, i)*b(2,j)+b(2,i)*b(1,j)) + sigmag(6)*(&
                              &b(1,i)*b(3,j)+b( 3,i)*b(1,j)) + sigmag(5)*(b(3,i)*b(2,j)+b(2,i)*b(&
                              &3,j))
100          continue
110      continue
!
        do 130 j = 1, 20
            do 120 i = 1, 20
                sitmp2(i,j) = sitmp2(i,j) + ajac*pxg5(ip)*sitmp1(i,j)
120          continue
130      continue
140  continue
    call r8inir(3600, 0.d0, tmpke, 1)
    do 170 kk = 1, 3
        do 160 i = 1, 20
            do 150 j = 1, 20
                tmpke(i+(kk-1)*20,j+(kk-1)*20) = sitmp2(i,j)
150          continue
160      continue
170  continue
!C
!C ON MET DE L'ORDRE:
!
    call r8inir(3600, 0.d0, tmpke2, 1)
    do 190 j = 1, 20
        do 180 i = 1, 60
            tmpke2(i,(j-1)*3+1)=tmpke(i,j)
            tmpke2(i,(j-1)*3+2)=tmpke(i,j+20)
            tmpke2(i,(j-1)*3+3)=tmpke(i,j+40)
180      continue
190  continue
!
    call r8inir(3600, 0.d0, tmpke, 1)
    do 210 i = 1, 20
        do 200 j = 1, 60
            tmpke((i-1)*3+1,j)=tmpke2(i,j)
            tmpke((i-1)*3+2,j)=tmpke2(i+20,j)
            tmpke((i-1)*3+3,j)=tmpke2(i+40,j)
200      continue
210  continue
!
    iprope = 1
!
    if (iprope .eq. 0) then
        call dcopy(3600, tmpke, 1, re, 1)
    endif
!
    if (iprope .eq. 1) then
        call daxpy(3600, 1.d0, tmpke, 1, re,&
                   1)
    endif
!
end subroutine
