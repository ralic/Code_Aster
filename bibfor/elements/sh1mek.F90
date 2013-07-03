subroutine sh1mek(xetemp, sigma, re)
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
!               ELEMENT SHB15
!
    implicit none
#include "jeveux.h"
#include "asterfort/chrp3d.h"
#include "asterfort/r8inir.h"
#include "asterfort/rlosh6.h"
#include "asterfort/s1calb.h"
#include "asterfort/sh1ksi.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: iprope
    real(kind=8) :: xe(45), re(45, 45)
    real(kind=8) :: xcoq(3, 3), bksip(3, 15, 15), b(3, 15)
    real(kind=8) :: xcent(3), ppp(3, 3), tmpke2(45, 45)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3), tmpke(45, 45)
    real(kind=8) :: xxg5(15), xyg5(15), xzg5(15), pxg5(15)
    real(kind=8) :: sigloc(6), sigma(*)
!
    real(kind=8) :: sigmag(6)
!
    real(kind=8) :: sitmp1(15, 15), sitmp2(15, 15)
    real(kind=8) :: xetemp(*)
!
!
! ON DEFINI LES POINTS GAUSS ET LES POIDS
!
! 5 points sur la facette 1-2-3:
!
!
!-----------------------------------------------------------------------
    integer :: i, ip, j, kk
    real(kind=8) :: ajac, rbid, zeta, zlamb
!-----------------------------------------------------------------------
    do 10 ip = 1, 5
        xzg5(ip) = 0.5d0
        xyg5(ip) = 0.5d0
        xzg5(ip+5) = 0.5d0
        xyg5(ip+5) = 0.d0
        xzg5(ip+10) = 0.d0
        xyg5(ip+10) = 0.5d0
10  end do
!
    do 20 ip = 1, 3
        xxg5(5*(ip-1)+1) = -0.906179845938664d0
        xxg5(5*(ip-1)+2) = -0.538469310105683d0
        xxg5(5*(ip-1)+3) = 0.d0
        xxg5(5*(ip-1)+4) = 0.538469310105683d0
        xxg5(5*(ip-1)+5) = 0.906179845938664d0
!
        pxg5(5*(ip-1)+1) = 0.236926885056189d0/6.d0
        pxg5(5*(ip-1)+2) = 0.478628670499366d0/6.d0
        pxg5(5*(ip-1)+3) = 0.568888888888889d0/6.d0
        pxg5(5*(ip-1)+4) = 0.478628670499366d0/6.d0
        pxg5(5*(ip-1)+5) = 0.236926885056189d0/6.d0
20  end do
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 30 i = 1, 45
        xe(i) = xetemp(i)
30  end do
!
! TYPE DE LOI DE COMPORTEMENT:
!     IRDC = 1 : SHB6 MEME TYPE QUE SHB8 DANS PLEXUS
!     IRDC = 2 : C.P.
!     IRDC = 3 : 3D COMPLETE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C CALCUL DE K.SIGMA                                                C
!C          EN ENTREE DANS WORK : SIGMA NORMALEMENT LONGUEUR 30     C
!C                         PROPEL(1): 1 POUR RE=RE+KSIGMA           C
!C                         PROPEL(1): 0 POUR RE=KSIGMA              C
!C             SORTIE           : RE(45*45)=RE(45*45)+KSIGMA        C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C
!C CALCUL DE B (1 2 3) AUX 5 POINTS DE GAUSS
!C
    call sh1ksi(15, xxg5, xyg5, xzg5, bksip)
    do 50 j = 1, 15
        do 40 i = 1, 15
            sitmp2(i,j) = 0.d0
40      continue
50  continue
!C
!C DEBUT DE LA BOUCLE SUR LES 5 PTS GAUSS
!C
    do 150 ip = 1, 15
!C
!C CALCUL DE B
!C
        call s1calb(bksip(1, 1, ip), xe, b, ajac)
!C
!C CALCUL DE MATRICE DE PASSAGE POUR POUVOIR CALCULER LES CONTRAINTES
!C DANS LE REPERE GLOBAL
!C
        do 60 i = 1, 6
!C CONTRAINTES LOCALES POUR POUVOIR TRAITER LA PLASTICITE AVANT
            sigloc(i)=sigma((ip-1)*6+i)
60      continue
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 80 i = 1, 3
            do 70 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(3*i+ 6+j)
70          continue
80      continue
!
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!C
!C PASSAGE DES CONTRAINTES AU REPERE GLOBAL
!C
        call chrp3d(ppp, sigloc, sigmag, 1)
        do 100 j = 1, 15
            do 90 i = 1, 15
                sitmp1(i,j) = 0.d0
90          continue
100      continue
!
        do 120 j = 1, 15
            do 110 i = 1, 15
                sitmp1(i,j) = sigmag(1)*b(1,i)*b(1,j) + sigmag(2)*b(2, i)*b(2,j) + sigmag(3)*b(3,&
                              &i)*b(3,j) + sigmag(4)*(b(1, i)*b(2,j)+b(2,i)*b(1,j)) + sigmag(6)*(&
                              &b(1,i)*b(3,j)+b( 3,i)*b(1,j)) + sigmag(5)*(b(3,i)*b(2,j)+b(2,i)*b(&
                              &3,j))
110          continue
120      continue
!
        do 140 j = 1, 15
            do 130 i = 1, 15
                sitmp2(i,j) = sitmp2(i,j) + ajac*pxg5(ip)*sitmp1(i,j)
130          continue
140      continue
150  end do
    call r8inir(2025, 0.d0, tmpke, 1)
    do 180 kk = 1, 3
        do 170 i = 1, 15
            do 160 j = 1, 15
                tmpke(i+(kk-1)*15,j+(kk-1)*15) = sitmp2(i,j)
160          continue
170      continue
180  continue
!C
!C ON MET DE L'ORDRE:
!C
    call r8inir(2025, 0.d0, tmpke2, 1)
    do 200 j = 1, 15
        do 190 i = 1, 45
            tmpke2(i,(j-1)*3+1)=tmpke(i,j)
            tmpke2(i,(j-1)*3+2)=tmpke(i,j+15)
            tmpke2(i,(j-1)*3+3)=tmpke(i,j+30)
190      continue
200  continue
    call r8inir(2025, 0.d0, tmpke, 1)
    do 220 i = 1, 15
        do 210 j = 1, 45
            tmpke((i-1)*3+1,j)=tmpke2(i,j)
            tmpke((i-1)*3+2,j)=tmpke2(i+15,j)
            tmpke((i-1)*3+3,j)=tmpke2(i+30,j)
210      continue
220  continue
!
    iprope = 1
!
    if (iprope .eq. 0) then
        call dcopy(2025, tmpke, 1, re, 1)
    endif
!
    if (iprope .eq. 1) then
        call daxpy(2025, 1.d0, tmpke, 1, re,&
                   1)
    endif
!
end subroutine
