subroutine sh2for(xetemp, sigma, xivect)
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
#include "asterfort/mulmat.h"
#include "asterfort/r8inir.h"
#include "asterfort/rloshb.h"
#include "asterfort/s2calb.h"
#include "asterfort/sh2ksi.h"
    real(kind=8) :: xivect(*), xetemp(*)
    real(kind=8) :: xe(60), sigma(*)
    real(kind=8) :: xcoq(3, 4), bksip(3, 20, 20), b(3, 20)
    real(kind=8) :: xcent(3), ppp(3, 3)
    real(kind=8) :: xl(3, 4), xxx(3), yyy(3)
    real(kind=8) :: xxg5(20), xyg5(20), xzg5(20), pxg5(20)
    real(kind=8) :: sigloc(6), sitmp2(20, 20)
    real(kind=8) :: f(3, 20), sigmag(6), poids
!
! INFOS:
! XE EST RANGE COMME CA: (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2
!... ZNOEUD2)
! DANS SHB15_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! ON DEFINI LES POINTS GAUSS ET LES POIDS
!
!-----------------------------------------------------------------------
    integer :: i, ip, iz, j, k
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
    do 71 iz = 1, 5
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
71  continue
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 60
        xe(i) = xetemp(i)
10  continue
!
    call r8inir(400, 0.d0, sitmp2, 1)
    do 400 j = 1, 20
        do 390 i = 1, 3
            f(i,j) = 0.d0
390      continue
400  continue
!
! CALCUL DE BKSIP(3,15,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!
    call sh2ksi(20, xxg5, xyg5, xzg5, bksip)
!
    do 460 ip = 1, 20
!
! RECHERCHE DE SIGMA DU POINT DE GAUSS GLOBAL
!
        do 409 i = 1, 6
            sigloc(i)=sigma((ip-1)*6+i)
409      continue
        zeta = xzg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 420 i = 1, 4
            do 410 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(3*i+ 9+j)
410          continue
420      continue
        call rloshb(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
! PASSAGE DES CONTRAINTES AU REPERE GLOBAL
!
        call chrp3d(ppp, sigloc, sigmag, 1)
        call s2calb(bksip(1, 1, ip), xe, b, ajac)
!
! CALCUL DE B.SIGMA EN GLOBAL
!
        poids = ajac*pxg5(ip)
        do 450 k = 1, 20
            f(1,k) = f(1,k) + poids * (b(1,k)*sigmag(1)+b(2,k)*sigmag( 4)+b(3,k)*sigmag(6))
            f(2,k) = f(2,k) + poids * (b(1,k)*sigmag(4)+b(2,k)*sigmag( 2)+b(3,k)*sigmag(5))
            f(3,k) = f(3,k) + poids * (b(1,k)*sigmag(6)+b(2,k)*sigmag( 5)+b(3,k)*sigmag(3))
450      continue
!
! SI LAGRANGIEN TOTAL: AJOUT DE FQ A F
!
460  continue
!
! ATTENTION A L'ORDRE DE XIVECT
!
    do 560 i = 1, 3
        do 550 j = 1, 20
            xivect((j-1)*3+i) = f(i,j)
550      continue
560  continue
!
end subroutine
