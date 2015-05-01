subroutine sh1eps(xetemp, xidepp, deploc, propel)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!               ELEMENT SHB15
!
    implicit none
#include "jeveux.h"
#include "asterfort/chrp3d.h"
#include "asterfort/dsdx3d.h"
#include "asterfort/mulmat.h"
#include "asterfort/rlosh6.h"
#include "asterfort/s1calb.h"
#include "asterfort/sh1ksi.h"
    real(kind=8) :: xe(45), xidepp(*)
    real(kind=8) :: xxg5(15), xyg5(15), xzg5(15)
    real(kind=8) :: xcoq(3, 3), bksip(3, 15, 15), b(3, 15)
    real(kind=8) :: xcent(3), ppp(3, 3), pppt(3, 3)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3)
    real(kind=8) :: deps(6), ue(3, 15)
    real(kind=8) :: depslo(6), deploc(*), propel(*), rr2(3, 3)
    real(kind=8) :: xetemp(*), rr12(3, 3), dusx(9)
!
!
! INITIALISATIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INFOS:
! XE EST RANGE COMME CA: (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2
!... ZNOEUD2)
! DANS SHB15_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! ON DEFINIT LES POINTS GAUSS ET LES POIDS
!
! 5 points sur la facette 1-2-3:
!
!
!-----------------------------------------------------------------------
    integer :: i, ip, j
    real(kind=8) :: ajac, rbid, zeta, zlamb
!-----------------------------------------------------------------------
    do 10 ip = 1, 5
        xzg5(ip) = 0.5d0
        xyg5(ip) = 0.5d0
        xzg5(ip+5) = 0.5d0
        xyg5(ip+5) = 0.d0
        xzg5(ip+10) = 0.d0
        xyg5(ip+10) = 0.5d0
10  continue
!
    do 20 ip = 1, 3
        xxg5(5*(ip-1)+1) = -0.906179845938664d0
        xxg5(5*(ip-1)+2) = -0.538469310105683d0
        xxg5(5*(ip-1)+3) = 0.d0
        xxg5(5*(ip-1)+4) = 0.538469310105683d0
        xxg5(5*(ip-1)+5) = 0.906179845938664d0
20  continue
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 30 i = 1, 45
        xe(i) = xetemp(i)
30  continue
!
!C
!C UE: INCREMENT DE DEPLACEMENT NODAL, REPERE GLOBAL
!C
!C XE: DEBUT DU PAS
    do 290 j = 1, 15
        do 280 i = 1, 3
            ue(i,j)=xidepp((j-1)*3+i)
280      continue
290  continue
!
!
!C CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
!C      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!C      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!C      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!C
    call sh1ksi(15, xxg5, xyg5, xzg5, bksip)
!C
    do 380 ip = 1, 15
!C
!C DEFINITION DES 4 POINTS  COQUES
!C
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 310 i = 1, 3
            do 300 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(3*i+ 6+j)
300          continue
310      continue
!C
!C CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
!C XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!C
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!C
!C CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
!C
        call s1calb(bksip(1, 1, ip), xe, b, ajac)
!C
!C CALCUL DE EPS DANS LE REPERE GLOBAL: 1 POUR DEFORMATIONS LINEAIRES
!C                                     2 POUR TERMES CARRES EN PLUS
        do 320 i = 1, 6
            deps(i)=0.d0
320      continue
        call dsdx3d(1, b, ue, deps, dusx,&
                    15)
!C
!C SORTIE DE DUSDX DANS PROPEL(1 A 9 * 15 PT DE GAUSS)
!C POUR UTILISATION ULTERIEURE DANS Q8PKCN_SHB8
!C
        do 340 i = 1, 3
            do 330 j = 1, 3
                pppt(j,i) = ppp(i,j)
330          continue
340      continue
        rr12(1,1) = dusx(1)
        rr12(1,2) = dusx(2)
        rr12(1,3) = dusx(3)
        rr12(2,1) = dusx(4)
        rr12(2,2) = dusx(5)
        rr12(2,3) = dusx(6)
        rr12(3,1) = dusx(7)
        rr12(3,2) = dusx(8)
        rr12(3,3) = dusx(9)
        call mulmat(3, 3, 3, pppt, rr12,&
                    rr2)
        call mulmat(3, 3, 3, rr2, ppp,&
                    rr12)
        dusx(1) = rr12(1,1)
        dusx(2) = rr12(1,2)
        dusx(3) = rr12(1,3)
        dusx(4) = rr12(2,1)
        dusx(5) = rr12(2,2)
        dusx(6) = rr12(2,3)
        dusx(7) = rr12(3,1)
        dusx(8) = rr12(3,2)
        dusx(9) = rr12(3,3)
!
        do 350 i = 1, 9
            propel(i+(ip-1)*9)=dusx(i)
350      continue
        do 360 i = 1, 6
            depslo(i) = 0.d0
360      continue
        call chrp3d(ppp, deps, depslo, 2)
!C
!C
!C CONTRAINTES ECRITES SOUS LA FORME:
!C               [SIG] = [S_11, S_22, S_33, S_12, S_23, S_13]
        do 370 i = 1, 6
!C ON LAISSE LES CONTRAINTES DANS LE REPERE LOCAL POUR LA PLASTICITE
            deploc((ip-1)*6+i)=depslo(i)
370      continue
380  continue
end subroutine
