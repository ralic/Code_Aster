subroutine sh2eps(xetemp, xidepp, deploc, propel)
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
!               ELEMENT SHB20
!
    implicit none
#include "jeveux.h"
#include "asterfort/chrp3d.h"
#include "asterfort/dsdx3d.h"
#include "asterfort/mulmat.h"
#include "asterfort/rloshb.h"
#include "asterfort/s2calb.h"
#include "asterfort/sh2ksi.h"
    real(kind=8) :: xe(60), xidepp(*)
    real(kind=8) :: xcoq(3, 4), bksip(3, 20, 20), b(3, 20)
    real(kind=8) :: xcent(3), ppp(3, 3), pppt(3, 3)
    real(kind=8) :: xl(3, 4), xxx(3), yyy(3)
    real(kind=8) :: xetemp(*)
    real(kind=8) :: deps(6), dusx(9), ue(3, 20)
    real(kind=8) :: depslo(6), deploc(*), propel(*)
    real(kind=8) :: rr12(3, 3), rr2(3, 3)
    real(kind=8) :: xxg5(20), xyg5(20), xzg5(20)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INITIALISATIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INFOS:
! XE EST RANGE:(XNOEUD1 YNOEUD1 ZNOEUD1,XNOEUD2 YNOEUD2 ZNOEUD2,..)
! DANS SHB8_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! ON DEFINIT LES POINTS DE GAUSS ET LES POIDS
!
! Des points de gauss sur la facette 1-2-3:
!
!
!-----------------------------------------------------------------------
    integer :: i, ip, iz, j
    real(kind=8) :: ajac, rbid, zeta, zlamb
!-----------------------------------------------------------------------
    xzg5(1) = -0.906179845938664d0
    xzg5(2) = -0.538469310105683d0
    xzg5(3) = 0.d0
    xzg5(4) = 0.538469310105683d0
    xzg5(5) = 0.906179845938664d0
!
!
    do 20 iz = 1, 5
        xxg5(iz) = -0.577350269189625d0
        xxg5(iz+5) = 0.577350269189625d0
        xxg5(iz+10) = 0.577350269189625d0
        xxg5(iz+15) = -0.577350269189625d0
        xyg5(iz) = -0.577350269189625d0
        xyg5(iz+5) = -0.577350269189625d0
        xyg5(iz+10) = 0.577350269189625d0
        xyg5(iz+15) = 0.577350269189625d0
        xzg5(iz+5) = xzg5(iz)
        xzg5(iz+10) = xzg5(iz)
        xzg5(iz+15) = xzg5(iz)
20  continue
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 30 i = 1, 60
        xe(i) = xetemp(i)
30  continue
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                  C
! ON CALCULE LES CONTRAINTES : SORTIE DANS OUT(120)                 C
!                       CONTRAINTES LOCALES DANS CHAQUE COUCHE     C
!                       SUR LA CONFIGURATION 1                     C
!  LE DEPLACEMENT NODAL A L'AIR D'ETRE DANS WORK(1 A 60)           C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! UE: INCREMENT DE DEPLACEMENT NODAL, REPERE GLOBAL
!
! XE: DEBUT DU PAS
    do 250 j = 1, 20
        do 240 i = 1, 3
            ue(i,j)=xidepp((j-1)*3+i)
240      continue
250  continue
!
!
! CALCUL DE BKSIP(3,20,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!
    call sh2ksi(20, xxg5, xyg5, xzg5, bksip)
!
    do 340 ip = 1, 20
!
! DEFINITION DES 4 POINTS  COQUES
!
        zeta = xzg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 270 i = 1, 4
            do 260 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(i*3+ 9+j)
260          continue
270      continue
!
! CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
! XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!
        call rloshb(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
! CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
!
        call s2calb(bksip(1, 1, ip), xe, b, ajac)
!
! CALCUL DE EPS DANS LE REPERE GLOBAL: 1 POUR DEFORMATIONS LINEAIRES
!                                     2 POUR TERMES CARRES EN PLUS
        do 280 i = 1, 6
            deps(i)=0.d0
280      continue
        call dsdx3d(1, b, ue, deps, dusx,&
                    20)
!
! SORTIE DE DUSDX DANS PROPEL(1 A 9 * 5 PT DE GAUSS)
! POUR UTILISATION ULTERIEURE DANS Q8PKCN_SHB8
!
!         CALL AEQBT(PPPT,PPP,3,3)
        do 300 i = 1, 3
            do 290 j = 1, 3
                pppt(j,i) = ppp(i,j)
290          continue
300      continue
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
        do 310 i = 1, 9
            propel(i+(ip-1)*9)=dusx(i)
310      continue
        do 320 i = 1, 6
            depslo(i) = 0.d0
320      continue
        call chrp3d(ppp, deps, depslo, 2)
!
!
! CONTRAINTES ECRITES SOUS LA FORME:
!               [SIG] = [S_11, S_22, S_33, S_12, S_23, S_13]
        do 330 i = 1, 6
! ON LAISSE LES DEFORMATIONS DANS LE REPERE LOCAL POUR LA PLASTICITE
!
            deploc((ip-1)*6+i)=depslo(i)
330      continue
340  continue
end subroutine
