subroutine sh6sig(xetemp, para, xidepp, dusx, sigma)
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
!               ELEMENT SHB6
!
    implicit none
#include "jeveux.h"
#include "asterfort/assebg.h"
#include "asterfort/depsh6.h"
#include "asterfort/mulmat.h"
#include "asterfort/r8inir.h"
#include "asterfort/rlosh6.h"
#include "asterfort/s6calb.h"
#include "asterfort/sh6ksi.h"
    integer :: lag, irdc
    real(kind=8) :: sigma(*), para(11)
    real(kind=8) :: xe(18), dusx(*), xidepp(*)
    real(kind=8) :: xxg5(5), xcoq(3, 3), bksip(3, 6, 5), b(3, 6)
    real(kind=8) :: xcent(3), ppp(3, 3), pppt(3, 3)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3)
    real(kind=8) :: cmatlo(6, 6), lambda
    real(kind=8) :: deps(6), dusdx(9), ue(3, 6), ueloc(3, 6)
    real(kind=8) :: sigloc(6)
    real(kind=8) :: bloc(6, 18), xmodif(18)
    real(kind=8) :: xetemp(*), blocal(3, 6)
    integer :: i, ip, j
    real(kind=8) :: ajac, rbid, xcooef, xmu, xnu, zeta, zlamb
!-----------------------------------------------------------------------
    data xmodif/1.d0,0.d0,0.d0,&
     &          0.d0,1.d0,0.d0,&
     &          0.d0,0.d0,1.d0,&
     &          1.d0,1.d0,0.d0,&
     &          0.d0,0.45d0,0.45d0,&
     &          0.45d0,0.d0,0.45d0/
!
!
!
!CCCCCCCCCCCCC ENTREES CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!          ICLE=7    ON CALCULE LES CONTRAINTES
!    OPTION=SIEF_ELGA    ON CALCULE LES CONTRAINTES
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INITIALISATIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INFOS:
! XE EST RANGE COMME CA:
! (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2 ZNOEUD2,shb6bg.f...)
! DANS SHB8_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!
! ON DEFINI LES POINTS GAUSS ET LES POIDS
!
    xxg5(1) = -0.906179845938664d0
    xxg5(2) = -0.538469310105683d0
    xxg5(3) = 0.d0
    xxg5(4) = 0.538469310105683d0
    xxg5(5) = 0.906179845938664d0
!
! -----------------------------------------------------
! ON VERIFIE QUE LA CONNECTIVITE DONNE UN REPERE DIRECT
! SI CE N EST PAS LE CAS ON PERMUTE LES NOEUDS
! -----------------------------------------------------
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 18
        xe(i) = xetemp(i)
10  end do
! TYPE DE LOI DE COMPORTEMENT:
!     IRDC = 1 : SHB8 TYPE PLEXUS
!     IRDC = 2 : C.P.
!     IRDC = 3 : 3D COMPLETE
    irdc = nint(para(5))
    call r8inir(36, 0.d0, cmatlo, 1)
!
! UE: INCREMENT DE DEPLACEMENT NODAL, REPERE GLOBAL
!
! XE: DEBUT DU PAS
    do 360 j = 1, 6
        do 350 i = 1, 3
            ue(i,j) = xidepp((j-1)*3+i)
350      continue
360  end do
!
    lag = nint(para(6))
! ON DEFINIT CMATLO LOI MODIFIEE SHB8
!
    lambda = para(1)*para(2)/(1-para(2)*para(2))
    xmu = 0.5d0*para(1)/ (1+para(2))
    cmatlo(1,1) = lambda + 2*xmu
    cmatlo(2,2) = lambda + 2*xmu
    if (irdc .eq. 1) then
! COMPORTEMENT SHB8 PLEXUS
!         CMATLO(3,3) = PROPEL(1)
        cmatlo(3,3) = para(1)
    endif
!
    if (irdc .eq. 2) then
! COMPORTEMENT C.P.
        cmatlo(3,3) = 0.d0
    endif
!
    cmatlo(1,2) = lambda
    cmatlo(2,1) = lambda
    cmatlo(4,4) = xmu
    cmatlo(5,5) = xmu
    cmatlo(6,6) = xmu
!
    if (irdc .eq. 3) then
! COMPORTEMENT LOI TRIDIM MMC 3D
!
        xnu = para(2)
        xcooef = para(1)/ ((1+xnu)*(1-2*xnu))
        cmatlo(1,1) = (1-xnu)*xcooef
        cmatlo(2,2) = (1-xnu)*xcooef
        cmatlo(3,3) = (1-xnu)*xcooef
        cmatlo(1,2) = xnu*xcooef
        cmatlo(2,1) = xnu*xcooef
        cmatlo(1,3) = xnu*xcooef
        cmatlo(3,1) = xnu*xcooef
        cmatlo(2,3) = xnu*xcooef
        cmatlo(3,2) = xnu*xcooef
        cmatlo(4,4) = (1-2*xnu)*0.5d0*xcooef
        cmatlo(5,5) = (1-2*xnu)*0.5d0*xcooef
        cmatlo(6,6) = (1-2*xnu)*0.5d0*xcooef
    endif
!
! CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!
    call sh6ksi(5, xxg5, bksip)
!
    do 450 ip = 1, 5
!
! DEFINITION DES 4 POINTS  COQUES
!
        zeta = xxg5(ip)
        zlamb = 0.5d0* (1.d0-zeta)
        do 380 i = 1, 3
            do 370 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe((i- 1+3)*3+j)
370          continue
380      continue
!
! CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
! XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
        do 382 i = 1, 3
            do 381 j = 1, 3
                pppt(j,i) = ppp(i,j)
381          continue
382      continue
!
!
! CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
!
        call s6calb(bksip(1, 1, ip), xe, b, ajac)
        call mulmat(3, 3, 6, pppt, b,&
                    blocal)
!
! CALCUL DE EPS DANS LE REPERE GLOBAL: 1 POUR DEFORMATIONS LINEAIRES
!                                     2 POUR TERMES CARRES EN PLUS
        do 390 i = 1, 6
            deps(i) = 0.d0
390      continue
! Transformer matrice BLOCAL(3,6) dans le repère local en BLOC(6,18)
! dans le repère local et en tenant
! compte également des modifications sur les termes croisés ZY,ZX :
        call assebg(bloc, blocal, xmodif)
! Transformer les déplacements UE dans le repère global en UELOC dans
! le repère local
        call mulmat(3, 3, 6, pppt, ue,&
                    ueloc)
!
        if (lag .eq. 1) then
! ON AJOUTE LA PARTIE NON-LINEAIRE DE EPS
            call depsh6(2, bloc, ueloc, deps, dusdx)
        else
            call depsh6(1, bloc, ueloc, deps, dusdx)
        endif
!
        do 420 i = 1, 9
            dusx(i+ (ip-1)*9) = dusdx(i)
420      continue
        do 430 i = 1, 6
            sigloc(i) = 0.d0
430      continue
!
! CALCUL DE SIGMA DANS LE REPERE LOCAL
!
        call mulmat(6, 6, 1, cmatlo, deps,&
                    sigloc)
!
! CONTRAINTES ECRITES SOUS LA FORME:
!               [SIG] = [S_11, S_22, S_33, S_12, S_23, S_13]
        do 440 i = 1, 6
! ON LAISSE LES CONTRAINTES DANS LE REPERE LOCAL POUR LA PLASTICITE
            sigma((ip-1)*6+i) = sigloc(i)
440      continue
450  end do
!
end subroutine
