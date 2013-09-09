subroutine nmas2d(fami, nno, npg, ipoids, ivf,&
                  idfde, geom, typmod, option, imate,&
                  compor, lgpg, crit, instam, instap,&
                  deplm, deplp, angmas, sigm, vim,&
                  dfdi, def, sigp, vip, matuu,&
                  vectu, codret)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/calsta.h"
#include "asterfort/codere.h"
#include "asterfort/dfda2d.h"
#include "asterfort/iniqs4.h"
#include "asterfort/lcegeo.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmgeom.h"
    integer :: nno, npg, imate, lgpg, codret, cod(9), npgs
    integer :: ipoids, ivf, idfde
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
    real(kind=8) :: instam, instap
    real(kind=8) :: geom(2, nno), crit(3)
    real(kind=8) :: deplm(1:2, 1:nno), deplp(1:2, 1:nno), dfdi(nno, 2)
    real(kind=8) :: def(4, nno, 2)
    real(kind=8) :: sigm(10, npg), sigp(10, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    real(kind=8) :: matuu(*), vectu(2, nno), angmas(3)
!
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN HYPO-ELASTICITE EN 2D POUR LE QUAD4 SOUS INTEGRE
!           STABILITE PAR ASSUMED STRAIN
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  POIDSG  : POIDS DES POINTS DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  DFDK    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODEELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
! IN  DEPLP   : INCREMENT DE DEPLACEMENT
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!.......................................................................
!
!
!
    logical :: grand, axi
    integer :: kpg, kk, kkd, n, i, m, j, j1, kl, kpgs, proj
    real(kind=8) :: dsidep(6, 6), f(3, 3), eps(6), deps(6), r, sigma(6), sign(6)
    real(kind=8) :: poids, tmp, sig(6), rbid(1)
    real(kind=8) :: elgeom(10, 9)
    real(kind=8) :: rac2
!
!     AJ. VARIABLES
    real(kind=8) :: jac, sigas(4, 4), pqx, pqy, defc(4, 4, 2)
    real(kind=8) :: dh(8), gamma(8), coopg(8)
    real(kind=8) :: sdkdx(4), sdkdy(4), sdedx(4), sdedy(4), poi2sg(4)
    real(kind=8) :: sdfdy(4, 4), sdfdx(4, 4), sdfde(4, 4), sdfdk(4, 4)
    real(kind=8) :: qplus(6), qmoins(6), dq(6), defn(4, 4, 2), kron(3, 3)
    character(len=16) :: optios
    data kron/1.d0,0.d0,0.d0,0.d0,1.d0,0.d0,0.d0,0.d0,1.d0/
!
!
! - INITIALISATION
!   ==============
!
!    PROJ : INDICATEUR DE LA PROJECTION
!           0 AUCUNE
!           1 OPTIMAL BENDING
!           2 INCOMPRESSIBLE
    proj = 2
    rac2 = sqrt(2.d0)
    grand = .false.
    axi = typmod(1) .eq. 'AXIS'
!
    do 20 i = 1, 3
        do 10 j = 1, 3
            f(i,j) = kron(i,j)
10      continue
20  end do
!
!
! - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES AU COMPORTEMENT
    call lcegeo(nno, npg, ipoids, ivf, idfde,&
                geom, typmod, compor, 2, dfdi,&
                deplm, deplp, elgeom)
!
! - INITIALISATION CODES RETOURS
    do 30 kpg = 1, npg
        cod(kpg) = 0
30  end do
!
! - INITIALISATION QUAS4
    call iniqs4(nno, sdfde, sdfdk, poi2sg, coopg)
!
! - CALCUL DU VECTEUR GAMMA
    gamma(1) = (&
               geom(1,4)* (geom(2,2)-geom(2,3))+ geom(1,2)* (geom(2,3)-geom(2,4))+ geom(1,3)* (ge&
               &om(2,4)-geom(2,2)))/ (2* (((geom(1,4)-geom(1,2))* (geom(2,1)-geom(2, 3)))+ (geom(&
               &1,1)-geom(1,3))* (geom(2,2)-geom(2,4)))&
               )
!
    gamma(2) = (&
               geom(1,4)* (geom(2,3)-geom(2,1))+ geom(1,3)* (geom(2,1)-geom(2,4))+ geom(1,1)* (ge&
               &om(2,4)-geom(2,3)))/ (2* (((geom(1,4)-geom(1,2))* (geom(2,1)-geom(2, 3)))+ (geom(&
               &1,1)-geom(1,3))* (geom(2,2)-geom(2,4)))&
               )
!
    gamma(3) = (&
               geom(1,4)* (geom(2,1)-geom(2,2))+ geom(1,1)* (geom(2,2)-geom(2,4))+ geom(1,2)* (ge&
               &om(2,4)-geom(2,1)))/ (2* (((geom(1,4)-geom(1,2))* (geom(2,1)-geom(2, 3)))+ (geom(&
               &1,1)-geom(1,3))* (geom(2,2)-geom(2,4)))&
               )
!
    gamma(4) = (&
               geom(1,3)* (geom(2,1)-geom(2,2))+ geom(1,1)* (geom(2,2)-geom(2,3))+ geom(1,2)* (ge&
               &om(2,3)-geom(2,1)))/ (2* (((geom(1,2)-geom(1,4))* (geom(2,1)-geom(2, 3)))- (geom(&
               &1,1)-geom(1,3))* (geom(2,2)-geom(2,4)))&
               )
!
!
!
!
! - CALCUL POUR LE POINT DE GAUSS CENTRAL
    kpg = 1
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
!     CALCUL DE DFDI,F,EPS,DEPS,R(EN AXI) ET POIDS
!
    do 70 j = 1, 6
        eps(j) = 0.d0
        deps(j) = 0.d0
70  end do
    call nmgeom(2, nno, axi, grand, geom,&
                kpg, ipoids, ivf, idfde, deplm,&
                .true., poids, dfdi, f, eps,&
                r)
!
!     CALCUL DE DEPS
    call nmgeom(2, nno, axi, grand, geom,&
                kpg, ipoids, ivf, idfde, deplp,&
                .true., poids, dfdi, f, deps,&
                r)
!
!      CALCUL DES PRODUITS SYMETR. DE F PAR N,
    do 90 n = 1, nno
        do 80 i = 1, 2
            def(1,n,i) = f(i,1)*dfdi(n,1)
            def(2,n,i) = f(i,2)*dfdi(n,2)
            def(3,n,i) = 0.d0
            def(4,n,i) = (f(i,1)*dfdi(n,2)+f(i,2)*dfdi(n,1))/rac2
80      continue
90  end do
!
!
    do 100 i = 1, 3
        sign(i) = sigm(i,kpg)
100  end do
    sign(4) = sigm(4,kpg)*rac2
!
!
! - LOI DE COMPORTEMENT
    if (option(1:9) .eq. 'RAPH_MECA') then
        optios = 'FULL_MECA'
    else
        optios = option
    endif
!
    call nmcomp(fami, kpg, 1, 2, typmod,&
                imate, compor, crit, instam, instap,&
                6, eps, deps, 6, sign,&
                vim(1, kpg), optios, angmas, 10, elgeom(1, kpg),&
                sigma, vip(1, kpg), 36, dsidep, 1,&
                rbid, cod(kpg))
!
! - ERREUR D'INTEGRATION
    if (cod(kpg) .eq. 1) then
        goto 320
    endif
!
!
!
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
!
!
!     CALCUL DE KC (MATRICE DE RIGIDITE AU CENTRE)
!     --------------------------------------------
        do 150 n = 1, nno
            do 140 i = 1, 2
                do 110,kl = 1,4
                sig(kl) = 0.d0
                sig(kl) = sig(kl) + def(1,n,i)*dsidep(1,kl)
                sig(kl) = sig(kl) + def(2,n,i)*dsidep(2,kl)
                sig(kl) = sig(kl) + def(3,n,i)*dsidep(3,kl)
                sig(kl) = sig(kl) + def(4,n,i)*dsidep(4,kl)
110              continue
                do 130 j = 1, 2
                    do 120 m = 1, n
                        if (m .eq. n) then
                            j1 = i
                        else
                            j1 = 2
                        endif
!               RIGIDITE ELASTIQUE
                        tmp = 0.d0
                        tmp = tmp + sig(1)*def(1,m,j)
                        tmp = tmp + sig(2)*def(2,m,j)
                        tmp = tmp + sig(3)*def(3,m,j)
                        tmp = tmp + sig(4)*def(4,m,j)
!               STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                        if (j .le. j1) then
                            kkd = (2* (n-1)+i-1)* (2* (n-1)+i)/2
                            kk = kkd + 2* (m-1) + j
                            matuu(kk) = matuu(kk) + tmp*poids
                        endif
120                  continue
130              continue
140          continue
150      continue
!
!
!
!
!           CORRECTION DE LA MATRICE DE RIGIDITE
!                 CALCUL DE KSTAB
!     --------------------------------------------
        npgs = 4
!
!        CALCUL DES TERMES EVALUES AUX 4 POINTS DE GAUSS
        do 160 kpgs = 1, npgs
!
            call dfda2d(kpgs, nno, poi2sg(kpgs), sdfde, sdfdk,&
                        sdedx, sdedy, sdkdx, sdkdy, sdfdx,&
                        sdfdy, geom, jac)
!
            dh(2*kpgs-1) = coopg(2*kpgs-1)*sdkdx(kpgs) + coopg(2*kpgs) *sdedx(kpgs)
            dh(2*kpgs) = coopg(2*kpgs-1)*sdkdy(kpgs) + coopg(2*kpgs)* sdedy(kpgs)
!
!
            call calsta(proj, gamma, dh, def, nno,&
                        kpgs, sig, tmp, kk, kkd,&
                        matuu, dsidep, jac)
!
160      continue
    endif
!
!
!
!
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
!     INITIALISATION
        npgs = 4
        pqx = 0.d0
        pqy = 0.d0
!
!
!     DEPLACEMENTS GENERALISES
        do 170 kl = 1, nno
            pqx = pqx + gamma(kl)*deplp(1,kl)
            pqy = pqy + gamma(kl)*deplp(2,kl)
170      continue
!
!
!      INCREMENT DES CONTRAINTES GENERALISEES
        do 180 i = 1, 6
            qmoins(i) = sigm(i+4,kpg)
!
!         QUAS4 SANS PROJECTION
!         ---------------------
            if (proj .eq. 0) then
                dq(1) = dsidep(1,1)*pqx
                dq(2) = dsidep(2,1)*pqy
                dq(3) = dsidep(1,2)*pqx
                dq(4) = dsidep(2,2)*pqy
                dq(5) = dsidep(4,4)*pqx
                dq(6) = dsidep(4,4)*pqy
!
!         INCOMPRESSIBLE
!         --------------
            else if (proj.eq.1.or.proj.eq.2) then
                dq(1) = (dsidep(1,1)-dsidep(2,1))*pqx
                dq(2) = (dsidep(2,1)-dsidep(1,1))*pqy
                dq(3) = (dsidep(1,2)-dsidep(2,2))*pqx
                dq(4) = (dsidep(2,2)-dsidep(1,2))*pqy
                dq(5) = 0.d0
                dq(6) = 0.d0
!
            endif
!
            qplus(i) = qmoins(i) + dq(i)
180      continue
!
!
!      OPERATEUR DE GRADIENT AU CENTRE
        do 200 n = 1, nno
            do 190 i = 1, 2
                defc(1,n,i) = def(1,n,i)
                defc(2,n,i) = def(2,n,i)
                defc(3,n,i) = def(3,n,i)
                defc(4,n,i) = def(4,n,i)
190          continue
200      continue
!
!
!      OPERATEUR DE STABILISATION DU GRADIENT AU 4 POINTS DE GAUSS
        do 290 kpgs = 1, npgs
!
!
            call dfda2d(kpgs, nno, poi2sg(kpgs), sdfde, sdfdk,&
                        sdedx, sdedy, sdkdx, sdkdy, sdfdx,&
                        sdfdy, geom, jac)
!
            dh(2*kpgs-1) = coopg(2*kpgs-1)*sdkdx(kpgs) + coopg(2*kpgs) *sdedx(kpgs)
            dh(2*kpgs) = coopg(2*kpgs-1)*sdkdy(kpgs) + coopg(2*kpgs)* sdedy(kpgs)
!
!
            do 220 n = 1, nno
                do 210 i = 1, 2
!
!         QUAS4 SANS PROJECTION
!         ---------------------
                    if (proj .eq. 0) then
                        defn(1,n,i) = f(i,1)*gamma(n)*dh(2*kpgs-1)
                        defn(2,n,i) = f(i,2)*gamma(n)*dh(2*kpgs)
                        defn(3,n,i) = 0.d0
                        defn(4,n,i) = (f(i,1)*gamma(n)*dh(2*kpgs)+ f(i,2)*gamma(n)*dh(2*kpgs-1))
!
!         OPTIMAL BENDING
!         ---------------
                    else if (proj.eq.1) then
                        defn(1,n,i) = f(i,1)*gamma(n)*dh(2*kpgs-1)
                        defn(2,n,i) = f(i,2)*gamma(n)*dh(2*kpgs)
                        defn(3,n,i) = 0.d0
                        defn(4,n,i) = 0.d0
!
!         INCOMPRESSIBLE
!         --------------
                    else if (proj.eq.2) then
                        defn(1,n,i) = f(i,1)*gamma(n)*dh(2*kpgs-1)* ( 0.5d0) + f(i,2)*gamma(n)*dh&
                                      &(2*kpgs)* (-0.5d0)
                        defn(2,n,i) = f(i,2)*gamma(n)*dh(2*kpgs)* 0.5d0 + f(i,1)*gamma(n)*dh(2*kp&
                                      &gs-1)* (-0.5d0)
                        defn(3,n,i) = 0.d0
                        defn(4,n,i) = 0.d0
!
                    endif
!
210              continue
220          continue
!
!
!    CONTRAINTES DE HOURGLASS
!
!         QUAS4 SANS PROJECTION
!         ---------------------
            if (proj .eq. 0) then
                sigas(1,kpgs) = qplus(1)*dh(2*kpgs-1) + qplus(2)*dh(2* kpgs)
                sigas(2,kpgs) = qplus(3)*dh(2*kpgs-1) + qplus(4)*dh(2* kpgs)
                sigas(3,kpgs) = 0.d0
                sigas(4,kpgs) = (qplus(5)*dh(2*kpgs)+qplus(6)*dh(2* kpgs-1))/ 2
!
!         OPTIMAL BENDING
!         ---------------
            else if (proj.eq.1) then
                sigas(1,kpgs) = qplus(1)*dh(2*kpgs-1) + qplus(2)*dh(2* kpgs)
                sigas(2,kpgs) = qplus(3)*dh(2*kpgs-1) + qplus(4)*dh(2* kpgs)
                sigas(3,kpgs) = 0.d0
                sigas(4,kpgs) = 0.d0
!
!         INCOMPRESSIBLE
!         --------------
            else if (proj.eq.2) then
                sigas(1,kpgs) = (qplus(1)*dh(2*kpgs-1)+qplus(2)*dh(2* kpgs))
                sigas(2,kpgs) = (qplus(3)*dh(2*kpgs-1)+qplus(4)*dh(2* kpgs))
                sigas(3,kpgs) = 0.d0
                sigas(4,kpgs) = 0.d0
            endif
!
!
!
!
!     CALCUL DES FORCES INTERNES
!
            do 250 n = 1, nno
                do 240 i = 1, 2
                    do 230 kl = 1, 3
                        vectu(i,n) = vectu(i,n) + defc(kl,n,i)*sigas( kl,kpgs)* jac + defn(kl,n,i&
                                     &)*sigas(kl,kpgs)* jac
230                  continue
                    vectu(i,n) = vectu(i,n) + defc(4,n,i)*sigas(4, kpgs)*jac* rac2 + defn(4,n,i)*&
                                 &sigas(4,kpgs)*jac
240              continue
250          continue
!
            do 280 n = 1, nno
                do 270 i = 1, 2
                    do 260 kl = 1, 3
                        vectu(i,n) = vectu(i,n) + defc(kl,n,i)*sigma( kl)*jac + defn(kl,n,i)*sigm&
                                     &a(kl)*jac
260                  continue
                    vectu(i,n) = vectu(i,n) + defc(4,n,i)*sigma(4)* jac + defn(4,n,i)*sigma(4)*ja&
                                 &c/rac2
270              continue
280          continue
290      continue
!
!
        do 300 kl = 1, 3
            sigp(kl,kpg) = sigma(kl)
300      continue
        sigp(4,kpg) = sigma(4)/rac2
!
!
        do 310 i = 1, 6
            sigp(i+4,kpg) = qplus(i)
310      continue
!
!
    endif
!
!
320  continue
! - SYNTHESE DES CODES RETOURS
    call codere(cod, npg, codret)
!
end subroutine
