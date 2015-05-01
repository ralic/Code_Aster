subroutine nmpl3d(fami, nno, npg, ipoids, ivf,&
                  idfde, geom, typmod, option, imate,&
                  compor, lgpg, crit, instam, instap,&
                  deplm, deplp, angmas, sigm, vim,&
                  matsym, dfdi, def, sigp, vip,&
                  matuu, vectu, codret)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!
! aslint: disable=W1504
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/codere.h"
#include "asterfort/crirup.h"
#include "asterfort/lcegeo.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmgeom.h"
    integer :: nno, npg, imate, lgpg, codret, ndim
!
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
!
    real(kind=8) :: instam, instap, angmas(*)
    real(kind=8) :: geom(3, nno), crit(*)
    real(kind=8) :: deplm(1:3, 1:nno), deplp(1:3, 1:nno), dfdi(nno, 3)
    real(kind=8) :: def(6, nno, 3), rbid(1)
    real(kind=8) :: sigm(6, npg), sigp(6, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    real(kind=8) :: matuu(*), vectu(3, nno)
!
    aster_logical :: matsym
    common / nmpale / unsurk,unsurm,valden
    real(kind=8) :: unsurk, unsurm, valden
!
!
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN HYPO-ELASTICITE EN 3D
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IPOIDS  : POIDS DES POINTS DE GAUSS
! IN  IVF     : VALEUR  DES FONCTIONS DE FORME
! IN  IDFDE   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
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
! IN  VARDEP  : VARIABLE DELOCALISEE EN T+
! IN  DELOCA  : VRAI SI VARIABLES DELOCALISEES PRESENTES
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!......................................................................
!
    aster_logical :: grand
!
    integer :: kpg, kk, n, i, m, j, j1, kl, kkd, cod(27), ipoids, ivf, idfde
!
    real(kind=8) :: dsidep(6, 6), f(3, 3), eps(6), deps(6), r, sigma(6), sign(6)
    real(kind=8) :: sig(6)
    real(kind=8) :: poids, tmp, rac2
    real(kind=8) :: elgeom(10, 27)
! - INITIALISATION
!
    rac2 = sqrt(2.d0)
    grand = .false.
!
! - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES LOIS DE COMPORTEMENT
!
    call lcegeo(nno, npg, ipoids, ivf, idfde,&
                geom, typmod, compor, 3, dfdi,&
                deplm, deplp, elgeom)
!
!
! - INITIALISATION CODES RETOURS
    do 1955 kpg = 1, npg
        cod(kpg)=0
1955 end do
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do 800 kpg = 1, npg
!
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
!     CALCUL DE DFDI,F,EPS,DEPS,R(EN AXI) ET POIDS
!
        do 20 j = 1, 6
            eps (j)=0.d0
            deps(j)=0.d0
 20     continue
!
        call nmgeom(3, nno, .false._1, grand, geom,&
                    kpg, ipoids, ivf, idfde, deplm,&
                    .true._1, poids, dfdi, f, eps,&
                    r)
!
!       CALCUL DE DEPS
!
        call nmgeom(3, nno, .false._1, grand, geom,&
                    kpg, ipoids, ivf, idfde, deplp,&
                    .false._1, poids, dfdi, f, deps,&
                    r)
!
!       CALCUL DES PRODUITS SYMETR. DE F PAR N,
        do 40 n = 1, nno
            do 30 i = 1, 3
                def(1,n,i) = f(i,1)*dfdi(n,1)
                def(2,n,i) = f(i,2)*dfdi(n,2)
                def(3,n,i) = f(i,3)*dfdi(n,3)
                def(4,n,i) = (f(i,1)*dfdi(n,2) + f(i,2)*dfdi(n,1))/ rac2
                def(5,n,i) = (f(i,1)*dfdi(n,3) + f(i,3)*dfdi(n,1))/ rac2
                def(6,n,i) = (f(i,2)*dfdi(n,3) + f(i,3)*dfdi(n,2))/ rac2
 30         continue
 40     continue
!
        do 60 i = 1, 3
            sign(i) = sigm(i,kpg)
 60     continue
        do 65 i = 4, 6
            sign(i) = sigm(i,kpg)*rac2
 65     continue
!
!
! - LOI DE COMPORTEMENT
!
! -    APPEL A LA LOI DE COMPORTEMENT
        call nmcomp(fami, kpg, 1, 3, typmod,&
                    imate, compor, crit, instam, instap,&
                    6, eps, deps, 6, sign,&
                    vim(1, kpg), option, angmas, 10, elgeom(1, kpg),&
                    sigma, vip(1, kpg), 36, dsidep, 1,&
                    rbid, cod(kpg))
        if (cod(kpg) .eq. 1) then
            goto 1956
        endif
!
!
! - CALCUL DE LA MATRICE DE RIGIDITE
!
        if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA') then
!
            if (matsym) then
                do 160 n = 1, nno
                    do 150 i = 1, 3
                        kkd = (3*(n-1)+i-1) * (3*(n-1)+i) /2
                        do 151 kl = 1, 6
                            sig(kl)=0.d0
                            sig(kl)=sig(kl)+def(1,n,i)*dsidep(1,kl)
                            sig(kl)=sig(kl)+def(2,n,i)*dsidep(2,kl)
                            sig(kl)=sig(kl)+def(3,n,i)*dsidep(3,kl)
                            sig(kl)=sig(kl)+def(4,n,i)*dsidep(4,kl)
                            sig(kl)=sig(kl)+def(5,n,i)*dsidep(5,kl)
                            sig(kl)=sig(kl)+def(6,n,i)*dsidep(6,kl)
151                     continue
                        do 140 j = 1, 3
                            do 130 m = 1, n
                                if (m .eq. n) then
                                    j1 = i
                                else
                                    j1 = 3
                                endif
!
!                   RIGIDITE ELASTIQUE
                                tmp=0.d0
                                tmp=tmp+sig(1)*def(1,m,j)
                                tmp=tmp+sig(2)*def(2,m,j)
                                tmp=tmp+sig(3)*def(3,m,j)
                                tmp=tmp+sig(4)*def(4,m,j)
                                tmp=tmp+sig(5)*def(5,m,j)
                                tmp=tmp+sig(6)*def(6,m,j)
!
!                   STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                                if (j .le. j1) then
                                    kk = kkd + 3*(m-1)+j
                                    matuu(kk) = matuu(kk) + tmp*poids
                                endif
!
130                         continue
140                     continue
150                 continue
160             continue
            else
                do 560 n = 1, nno
                    do 550 i = 1, 3
                        do 551 kl = 1, 6
                            sig(kl)=0.d0
                            sig(kl)=sig(kl)+def(1,n,i)*dsidep(1,kl)
                            sig(kl)=sig(kl)+def(2,n,i)*dsidep(2,kl)
                            sig(kl)=sig(kl)+def(3,n,i)*dsidep(3,kl)
                            sig(kl)=sig(kl)+def(4,n,i)*dsidep(4,kl)
                            sig(kl)=sig(kl)+def(5,n,i)*dsidep(5,kl)
                            sig(kl)=sig(kl)+def(6,n,i)*dsidep(6,kl)
551                     continue
                        do 540 j = 1, 3
                            do 530 m = 1, nno
!
!                   RIGIDITE ELASTIQUE
                                tmp=0.d0
                                tmp=tmp+sig(1)*def(1,m,j)
                                tmp=tmp+sig(2)*def(2,m,j)
                                tmp=tmp+sig(3)*def(3,m,j)
                                tmp=tmp+sig(4)*def(4,m,j)
                                tmp=tmp+sig(5)*def(5,m,j)
                                tmp=tmp+sig(6)*def(6,m,j)
!
!                   STOCKAGE SANS SYMETRIE
                                kk = 3*nno*(3*(n-1)+i-1) + 3*(m-1)+j
                                matuu(kk) = matuu(kk) + tmp*poids
!
530                         continue
540                     continue
550                 continue
560             continue
            endif
        endif
!
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
            do 230 n = 1, nno
                vectu(1,n)=vectu(1,n)+ poids* (def(1,n,1)*sigma(1)+&
                def(4,n,1)*sigma(4)+ def(5,n,1)*sigma(5))
                vectu(2,n)=vectu(2,n)+ poids* (def(2,n,2)*sigma(2)+&
                def(4,n,2)*sigma(4)+ def(6,n,2)*sigma(6))
                vectu(3,n)=vectu(3,n)+ poids* (def(3,n,3)*sigma(3)+&
                def(5,n,3)*sigma(5)+ def(6,n,3)*sigma(6))
230         continue
!
            do 310 kl = 1, 3
                sigp(kl,kpg) = sigma(kl)
310         continue
            do 320 kl = 4, 6
                sigp(kl,kpg) = sigma(kl)/rac2
320         continue
!
        endif
!
! - CALCUL DES CONTRAINTES DE CAUCHY POUR LA METHODE IMPLEX
!
        if (option(1:16) .eq. 'RIGI_MECA_IMPLEX') then
!
            do 330 kl = 1, 3
                sigp(kl,kpg) = sigma(kl)
330         continue
            do 340 kl = 4, 6
                sigp(kl,kpg) = sigma(kl)/rac2
340         continue
!
        endif
!
!
800 end do
!
!     POST_ITER='CRIT_RUPT'
    if (crit(13) .gt. 0.d0) then
        ndim = 3
        call crirup(fami, imate, ndim, npg, lgpg,&
                    option, compor, sigp, vip, vim,&
                    instam, instap)
    endif
!
1956 continue
! - SYNTHESE DES CODES RETOURS
    call codere(cod, npg, codret)
end subroutine
