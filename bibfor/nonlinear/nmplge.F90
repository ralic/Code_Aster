subroutine nmplge(ndim, nno1, vff1, idfde1, nno2,&
                  vff2, idfde2, npg, iw, geom,&
                  typmod, option, mate, compor, crit,&
                  instam, instap, angmas, ddlm, ddld,&
                  sigm, lgpg, vim, sigp, vip,&
                  matr, vect, codret, dfdi2)
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
!
! aslint: disable=W1504
    implicit none
!
#include "asterf_types.h"
#include "asterfort/codere.h"
#include "asterfort/dfdmip.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmepsb.h"
#include "asterfort/nmepsi.h"
#include "asterfort/nmmabu.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
!
    integer :: ndim, nno1, nno2, npg, idfde1, idfde2, iw, mate, lgpg, codret
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), geom(ndim, nno1)
    real(kind=8) :: crit(*), instam, instap
    real(kind=8) :: ddlm(*), ddld(*), sigm(2*ndim, npg), sigp(2*ndim, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg), matr(*), vect(*)
    real(kind=8) :: dfdi2(nno2, ndim), angmas(3)
! ----------------------------------------------------------------------
!
!     RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR GRAD_EPSI (2D ET 3D)
!
! IN  NDIM    : DIMENSION DES ELEMENTS
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
! IN  IDFDE1  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE E)
! IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE E)
! IN  IDFDE2  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE E)
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS DE REFERENCE (INDICE)
! IN  GEOM    : COORDONNEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODEELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  MATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  DDLM    : DDL A L'INSTANT PRECEDENT
! IN  DDLD    : INCREMENT DES DDL
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  LGPG    : LONGUEUR DU TABLEAU DES VARIABLES INTERNES
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! IN  ANGMAS  : REPERE LOCAL 3D
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA   ET FULL_MECA_*)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
! OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
! OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! OUT CODRET  : CODE RETOUR
! MEM DFDI2   :
! ----------------------------------------------------------------------
!
    integer :: k2(1), kpg, spt
    character(len=8) :: fami, poum
!
    aster_logical :: resi, rigi, grand, axi
    integer :: ndimsi, nddl, g, cod(27), n, i, m, j, kl, pq, os, kk
    integer :: iu(3, 27), ie(6, 8)
    real(kind=8) :: rac2, lc(1), c, deplm(3*27), depld(3*27), dfdi1(27, 3)
    real(kind=8) :: r, wg, epsgm(6, 2), epsgd(6, 2), gepsm(6, 3), geps(6, 3)
    real(kind=8) :: f(3, 3)
    real(kind=8) :: b(6, 3, 27), de(6), sigma(6), dsidep(6, 6, 2), t1, t2
    real(kind=8) :: p(6, 6), sigmam(6), pert, q(6, 6)
!
    parameter (pert = 1.d-4)
! ----------------------------------------------------------------------
!
!
!
! - INITIALISATION
!
    resi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RAPH_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
    rac2 = sqrt(2.d0)
    grand = .false.
    axi = .false.
    ndimsi = 2*ndim
    nddl = nno1*ndim + nno2*ndimsi
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'NON_LOCAL', 0, ' ', [0.d0],&
                1, 'LONG_CAR', lc, k2, 1)
    c = lc(1)**2
    do 5 g = 1, npg
        cod(g)=0
  5 end do
    if (rigi) call r8inir(nddl*nddl, 0.d0, matr, 1)
    if (resi) call r8inir(nddl, 0.d0, vect, 1)
    call r8inir(6, 0.d0, sigmam, 1)
!
!    POSITION DES INDICES POUR LES DEPLACEMENTS ET LES DEFORMATIONS
!
    do 10 n = 1, nno2
        do 20 i = 1, ndim
            iu(i,n) = i + (n-1)*(ndim+ndimsi)
 20     continue
        do 30 kl = 1, ndimsi
            ie(kl,n) = kl + ndim + (n-1)*(ndim+ndimsi)
 30     continue
 10 end do
    os = (ndimsi+ndim)*nno2
    do 40 n = 1, nno1-nno2
        do 50 i = 1, ndim
            iu(i,n+nno2) = i + (n-1)*ndim + os
 50     continue
 40 end do
!
!
!    EXTRACTION DES DEPLACEMENTS
!
    do 100 n = 1, nno1
        do 110 i = 1, ndim
            deplm(i+(n-1)*ndim) = ddlm(iu(i,n))
            depld(i+(n-1)*ndim) = ddld(iu(i,n))
110     continue
100 end do
!
!
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do 1000 g = 1, npg
!
!
!      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR E-BARRE
!
        call dfdmip(ndim, nno2, axi, geom, g,&
                    iw, vff2(1, g), idfde2, r, wg,&
                    dfdi2)
        call nmepsb(ndim, nno2, axi, vff2(1, g), dfdi2,&
                    ddlm, epsgm(1, 2), gepsm)
        call nmepsb(ndim, nno2, axi, vff2(1, g), dfdi2,&
                    ddld, epsgd(1, 2), geps)
!
!
!      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR U
!
        call dfdmip(ndim, nno1, axi, geom, g,&
                    iw, vff1(1, g), idfde1, r, wg,&
                    dfdi1)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dfdi1, deplm, f, epsgm)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dfdi1, depld, f, epsgd)
        call nmmabu(ndim, nno1, axi, grand, dfdi1,&
                    b)
!
!
!      DEFORMATIONS ET ECARTS EN FIN DE PAS DE TEMPS
!
        call daxpy(18, 1.d0, gepsm, 1, geps,&
                   1)
        do 200 kl = 1, ndimsi
            de(kl) = epsgm(kl,2)+epsgd(kl,2) - epsgm(kl,1)-epsgd(kl,1)
200     continue
!
!
!      LOI DE COMPORTEMENT
!
        call dcopy(ndimsi, sigm(1, g), 1, sigmam, 1)
        call dscal(3, rac2, sigmam(4), 1)
!
        call nmcomp('RIGI', g, 1, ndim, typmod,&
                    mate, compor, crit, instam, instap,&
                    12, epsgm, epsgd, 6, sigmam,&
                    vim(1, g), option, angmas, 36, p,&
                    sigma, vip(1, g), 72, dsidep, 36,&
                    p, cod(g))
        if (cod(g) .eq. 1) goto 9000
!
!      FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
        if (resi) then
!
!        VECTEUR FINT:U
            do 300 n = 1, nno1
                do 310 i = 1, ndim
                    kk = iu(i,n)
                    t1 = 0
                    do 320 kl = 1, ndimsi
                        t1 = t1 + sigma(kl)*b(kl,i,n)
320                 continue
                    vect(kk) = vect(kk) + wg*t1
310             continue
300         continue
!
!        VECTEUR FINT:E
            do 350 n = 1, nno2
                do 360 kl = 1, ndimsi
                    kk = ie(kl,n)
                    t1 = 0
                    do 365 pq = 1, ndimsi
                        t1 = t1 + p(kl,pq)*de(pq)*vff2(n,g)
365                 continue
                    t2 = 0
                    do 370 i = 1, ndim
                        do 375 pq = 1, ndimsi
                            t2 = t2 + c*dfdi2(n,i)*p(kl,pq)*geps(pq,i)
375                     continue
370                 continue
                    vect(kk) = vect(kk) + wg*(t1+t2)
360             continue
350         continue
!
!        CONTRAINTES
            call dcopy(ndimsi, sigma, 1, sigp(1, g), 1)
            call dscal(ndimsi-3, 1.d0/rac2, sigp(4, g), 1)
!
        endif
!
!
! - CALCUL DE LA MATRICE DE RIGIDITE (STOCKAGE LIGNE DE DFI/DUJ)
!
        if (rigi) then
!
!        MATRICE K:U(I,N),U(J,M)
            do 500 n = 1, nno1
                do 510 i = 1, ndim
                    os = nddl*(iu(i,n)-1)
                    do 520 m = 1, nno1
                        do 530 j = 1, ndim
                            kk = os+iu(j,m)
                            t1 = 0
                            do 540 kl = 1, ndimsi
                                do 550 pq = 1, ndimsi
                                    t1 = t1+dsidep(kl,pq,1)*b(pq,j,m)* b(kl,i,n)
550                             continue
540                         continue
                            matr(kk) = matr(kk) + wg*t1
530                     continue
520                 continue
!
!        MATRICE K:U(I,N),E(PQ,M)
                    do 600 m = 1, nno2
                        do 610 pq = 1, ndimsi
                            kk = os+ie(pq,m)
                            t1 = 0
                            do 620 kl = 1, ndimsi
!    ????, C EST LA LDC QUI DEVRAIT FAIRE CA  ???
                                if (nint(vim(2,g)) .ne. 2) t1 = t1 + dsidep(kl,pq,2)*vff2(m,g)*b(&
                                                                &kl,i,n)
620                         continue
                            matr(kk) = matr(kk) + wg*t1
610                     continue
600                 continue
510             continue
500         continue
!
!        MATRICE K:E(KL,N),U(J,M)
            do 700 n = 1, nno2
                do 710 kl = 1, ndimsi
                    os = nddl*(ie(kl,n)-1)
                    do 720 m = 1, nno1
                        do 730 j = 1, ndim
                            kk = os+iu(j,m)
                            t1 = 0
                            do 735 pq = 1, ndimsi
                                t1 = t1 - p(kl,pq)*b(pq,j,m)*vff2(n,g)
735                         continue
                            matr(kk) = matr(kk) + wg*t1
730                     continue
720                 continue
710             continue
700         continue
!
!
!        MATRICE K:E(KL,N),E(PQ,M)
!        A n'affecte pas tous les termes de l'integrale
!        (p.189 these VG, ou doc R) FICHE 15979
!
!        RIGIDITE FICTIVE SI POINT SATURE
            call dscal(36, 1-pert, p, 1)
            do 780 kl = 1, 6
                p(kl,kl) = p(kl,kl) + pert
780         continue
!
            call r8inir(36, 0.d0, q, 1)
            do 790 i = 1, 6
                q(i,i)=1.d0
790         continue
            do 800 n = 1, nno2
                do 810 m = 1, nno2
                    t1 = vff2(n,g)*vff2(m,g)
                    t2 = 0.d0
                    do 820 i = 1, ndim
                        t2 = t2 + c*dfdi2(n,i)*dfdi2(m,i)
820                 continue
                    do 830 kl = 1, ndimsi
                        do 835 pq = 1, ndimsi
                            kk = (ie(kl,n)-1)*nddl + ie(pq,m)
                            matr(kk) = matr(kk) + wg*t1*p(kl,pq) + wg* t2*q(kl,pq)
835                     continue
830                 continue
810             continue
800         continue
!
        endif
!
1000 end do
!
!
! - SYNTHESE DES CODES RETOUR
!
9000 continue
    call codere(cod, npg, codret)
!
end subroutine
