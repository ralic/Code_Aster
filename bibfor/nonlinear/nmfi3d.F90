subroutine nmfi3d(nno, nddl, npg, lgpg, wref,&
                  vff, dfde, mate, option, geom,&
                  deplm, ddepl, sigm, sigp, fint,&
                  ktan, vim, vip, crit, compor,&
                  matsym, coopg, tm, tp, codret)
!
! ======================================================================
! COPYRIGHT (C) 2007 NECS - BRUNO ZUBER   WWW.NECS.FR
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
!
! aslint: disable=W1306,W1504
    implicit none
#include "asterc/r8vide.h"
#include "asterfort/codere.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmfici.h"
#include "asterfort/r8inir.h"
#include "blas/ddot.h"
    integer :: nno, nddl, npg, lgpg, mate, codret
    real(kind=8) :: wref(npg), vff(nno, npg), dfde(2, nno, npg), crit(*)
    real(kind=8) :: geom(nddl), deplm(nddl), ddepl(nddl), tm, tp
    real(kind=8) :: fint(nddl), ktan(*), coopg(4, npg)
    real(kind=8) :: sigm(3, npg), sigp(3, npg), vim(lgpg, npg), vip(lgpg, npg)
    character(len=16) :: option, compor(*)
    logical :: matsym
!
!-----------------------------------------------------------------------
!  OPTIONS DE MECANIQUE NON LINEAIRE POUR LES JOINTS 3D (TE0206)
!-----------------------------------------------------------------------
! IN  NNO    NOMBRE DE NOEUDS DE LA FACE (*2 POUR TOUT L'ELEMENT)
! IN  NDDL   NOMBRE DE DEGRES DE LIBERTE EN DEPL TOTAL (3 PAR NOEUDS)
! IN  NPG    NOMBRE DE POINTS DE GAUSS
! IN  LGPG   NOMBRE DE VARIABLES INTERNES
! IN  WREF   POIDS DE REFERENCE DES POINTS DE GAUSS
! IN  VFF    VALEUR DES FONCTIONS DE FORME (DE LA FACE)
! IN  DFDE   DERIVEE DES FONCTIONS DE FORME (DE LA FACE)
! IN  MATE   MATERIAU CODE
! IN  OPTION OPTION DE CALCUL
! IN  GEOM   COORDONNEES DES NOEUDS
! IN  DEPLM  DEPLACEMENTS NODAUX AU DEBUT DU PAS DE TEMPS
! IN  DDEPL  INCREMENT DES DEPLACEMENTS NODAUX
! IN  SIGM   CONTR LOCALES AUX POINTS DE GAUSS - (SIGN, SITX, SITY)
! OUT SIGP   CONTR LOCALES AUX POINTS DE GAUSS + (SIGN, SITX, SITY)
! OUT FINT   FORCES NODALES
! OUT KTAN   MATRICE TANGENTE (STOCKEE EN TENANT COMPTE DE LA SYMETRIE)
! IN  VIM    VARIABLES INTERNES AU DEBUT DU PAS DE TEMPS
! OUT VIP    VARIABLES INTERNES A LA FIN DU PAS DE TEMPS
! IN  CRIT   VALEURS DE L'UTILISATEUR POUR LES CRITERES DE CONVERGENCE
! IN  COMPOR NOM DE LA LOI DE COMPORTEMENT
! IN  MATSYM INFORMATION SUR LA MATRICE TANGENTE : SYMETRIQUE OU PAS
! IN  COOPG  COORDONNEES GEOMETRIQUES DES PG + POIDS
! OUT CODRET CODE RETOUR DE L'INTEGRATION
!-----------------------------------------------------------------------
    logical :: resi, rigi
    integer :: code(9), ni, mj, kk, p, q, kpg, ibid, n
    real(kind=8) :: b(3, 60), sigmo(6), sigma(6)
    real(kind=8) :: sum(3), dsu(3), dsidep(6, 6), poids
    real(kind=8) :: rbid
    real(kind=8) :: angmas(3)
!
    character(len=8) :: typmod(2)
    data typmod /'3D','ELEMJOIN'/
!-----------------------------------------------------------------------
!
    resi = option.eq.'RAPH_MECA' .or. option(1:9).eq.'FULL_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
!
! --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! --- INITIALISE A R8VIDE (ON NE S'EN SERT PAS)
    call r8inir(3, r8vide(), angmas, 1)
!
    call r8inir(3, 0.d0, sum, 1)
    call r8inir(3, 0.d0, dsu, 1)
!
    if (resi) call r8inir(nddl, 0.d0, fint, 1)
!
    if (rigi) then
        if (matsym) then
            call r8inir((nddl*(nddl+1))/2, 0.d0, ktan, 1)
        else
            call r8inir(nddl*nddl, 0.d0, ktan, 1)
        endif
    endif
!
    do 10 kpg = 1, npg
!
! CALCUL DE LA MATRICE B DONNANT LES SAUT PAR ELEMENTS A PARTIR DES
! DEPLACEMENTS AUX NOEUDS , AINSI QUE LE POIDS DES PG :
!
        call nmfici(nno, nddl, wref(kpg), vff(1, kpg), dfde(1, 1, kpg),&
                    geom, poids, b)
!
! CALCUL DU SAUT DE DEPLACEMENT - : SUM, ET DE L'INCREMENT : DSU
! AU POINT DE GAUSS KPG
!
        sum(1) = ddot(nddl,b(1,1),3,deplm,1)
        sum(2) = ddot(nddl,b(2,1),3,deplm,1)
        sum(3) = ddot(nddl,b(3,1),3,deplm,1)
!
        if (resi) then
            dsu(1) = ddot(nddl,b(1,1),3,ddepl,1)
            dsu(2) = ddot(nddl,b(2,1),3,ddepl,1)
            dsu(3) = ddot(nddl,b(3,1),3,ddepl,1)
        endif
!
! -   APPEL A LA LOI DE COMPORTEMENT
!
        rbid = r8vide()
        code(kpg) = 0
!
!       CONTRAINTES -
        call r8inir(6, 0.d0, sigmo, 1)
        do 12 n = 1, 3
            sigmo(n) = sigm(n,kpg)
12      continue
!
        call nmcomp('RIGI', kpg, 1, 3, typmod,&
                    mate, compor, crit, tm, tp,&
                    3, sum, dsu, 6, sigmo,&
                    vim(1, kpg), option, angmas, 4, coopg(1, kpg),&
                    sigma, vip(1, kpg), 36, dsidep, 1,&
                    rbid, ibid)
!
        if (resi) then
!
!         CONTRAINTES +
            do 11 n = 1, 3
                sigp(n,kpg) = sigma(n)
11          continue
!
!         FORCES INTERIEURES
            do 20 ni = 1, nddl
                fint(ni) = fint(ni) + poids*ddot(3,b(1,ni),1,sigma,1)
20          continue
!
        endif
!
! MATRICE TANGENTE
!
        if (rigi) then
!
            if (matsym) then
!
!           STOCKAGE SYMETRIQUE
                kk = 0
                do 50 ni = 1, nddl
                    do 52 mj = 1, ni
                        kk = kk+1
                        do 60 p = 1, 3
                            do 62 q = 1, 3
                                ktan(kk) = ktan(kk) + poids*b(p,ni)* dsidep(p,q)*b(q,mj)
62                          continue
60                      continue
52                  continue
50              continue
!
            else
!
!           STOCKAGE COMPLET
                kk = 0
                do 51 ni = 1, nddl
                    do 53 mj = 1, nddl
                        kk = kk+1
                        do 61 p = 1, 3
                            do 63 q = 1, 3
                                ktan(kk) = ktan(kk) + poids*b(p,ni)* dsidep(p,q)*b(q,mj)
63                          continue
61                      continue
53                  continue
51              continue
!
            endif
!
        endif
!
10  end do
!
    if (resi) call codere(code, npg, codret)
end subroutine
