subroutine xxnmgr(elrefp, elrese, ndim, coorse, igeom,&
                  he, nfh, ddlc, ddlm, nfe,&
                  instam, instap, ideplp, sigm, vip,&
                  basloc, nnop, npg, typmod, option,&
                  imate, compor, lgpg, idecpg, crit,&
                  idepl, lsn, lst, nfiss, fisno,&
                  sigp, vi, matuu, ivectu, codret)
!
! aslint: disable=W1306,W1504
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/elref5.h"
#include "asterfort/indent.h"
#include "asterfort/lcegeo.h"
#include "asterfort/matinv.h"
#include "asterfort/nmcomp.h"
#include "asterfort/r8inir.h"
#include "asterfort/reeref.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalf2.h"
#include "asterfort/xcalfe.h"
    integer :: ndim, igeom, imate, lgpg, codret, nnop, npg
    integer :: nfiss, fisno(nnop, nfiss), idecpg
    integer :: nfh, ddlc, ddlm, nfe
    character(len=8) :: elrefp, typmod(*), elrese
    character(len=16) :: option, compor(4)
    real(kind=8) :: basloc(3*ndim*nnop), crit(3), he(nfiss)
    integer :: idepl, ideplp, ivectu
    real(kind=8) :: lsn(nnop), lst(nnop), coorse(*)
    real(kind=8) :: vi(lgpg, npg), vip(lgpg, npg), sigp(2*ndim, npg), matuu(*)
    real(kind=8) :: instam, instap, sigm(2*ndim, npg), sign(6)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: patrick.massin at edf.fr
!
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN GRANDE ROTATION ET PETITE DEFORMATION AVEC X-FEM EN 2D
!
!     TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!.......................................................................
!
! IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
! IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
! IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
! IN  NFH     : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  DDLM    : NOMBRE DE DDL PAR NOEUD MILIEU (EN 2D)
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE AUX NOEUDS
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ÉLÉMENT
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  IDEPL   : ADRESSE DU DEPLACEMENT A PARTIR DE LA CONF DE REF
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
!
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VI      : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!......................................................................
!
    integer :: i, ig, iret, j, j1, k, kk, kkd, kpg, l, m, mn, n, nn
    integer :: ddls, ddld, ddldn, cpt, dec(nnop)
    integer :: ibid, idfde, ipoids, ivf, jcoopg, jdfd2, jgano
    integer :: ndimb, nno, nnops, nnos, npgbis
    real(kind=8) :: f(3, 3), fm(3, 3), fr(3, 3), epsm(6), epsp(6), deps(6)
    real(kind=8) :: dsidep(6, 6), sigma(6), ftf, detf
    real(kind=8) :: tmp1, tmp2, sig(6), fe(4), baslog(3*ndim)
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac, lsng, lstg
    real(kind=8) :: rbid, rbid10(10), rbid4(4), rbid33(3, 3), rbid1(1)
    real(kind=8) :: dfdi(nnop, ndim), pff(6, nnop, ndim), dgdgl(4, 3)
    real(kind=8) :: def(6, nnop, ndim*(1+nfh+nfe))
    real(kind=8) :: elgeom(10, 27), dfdib(27, 3)
    real(kind=8) :: fmm(3, 3), deplb1(3, 27), deplb2(3, 27)
    logical :: grdepl, axi, cplan, resi, rigi
!
    integer :: indi(6), indj(6)
    real(kind=8) :: rind(6), rind1(6), rac2, angmas(3)
    data    indi / 1 , 2 , 3 , 1 , 1 , 2 /
    data    indj / 1 , 2 , 3 , 2 , 3 , 3 /
    data    rind / 0.5d0,0.5d0,0.5d0,0.70710678118655d0,&
     &               0.70710678118655d0,0.70710678118655d0 /
    data    rac2 / 1.4142135623731d0 /
    data    angmas /0.d0, 0.d0, 0.d0/
    data    rind1 / 0.5d0 , 0.5d0 , 0.5d0 , 1.d0, 1.d0, 1.d0 /
!--------------------------------------------------------------------
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld = ndim*(1+nfh+nfe)
    ddldn = ddld/ndim
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls = ddld+ddlc
!
!     RECUPERATION DU NOMBRE DE NOEUDS SOMMETS DE L'ELEMENT PARENT
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
! - INITIALISATION
    grdepl = compor(3) .eq. 'GROT_GDEP'
    axi = typmod(1) .eq. 'AXIS'
    cplan = typmod(1) .eq. 'C_PLAN'
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
!
    if (axi) then
        call utmess('F', 'XFEM2_5')
    endif
!
    call elref5(elrese, 'XINT', ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
!
    ASSERT(npg.eq.npgbis.and.ndim.eq.ndimb)
!
! - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES LOIS DE COMPORTEMENT
! - LES ARGUMENTS DFDIB, DEPLB1, DEPLB2 NE SERVENT PAS DANS CE CAS
    call lcegeo(nno, npg, ipoids, ivf, idfde,&
                zr(igeom), typmod, compor, ndim, dfdib,&
                deplb1, deplb2, elgeom)
!
    do 178 n = 1, nnop
        call indent(n, ddls, ddlm, nnops, dec(n))
178  end do
!
!-----------------------------------------------------------------------
! - CALCUL POUR CHAQUE POINT DE GAUSS DU SOUS-ELEMENT
    do 1000 kpg = 1, npg
!
!       COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
        call vecini(ndim, 0.d0, xg)
        do 100 i = 1, ndim
            do 101 n = 1, nno
                xg(i) = xg(i) + zr(ivf-1+nno*(kpg-1)+n)*coorse(ndim*( n-1)+i)
101          continue
100      continue
!
        if (nfe .gt. 0) then
!         JUSTE POUR CALCULER LES FF
            call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                        xg, idepl, grdepl, ndim, he,&
                        rbid, rbid, fisno, nfiss, nfh,&
                        nfe, ddls, ddlm, fe, dgdgl,&
                        'NON', xe, ff, dfdi, fm,&
                        epsm, rbid33)
!
!         BASE LOCALE  ET LEVEL SETS AU POINT DE GAUSS
            call vecini(3*ndim, 0.d0, baslog)
            lsng = 0.d0
            lstg = 0.d0
            do 110 n = 1, nnop
                lsng = lsng + lsn(n) * ff(n)
                lstg = lstg + lst(n) * ff(n)
                do 111 i = 1, 3*ndim
                    baslog(i) = baslog(i) + basloc(3*ndim*(n-1)+i) * ff(n)
111              continue
110          continue
!
!         FONCTION D'ENRICHISSEMENT AU POINT DE GAUSS ET LEURS DÉRIVÉES
            if (ndim .eq. 2) then
                call xcalf2(he, lsng, lstg, baslog, fe,&
                            dgdgl, iret)
            else if (ndim.eq.3) then
                call xcalfe(he, lsng, lstg, baslog, fe,&
                            dgdgl, iret)
            endif
!         ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
!         CAR ON SE TROUVE SUR LE FOND DE FISSURE
            ASSERT(iret.ne.0)
        endif
!
!       COORDONNÉES DU POINT DE GAUSS DANS L'ÉLÉMENT DE RÉF PARENT : XE
!       ET CALCUL DE FF, DFDI, EPSM ET EPSP
!       CALCUL EN T-
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    rbid, rbid, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'OUI', xe, ff, dfdi, fm,&
                    epsm, rbid33)
!
!       CALCUL EN T+
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, ideplp, grdepl, ndim, he,&
                    rbid, rbid, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'OUI', xe, ff, dfdi, f,&
                    epsp, rbid33)
!
!       CALCUL DE DEPS POUR LDC
        do 120 i = 1, 6
            deps(i) = epsp(i)-epsm(i)
120      continue
!
!      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        if (resi) then
            do 130 i = 1, 3
                do 131 j = 1, 3
                    fr(i,j) = f(i,j)
131              continue
130          continue
        else
            do 140 i = 1, 3
                do 141 j = 1, 3
                    fr(i,j) = fm(i,j)
141              continue
140          continue
        endif
!
!
!       CALCUL DES PRODUITS SYMETR. DE F PAR N,
        do 150 n = 1, nnop
!         FONCTIONS DE FORME CLASSIQUES
            do 151 i = 1, ndim
                def(1,n,i) = fr(i,1)*dfdi(n,1)
                def(2,n,i) = fr(i,2)*dfdi(n,2)
                def(3,n,i) = 0.d0
                def(4,n,i) = (fr(i,1)*dfdi(n,2) + fr(i,2)*dfdi(n,1))/ rac2
                if (ndim .eq. 3) then
                    def(3,n,i) = fr(i,3)*dfdi(n,3)
                    def(5,n,i) = (fr(i,1)*dfdi(n,3) + fr(i,3)*dfdi(n, 1))/rac2
                    def(6,n,i) = (fr(i,2)*dfdi(n,3) + fr(i,3)*dfdi(n, 2))/rac2
                endif
151          continue
!         ENRICHISSEMENT PAR HEAVYSIDE
            do 152 ig = 1, nfh
                do 153 i = 1, ndim
                    cpt = ndim*(1+ig-1)+i
                    do 154 m = 1, 2*ndim
                        def(m,n,cpt) = def(m,n,i) * he(fisno(n,ig))
154                  continue
                    if (ndim .eq. 2) def(3,n,cpt) = 0.d0
153              continue
152          continue
!         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIÈRES
            do 155 ig = 1, nfe
                do 156 i = 1, ndim
                    cpt = ndim*(1+nfh+ig-1)+i
                    def(1,n,cpt) = fr(i,1)* (dfdi(n,1) * fe(ig) + ff( n)*dgdgl(ig,1))
!
                    def(2,n,cpt) = fr(i,2)* (dfdi(n,2) * fe(ig) + ff( n)*dgdgl(ig,2))
!
                    def(3,n,cpt) = 0.d0
!
                    def(4,n,cpt) = (&
                                   fr(i,1)* (dfdi(n,2)*fe(ig)+ff(n)* dgdgl(ig,2)) + fr(i,2)* (dfd&
                                   &i(n,1)*fe(ig)+ff(n)* dgdgl(ig,1))&
                                   )/rac2
                    if (ndim .eq. 3) then
                        def(3,n,cpt) = fr(i,3)* (dfdi(n,3) * fe(ig) + ff(n)*dgdgl(ig,3))
                        def(5,n,cpt) = (&
                                       fr(i,1)* (dfdi(n,3)*fe(ig)+ ff(n)*dgdgl(ig,3)) + fr(i,3)* &
                                       &(dfdi(n,1)*fe( ig)+ff(n)*dgdgl(ig,1))&
                                       )/rac2
                        def(6,n,cpt) = (&
                                       fr(i,3)* (dfdi(n,2)*fe(ig)+ ff(n)*dgdgl(ig,2)) + fr(i,2)* &
                                       &(dfdi(n,3)*fe( ig)+ff(n)*dgdgl(ig,3))&
                                       )/rac2
                    endif
156              continue
155          continue
            ASSERT(cpt.eq.ddld)
150      continue
!
!       POUR CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
!       ON ENVOIE DFDM2D OU DFDM3D AVEC LES COORD DU SS-ELT
        if (ndim .eq. 2) then
            call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                        rbid10, rbid10, jac)
        else if (ndim.eq.3) then
            call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                        rbid4, rbid4, rbid4, jac)
        endif
!
!      CALCUL DES PRODUITS DE FONCTIONS DE FORMES (ET DERIVEES)
        if (rigi) then
            do 160 i = 1, ndim
                do 161 n = 1, nnop
                    cpt = 1
                    pff(cpt,n,i) = dfdi(n,i)
                    do 162 ig = 1, nfh
                        cpt = cpt+1
                        pff(cpt,n,i) = dfdi(n,i) * he(fisno(n,ig))
162                  continue
                    do 163 ig = 1, nfe
                        cpt = cpt+1
                        pff(cpt,n,i) = dfdi(n,i)*fe(ig) + ff(n)*dgdgl( ig,i)
163                  continue
                    ASSERT(cpt.eq.ddldn)
161              continue
160          continue
        endif
!
!       LOI DE COMPORTEMENT
!       CONTRAINTE CAUCHY -> CONTRAINTE LAGRANGE POUR LDC EN T-
        if (cplan) fm(3,3) = sqrt(abs(2.d0*epsm(3)+1.d0))
        call matinv('S', 3, fm, fmm, detf)
        call vecini(6, 0.d0, sign)
        do 170 i = 1, 2*ndim
            do 171 l = 1, 2*ndim
                ftf = (&
                      fmm(&
                      indi(i), indi(l)) * fmm(indj(i), indj(l)) + fmm(indi(i),&
                      indj(l)) * fmm(indj(i), indi(l))&
                      ) * rind1(l&
                      )
                sign(i) = sign(i) + ftf * sigm(l,kpg)
171          continue
            sign(i) = sign(i) * detf
170      continue
        if (ndim .eq. 2) sign(4) = sign(4) * rac2
        if (ndim .eq. 3) then
            do 180 m = 4, 2*ndim
                sign(m) = sigm(m,kpg) * rac2
180          continue
        endif
!
!
!       INTEGRATION
!
        call r8inir(6, 0.0d0, sigma, 1)
        call nmcomp('XFEM', idecpg+kpg, 1, ndim, typmod,&
                    imate, compor, crit, instam, instap,&
                    6, epsm, deps, 6, sign,&
                    vi(1, kpg), option, angmas, 10, elgeom(1, kpg),&
                    sigma, vip(1, kpg), 36, dsidep, 1,&
                    rbid1, codret)
!
! - CALCUL DE LA MATRICE DE RIGIDITE
        if (rigi) then
!
!          RIGIDITÉ GEOMETRIQUE
            do 190 n = 1, nnop
                nn=dec(n)
!
                do 191 m = 1, n
                    mn=dec(m)
!
                    do 192 i = 1, ddldn
                        do 193 j = 1, ddldn
                            tmp1 = 0.d0
                            if (option(1:4) .eq. 'RIGI') then
                                tmp1 = sign(1)*pff(i,n,1)*pff(j,m,1) + sign(2)*pff(i,n,2)*pff(j,m&
                                       &,2) + sign(4)*(pff(i,n,1)*pff(j,m,2) +pff(i,n,2)*pff(j,m,&
                                       &1))/rac2
                                if (ndim .eq. 3) then
                                    tmp1 = tmp1 + sign(3)*pff(i,n,3)* pff(j,m,3) + sign(5)*(pff(i&
                                           &,n,1)* pff(j,m,3) +pff(i,n,3)*pff(j,m,1)) /rac2 + sig&
                                           &n(6)*(pff(i,n,3)*pff(j, m,2) +pff(i,n,2)*pff(j,m,3))/&
                                           &rac2
                                endif
                            else
                                tmp1 = sigma(1)*pff(i,n,1)*pff(j,m,1) + sigma(2)*pff(i,n,2)*pff(j&
                                       &,m,2) + sigma(4)*(pff(i,n,1)*pff(j,m,2) +pff(i,n,2)*pff(j&
                                       &,m,1))/rac2
                                if (ndim .eq. 3) then
                                    tmp1 = tmp1 + sigma(3)*pff(i,n,3)* pff(j,m,3) + sigma(5)*(pff&
                                           &(i,n,1)* pff(j,m,3) +pff(i,n,3)*pff(j,m,1)) /rac2 + s&
                                           &igma(6)*(pff(i,n,3)*pff( j,m,2) +pff(i,n,2)*pff(j,m,3&
                                           &))/ rac2
                                endif
                            endif
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (m .eq. n) then
                                j1 = i
                            else
                                j1 = ddldn
                            endif
                            if (j .le. j1) then
                                do 194 l = 1, ndim
                                    kkd = ( nn+(i-1)*ndim+l-1) * (nn+( i-1)*ndim+l ) /2
!
                                    kk = kkd + mn+(j-1)*ndim+l
!
                                    matuu(kk) = matuu(kk) + tmp1*jac
194                              continue
                            endif
193                      continue
192                  continue
191              continue
190          continue
!         RIGIDITE ELASTIQUE
            do 200 n = 1, nnop
                nn=dec(n)
!
                do 201 i = 1, ddld
                    do 202 l = 1, 2*ndim
                        sig(l) = 0.d0
                        do 203 k = 1, 2*ndim
                            sig(l) = sig(l) + def(k,n,i) * dsidep(k,l)
203                      continue
202                  continue
                    do 204 j = 1, ddld
                        do 205 m = 1, n
                            tmp2 = 0.d0
                            mn=dec(m)
                            do 206 k = 1, 2*ndim
                                tmp2 = tmp2 + sig(k) * def(k,m,j)
206                          continue
!
!                STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (m .eq. n) then
                                j1 = i
                            else
                                j1 = ddld
                            endif
                            if (j .le. j1) then
                                kkd = (nn+i-1) * (nn+i) /2
                                matuu(kkd+mn+j) = matuu(kkd+mn+j) + tmp2*jac
                            endif
205                      continue
204                  continue
201              continue
200          continue
        endif
!
! - CALCUL DE LA FORCE INTERIEURE
!
        if (resi) then
!
            do 210 n = 1, nnop
                nn=dec(n)
                do 211 i = 1, ddld
                    do 212 l = 1, 2*ndim
                        zr(ivectu-1+nn+i)= zr(ivectu-1+nn+i) + def(l,&
                        n,i)*sigma(l)*jac
212                  continue
211              continue
210          continue
!
!    CALCUL DES CONTRAINTES DE CAUCHY, CONVERSION LAGRANGE -> CAUCHY
!
            if (cplan) f(3,3) = sqrt(abs(2.d0*epsp(3)+1.d0))
            detf = f(3,3) * (f(1,1)*f(2,2)-f(1,2)*f(2,1))
            if (ndim .eq. 3) then
                detf = detf - f(2,3)*(f(1,1)*f(3,2)-f(3,1)*f(1,2)) + f(1,3)*(f(2,1)*f(3,2)-f(3,1)&
                       &*f(2,2))
            endif
            do 220 i = 1, 2*ndim
                sigp(i,kpg) = 0.d0
                do 221 l = 1, 2*ndim
                    ftf = (&
                          f(&
                          indi(i), indi(l))*f(indj(i), indj(l)) + f(indi(i), indj(l))*f(indj(i),&
                          indi(l))&
                          )*rind(l&
                          )
                    sigp(i,kpg) = sigp(i,kpg) + ftf*sigma(l)
221              continue
                sigp(i,kpg) = sigp(i,kpg)/detf
220          continue
        endif
!
1000  end do
end subroutine
