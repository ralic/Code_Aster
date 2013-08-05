subroutine xsifel(elrefp, ndim, coorse, igeom, jheavt,&
                  ise, nfh, ddlc, ddlm, nfe,&
                  rho, puls, lmoda, basloc, nnop,&
                  idepl, lsn, lst, idecpg, igthet,&
                  fno, nfiss, jfisno)
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
! person_in_charge: samuel.geniaut at edf.fr
!
! aslint: disable=W1306,W1504
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/chauxi.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/elref5.h"
#include "asterfort/gbil3d.h"
#include "asterfort/gbilin.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvad2.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/reeref.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mess.h"
#include "asterfort/vecini.h"
#include "asterfort/xdeffe.h"
#include "asterfort/xderfe.h"
    character(len=8) :: elrefp
    integer :: igeom, ndim, nfh, ddlc, ddlm, nfe, nnop, idecpg, idepl
    integer :: nfiss, jfisno, jheavt, ise
    real(kind=8) :: fno(ndim*nnop), coorse(*)
    real(kind=8) :: basloc(3*ndim*nnop), lsn(nnop), lst(nnop), rho, puls
    logical :: lmoda
!
!
!    - FONCTION REALISEE:  CALCUL DU TAUX DE RESTITUTION D'ENERGIE
!                          ET DES FACTEURS D'INTENSITE DE CONTRAINTES
!                          PAR LA METHODE ENERGETIQUE G-THETA
!                          POUR LES ELEMENTS X-FEM
!
! IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
! IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
! IN  NFH     : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  NFISS   : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT
! IN  JFISNO  : CONNECTIVITE DES FISSURES ET DES DDL HEAVISIDES
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE AUX NOEUDS
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ÉLÉMENT
! IN  DEPL    : DÉPLACEMENTS
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  IDECPG  : POSITION DANS LA FAMILLE 'XFEM' DU 1ER POINT DE GAUSS
!               DU SOUS ELEMENT COURRANT (EN FAIT 1ER POINT : IDECPG+1)
! IN  FNO     : FORCES VOLUMIQUES AUX NOEUDS DE L'ELEMENT PARENT
! IN  RHO     : MASSE VOLUMIQUE
! OUT IGTHET  : G, K1, K2, K3
!
!
    integer :: ithet, imate, icomp, icour, igthet, jtab(2), ncomp
    integer :: ipoids, jcoopg, ivf, idfde, jdfd2, jgano
    integer :: i, j, k, kpg, n, ino, iret, cpt, ig, ipg, in
    integer :: ndimb, nno, nnos, npgbis, ddld, ddls
    integer :: fisno(nnop, nfiss), ifiss
    real(kind=8) :: g, k1, k2, k3, coefk, coeff3, valres(3), alpha, he(nfiss)
    real(kind=8) :: devres(3), e, nu, lambda, mu, ka, c1, c2, c3, xg(ndim)
    real(kind=8) :: fe(4), k3a
    real(kind=8) :: dgdgl(4, 3), xe(ndim), ff(nnop), dfdi(nnop, ndim), f(3, 3)
    real(kind=8) :: eps(6), e1(3), e2(3), norme, e3(3), p(3, 3)
    real(kind=8) :: invp(3, 3), rg, tg, rbid1(4)
    real(kind=8) :: dgdpo(4, 2), dgdlo(4, 3), courb(3, 3, 3), du1dm(3, 3)
    real(kind=8) :: du2dm(3, 3)
    real(kind=8) :: du3dm(3, 3), grad(ndim, ndim), dudm(3, 3), poids, rbid2(4)
    real(kind=8) :: rbid
    real(kind=8) :: dtdm(3, 3), tzero(3), dzero(3, 4), lsng, lstg, th, rbid3(4)
    real(kind=8) :: dudme(3, 4), dtdme(3, 4), du1dme(3, 4), du2dme(3, 4)
    real(kind=8) :: du3dme(3, 4)
    real(kind=8) :: u1l(3), u2l(3), u3l(3), u1(3), u2(3), u3(3), ur, r
    real(kind=8) :: depla(3), theta(3), tgudm(3), tpn(27), tref, tempg
    real(kind=8) :: ttrgu, ttrgv, dfdm(3, 4), cs, coef
    integer :: icodre(3), codrho
    character(len=8) :: nomres(3), elrese(6), fami(6)
    character(len=16) :: phenom
    logical :: lcour, grdepl, axi
    integer :: irese, nnops, ibid
    integer :: ddln, nnon, indenn, mxstac
    parameter      (mxstac=1000)
!
    data     nomres /'E','NU','ALPHA'/
    data     elrese /'SE2','TR3','TE4','SE3','TR6','TE4'/
    data     fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
    call jemarq()
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
!
    ASSERT(nnop.le.mxstac)
    ASSERT((3*ndim*nnop).le.mxstac)
!
    grdepl=.false.
!
!     ATTENTION, DEPL ET VECTU SONT ICI DIMENSIONNÉS DE TELLE SORTE
!     QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES NOEUDS MILIEU
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld=ndim*(1+nfh+nfe)
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls=ddld+ddlc
!
!     NOMBRE DE COMPOSANTES DE PHEAVTO (DANS LE CATALOGUE)
    call tecach('OOO', 'PHEAVTO', 'L', 2, jtab,&
                iret)
    ncomp = jtab(2)
!
!     ELEMENT DE REFERENCE PARENT : RECUP DE NNOPS
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
    axi = lteatt(' ','AXIS','OUI')
!
    if (.not.iselli(elrefp) .and. ndim .le. 2) then
        irese=3
    else
        irese=0
    endif
!
    call jevech('PTHETAR', 'L', ithet)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCOMPOR', 'L', icomp)
!
!     VÉRIFICATION DU CADRE THÉORIQUE DU CALCUL
    if (zk16(icomp-1+1) .ne. 'ELAS' .or. zk16(icomp-1+3) .ne. 'PETIT') call u2mess('F',&
                                                                                   'RUPTURE1_24')
!
!     SOUS-ELEMENT DE REFERENCE
    call elref5(elrese(ndim+irese), fami(ndim+irese), ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
    ASSERT(ndim.eq.ndimb)
!
!     TEMPERATURE DE REF
    call rcvarc(' ', 'TEMP', 'REF', 'RIGI', 1,&
                1, tref, iret)
    if (iret .ne. 0) tref = 0.d0
!
!     TEMPERATURE AUX NOEUDS PARENT
    do 30 ino = 1, nnop
        call rcvarc(' ', 'TEMP', '+', 'NOEU', ino,&
                    1, tpn(ino), iret)
        if (iret .ne. 0) tpn(ino) = 0.d0
30  end do
!
!     FONCTION HEAVYSIDE CSTE SUR LE SS-ÉLT ET PAR FISSURE
!
    do 70 ifiss = 1, nfiss
        he(ifiss) = zi(jheavt-1+ncomp*(ifiss-1)+ise)
70  end do
!
!     RECUPERATION DE LA CONNECTIVITÉ FISSURE - DDL HEAVISIDES
!     ATTENTION !!! FISNO PEUT ETRE SURDIMENTIONNÉ
    if (nfiss .eq. 1) then
        do 40 ino = 1, nnop
            fisno(ino,1) = 1
40      continue
    else
        do 50 ig = 1, nfh
!    ON REMPLIT JUSQU'A NFH <= NFISS
            do 60 ino = 1, nnop
                fisno(ino,ig) = zi(jfisno-1+(ino-1)*nfh+ig)
60          continue
50      continue
    endif
!
!     ------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DU SOUS-TÉTRA
!     ------------------------------------------------------------------
!
    do 10 kpg = 1, npgbis
!
!       INITIALISATIONS
        call vecini(9, 0.d0, dtdm)
        call vecini(9, 0.d0, du1dm)
        call vecini(9, 0.d0, du2dm)
        call vecini(9, 0.d0, du3dm)
!
!       RECUPERATION DES DONNEES MATERIAUX
        ipg = idecpg + kpg
        call rcvad2('XFEM', ipg, 1, '+', zi(imate),&
                    'ELAS', 3, nomres, valres, devres,&
                    icodre)
        if (icodre(3) .ne. 0) then
            valres(3)= 0.d0
            devres(3)= 0.d0
        endif
        e = valres(1)
        nu = valres(2)
        alpha = valres(3)
        k3a = alpha * e / (1.d0-2.d0*nu)
!
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
        call rcvalb('XFEM', ipg, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', 0.d0,&
                    1, 'RHO', rho, codrho, 0)
!
        if ((codrho.ne.0) .and. lmoda) then
            call u2mess('F', 'RUPTURE1_26')
        endif
!
        if (ndim .eq. 3 .or. (ndim.eq.2.and.lteatt(' ','D_PLAN','OUI')) .or. axi) then
!
            lambda = nu*e/((1.d0+nu)*(1.d0-2.d0*nu))
            mu = e/(2.d0*(1.d0+nu))
            ka = 3.d0-4.d0*nu
            coefk = e/(1.d0-nu*nu)
            coeff3=2.d0 * mu
            c1 = lambda + 2.d0 * mu
            c2 = lambda
            c3 = mu
            th = 1.d0
!
        else if (ndim.eq.2.and.lteatt(' ','C_PLAN','OUI')) then
!
            ka = (3.d0-nu)/(1.d0+nu)
            mu = e/(2.d0*(1.d0+nu))
            coefk = e
            coeff3 = mu
            c1 = e/(1.d0-nu*nu)
            c2 = nu*c1
            c3 = mu
            th = (1.d0-2.d0*nu)/(1.d0-nu)
!
        endif
!
!       COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
        call vecini(ndim, 0.d0, xg)
        do 101 i = 1, ndim
            do 102 n = 1, nno
                xg(i) = xg(i) + zr(ivf-1+nno*(kpg-1)+n) * coorse(ndim* (n-1)+i)
102          continue
101      continue
!
!       CALCUL DES FF
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    rbid, rbid, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'NON', xe, ff, dfdi, f,&
                    eps, grad)
!
!       POUR CALCULER LE JACOBIEN DE LA TRANSFO SS-ELT -> SS-ELT REF
!       ON ENVOIE DFDM3D/DFDM2D AVEC LES COORD DU SS-ELT
        if (ndim .eq. 3) call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                                     rbid1, rbid2, rbid3, poids)
        if (ndim .eq. 2) call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                                     rbid1, rbid2, poids)
!
!       --------------------------------------
!       1) COORDONNÉES POLAIRES ET BASE LOCALE
!       --------------------------------------
!
!       BASE LOCALE ET LEVEL SETS AU POINT DE GAUSS
        call vecini(3, 0.d0, e1)
        call vecini(3, 0.d0, e2)
        lsng=0.d0
        lstg=0.d0
        do 100 ino = 1, nnop
            lsng = lsng + lsn(ino) * ff(ino)
            lstg = lstg + lst(ino) * ff(ino)
            do 110 i = 1, ndim
                e1(i) = e1(i) + basloc(3*ndim*(ino-1)+i+ndim) * ff( ino)
                e2(i) = e2(i) + basloc(3*ndim*(ino-1)+i+2*ndim) * ff( ino)
110          continue
100      continue
!
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
!       ET DU DEPL. RADIAL
        if (axi) then
            r = 0.d0
            ur = 0.d0
            do 1000 ino = 1, nnop
                r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
                ur = ur + ff(ino)*zr(idepl-1+ddls*(ino-1)+1)
                do 1001 ig = 1, nfh
                    ur = ur + ff(ino) *zr(idepl-1+ddls*(ino-1)+ndim* ig+1) *he(fisno(ino,ig))
1001              continue
                do 1002 ig = 1, nfe
                    ur = ur + ff(ino) *zr(idepl-1+ddls*(ino-1)+ndim*( nfh+ig)+1) *fe(ig)
1002              continue
!
1000          continue
!
            if (axi) then
                poids= poids * r
            endif
! Si R négative, on s'arrete
!
            ASSERT(r.gt.0d0)
        endif
!
!       NORMALISATION DE LA BASE
        call normev(e1, norme)
        call normev(e2, norme)
        call provec(e1, e2, e3)
!
!       CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
        call vecini(9, 0.d0, p)
        do 120 i = 1, ndim
            p(i,1)=e1(i)
            p(i,2)=e2(i)
            p(i,3)=e3(i)
120      continue
!
!       CALCUL DE L'INVERSE DE LA MATRICE DE PASSAGE : INV=TRANSPOSE(P)
        do 130 i = 1, 3
            do 131 j = 1, 3
                invp(i,j)=p(j,i)
131          continue
130      continue
!
!       COORDONNÉES POLAIRES DU POINT
        rg=sqrt(lsng**2+lstg**2)
!
        if (rg .gt. r8prem()) then
!         LE POINT N'EST PAS SUR LE FOND DE FISSURE
            tg = he(1) * abs(atan2(lsng,lstg))
            iret=1
        else
!         LE POINT EST SUR LE FOND DE FISSURE :
!         L'ANGLE N'EST PAS DÉFINI, ON LE MET À ZÉRO
!         ON NE FERA PAS LE CALCUL DES DÉRIVÉES
            tg=0.d0
            iret=0
        endif
!       ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
!       CAR ON SE TROUVE SUR LE FOND DE FISSURE
        ASSERT(iret.ne.0)
!
!       ---------------------------------------------
!       2) CALCUL DU DEPLACEMENT ET DE SA DERIVEE (DUDM)
!       ---------------------------------------------
!
!       FONCTIONS D'ENRICHISSEMENT
        call xdeffe(rg, tg, fe)
!
        call vecini(ndim, 0.d0, depla)
!
!       CALCUL DE L'APPROXIMATION DU DEPLACEMENT
        do 200 in = 1, nnop
            if (in .le. nnops) then
                nnon=0
                ddln=ddls
            else if (in.gt.nnops) then
                nnon=nnops
                ddln=ddlm
            endif
            indenn = ddls*nnon+ddln*(in-nnon-1)
!
            cpt=0
!         DDLS CLASSIQUES
            do 201 i = 1, ndim
                cpt=cpt+1
                depla(i) = depla(i) + ff(in) * zr(idepl-1+indenn+cpt)
201          continue
!         DDLS HEAVISIDE
            do 202 ig = 1, nfh
                do 203 i = 1, ndim
                    cpt=cpt+1
                    depla(i) = depla(i) + he(fisno(in,ig)) * ff(in) * zr(idepl-1+indenn+cpt)
203              continue
202          continue
!         DDL ENRICHIS EN FOND DE FISSURE
            do 204 ig = 1, nfe
                do 205 i = 1, ndim
                    cpt=cpt+1
                    depla(i) = depla(i) + fe(ig) * ff(in) * zr(idepl- 1+indenn+cpt)
205              continue
204          continue
200      continue
!
!       DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE
        call xderfe(rg, tg, dgdpo)
!
!       DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE LOCALE
        do 210 i = 1, 4
            dgdlo(i,1)=dgdpo(i,1)*cos(tg)-dgdpo(i,2)*sin(tg)/rg
            dgdlo(i,2)=dgdpo(i,1)*sin(tg)+dgdpo(i,2)*cos(tg)/rg
            dgdlo(i,3)=0.d0
210      continue
!
!       DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE GLOBALE
        do 220 i = 1, 4
            do 221 j = 1, 3
                dgdgl(i,j)=0.d0
                do 222 k = 1, 3
                    dgdgl(i,j)=dgdgl(i,j)+dgdlo(i,k)*invp(k,j)
222              continue
221          continue
220      continue
!
!       CALCUL DU GRAD DE U AU POINT DE GAUSS
!
!
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    r, ur, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'OUI', xe, ff, dfdi, f,&
                    eps, grad)
!
!       ON RECOPIE GRAD DANS DUDM (CAR PB DE DIMENSIONNEMENT SI 2D)
        do 230 i = 1, ndim
            do 231 j = 1, ndim
                dudm(i,j)=grad(i,j)
231          continue
230      continue
!
!       ------------------------------------------------
!       3) CALCUL DU CHAMP THETA ET DE SA DERIVEE (DTDM)
!       ------------------------------------------------
!
        do 300 i = 1, ndim
!
            theta(i)=0.d0
            do 301 ino = 1, nnop
                theta(i) = theta(i) + ff(ino) * zr(ithet-1+ndim*(ino- 1)+i)
301          continue
!
            do 310 j = 1, ndim
                do 311 ino = 1, nnop
                    dtdm(i,j) = dtdm(i,j) + zr(ithet-1+ndim*(ino-1)+i) * dfdi(ino,j)
311              continue
310          continue
300      continue
!
!       --------------------------------------------------
!       4) CALCUL DU CHAMP DE TEMPERATURE ET DE SA DERIVEE
!       --------------------------------------------------
!
!       TEMPERATURE AU POINT DE GAUSS
        call rcvarc(' ', 'TEMP', '+', 'XFEM', ipg,&
                    1, tempg, iret)
        if (iret .ne. 0) tempg=0.d0
        ttrgu = tempg - tref
        ttrgv = 0.d0
!
        do 400 i = 1, ndim
            tgudm(i)=0.d0
            do 401 ino = 1, nnop
                tgudm(i) = tgudm(i) + dfdi(ino,i) * tpn(ino)
401          continue
400      continue
!
!       ------------------------------------------------
!       5) CALCUL DES CHAMPS AUXILIAIRES ET DE LEURS DERIVEES
!       -----------------------------------------------------
!
        if (ndim .eq. 2) then
!         NON PRISE EN COMPTE DE LA COURBURE
            lcour=.false.
        else if (ndim.eq.3) then
!         PRISE EN COMPTE DE LA COURBURE
            lcour=.true.
!         RECUPERATION DU TENSEUR DE COURBURE
            call jevech('PCOURB', 'L', icour)
            do 500 i = 1, ndim
                do 501 j = 1, ndim
                    courb(i,1,j)=zr(icour-1+ndim*(i-1)+j)
                    courb(i,2,j)=zr(icour-1+ndim*(i+3-1)+j)
                    courb(i,3,j)=zr(icour-1+ndim*(i+6-1)+j)
501              continue
500          continue
        endif
!
        call chauxi(ndim, mu, ka, rg, tg,&
                    invp, lcour, courb, du1dm, du2dm,&
                    du3dm, u1l, u2l, u3l)
!
!       CHAMPS SINGULIERS DANS LA BASE GLOBALE
        call vecini(ndim, 0.d0, u1)
        call vecini(ndim, 0.d0, u2)
        call vecini(ndim, 0.d0, u3)
        do 510 i = 1, ndim
            do 511 j = 1, ndim
                u1(i) = u1(i) + p(i,j) * u1l(j)
                u2(i) = u2(i) + p(i,j) * u2l(j)
                if (ndim .eq. 3) u3(i) = u3(i) + p(i,j) * u3l(j)
511          continue
510      continue
!
!
!       -----------------------------------------------------------
!       6) CALCUL DES FORCES VOLUMIQUES ET DE LEURS DERIVEES (DFDM)
!       -----------------------------------------------------------
!
        call vecini(12, 0.d0, dfdm)
        do 600 ino = 1, nnop
            do 610 j = 1, ndim
                do 620 k = 1, ndim
                    dfdm(j,k) = dfdm(j,k) + fno(ndim*(ino-1)+j)*dfdi( ino,k)
620              continue
!           VALEUR DE LA FORCE DANS LA QUATRIEME COLONNE :
                dfdm(j,4) = dfdm(j,4) + fno(ndim*(ino-1)+j)*ff(ino)
610          continue
600      continue
!
        if (axi) then
            dfdm(3,3)= dfdm(1,4)/r
        endif
!
!       ---------------------------------------------
!       7) CALCUL DE G, K1, K2, K3 AU POINT DE GAUSS
!       --------------------------------------------
!
        call vecini(3, 0.d0, tzero)
        call vecini(12, 0.d0, dzero)
!
!       POUR L'APPEL A GIL3D/GBIL2D, ON STOCKE LES CHAMPS
!       EN DERNIERE COLONNE DES MATRICES DES DERIVEES DES CHAMPS
!       ON ETEND LES MATRICES :
!       EX : DUDM DE DIM (3,3) -> DUDME DE DIM (3,4)
        call vecini(12, 0.d0, dudme)
        call vecini(12, 0.d0, dtdme)
        call vecini(12, 0.d0, du1dme)
        call vecini(12, 0.d0, du2dme)
        call vecini(12, 0.d0, du3dme)
        do 700 i = 1, ndim
            do 701 j = 1, ndim
                dudme(i,j) = dudm(i,j)
                dtdme(i,j) = dtdm(i,j)
                du1dme(i,j) = du1dm(i,j)
                du2dme(i,j) = du2dm(i,j)
                du3dme(i,j) = du3dm(i,j)
701          continue
            dudme(i,4) = depla(i)
            dtdme(i,4) = theta(i)
            du1dme(i,4) = u1(i)
            du2dme(i,4) = u2(i)
            du3dme(i,4) = u3(i)
700      continue
!
        if (axi) then
            dudme(3,3) = dudme(1,4)/r
            dtdme(3,3) = dtdme(1,4)/r
            du1dme(3,3) = du1dme(1,4)/r
            du2dme(3,3) = du2dme(1,4)/r
            du3dme(3,3) = du3dme(1,4)/r
        endif
!
        if (ndim .eq. 3) then
!
            call gbil3d(dudme, dudme, dtdme, dfdm, dfdm,&
                        tgudm, tgudm, ttrgu, ttrgu, poids,&
                        c1, c2, c3, k3a, rho,&
                        puls, g)
            zr(igthet )= zr(igthet) + g
!
            call gbil3d(dudme, du1dme, dtdme, dfdm, dzero,&
                        tgudm, tzero, ttrgu, ttrgv, poids,&
                        c1, c2, c3, k3a, rho,&
                        puls, k1)
            zr(igthet+4 )= zr(igthet+4) + k1 * coefk
            zr(igthet+1 )= zr(igthet+1) + k1 * sqrt(coefk)
!
            call gbil3d(dudme, du2dme, dtdme, dfdm, dzero,&
                        tgudm, tzero, ttrgu, ttrgv, poids,&
                        c1, c2, c3, k3a, rho,&
                        puls, k2)
            zr(igthet+5) = zr(igthet+5) + k2 * coefk
            zr(igthet+2) = zr(igthet+2) + k2 * sqrt(coefk)
!
            call gbil3d(dudme, du3dme, dtdme, dfdm, dzero,&
                        tgudm, tzero, ttrgu, ttrgv, poids,&
                        c1, c2, c3, k3a, rho,&
                        puls, k3)
            zr(igthet+6) = zr(igthet+6) + k3 * coeff3
            zr(igthet+3) = zr(igthet+3) + k3 * sqrt(coeff3)
!
        else if (ndim.eq.2) then
!
!         POUR G, COEF = 2
            coef = 2.d0
            cs = 1.d0
            call gbilin('XFEM', ipg, zi(imate), dudme, dudme,&
                        dtdme, dfdm, tgudm, poids, c1,&
                        c2, c3, cs, th, coef,&
                        rho, puls, axi, g)
!
!         POUR K1, COEF = 1
            coef = 1.d0
            cs = 0.5d0
            call gbilin('XFEM', ipg, zi(imate), dudme, du1dme,&
                        dtdme, dfdm, tgudm, poids, c1,&
                        c2, c3, cs, th, coef,&
                        rho, puls, axi, k1)
            k1 = k1*coefk
!
!         POUR K2, COEF = 1
            coef = 1.d0
            cs = 0.5d0
            call gbilin('XFEM', ipg, zi(imate), dudme, du2dme,&
                        dtdme, dfdm, tgudm, poids, c1,&
                        c2, c3, cs, th, coef,&
                        rho, puls, axi, k2)
            k2 = k2*coefk
            if (e3(3) .lt. 0) k2=-k2
!
            zr(igthet) = zr(igthet) + g
            zr(igthet+1) = zr(igthet+1) + k1/sqrt(coefk)
            zr(igthet+2) = zr(igthet+2) + k2/sqrt(coefk)
            zr(igthet+3) = zr(igthet+3) + k1
            zr(igthet+4) = zr(igthet+4) + k2
!
        endif
!
10  end do
!
!     ------------------------------------------------------------------
!     FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-TÉTRA
!     ------------------------------------------------------------------
!
    call jedema()
end subroutine
