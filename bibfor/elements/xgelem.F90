subroutine xgelem(elrefp, ndim, coorse, igeom, jheavt,&
                  ise, nfh, ddlc, ddlm, nfe,&
                  basloc, nnop, idepl, lsn, lst,&
                  igthet, fno, nfiss, jfisno)
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
!
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/elref5.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nmelnl.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/rcvarc.h"
#include "asterfort/reeref.h"
#include "asterfort/tecach.h"
#include "asterfort/vecini.h"
#include "asterfort/xdeffe.h"
#include "asterfort/xderfe.h"
    character(len=8) :: elrefp
    integer :: igeom, ndim, nfh, ddlc, nfe, nnop, ddlm
    integer :: idepl, nfiss, jfisno, jheavt, ise
    real(kind=8) :: basloc(3*ndim*nnop), lsn(nnop), lst(nnop)
    real(kind=8) :: fno(ndim*nnop), coorse(*)
!
!
!    - FONCTION REALISEE:  CALCUL DU TAUX DE RESTITUTION D'ENERGIE
!                          PAR LA METHODE ENERGETIQUE G-THETA
!                          POUR LES ELEMENTS X-FEM (2D ET 3D)
!
! IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
! IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
! IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
! IN  NFH     : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  NFISS   : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT
! IN  JFISNO  : CONNECTIVITE DES FISSURES ET DES DDL HEAVISIDES
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE AUX NOEUDS
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  DEPL    : DÉPLACEMENTS
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  FNO     : FORCES VOLUMIQUES AUX NOEUDS DE L'ELEMENT PARENT
! OUT IGTHET  : G
!
!
    integer :: ithet, imate, icomp, igthet, jtab(2), ncomp
    integer :: ipoids, jcoopg, ivf, idfde, jdfd2, jgano
    integer :: i, j, k, kpg, n, ino, iret, cpt, ig, in, mxstac
    integer :: ndimb, nno, nnos, npgbis, ddld, ddls, matcod, m, irett
    real(kind=8) :: xg(ndim), fe(4), he(nfiss)
    real(kind=8) :: dgdgl(4, 3), xe(ndim), ff(nnop), dfdi(nnop, ndim), f(3, 3)
    real(kind=8) :: eps(6), e1(3), e2(3), norme, e3(3), p(3, 3)
    real(kind=8) :: invp(3, 3), rg, tg, rbid1(4)
    real(kind=8) :: dgdpo(4, 2), dgdlo(4, 3)
    real(kind=8) :: grad(ndim, ndim), dudm(3, 4), poids, rbid2(4)
    real(kind=8) :: dtdm(3, 4), lsng, lstg, rbid3(4)
    real(kind=8) :: rbid
    real(kind=8) :: tthe, r
    real(kind=8) :: depla(3), theta(3), tgudm(3), tpn(27), tref
    real(kind=8) :: crit(3), dfdm(3, 4)
    real(kind=8) :: energi(2), sigl(6), prod, prod2, rac2, sr(3, 3), tcla, divt
    real(kind=8) :: tfor,dsidep(6,6)
    character(len=8) :: elrese(6), fami(6), typmod(2)
    character(len=16) :: compor(4), oprupt
    logical :: grdepl, cp, axi
    integer :: irese, ddli, nnoi, indeni, ibid, nnops, fisno(nnop, nfiss), ifiss
!
    parameter      (mxstac=1000)
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','TE4'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
!
    call jemarq()
!
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    ASSERT(ndim.le.mxstac)
    ASSERT(nnop.le.mxstac)
    ASSERT(nfiss.le.mxstac)
!
    grdepl=.false.
!
    if (.not.iselli(elrefp) .and. ndim .le. 2) then
        irese=3
    else
        irese=0
    endif
!
    typmod(2) = ' '
    cp = .false.
    oprupt = 'RUPTURE'
    rac2 = sqrt(2.d0)
    tcla = 0.d0
    tthe = 0.d0
    tfor = 0.d0
!
    if (lteatt(' ','C_PLAN','OUI')) then
        typmod(1) = 'C_PLAN'
        cp = .true.
    else if (lteatt(' ','D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN'
    endif
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld=ndim*(1+nfh+nfe)
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls=ddld+ddlc
!
!     NOMBRE DE COMPOSANTES DE PHEAVTO (DANS LE CATALOGUE)
    call tecach('OOO', 'PHEAVTO', 'L', iret, nval=2,&
                itab=jtab)
    ncomp = jtab(2)
!
!     ELEMENT DE REFERENCE PARENT : RECUP DE NNOPS
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
    axi = lteatt(' ','AXIS','OUI')
!
    call jevech('PTHETAR', 'L', ithet)
    call jevech('PMATERC', 'L', imate)
    matcod = zi(imate)
    call jevech('PCOMPOR', 'L', icomp)
    do 11 i = 1, 4
        compor(i)= zk16(icomp+i-1)
11  end do
!
!     SOUS-ELEMENT DE REFERENCE
    call elref5(elrese(ndim+irese), fami(ndim+irese), ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
    ASSERT(ndim.eq.ndimb)
!
!     TEMPERATURE DE REF
    call rcvarc(' ', 'TEMP', 'REF', 'RIGI', 1,&
                1, tref, irett)
    if (irett .ne. 0) tref = 0.d0
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
!     ------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DU SOUS-TÉTRA
!     ------------------------------------------------------------------
!
    do 10 kpg = 1, npgbis
!
!       INITIALISATIONS
        call vecini(12, 0.d0, dtdm)
        call vecini(12, 0.d0, dudm)
!
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
!
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
        if (axi) then
            r = 0.d0
            do 1000 ino = 1, nnop
                r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
1000          continue
!
            poids= poids * r
            ASSERT(r.gt.0d0)
!
        endif
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
                nnoi=0
                ddli=ddls
            else if (in.gt.nnops) then
                nnoi=nnops
                ddli=ddlm
            endif
            indeni = ddls*nnoi+ddli*(in-nnoi-1)
!
            cpt=0
!         DDLS CLASSIQUES
            do 201 i = 1, ndim
                cpt=cpt+1
                depla(i) = depla(i) + ff(in)*zr(idepl-1+indeni+cpt)
201          continue
!         DDLS HEAVISIDE
            do 202 ig = 1, nfh
                do 203 i = 1, ndim
                    cpt=cpt+1
                    depla(i) = depla(i) + he(fisno(in,ig)) * ff(in) * zr(idepl-1+indeni+cpt)
203              continue
202          continue
!         DDL ENRICHIS EN FOND DE FISSURE
            do 204 ig = 1, nfe
                do 205 i = 1, ndim
                    cpt=cpt+1
                    depla(i) = depla(i) + fe(ig) * ff(in) * zr(idepl- 1+indeni+cpt)
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
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    rbid, rbid, fisno, nfiss, nfh,&
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
!       VALEUR DU DEPLACEMENT DANS LA QUATRIEME COLONNE :
        do 240 i = 1, ndim
            dudm(i,4) = depla(i)
240      continue
!
!       TRAITEMENTS DEPENDANT DE LA MODELISATION
        if (cp) then
            dudm(3,3)= eps(3)
        endif
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
        divt = 0.d0
        do 437 i = 1, ndim
            divt = divt + dtdm(i,i)
437      continue
!
!       --------------------------------------------------
!       4) CALCUL DU CHAMP DE TEMPERATURE ET DE SA DERIVEE
!       --------------------------------------------------
!
        do 400 i = 1, ndim
            tgudm(i)=0.d0
            do 401 ino = 1, nnop
                tgudm(i) = tgudm(i) + dfdi(ino,i) * tpn(ino)
401          continue
400      continue
!
!       --------------------------------------------------
!       5) CALCUL DE LA CONTRAINTE ET DE L ENERGIE
!       --------------------------------------------------
!
        crit(1) = 300
        crit(2) = 0.d0
        crit(3) = 1.d-3
        call nmelnl('RIGI', kpg, 1, '+', ndim,&
                    typmod, matcod, compor, crit, oprupt,&
                    eps, sigl, rbid, dsidep, energi)
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
!
!       --------------------------------------------------
!              TERME THERMOELAS. CLASSIQUE :
!       F.SIG:(GRAD(U).GRAD(THET))-ENER*DIVT
!       --------------------------------------------------
!
        sr(1,1)= sigl(1)
        sr(2,2)= sigl(2)
        sr(3,3)= sigl(3)
        sr(1,2)= sigl(4)/rac2
        sr(2,1)= sr(1,2)
        sr(1,3)= sigl(5)/rac2
        sr(3,1)= sr(1,3)
        sr(2,3)= sigl(6)/rac2
        sr(3,2)= sr(2,3)
!
        prod = 0.d0
        prod2 = 0.d0
        do 490 i = 1, ndim
            do 480 j = 1, ndim
                do 475 k = 1, ndim
                    do 470 m = 1, ndim
                        prod =prod+f(i,j)*sr(j,k)*dudm(i,m)*dtdm(m,k)
470                  continue
475              continue
480          continue
490      continue
        prod2 = poids*( prod - energi(1)*divt)
        tcla = tcla + prod2
!
!
!       =======================================================
!       TERME THERMIQUE :   -(D(ENER)/DT)(GRAD(T).THETA)
!       =======================================================
        if (irett .eq. 0) then
            prod = 0.d0
            prod2 = 0.d0
            do 500 i = 1, ndim
                prod = prod + tgudm(i)*theta(i)
500          continue
            prod2 = - poids*prod*energi(2)
            tthe = tthe + prod2
        endif
!
!       =======================================================
!       TERME FORCE VOLUMIQUE
!       =======================================================
!
        do 580 i = 1, ndim
            prod = 0.d0
            do 570 j = 1, ndim
                prod = prod + dfdm(i,j)*dtdm(j,4)
570          continue
            tfor = tfor + dudm(i,4)* (prod + dfdm(i,4)*divt) * poids
580      continue
!
10  end do
!
!     ------------------------------------------------------------------
!     FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-TÉTRA
!     ------------------------------------------------------------------
!
    zr(igthet) = zr(igthet) + tcla + tthe + tfor
!
    call jedema()
end subroutine
