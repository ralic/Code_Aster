subroutine xgelem(elrefp, ndim, coorse, igeom, jheavt,&
                  ise, nfh, ddlc, ddlm, nfe,&
                  basloc, nnop, idepl, lsn, lst,&
                  igthet, fno, nfiss, jfisno, isig,&
                  incr)
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
#include "asterfort/rccoma.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvarc.h"
#include "asterfort/reeref.h"
#include "asterfort/tecach.h"
#include "asterfort/vecini.h"
#include "asterfort/xcinem.h"
#include "asterfort/xdeffe.h"
#include "asterfort/xderfe.h"
    character(len=8) :: elrefp
    integer :: igeom, ndim, nfh, ddlc, nfe, nnop, ddlm
    integer :: idepl, nfiss, jfisno, jheavt, ise
    real(kind=8) :: basloc(3*ndim*nnop), lsn(nnop), lst(nnop)
    real(kind=8) :: fno(ndim*nnop), coorse(*)
!
    integer :: isig
    logical :: incr
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
    real(kind=8) :: invp(3, 3), rg, tg
    real(kind=8) :: dgdpo(4, 2), dgdlo(4, 3)
    real(kind=8) :: grad(ndim, ndim), dudm(3, 4), poids
    real(kind=8) :: dtdm(3, 4), lsng, lstg
    real(kind=8) :: rbid
    real(kind=8) :: tthe, r
    real(kind=8) :: depla(3), theta(3), tgudm(3), tpn(27), tref
    real(kind=8) :: crit(3), dfdm(3, 4)
    real(kind=8) :: energi(2), sigl(6), prod, prod2, rac2, sr(3, 3), tcla, divt
    real(kind=8) :: tfor, dsidep(6, 6)
    character(len=8) :: elrese(6), fami(6), typmod(2)
    character(len=16) :: compor(4), oprupt
    logical :: grdepl, cp,  axi
    integer :: irese, ddli, nnoi, indeni, ibid, nnops, fisno(nnop, nfiss), ifiss
!
!
    real(kind=8) :: tini, prod1, dsigin(6, 3), sigin(6), epsref(6)
    real(kind=8) :: mu, nu(1), e(1)
    integer :: ij, l, i1, icodre(1), ncmp
    character(len=16) :: phenom
!
!
!
    parameter      (mxstac=1000)
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
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
    if (.not.iselli(elrefp)) then
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
    tini = 0.d0
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
!     NOMBRE DE COMPOSANTES DES TENSEURS
    ncmp = 2*ndim
!
    call jevech('PTHETAR', 'L', ithet)
    call jevech('PMATERC', 'L', imate)
    matcod = zi(imate)
    call jevech('PCOMPOR', 'L', icomp)
    do i = 1, 4
        compor(i)= zk16(icomp+i-1)
    end do
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
    do ino = 1, nnop
        call rcvarc(' ', 'TEMP', '+', 'NOEU', ino,&
                    1, tpn(ino), iret)
        if (iret .ne. 0) tpn(ino) = 0.d0
    end do
!
!     FONCTION HEAVYSIDE CSTE SUR LE SS-ÉLT ET PAR FISSURE
!
    do ifiss = 1, nfiss
        he(ifiss) = zi(jheavt-1+ncomp*(ifiss-1)+ise)
    end do
!
!     RECUPERATION DE LA CONNECTIVITÉ FISSURE - DDL HEAVISIDES
!     ATTENTION !!! FISNO PEUT ETRE SURDIMENTIONNÉ
    if (nfiss .eq. 1) then
        do ino = 1, nnop
            fisno(ino,1) = 1
        end do
    else
        do ig = 1, nfh
!    ON REMPLIT JUSQU'A NFH <= NFISS
            do ino = 1, nnop
                fisno(ino,ig) = zi(jfisno-1+(ino-1)*nfh+ig)
            end do
        end do
    endif
!     ------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DU SOUS-TÉTRA
!     ------------------------------------------------------------------
!
    do kpg = 1, npgbis
        l = (kpg-1)*nno
!
!       INITIALISATIONS
        call vecini(12, 0.d0, dtdm)
        call vecini(12, 0.d0, dudm)
        do 12 i = 1, ncmp
            sigin(i) = 0.d0
            epsref(i)= 0.d0
            do 13 j = 1, 3
                dsigin(i,j) = 0.d0
13          continue
12      continue
!
!
!       COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
        call vecini(ndim, 0.d0, xg)
        do i = 1, ndim
            do n = 1, nno
                xg(i) = xg(i) + zr(ivf-1+nno*(kpg-1)+n) * coorse(ndim* (n-1)+i)
            end do
        end do
!
!       CALCUL DES FF
        call reeref(elrefp, nnop, zr(igeom), xg, ndim, xe, ff)
!
!       POUR CALCULER LE JACOBIEN DE LA TRANSFO SS-ELT -> SS-ELT REF
!       ON ENVOIE DFDM3D/DFDM2D AVEC LES COORD DU SS-ELT
        if (ndim .eq. 3) call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                                     poids)
        if (ndim .eq. 2) call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                                     poids)
!
!
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
        if (axi) then
            r = 0.d0
            do ino = 1, nnop
                r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
            end do
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
        do ino = 1, nnop
            lsng = lsng + lsn(ino) * ff(ino)
            lstg = lstg + lst(ino) * ff(ino)
            do i = 1, ndim
                e1(i) = e1(i) + basloc(3*ndim*(ino-1)+i+ndim) * ff( ino)
                e2(i) = e2(i) + basloc(3*ndim*(ino-1)+i+2*ndim) * ff( ino)
            end do
        end do
!
!       NORMALISATION DE LA BASE
        call normev(e1, norme)
        call normev(e2, norme)
        call provec(e1, e2, e3)
!
!       CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
        call vecini(9, 0.d0, p)
        do i = 1, ndim
            p(i,1)=e1(i)
            p(i,2)=e2(i)
            p(i,3)=e3(i)
        end do
!
!       CALCUL DE L'INVERSE DE LA MATRICE DE PASSAGE : INV=TRANSPOSE(P)
        do i = 1, 3
            do j = 1, 3
                invp(i,j)=p(j,i)
            end do
        end do
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
        do in = 1, nnop
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
            do i = 1, ndim
                cpt=cpt+1
                depla(i) = depla(i) + ff(in)*zr(idepl-1+indeni+cpt)
            end do
!         DDLS HEAVISIDE
            do ig = 1, nfh
                do i = 1, ndim
                    cpt=cpt+1
                    depla(i) = depla(i) + he(fisno(in,ig)) * ff(in) * zr(idepl-1+indeni+cpt)
                end do
            end do
!         DDL ENRICHIS EN FOND DE FISSURE
            do ig = 1, nfe
                do i = 1, ndim
                    cpt=cpt+1
                    depla(i) = depla(i) + fe(ig) * ff(in) * zr(idepl- 1+indeni+cpt)
                end do
            end do
        end do
!
!       DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE
        call xderfe(rg, tg, dgdpo)
!
!       DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE LOCALE
        do i = 1, 4
            dgdlo(i,1)=dgdpo(i,1)*cos(tg)-dgdpo(i,2)*sin(tg)/rg
            dgdlo(i,2)=dgdpo(i,1)*sin(tg)+dgdpo(i,2)*cos(tg)/rg
            dgdlo(i,3)=0.d0
        end do
!
!       DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE GLOBALE
        do i = 1, 4
            do j = 1, 3
                dgdgl(i,j)=0.d0
                do k = 1, 3
                    dgdgl(i,j)=dgdgl(i,j)+dgdlo(i,k)*invp(k,j)
                end do
            end do
        end do
!
!       CALCUL DU GRAD DE U AU POINT DE GAUSS
        call reeref(elrefp, nnop, zr(igeom), xg, ndim, xe, ff, dfdi=dfdi)
        call xcinem(axi, nnop, nnos, idepl, grdepl, ndim, he,&
                    rbid, rbid, fisno, nfiss, nfh, nfe, ddls, ddlm,&
                    fe, dgdgl, ff, dfdi, f, eps, grad)
!
!       ON RECOPIE GRAD DANS DUDM (CAR PB DE DIMENSIONNEMENT SI 2D)
        do i = 1, ndim
            do j = 1, ndim
                dudm(i,j)=grad(i,j)
            end do
        end do
!
!       VALEUR DU DEPLACEMENT DANS LA QUATRIEME COLONNE :
        do i = 1, ndim
            dudm(i,4) = depla(i)
        end do
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
        do i = 1, ndim
!
            theta(i)=0.d0
            do ino = 1, nnop
                theta(i) = theta(i) + ff(ino) * zr(ithet-1+ndim*(ino- 1)+i)
            end do
!
            do j = 1, ndim
                do ino = 1, nnop
                    dtdm(i,j) = dtdm(i,j) + zr(ithet-1+ndim*(ino-1)+i) * dfdi(ino,j)
                end do
            end do
        end do
!
        divt = 0.d0
        do i = 1, ndim
            divt = divt + dtdm(i,i)
        end do
!
!       --------------------------------------------------
!       4) CALCUL DU CHAMP DE TEMPERATURE ET DE SA DERIVEE
!       --------------------------------------------------
!
        do i = 1, ndim
            tgudm(i)=0.d0
            do ino = 1, nnop
                tgudm(i) = tgudm(i) + dfdi(ino,i) * tpn(ino)
            end do
        end do
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
!       --------------------------------------------------
!       6)   CORRECTIONS LIEES A LA CONTRAINTE INITIALE (SIGM_INIT DE CA
!       --------------------------------------------------
!
        if (incr) then
            do 460 i = 1, nnop
                i1 = i-1
!
!           CALCUL DE SIGMA INITIAL
                ij = isig+ncmp*i1-1
                do 440 j = 1, ncmp
                    sigin(j) = sigin(j)+ zr(ij+j)*zr(ivf+l+i1)
440             continue
!
!           CALCUL DU GRADIENT DE SIGMA INITIAL
                do 455 j = 1, ncmp
                    do 450 k = 1, ndim
                        dsigin(j,k)=dsigin(j,k)+zr(ij+j)*dfdi(i,k)
450                 continue
455             continue
460         continue

!
!         TRAITEMENTS PARTICULIERS DES TERMES CROISES
            do 463 i = 4, ncmp
                sigin(i) = sigin(i)*rac2
                do 462 j = 1, ndim
                    dsigin(i,j) = dsigin(4,1)*rac2
462             continue
463         continue
!
!         CALCUL DE LA DEFORMATION DE REFERENCE
            call rccoma(matcod, 'ELAS', 1, phenom, icodre(1))
            call rcvala(matcod, ' ', phenom, 1, ' ',&
                        [rbid], 1, 'NU', nu(1), icodre(1),&
                        1)
            call rcvala(matcod, ' ', phenom, 1, ' ',&
                        [rbid], 1, 'E', e(1), icodre(1),&
                        1)
!
            mu = e(1)/(2.d0*(1.d0+nu(1)))
!
            epsref(1)=-(1.d0/e(1))*(sigin(1)-(nu(1)*(sigin(2)+sigin(3))))
            epsref(2)=-(1.d0/e(1))*(sigin(2)-(nu(1)*(sigin(3)+sigin(1))))
            epsref(3)=-(1.d0/e(1))*(sigin(3)-(nu(1)*(sigin(1)+sigin(2))))
            epsref(4)=-(1.d0/mu)*sigin(4)
            epsref(5)=-(1.d0/mu)*sigin(5)
            epsref(6)=-(1.d0/mu)*sigin(6)
!
!         ENERGIE ELASTIQUE (expression WADIER)
            do 465 i = 1, ncmp
                energi(1) = energi(1) + (eps(i)-0.5d0*epsref(i))* sigin(i)
465         continue
        endif
!
!       -----------------------------------------------------------
!       7) CALCUL DES FORCES VOLUMIQUES ET DE LEURS DERIVEES (DFDM)
!       -----------------------------------------------------------
!
        call vecini(12, 0.d0, dfdm)
        do ino = 1, nnop
            do j = 1, ndim
                do k = 1, ndim
                    dfdm(j,k) = dfdm(j,k) + fno(ndim*(ino-1)+j)*dfdi( ino,k)
                end do
!           VALEUR DE LA FORCE DANS LA QUATRIEME COLONNE :
                dfdm(j,4) = dfdm(j,4) + fno(ndim*(ino-1)+j)*ff(ino)
            end do
        end do
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
        do i = 1, ndim
            do j = 1, ndim
                do k = 1, ndim
                    do m = 1, ndim
                        prod =prod+f(i,j)*sr(j,k)*dudm(i,m)*dtdm(m,k)
                    end do
                end do
            end do
        end do
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
            do i = 1, ndim
                prod = prod + tgudm(i)*theta(i)
            end do
            prod2 = - poids*prod*energi(2)
            tthe = tthe + prod2
        endif
!
!       =======================================================
!       TERME FORCE VOLUMIQUE
!       =======================================================
!
        do i = 1, ndim
            prod = 0.d0
            do j = 1, ndim
                prod = prod + dfdm(i,j)*dtdm(j,4)
            end do
            tfor = tfor + dudm(i,4)* (prod + dfdm(i,4)*divt) * poids
        end do
!
!       =======================================================
!       TERME CONTRAINTE INITIALE SIGIN
!       =======================================================
!
        if (incr) then
            prod1=0.d0
            do 670 i = 1, ncmp
                do 660 j = 1, ndim
!                     prod1=prod1-(eps(i)-epsref(i))*dsigin(i,j)*theta(&
!                     j)
                    prod1=prod1-(eps(i)-epsref(i))*dsigin(i,j)*dtdm(j,4)
660              continue
670          continue
            tini = tini + prod1*poids
        endif
    end do
!
!     ------------------------------------------------------------------
!     FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-TÉTRA
!     ------------------------------------------------------------------
!
!      ZR(IGTHET) = ZR(IGTHET) + TCLA + TTHE + TFOR + TINI
!
    zr(igthet) = zr(igthet) + tcla + tthe + tfor + tini
!
    call jedema()
end subroutine
