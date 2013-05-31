subroutine xxbsig(option, elrefp, elrese, ndim, coorse,&
                  igeom, he, nfh, ddlc, ddlm,&
                  nfe, basloc, nnop, npg, sigma,&
                  compor, idepl, lsn, lst, nfiss,&
                  fisno, codopt, ivectu)
!
! aslint: disable=W1306,W1504
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/elref5.h'
    include 'asterfort/indent.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/reeref.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xcalf2.h'
    include 'asterfort/xcalfe.h'
    integer :: ddlc, ddlm, fisno(nnop, nfiss)
    integer :: codopt, idepl, igeom, ivectu
    integer :: ndim, nfe, nfh, nfiss, nnop, npg
    real(kind=8) :: basloc(3*ndim*nnop), coorse(*), he(nfiss)
    real(kind=8) :: lsn(nnop), lst(nnop)
    real(kind=8) :: sigma(codopt*(2*ndim-1)+1, codopt*(npg-1)+1)
    character(len=8) :: elrefp, elrese
    character(len=16) :: option, compor(4)
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
!.......................................................................
!
!     BUT:  CALCUL  DU PRODUIT BT. SIGMA SUR UN SOUS-ELEMENT X-FEM
!.......................................................................
!
! IN  OPTION  : NOM DE L'OPTION CALCULEE PAR LE TE APPELANT
! IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
! IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
! IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
! IN  NFH     : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  DDLM    : NOMBRE DE DDL PAR NOEUD MILIEU (EN 2D)
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ÉLÉMENT
! IN  SIGMA   : CONTRAINTES DE CAUCHY
! IN  COMPOR  : COMPORTEMENT
! IN  IDEPL   : ADRESSE DU DEPLACEMENT A PARTIR DE LA CONF DE REF
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  CODOPT  : CODE DE L OPTION, 0:REFE_FORC_NODA, 1:FORC_NODA
!
! OUT IVECTU  : ADRESSE DU VECTEUR BT.SIGMA
!
!......................................................................
    integer :: kpg, i, ig, n, nn, m, dec(nnop)
    integer :: ddld, ddls, nno, nnops, nnos, npgbis, cpt, iret
    integer :: ibid, idfde, ipoids, ivf, jcoopg, jdfd2, jgano
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac, lsng, lstg
    real(kind=8) :: rbid, rbid4(4), rbid6(6), rbid10(10), rbid33(3, 3)
    real(kind=8) :: dfdi(nnop, ndim), f(3, 3), fe(4), baslog(3*ndim)
    real(kind=8) :: dgdgl(4, 3)
    real(kind=8) :: def(6, nnop, ndim*(1+nfh+nfe)), sign(2*ndim)
    real(kind=8) :: r
    logical :: grdepl, axi
!
    character(len=3) :: cinem
!
    real(kind=8) :: rac2
    data     rac2 / 1.4142135623731D0 /
!--------------------------------------------------------------------
!
!     ATTENTION, EN 3D, ZR(IDEPL) ET ZR(VECTU) SONT DIMENSIONNÉS DE
!     TELLE SORTE QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES
!     NOEUDS MILIEU
!
!     INITIALISATION POUR APPEL A REEREF() SELON LE NOM DE L'OPTION
    if (option .eq. 'FORC_NODA       ') then
        cinem = 'OUI'
    else if (option.eq.'CHAR_MECA_TEMP_R') then
        cinem = 'INI'
    else
        call assert(.false.)
    endif
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld = ndim*(1+nfh+nfe)
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls = ddld+ddlc
!
!     MODELE H.P.P ou GROT_GDEP ?
    grdepl = compor(3) .eq. 'GROT_GDEP'
!
!     RECUPERATION DU NOMBRE DE NOEUDS SOMMETS DE L'ELEMENT PARENT
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
    if (ndim .eq. 2) then
        axi = lteatt(' ','AXIS','OUI')
    else if (ndim.eq.3) then
        axi = .false.
    endif
!     ADRESSE DES COORD DU SOUS ELT EN QUESTION
    call elref5(elrese, 'XINT', ndim, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
!
    call assert(npg.eq.npgbis)
    do 178 n = 1, nnop
        call indent(n, ddls, ddlm, nnops, dec(n))
178  end do
!
!-----------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS
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
!       JUSTE POUR CALCULER LES FF
!
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    r, rbid, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'NON', xe, ff, dfdi, f,&
                    rbid6, rbid33)
!
!
        if (nfe .gt. 0) then
!         BASE LOCALE ET LEVEL SETS AU POINT DE GAUSS
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
!
!         PB DE CALCUL DES DERIVEES DES FONCTIONS SINGULIERES
!         CAR ON SE TROUVE SUR LE FOND DE FISSURE
            call assert(iret.ne.0)
!
        endif
!
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
        if (axi) then
            r = 0.d0
            do 120 n = 1, nnop
                r = r + ff(n)*zr(igeom-1+2*(n-1)+1)
120          continue
!
            call assert(r.gt.0d0)
!          ATTENTION : LE POIDS N'EST PAS X R
!          CE SERA FAIT PLUS TARD AVEC JAC = JAC X R
        endif
!
!       COORDONNÉES DU POINT DE GAUSS DANS L'ÉLÉMENT DE RÉF PARENT : XE
!       ET CALCUL DE FF, DFDI, ET EPS
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    r, rbid, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    cinem, xe, ff, dfdi, f,&
                    rbid6, rbid33)
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
!
!      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        do 140 n = 1, nnop
            cpt = 0
!         FONCTIONS DE FORME CLASSIQUES
            do 141 i = 1, ndim
                cpt = cpt+1
                def(1,n,i) = f(i,1)*dfdi(n,1)
                def(2,n,i) = f(i,2)*dfdi(n,2)
                def(3,n,i) = 0.d0
                def(4,n,i) = (f(i,1)*dfdi(n,2) + f(i,2)*dfdi(n,1))/ rac2
                if (ndim .eq. 3) then
                    def(3,n,i) = f(i,3)*dfdi(n,3)
                    def(5,n,i) = (f(i,1)*dfdi(n,3) + f(i,3)*dfdi(n,1)) /rac2
                    def(6,n,i) = (f(i,2)*dfdi(n,3) + f(i,3)*dfdi(n,2)) /rac2
                endif
141          continue
!
!         TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
            if (axi) then
                def(3,n,1) = f(3,3) * ff(n)/r
            endif
!
!         ENRICHISSEMENT PAR HEAVYSIDE
            do 142 ig = 1, nfh
                do 143 i = 1, ndim
                    cpt = cpt+1
                    do 144 m = 1, 2*ndim
                        def(m,n,cpt) = def(m,n,i) * he(fisno(n,ig))
144                  continue
                    if (ndim .eq. 2) then
                        def(3,n,cpt) = 0.d0
                    endif
143              continue
!
!   TERME DE CORRECTION (3,3) AXI PORTE SUR LE DDL 1+NDIM*IG
                if (axi) then
                    def(3,n,1+ndim*ig) = f(3,3) * ff(n)/r * he(fisno( n,ig))
                endif
!
142          continue
!
!         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIÈRES
            do 145 ig = 1, nfe
                do 146 i = 1, ndim
                    cpt=cpt+1
                    def(1,n,cpt) = f(i,1)* (dfdi(n,1) * fe(ig) + ff(n) *dgdgl(ig,1))
!
                    def(2,n,cpt) = f(i,2)* (dfdi(n,2) * fe(ig) + ff(n) *dgdgl(ig,2))
!
                    def(3,n,cpt) = 0.d0
!
                    def(4,n,cpt) = (&
                                   f(i,1)* (dfdi(n,2)*fe(ig)+ff(n)* dgdgl(ig,2)) + f(i,2)* (dfdi(&
                                   &n,1)*fe(ig)+ff(n)* dgdgl(ig,1))&
                                   )/rac2
!
                    if (ndim .eq. 3) then
                        def(3,n,cpt) = f(i,3)* (dfdi(n,3) * fe(ig) + ff(n)*dgdgl(ig,3))
                        def(5,n,cpt) = (&
                                       f(i,1)* (dfdi(n,3)*fe(ig)+ff( n)*dgdgl(ig,3)) + f(i,3)* (d&
                                       &fdi(n,1)*fe(ig)+ ff(n)*dgdgl(ig,1))&
                                       )/rac2
                        def(6,n,cpt) = (&
                                       f(i,3)* (dfdi(n,2)*fe(ig)+ff( n)*dgdgl(ig,2)) + f(i,2)* (d&
                                       &fdi(n,3)*fe(ig)+ ff(n)*dgdgl(ig,3))&
                                       )/rac2
                    endif
146              continue
!
!   TERME DE CORRECTION (3,3) AXI PORTE SUR LE DDL 1+NDIM*(NFH+IG)
                if (axi) then
                    def(3,n,1+ndim*(nfh+ig)) = f(3,3) * ff(n)/r * fe( ig)
                endif
!
145          continue
!
            call assert(cpt.eq.ddld)
!
140      continue
!
!       CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
!       AVEC LES COORDONNEES DU SOUS-ELEMENT
        if (ndim .eq. 2) then
            call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                        rbid10, rbid10, jac)
        else if (ndim.eq.3) then
            call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                        rbid4, rbid4, rbid4, jac)
        endif
!
!       MODIFICATION DU JACOBIEN SI AXI
        if (axi) then
            jac = jac * r
        endif
!
        if (codopt .eq. 1) then
            do 150 n = 1, 3
                sign(n) = sigma(n ,kpg)
150          continue
            sign(4) = sigma(4,kpg) * rac2
            if (ndim .eq. 3) then
                sign(5) = sigma(5,kpg) * rac2
                sign(6) = sigma(6,kpg) * rac2
            endif
        endif
!
        do 160 n = 1, nnop
            nn=dec(n)
!
            do 161 i = 1, ddld
                do 162 m = 1, 2*ndim
                    if (codopt .eq. 1) then
                        zr(ivectu-1+nn+i)= zr(ivectu-1+nn+i) + def(m,&
                        n,i)*sign(m)*jac
                    else if (codopt.eq.0) then
                        zr(ivectu-1+nn+i)= zr(ivectu-1+nn+i) + abs(&
                        def(m,n,i)*sigma(1,1)*jac)
                    else
                        call assert(.false.)
                    endif
162              continue
161          continue
!
160      continue
!
!
1000  end do
!
end subroutine
