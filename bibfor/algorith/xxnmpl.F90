subroutine xxnmpl(elrefp, elrese, ndim, coorse, igeom,&
                  he, nfh, ddlc, ddlm, nfe,&
                  instam, instap, ideplp, sigm, vip,&
                  basloc, nnop, npg, typmod, option,&
                  imate, compor, lgpg, crit, idepl,&
                  lsn, lst, idecpg, sig, vi,&
                  matuu, ivectu, codret, nfiss, fisno)
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
    include 'asterfort/lcegeo.h'
    include 'asterfort/nmcomp.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/reeref.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xcalf2.h'
    include 'asterfort/xcalfe.h'
    integer :: ndim, igeom, imate, lgpg, codret, nnop, npg
    integer :: nfh, ddlc, ddlm, nfe, idepl, ivectu, ideplp
    integer :: nfiss, fisno(nnop, nfiss), idecpg
    character(len=8) :: elrefp, typmod(*)
    character(len=8) :: elrese
    character(len=16) :: option, compor(4)
    real(kind=8) :: basloc(3*ndim*nnop), crit(3), he(nfiss)
    real(kind=8) :: lsn(nnop), lst(nnop), coorse(*)
    real(kind=8) :: vi(lgpg, npg), vip(lgpg, npg), sig(2*ndim, npg), matuu(*)
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
! person_in_charge: samuel.geniaut at edf.fr
!
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN HYPER-ELASTICITE AVEC X-FEM EN 2D ET EN 3D
!.......................................................................
!
! IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
! IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
! IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
! IN  NFH     : NOMBRE DE FONCTIONS HEAVYSIDE
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
! OUT SIG     : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VI      : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT IVECTU  : VECTEUR FORCES NODALES (RAPH_MECA ET FULL_MECA)
!......................................................................
!
    integer :: i, ig, iret, j, j1, kkd, kl, kpg, l, m, n, nn, mn
    integer :: ddls, ddld, cpt, ibid, idfde, ipoids, ivf, dec(nnop)
    integer :: jcoopg, jdfd2, jgano, ndimb, nno, nnops, nnos, npgbis
    real(kind=8) :: dsidep(6, 6), f(3, 3), eps(6), deps(6), sigma(6), ftf, detf
    real(kind=8) :: tmp1, tmp2, sigp(6), fe(4), baslog(3*ndim)
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac, lsng, lstg
    real(kind=8) :: rbid, rbid4(4), rbid10(10), rbid33(3, 3)
    real(kind=8) :: dfdi(nnop, ndim), pff(6, nnop, nnop), dgdgl(4, 3)
    real(kind=8) :: def(6, nnop, ndim*(1+nfh+nfe)), r, ur
    real(kind=8) :: elgeom(10, 27), dfdib(27, 3), deplb1(3, 27), deplb2(3, 27)
    logical :: grdepl, axi, cplan
!
    integer :: indi(6), indj(6)
    real(kind=8) :: rind(6), rac2, angmas(3)
!
    data    indi / 1 , 2 , 3 , 1 , 1 , 2 /
    data    indj / 1 , 2 , 3 , 2 , 3 , 3 /
    data    rind / 0.5d0,0.5d0,0.5d0,0.70710678118655D0,&
     &               0.70710678118655D0,0.70710678118655D0 /
    data    rac2 / 1.4142135623731D0 /
    data    angmas /0.d0, 0.d0, 0.d0/
!--------------------------------------------------------------------
!     ATTENTION, EN 3D, ZR(IDEPL) ET ZR(VECTU) SONT DIMENSIONNÉS DE
!     TELLE SORTE QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES
!     NOEUDS MILIEU
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
    ddld = ndim*(1+nfh+nfe)
!
!     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
    ddls = ddld+ddlc
!
!     RECUPERATION DU NOMBRE DE NOEUDS SOMMETS DE L'ELEMENT PARENT
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
! - INITIALISATION
    grdepl = .false.
    axi = typmod(1) .eq. 'AXIS'
    cplan = typmod(1) .eq. 'C_PLAN'
!
!
    call elref5(elrese, 'XINT', ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
!
    call assert(npg.eq.npgbis.and.ndim.eq.ndimb)
!
! - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES LOIS DE COMPORTEMENT
    call lcegeo(nno, npg, ipoids, ivf, idfde,&
                zr(igeom), typmod, compor, ndim, dfdib,&
                deplb1, deplb2, elgeom)
!
    do 178 n = 1, nnop
        call indent(n, ddls, ddlm, nnops, dec(n))
178  end do
!-----------------------------------------------------------------------
! - CALCUL POUR CHAQUE POINT DE GAUSS
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
!             JUSTE POUR CALCULER LES FF
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    rbid, rbid, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'NON', xe, ff, dfdi, f,&
                    eps, rbid33)
!
        if (nfe .gt. 0) then
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
            call assert(iret.ne.0)
        endif
!
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
!       ET DU DEPL. RADIAL
        if (axi) then
            r = 0.d0
            ur = 0.d0
            do 120 n = 1, nnop
                r = r + ff(n)*zr(igeom-1+2*(n-1)+1)
                ur = ur + ff(n)*zr(idepl-1+ddls*(n-1)+1)
                do 121 ig = 1, nfh
                    ur = ur + ff(n) *zr(idepl-1+ddls*(n-1)+ndim*ig+1)
121              continue
                do 122 ig = 1, nfe
                    ur = ur + ff(n) *zr(idepl-1+ddls*(n-1)+ndim*(nfh+ ig)+1) *fe(ig)
!
122              continue
!
120          continue
!
            call assert(r.gt.0d0)
!          ATTENTION : LE POIDS N'EST PAS X R
!          CE SERA FAIT PLUS TARD AVEC JAC = JAC X R
        endif
!
!       COORDONNÉES DU POINT DE GAUSS DANS L'ÉLÉMENT DE RÉF PARENT : XE
!       ET CALCUL DE FF, DFDI, DEPS ET EPS
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, ideplp, grdepl, ndim, he,&
                    r, ur, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'OUI', xe, ff, dfdi, f,&
                    deps, rbid33)
!
        call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                    xg, idepl, grdepl, ndim, he,&
                    r, ur, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'OUI', xe, ff, dfdi, f,&
                    eps, rbid33)
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
!       CALCUL DES PRODUITS SYMETR. DE F PAR N,
        do 130 n = 1, nnop
!         FONCTIONS DE FORME CLASSIQUES
            do 131 i = 1, ndim
                def(1,n,i) = f(i,1)*dfdi(n,1)
                def(2,n,i) = f(i,2)*dfdi(n,2)
                def(3,n,i) = 0.d0
                def(4,n,i) = (f(i,1)*dfdi(n,2) + f(i,2)*dfdi(n,1))/ rac2
                if (ndim .eq. 3) then
                    def(3,n,i) = f(i,3)*dfdi(n,3)
                    def(5,n,i) = (f(i,1)*dfdi(n,3) + f(i,3)*dfdi(n,1)) /rac2
                    def(6,n,i) = (f(i,2)*dfdi(n,3) + f(i,3)*dfdi(n,2)) /rac2
                endif
131          continue
!
!       TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
            if (axi) then
                def(3,n,1) = f(3,3) * ff(n)/r
            endif
!
!         ENRICHISSEMENT PAR HEAVYSIDE
            do 132 ig = 1, nfh
                do 133 i = 1, ndim
                    cpt=ndim*(1+ig-1)+i
                    do 134 m = 1, 2*ndim
                        def(m,n,cpt) = def(m,n,i) * he(fisno(n,ig))
134                  continue
                    if (ndim .eq. 2) then
                        def(3,n,cpt) = 0.d0
                    endif
133              continue
!
!  ATTENTION:TERME DE CORRECTION (3,3) AXI SUR LE DDL 1+NDIM*IG
                if (axi) then
                    def(3,n,(1+ndim*ig)) = f(3,3) * ff(n)/r * he( fisno(n,ig))
                endif
!
132          continue
!         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIÈRES
            do 135 ig = 1, nfe
                do 136 i = 1, ndim
                    cpt = ndim*(1+nfh+ig-1)+i
                    def(1,n,cpt) = f(i,1) * (dfdi(n,1) * fe(ig) + ff( n)*dgdgl(ig,1))
!
                    def(2,n,cpt) = f(i,2) * (dfdi(n,2) * fe(ig) + ff( n)*dgdgl(ig,2))
!
                    def(3,n,cpt) = 0.d0
!
                    def(4,n,cpt) = (&
                                   f(i,1)* (dfdi(n,2)*fe(ig)+ff(n)* dgdgl(ig,2)) + f(i,2)* (dfdi(&
                                   &n,1)*fe(ig)+ff(n)* dgdgl(ig,1))&
                                   )/rac2
                    if (ndim .eq. 3) then
                        def(3,n,cpt) = f(i,3) * (dfdi(n,3) * fe(ig) + ff(n)*dgdgl(ig,3))
                        def(5,n,cpt) = (&
                                       f(i,1)* (dfdi(n,3)*fe(ig)+ff( n)*dgdgl(ig,3)) + f(i,3)* (d&
                                       &fdi(n,1)*fe(ig)+ ff(n)*dgdgl(ig,1))&
                                       )/rac2
                        def(6,n,cpt) = (&
                                       f(i,3)* (dfdi(n,2)*fe(ig)+ff( n)*dgdgl(ig,2)) + f(i,2)* (d&
                                       &fdi(n,3)*fe(ig)+ ff(n)*dgdgl(ig,3))&
                                       )/rac2
                    endif
136              continue
!
!   ATTENTION:TERME DE CORRECTION (3,3) AXI LE DDL 1+NDIM*(NFH+IG)
                if (axi) then
                    def(3,n,(1+ndim*(nfh+ig))) = f(3,3)* ff(n)/r * fe( ig)
                endif
135          continue
!
            call assert(cpt.eq.ddld)
!
130      continue
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
        if (axi) then
            jac= jac * r
        endif
!
!      CALCUL DES PRODUITS DE FONCTIONS DE FORMES (ET DERIVEES)
        if (( option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA' ) .and.&
            grdepl) then
            do 140 n = 1, nnop
                do 141 m = 1, n
                    pff(1,n,m) = dfdi(n,1)*dfdi(m,1)
                    pff(2,n,m) = dfdi(n,2)*dfdi(m,2)
                    pff(3,n,m) = 0.d0
                    pff(4,n,m) =(dfdi(n,1)*dfdi(m,2)+dfdi(n,2)*dfdi(m,&
                    1))/rac2
                    if (ndim .eq. 3) then
                        pff(3,n,m)= dfdi(n,3)*dfdi(m,3)
                        pff(5,n,m)=(dfdi(n,1)*dfdi(m,3)+dfdi(n,3)*&
                        dfdi(m,1))/rac2
                        pff(6,n,m)=(dfdi(n,2)*dfdi(m,3)+dfdi(n,3)*&
                        dfdi(m,2))/rac2
                    endif
141              continue
140          continue
        endif
!
!
! - LOI DE COMPORTEMENT : S(E) ET DS/DE
!
!      CALL NMCPEL('RIGI',KPG,1,POUM,NDIM,TYPMOD,ANGMAS,IMATE,COMPOR,
!   &               CRIT,OPTION,EPS,SIGMA,VI(1,KPG),DSIDEP,CODRET)
!
        do 150 m = 1, 3
            sign(m) = sigm(m,kpg)
150      continue
        do 160 m = 4, 2*ndim
            sign(m) = sigm(m,kpg)*rac2
160      continue
!
        call r8inir(6, 0.0d0, sigma, 1)
        call nmcomp('XFEM', idecpg+kpg, 1, ndim, typmod,&
                    imate, compor, crit, instam, instap,&
                    6, eps, deps, 6, sign,&
                    vi(1, kpg), option, angmas, 10, elgeom(1, kpg),&
                    sigma, vip(1, kpg), 36, dsidep, 1,&
                    rbid, codret)
!
! - CALCUL DE LA MATRICE DE RIGIDITE
!
        if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA') then
!
            do 170 n = 1, nnop
                nn=dec(n)
!
                do 171 i = 1, ddld
                    kkd = (nn+i-1) * (nn+i) /2
                    do 172 kl = 1, 2*ndim
                        sigp(kl) = 0.d0
                        do 173 l = 1, 2*ndim
                            sigp(kl) = sigp(kl)+def(l,n,i)*dsidep(l, kl)
173                      continue
172                  continue
                    do 174 j = 1, ddld
                        do 175 m = 1, n
                            mn=dec(m)
!
                            if (m .eq. n) then
                                j1 = i
                            else
                                j1 = ddld
                            endif
!
!                 RIGIDITE GEOMETRIQUE
                            tmp1 = 0.d0
                            if (grdepl .and. i .eq. j) then
                                tmp1 = 0.d0
                                do 176 l = 1, 2*ndim
                                    tmp1 = tmp1+pff(l,n,m)*sigma(l)
176                              continue
!
!                  TERME DE CORRECTION AXISYMETRIQUE
!                    IF (AXI .AND. I.EQ.1) THEN
!                      TMP1 = TMP1+ZR(IVF+N+(KPG-1)*NNO-1)*
!     &                           ZR(IVF+M+(KPG-1)*NNO-1)/(R*R)*SIGMA(3)
!                    ENDIF
                            endif
!
!                 RIGIDITE ELASTIQUE
                            tmp2=0.d0
                            do 177 l = 1, 2*ndim
                                tmp2=tmp2+sigp(l)*def(l,m,j)
177                          continue
!
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                matuu(kkd+mn+j) = matuu(kkd+mn+j) + ( tmp1+tmp2)*jac
                            endif
!
175                      continue
174                  continue
171              continue
170          continue
        endif
!
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
            do 180 n = 1, nnop
                nn=dec(n)
                do 181 i = 1, ddld
                    do 182 l = 1, 2*ndim
                        zr(ivectu-1+nn+i)= zr(ivectu-1+nn+i) + def(l,&
                        n,i)*sigma(l)*jac
182                  continue
181              continue
180          continue
!
            if (grdepl) then
!           CONVERSION LAGRANGE -> CAUCHY
                if (cplan) f(3,3) = sqrt(abs(2.d0*eps(3)+1.d0))
                detf = f(3,3) * (f(1,1)*f(2,2)-f(1,2)*f(2,1))
                if (ndim .eq. 3) then
                    detf = detf - f(2,3)*(f(1,1)*f(3,2)-f(3,1)*f(1,2)) + f(1,3)*(f(2,1)*f(3,2)-f(&
                           &3,1)*f(2,2))
                endif
                do 190 i = 1, 2*ndim
                    sig(i,kpg) = 0.d0
                    do 191 l = 1, 2*ndim
                        ftf = (&
                              f(&
                              indi(i), indi(l))*f(indj(i), indj(l)) + f(indi(i),&
                              indj(l))*f(indj(i), indi(l))&
                              )*rind(l&
                              )
                        sig(i,kpg) = sig(i,kpg) + ftf*sigma(l)
191                  continue
                    sig(i,kpg) = sig(i,kpg)/detf
190              continue
            else
!           SIMPLE CORRECTION DES CONTRAINTES
                do 200 l = 1, 3
                    sig(l,kpg) = sigma(l)
200              continue
                do 210 l = 4, 2*ndim
                    sig(l,kpg) = sigma(l)/rac2
210              continue
            endif
        endif
!
1000  end do
!
end subroutine
