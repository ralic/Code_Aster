subroutine xxnmel(poum, elrefp, elrese, ndim, coorse,&
                  igeom, he, nfh, ddlc, ddlm,&
                  nnops, nfe, basloc, nnop, npg,&
                  typmod, option, imate, compor, lgpg,&
                  crit, idepl, lsn, lst, idecpg,&
                  sig, vi, matuu, ivectu, codret,&
                  nfiss, fisno)
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref5.h'
    include 'asterfort/indent.h'
    include 'asterfort/nmcpel.h'
    include 'asterfort/reeref.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xcalf2.h'
    include 'asterfort/xcalfe.h'
    integer :: codret, ddlc, ddlm, fisno(nnop, nfiss)
    integer :: idecpg, idepl, igeom, imate, ivectu, nnops
    integer :: lgpg, ndim, nfe, nfh, nfiss, nnop, npg
    real(kind=8) :: basloc(3*ndim*nnop), coorse(*), crit(3), he(nfiss)
    real(kind=8) :: lsn(nnop), lst(nnop), sig(2*ndim, npg)
    real(kind=8) :: matuu(*), vi(lgpg, npg)
    character(len=*) :: poum
    character(len=8) :: elrefp, elrese, typmod(*)
    character(len=16) :: option, compor(4)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21 CRP_20 CRS_1404
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
! IN  NFH     : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  DDLM    : NOMBRE DE DDL PAR NOEUD MILIEU
! IN  NNOPS   : NOMBRE DE NOEUDS SOMMET ELEMENTS PARENT
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE
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
! IN  IDECPG  : POSITION DANS LA FAMILLE 'XFEM' DU 1ER POINT DE GAUSS
!               DU SOUS ELEMENT COURRANT (EN FAIT IDECPG+1)
!
! OUT SIG     : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VI      : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT IVECTU  : VECTEUR FORCES NODALES (RAPH_MECA ET FULL_MECA)
!......................................................................
!
    character(len=16) :: compo2(4)
    integer :: kpg, i, ig, n, nn, m, mn, j, j1, kl, l, kkd, ipg, iret
    integer :: ddld, ddls, nno, nnos, npgbis, cpt, ndimb, dec(nnop)
    integer :: ibid, idfde, ipoids, ivf, jcoopg, jdfd2, jgano
    real(kind=8) :: dsidep(6, 6), eps(6), sigma(6), ftf, detf
    real(kind=8) :: tmp1, tmp2, sigp(6, 3*(1+nfe+nfh)), rbid33(3, 3)
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac, lsng, lstg, r8bid
    real(kind=8) :: rbid, rbid4(4), rb4(4), rbid10(10), rb10(10), rbd4(4)
    real(kind=8) :: dfdi(nnop, ndim), f(3, 3), fe(4), baslog(3*ndim)
    real(kind=8) :: dgdgl(4, 3), pff(6, nnop, nnop)
    real(kind=8) :: def(6, ndim*(1+nfh+nfe), nnop)
    real(kind=8) :: ur, r
    logical :: grdepl, axi, cplan
!
    integer :: indi(6), indj(6)
    real(kind=8) :: rind(6), rac2, angmas(3)
    data    indi / 1 , 2 , 3 , 1 , 1 , 2 /
    data    indj / 1 , 2 , 3 , 2 , 3 , 3 /
    data    rind / 0.5d0,0.5d0,0.5d0,0.70710678118655D0,&
     &               0.70710678118655D0,0.70710678118655D0 /
    data    rac2 / 1.4142135623731D0 /
    data    angmas /0.d0, 0.d0, 0.d0/
!--------------------------------------------------------------------
!
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
! - INITIALISATION
    grdepl = compor(3) .eq. 'GROT_GDEP'
    axi = typmod(1) .eq. 'AXIS'
    cplan = typmod(1) .eq. 'C_PLAN'
!
    if (grdepl) then
        call u2mess('F', 'XFEM2_2')
    endif
!
!     ADRESSE DES COORD DU SOUS ELT EN QUESTION
    call elref5(elrese, 'XINT', ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
!
    call assert(npg.eq.npgbis.and.ndim.eq.ndimb)
!
! DECALAGES CALCULES EN AMONT: PERF
!
    do 179 n = 1, nnop
        call indent(n, ddls, ddlm, nnops, dec(n))
179  continue
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
                    rbid, r8bid, fisno, nfiss, nfh,&
                    nfe, ddls, ddlm, fe, dgdgl,&
                    'NON', xe, ff, dfdi, f,&
                    eps, rbid33)
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
                call xcalf2(he(1), lsng, lstg, baslog, fe,&
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
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE) ET DU DEPL. RADIAL
        if (axi) then
            r = 0.d0
            ur = 0.d0
            do 120 n = 1, nnop
                r = r + ff(n)*zr(igeom-1+2*(n-1)+1)
                ur = ur + ff(n)*zr(idepl-1+ddls*(n-1)+1)
                do 121 ig = 1, nfh
                    ur = ur + ff(n) *zr(idepl-1+ddls*(n-1)+ndim*ig+1) *he(fisno(n,ig))
!
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
!       ET CALCUL DE FF, DFDI, ET EPS
        if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA' .or.&
            option(1: 9) .eq. 'RAPH_MECA') then
            call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                        xg, idepl, grdepl, ndim, he,&
                        r, ur, fisno, nfiss, nfh,&
                        nfe, ddls, ddlm, fe, dgdgl,&
                        'OUI', xe, ff, dfdi, f,&
                        eps, rbid33)
!
!       SI OPTION 'RIGI_MECA', ON INITIALISE À 0 LES DEPL
        else if (option .eq. 'RIGI_MECA') then
            call reeref(elrefp, axi, nnop, nnops, zr(igeom),&
                        xg, idepl, .false., ndim, he,&
                        r, ur, fisno, nfiss, nfh,&
                        nfe, ddls, ddlm, fe, dgdgl,&
                        'INI', xe, ff, dfdi, f,&
                        eps, rbid33)
        endif
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
                def(1,i,n) = f(i,1)*dfdi(n,1)
                def(2,i,n) = f(i,2)*dfdi(n,2)
                def(3,i,n) = 0.d0
                def(4,i,n) = (f(i,1)*dfdi(n,2) + f(i,2)*dfdi(n,1))/ rac2
                if (ndim .eq. 3) then
                    def(3,i,n) = f(i,3)*dfdi(n,3)
                    def(5,i,n) = (f(i,1)*dfdi(n,3) + f(i,3)*dfdi(n,1)) /rac2
                    def(6,i,n) = (f(i,2)*dfdi(n,3) + f(i,3)*dfdi(n,2)) /rac2
                endif
141          continue
!
!         TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
            if (axi) then
                def(3,1,n) = f(3,3) * ff(n)/r
            endif
!
!         ENRICHISSEMENT PAR HEAVYSIDE
            do 142 ig = 1, nfh
                do 143 i = 1, ndim
                    cpt = cpt+1
                    do 144 m = 1, 2*ndim
                        def(m,cpt,n) = def(m,i,n) * he(fisno(n,ig))
144                  continue
                    if (ndim .eq. 2) then
                        def(3,cpt,n) = 0.d0
                    endif
143              continue
!
!   TERME DE CORRECTION (3,3) A PORTE SUR LE DDL 1+NDIM*IG
                if (axi) then
                    def(3,1+ndim*ig,n) = f(3,3) * ff(n)/r * he(fisno( n,ig))
                endif
!
142          continue
!
!         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIÈRES
            do 145 ig = 1, nfe
                do 146 i = 1, ndim
                    cpt=cpt+1
                    def(1,cpt,n) = f(i,1)* (dfdi(n,1) * fe(ig) + ff(n) *dgdgl(ig,1))
!
                    def(2,cpt,n) = f(i,2)* (dfdi(n,2) * fe(ig) + ff(n) *dgdgl(ig,2))
!
                    def(3,cpt,n) = 0.d0
!
                    def(4,cpt,n) = (&
                                   f(i,1)* (dfdi(n,2)*fe(ig)+ff(n)* dgdgl(ig,2)) + f(i,2)* (dfdi(&
                                   &n,1)*fe(ig)+ff(n)* dgdgl(ig,1))&
                                   )/rac2
!
                    if (ndim .eq. 3) then
                        def(3,cpt,n) = f(i,3)* (dfdi(n,3) * fe(ig) + ff(n)*dgdgl(ig,3))
                        def(5,cpt,n) = (&
                                       f(i,1)* (dfdi(n,3)*fe(ig)+ff( n)*dgdgl(ig,3)) + f(i,3)* (d&
                                       &fdi(n,1)*fe(ig)+ ff(n)*dgdgl(ig,1))&
                                       )/rac2
                        def(6,cpt,n) = (&
                                       f(i,3)* (dfdi(n,2)*fe(ig)+ff( n)*dgdgl(ig,2)) + f(i,2)* (d&
                                       &fdi(n,3)*fe(ig)+ ff(n)*dgdgl(ig,3))&
                                       )/rac2
                    endif
146              continue
!
!   TERME DE CORRECTION (3,3) AXI PORTE SUR LE DDL 1+NDIM*(NFH+IG)
                if (axi) then
                    def(3,1+ndim*(nfh+ig),n) = f(3,3) * ff(n)/r * fe( ig)
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
                        rbid10, rb10, jac)
        else if (ndim.eq.3) then
            call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                        rbid4, rb4, rbd4, jac)
        endif
!
!       MODIFICATION DU JACOBIEN SI AXI
        if (axi) then
            jac = jac * r
        endif
!
!      TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
!        IF (AXI) THEN
!          DO 150 N=1,NNOP
!            DEF(3,N,1) = F(3,3)* ZR(IVF+N+(KPG-1)*NNO-1)/R
! 150      CONTINUE
!        ENDIF
!
!       CALCUL DES PRODUITS DE FONCTIONS DE FORMES (ET DERIVEES)
        if (( option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA' ) .and.&
            grdepl) then
            do 160 n = 1, nnop
                do 161 m = 1, n
                    pff(1,m,n) = dfdi(n,1)*dfdi(m,1)
                    pff(2,m,n) = dfdi(n,2)*dfdi(m,2)
                    pff(3,m,n) = 0.d0
                    pff(4,m,n) =(dfdi(n,1)*dfdi(m,2)+dfdi(n,2)*dfdi(m,&
                    1))/rac2
                    if (ndim .eq. 3) then
                        pff(3,m,n)= dfdi(n,3)*dfdi(m,3)
                        pff(5,m,n)=(dfdi(n,1)*dfdi(m,3)+dfdi(n,3)*&
                        dfdi(m,1))/rac2
                        pff(6,m,n)=(dfdi(n,2)*dfdi(m,3)+dfdi(n,3)*&
                        dfdi(m,2))/rac2
                    endif
161              continue
160          continue
        endif
!
! - CALCUL DE LA MATRICE DE RIGIDITE POUR L'OPTION RIGI_MECA
!
!
        if (option .eq. 'RIGI_MECA') then
!
! -       LOI DE COMPORTEMENT : ON VA OBTENIR ICI LA MATRICE DE HOOKE
!         POUR LE CAS ELASTIQUE ISOTROPE - DEFO/CONTR PLANES OU 3D
            ipg= idecpg + kpg
            compo2(1)='ELAS'
            compo2(2)=' '
            compo2(3)=' '
            compo2(4)=' '
!
            call nmcpel('XFEM', ipg, 1, poum, ndim,&
                        typmod, angmas, imate, compo2, crit,&
                        option, eps, sigma, vi(1, kpg), dsidep,&
                        codret)
!
            do 170 n = 1, nnop
                nn=dec(n)
                do 178 i = 1, ddld
                    do 172 kl = 1, 2*ndim
                        sigp(kl,i) = 0.d0
                        do 173 l = 1, 2*ndim
                            sigp(kl,i) = sigp(kl,i) + def(l,i,n)* dsidep(l,kl)
173                      continue
172                  continue
178              continue
!
                do 175 m = 1, n
                    mn=dec(m)
                    do 171 i = 1, ddld
                        kkd = (nn+i-1) * (nn+i) /2
                        if (m .eq. n) then
                            j1 = i
                        else
                            j1 = ddld
                        endif
!
                        do 174 j = 1, ddld
!
!                 RIGIDITE ELASTIQUE
                            tmp2 = 0.d0
                            do 176 l = 1, 2*ndim
                                tmp2 = tmp2 + sigp(l,i)*def(l,j,m)
176                          continue
!
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                matuu(kkd+mn+j) = matuu(kkd+mn+j) + tmp2*jac
                            endif
!
174                      continue
171                  continue
175              continue
170          continue
            goto 9999
        endif
!
! - LOI DE COMPORTEMENT : CALCUL DE S(E) ET DS/DE À PARTIR DE EPS
!                       {XX YY ZZ SQRT(2)*XY SQRT(2)*XZ SQRT(2)*YZ}
!
!       POUR LES VARIABLES DE COMMANDES (TEMP...), IL EST NECESSSAIRE
!       DE DONNER LA POSITION DU POINT DE GAUSS COURRANT DANS LA
!       FAMILLE 'XFEM'
        ipg = idecpg + kpg
        call nmcpel('XFEM', ipg, 1, poum, ndim,&
                    typmod, angmas, imate, compor, crit,&
                    option, eps, sigma, vi(1, kpg), dsidep,&
                    codret)
!
!
! - CALCUL DE LA MATRICE DE RIGIDITE POUR LES OPTIONS RIGI_MECA_TANG
!   ET FULL_MECA
!
        if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA') then
!
            do 180 n = 1, nnop
                nn=dec(n)
!
                do 181 i = 1, ddld
                    kkd = (nn+i-1) * (nn+i) /2
                    do 182 kl = 1, 2*ndim
                        sigp(kl,i) = 0.d0
                        do 183 l = 1, 2*ndim
                            sigp(kl,i) = sigp(kl,i) + def(l,i,n)* dsidep(l,kl)
183                      continue
182                  continue
                    do 184 j = 1, ddld
                        do 185 m = 1, n
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
                                do 186 l = 1, 2*ndim
                                    tmp1 = tmp1 + pff(l,m,n)*sigma(l)
186                              continue
!
!                  TERME DE CORRECTION AXISYMETRIQUE
!                    IF (AXI .AND. I.EQ.1) THEN
!                      TMP1 = TMP1 + ZR(IVF+N+(KPG-1)*NNO-1) *
!     &                      ZR(IVF+M+(KPG-1)*NNO-1)/(R*R) * SIGMA(3)
!                    ENDIF
                            endif
!
!                 RIGIDITE ELASTIQUE
                            tmp2 = 0.d0
                            do 187 l = 1, 2*ndim
                                tmp2 = tmp2 + sigp(l,i)*def(l,j,m)
187                          continue
!
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                matuu(kkd+mn+j) = matuu(kkd+mn+j) + ( tmp1+tmp2)*jac
                            endif
!
185                      continue
184                  continue
181              continue
180          continue
        endif
!
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
            do 190 n = 1, nnop
                nn=dec(n)
!
                do 191 i = 1, ddld
                    do 192 m = 1, 2*ndim
                        zr(ivectu-1+nn+i)= zr(ivectu-1+nn+i) + def(m,&
                        i,n)*sigma(m)*jac
192                  continue
191              continue
190          continue
!
            if (grdepl) then
!          CONVERSION LAGRANGE -> CAUCHY
                if (cplan) f(3,3) = sqrt(abs(2.d0*eps(3)+1.d0))
                detf = f(3,3) * (f(1,1)*f(2,2)-f(1,2)*f(2,1))
                if (ndim .eq. 3) then
                    detf = detf - f(2,3)*(f(1,1)*f(3,2)-f(3,1)*f(1,2)) + f(1,3)*(f(2,1)*f(3,2)-f(&
                           &3,1)*f(2,2))
                endif
                do 200 i = 1, 2*ndim
                    sig(i,kpg) = 0.d0
                    do 210 l = 1, 2*ndim
                        ftf = (&
                              f(&
                              indi(i), indi(l))*f(indj(i), indj(l)) + f(indi(i),&
                              indj(l))*f(indj(i), indi(l))&
                              )*rind(l&
                              )
                        sig(i,kpg) = sig(i,kpg) + ftf*sigma(l)
210                  continue
                    sig(i,kpg) = sig(i,kpg)/detf
200              continue
            else
!          SIMPLE CORRECTION DES CONTRAINTES
                do 300 l = 1, 3
                    sig(l,kpg) = sigma(l)
300              continue
                sig(4,kpg) = sigma(4)/rac2
                if (ndim .eq. 3) then
                    sig(5,kpg) = sigma(5)/rac2
                    sig(6,kpg) = sigma(6)/rac2
                endif
            endif
        endif
!
9999      continue
!
1000  end do
!
end subroutine
