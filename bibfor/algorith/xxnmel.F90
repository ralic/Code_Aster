subroutine xxnmel(poum, elrefp, elrese, ndim, coorse,&
                  igeom, he, nfh, ddlc, ddlm,&
                  nnops, nfe, basloc, nnop, npg,&
                  typmod, option, imate, compor, lgpg,&
                  crit, idepl, lsn, lst, idecpg,&
                  sig, vi, matuu, ivectu, codret,&
                  nfiss, heavn, jstno)
!
! aslint: disable=W1306,W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/iselli.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/indent.h"
#include "asterfort/matini.h"
#include "asterfort/nmcpel.h"
#include "asterfort/reeref.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xcinem.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalfev_wrap.h"
#include "asterfort/xkamat.h"
#include "asterfort/iimatu.h"
#include "asterfort/xnbddl.h"
    integer :: nnop, nfiss, codret, ddlc, ddlm
    integer :: idecpg, idepl, igeom, imate, ivectu, nnops
    integer :: lgpg, ndim, nfe, nfh, npg, heavn(nnop, 5)
    integer :: jstno
    real(kind=8) :: basloc(3*ndim*nnop), coorse(*), crit(*), he(nfiss)
    real(kind=8) :: lsn(nnop), lst(nnop), sig(2*ndim, npg)
    real(kind=8) :: matuu(*), vi(lgpg, npg)
    character(len=*) :: poum
    character(len=8) :: elrefp, elrese, typmod(*), fami_se
    character(len=16) :: option, compor(*)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: kpg, i, ig, n, nn, m, mn, j, j1, kl, l, kkd, ipg
    integer :: ddld, ddls, nno, nnos, npgbis, cpt, ndimb, dec(nnop)
    integer :: idfde, ipoids, ivf, jcoopg, jdfd2, jgano, hea_se
    integer :: singu, alp, ii, jj
    real(kind=8) :: dsidep(6, 6), eps(6), sigma(6), ftf, detf
    real(kind=8) :: tmp1, tmp2, sigp(6, 3*(1+nfh+nfe*ndim)), rbid33(3, 3)
    real(kind=8) :: xg(ndim), xe(ndim), ff(nnop), jac
    real(kind=8) :: dfdi(nnop, ndim), f(3, 3)
    real(kind=8) :: pff(6, nnop, nnop)
    real(kind=8) :: def(6, ndim*(1+nfh+ndim), nnop)
    real(kind=8) :: r
    real(kind=8) :: fk(27,3,3), dkdgl(27,3,3,3), ka, mu
    aster_logical :: grdepl, axi, cplan
!
    integer :: indi(6), indj(6)
    real(kind=8) :: rind(6), rac2, angmas(3)
    data    indi / 1 , 2 , 3 , 1 , 1 , 2 /
    data    indj / 1 , 2 , 3 , 2 , 3 , 3 /
    data    rind / 0.5d0,0.5d0,0.5d0,0.70710678118655d0,&
     &               0.70710678118655d0,0.70710678118655d0 /
    data    rac2 / 1.4142135623731d0 /
    data    angmas /0.d0, 0.d0, 0.d0/
!--------------------------------------------------------------------
!
!     ATTENTION, EN 3D, ZR(IDEPL) ET ZR(VECTU) SONT DIMENSIONNÉS DE
!     TELLE SORTE QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES
!     NOEUDS MILIEU
!
!     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD
    call xnbddl(ndim, nfh, nfe, ddlc, ddld, ddls, singu)
    call xkamat(imate, ndim, axi, ka, mu)
!
! - INITIALISATION
    grdepl = compor(3) .eq. 'GROT_GDEP'
    axi = typmod(1) .eq. 'AXIS'
    cplan = typmod(1) .eq. 'C_PLAN'
!
    if (grdepl) then
        call utmess('F', 'XFEM2_2')
    endif
!
!     ADRESSE DES COORD DU SOUS ELT EN QUESTION
    fami_se='XINT'
    if (nfe.gt.0) then
      if (ndim.eq.3 .and. &
        (count(zi((jstno-1+1):(jstno-1+nnop)).eq.2)+&
         count(zi((jstno-1+1):(jstno-1+nnop)).eq.0)).eq.nnop) fami_se='XGEO'
    endif
    call elrefe_info(elrefe=elrese, fami=fami_se, ndim=ndimb, nno=nno, nnos=nnos,&
                     npg=npgbis, jpoids=ipoids, jcoopg=jcoopg, jvf=ivf, jdfde=idfde,&
                     jdfd2=jdfd2, jgano=jgano)
!
    ASSERT(npg.eq.npgbis.and.ndim.eq.ndimb)
!
! DECALAGES CALCULES EN AMONT: PERF
!
    do n = 1, nnop
        call indent(n, ddls, ddlm, nnops, dec(n))
    end do
!
! CALCUL DE L IDENTIFIANT DU SS ELEMENT
    hea_se=xcalc_code(nfiss, he_real=[he])
!
!-----------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS
    do kpg = 1, npg
!
!       COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
        call vecini(ndim, 0.d0, xg)
        do i = 1, ndim
            do n = 1, nno
                xg(i) = xg(i) + zr(ivf-1+nno*(kpg-1)+n)*coorse(ndim*( n-1)+i)
            end do
        end do
!
!       COORDONNÉES DU POINT DE GAUSS DANS L'ÉLÉMENT DE RÉF PARENT : XE
!       ET CALCUL DE FF ET DFDI
        call reeref(elrefp, nnop, zr(igeom), xg, ndim,&
                    xe, ff, dfdi=dfdi)
!
!       FONCTION D'ENRICHISSEMENT AU POINT DE GAUSS ET LEURS DÉRIVÉES
        if (singu .gt. 0) then
            call xcalfev_wrap(ndim, nnop, basloc, zi(jstno), he(1),&
                         lsn, lst, zr(igeom), ka, mu, ff, fk, dfdi=dfdi, dkdgl=dkdgl,&
                         elref=elrefp, kstop='C')
        endif
!
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE):
        if (axi) then
            r = 0.d0
            do n = 1, nnop
                r = r + ff(n)*zr(igeom-1+2*(n-1)+1)
            end do
!
            ASSERT(r.gt.0d0)
!           ATTENTION : LE POIDS N'EST PAS X R
!           CE SERA FAIT PLUS TARD AVEC JAC = JAC X R
        endif
!
!       COORDONNÉES DU POINT DE GAUSS DANS L'ÉLÉMENT DE RÉF PARENT : XE
!       ET CALCUL DE FF, DFDI, ET EPS
        if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1: 9) .eq. 'FULL_MECA' .or.&
            option(1: 9) .eq. 'RAPH_MECA') then
            call xcinem(axi, igeom, nnop, nnops, idepl, grdepl, ndim, he,&
                        nfiss, nfh, singu, ddls, ddlm,&
                        fk, dkdgl, ff, dfdi, f, eps, rbid33, heavn)
!
!       SI OPTION 'RIGI_MECA', ON INITIALISE À 0 LES DEPL
        else if (option .eq. 'RIGI_MECA') then
            call matini(3, 3, 0.d0, f)
            do i = 1, 3
                f(i,i) = 1.d0
            end do
            call vecini(6, 0.d0, eps)
        endif
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
!
!      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        def(:,:,:)=0.d0
        do n = 1, nnop
            cpt = 0
!         FONCTIONS DE FORME CLASSIQUES
            do i = 1, ndim
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
            end do
!
!         TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
            if (axi) then
                def(3,1,n) = f(3,3) * ff(n)/r
            endif
!
!         ENRICHISSEMENT PAR HEAVYSIDE
            do ig = 1, nfh
                do i = 1, ndim
                    cpt = cpt+1
                    do m = 1, 2*ndim
                        def(m,cpt,n) = def(m,i,n) * xcalc_heav(heavn(n,ig),hea_se,heavn(n,5))
                    end do
                    if (ndim .eq. 2) then
                        def(3,cpt,n) = 0.d0
                    endif
                end do
!
!   TERME DE CORRECTION (3,3) A PORTE SUR LE DDL 1+NDIM*IG
                if (axi) then
                    def(3,1+ndim*ig,n) = f(3,3)*ff(n)/r*xcalc_heav(heavn(n,ig),hea_se,heavn(n,5))
                endif
!
            end do
!
!         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIÈRES
            do alp = 1, ndim*nfe
                do i = 1, ndim
                    cpt=cpt+1
                    def(1,cpt,n) = f(i,1)* dkdgl(n,alp,i,1)
!
                    def(2,cpt,n) = f(i,2)* dkdgl(n,alp,i,2)
!
                    def(3,cpt,n) = 0.d0
!
                    def(4,cpt,n) = (&
                                   f(i,1)* dkdgl(n,alp,i,2) + f(i,2)* dkdgl(n,alp,i,1)&
                                   )/rac2
!
                    if (ndim .eq. 3) then
                        def(3,cpt,n) = f(i,3)* dkdgl(n,alp,i,3)
                        def(5,cpt,n) = (&
                                       f(i,1)* dkdgl(n,alp,i,3) + f(i,3)* dkdgl(n,alp,i,1)&
                                       )/rac2
                        def(6,cpt,n) = (&
                                       f(i,3)* dkdgl(n,alp,i,2) + f(i,2)* dkdgl(n,alp,i,3)&
                                       )/rac2
                    endif
                enddo
!
            enddo
!
!   TERME DE CORRECTION (3,3) AXI PORTE SUR LE DDL 1+NDIM*(NFH+ALP)
!      EN AXI: ON PROJETTE L ENRICHISSEMENT VECTORIEL SUIVANT X
            if (axi) then
               do alp = 1, ndim*nfe
                  def(3,1+ndim*(nfh+alp),n) = f(3,3)* fk(n,alp,1)/r
               end do
             endif
!
            ASSERT(cpt.eq.ddld)
!
        end do
!
!       CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
!       AVEC LES COORDONNEES DU SOUS-ELEMENT
        if (ndim .eq. 2) then
            call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                        jac)
        else if (ndim.eq.3) then
            call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                        jac)
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
            do n = 1, nnop
                do m = 1, n
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
                end do
            end do
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
            do n = 1, nnop
                nn=dec(n)
                do i = 1, ddld
                    do kl = 1, 2*ndim
                        sigp(kl,i) = 0.d0
                        do l = 1, 2*ndim
                            sigp(kl,i) = sigp(kl,i) + def(l,i,n)* dsidep(l,kl)
                        end do
                    end do
                end do
!
                do m = 1, n
                    mn=dec(m)
                    do i = 1, ddld
                        ii=iimatu(i,ndim,nfh,nfe)
                        kkd = (nn+ii-1) * (nn+ii) /2
                        if (m .eq. n) then
                            j1 = ii
                        else
                            j1 = ddld
                        endif
!
                        do j = 1, ddld
!
                            jj=iimatu(j,ndim,nfh,nfe)
!
!                 RIGIDITE ELASTIQUE
                            tmp2 = 0.d0
                            do l = 1, 2*ndim
                                tmp2 = tmp2 + sigp(l,i)*def(l,j,m)
                            end do
!
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (jj .le. j1) then
                                matuu(kkd+mn+jj) = matuu(kkd+mn+jj) + tmp2*jac
                            endif
!
                        end do
                    end do
                end do
            end do
            goto 999
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
            do n = 1, nnop
                nn=dec(n)
!
                do i = 1, ddld
                    ii=iimatu(i,ndim,nfh,nfe)
                    kkd = (nn+ii-1) * (nn+ii) /2
                    do kl = 1, 2*ndim
                        sigp(kl,i) = 0.d0
                        do l = 1, 2*ndim
                            sigp(kl,i) = sigp(kl,i) + def(l,i,n)* dsidep(l,kl)
                        end do
                    end do
                    do j = 1, ddld
                        jj=iimatu(j,ndim,nfh,nfe)
                        do m = 1, n
                            mn=dec(m)
!
                            if (m .eq. n) then
                                j1 = ii
                            else
                                j1 = ddld
                            endif
!
!                 RIGIDITE GEOMETRIQUE
                            tmp1 = 0.d0
                            if (grdepl .and. i .eq. j) then
                                tmp1 = 0.d0
                                do l = 1, 2*ndim
                                    tmp1 = tmp1 + pff(l,m,n)*sigma(l)
                                end do
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
                            do l = 1, 2*ndim
                                tmp2 = tmp2 + sigp(l,i)*def(l,j,m)
                            end do
!
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (jj .le. j1) then
                                matuu(kkd+mn+jj) = matuu(kkd+mn+jj) + ( tmp1+tmp2)*jac
                            endif
!
                        end do
                    end do
                end do
            end do
        endif
!
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
        if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
            do n = 1, nnop
                nn=dec(n)
!
                do i = 1, ddld
                    ii=iimatu(i,ndim,nfh,nfe)
                    do m = 1, 2*ndim
                        zr(ivectu-1+nn+ii)= zr(ivectu-1+nn+ii) + def(m,&
                        i,n)*sigma(m)*jac
                    end do
                end do
            end do
!
            if (grdepl) then
!          CONVERSION LAGRANGE -> CAUCHY
                if (cplan) f(3,3) = sqrt(abs(2.d0*eps(3)+1.d0))
                detf = f(3,3) * (f(1,1)*f(2,2)-f(1,2)*f(2,1))
                if (ndim .eq. 3) then
                    detf = detf - f(2,3)*(f(1,1)*f(3,2)-f(3,1)*f(1,2)) + f(1,3)*(f(2,1)*f(3,2)-f(&
                           &3,1)*f(2,2))
                endif
                do i = 1, 2*ndim
                    sig(i,kpg) = 0.d0
                    do l = 1, 2*ndim
                        ftf = (&
                              f(&
                              indi(i), indi(l))*f(indj(i), indj(l)) + f(indi(i),&
                              indj(l))*f(indj(i), indi(l))&
                              )*rind(l&
                              )
                        sig(i,kpg) = sig(i,kpg) + ftf*sigma(l)
                    end do
                    sig(i,kpg) = sig(i,kpg)/detf
                end do
            else
!          SIMPLE CORRECTION DES CONTRAINTES
                do l = 1, 3
                    sig(l,kpg) = sigma(l)
                end do
                sig(4,kpg) = sigma(4)/rac2
                if (ndim .eq. 3) then
                    sig(5,kpg) = sigma(5)/rac2
                    sig(6,kpg) = sigma(6)/rac2
                endif
            endif
        endif
!
999     continue
!
    end do
!
end subroutine
