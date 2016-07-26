subroutine te0048(option, nomte)
    implicit none
    character(len=16) :: option, nomte
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dismoi.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/jevecd.h"
#include "asterfort/jevech.h"
#include "asterfort/provec.h"
#include "asterfort/rcvad2.h"
#include "asterfort/reeref.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/lteatt.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xcalfev_wrap.h"
#include "asterfort/xnormv.h"
#include "asterfort/ltequa.h"
#include "asterfort/indent.h"
#include "asterfort/coor_cyl.h"
#include "asterfort/xdeffk.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! Calcul du taux de restitution d'energie elementaire et des facteurs 
! d'intensite des contraintes K1 K2 K3 sur les elements de bord 3D XFEM
!
! option : 'CALC_K_G' (charges reelles)
!
! in / option : nom de l'option
! in / nomte  : nom du type element
!
! ======================================================================
!
!   nno_par_max = nombre max de noeuds pour l'element de reference parent
!   -> te dedie aux elements de bords, max atteint en 3D pour les QUAD8
    integer :: nno_par_max
    parameter (nno_par_max=8)
!
!   nno_se_max = nombre max de noeuds pour le sous-element de reference
!   -> te dedie aux elements de bords, max atteint en 3D pour les TRIA6
    integer :: nno_se_max
    parameter (nno_se_max=6)
!
    integer :: ndime, ndim, nnop, nnops, cpt, nno, nnos, npg, nfe, nfh
    integer :: nfiss, ncomp, ncompn, nse, hea_se
    integer :: ipoids, ivf, idfde, ipres, iadzi, iazk24, igeom, idepl
    integer :: jlsn, jlst, jpintt, jcnset, jheavt, jlonch, ibalo
    integer :: jpmilt, jheavn, iforc, imate
    integer :: ino, j, ise, ifiss, in, inop, kpg, ig, k
    integer :: nlong_ddl, ddld, ddls, ddlm, indeni
    integer :: ithet, igthet
    integer :: ier, iret, irese, jtab(7), icodre(3)
    real(kind=8) :: th1, th2, dth1d1, dth2d2, divt, pres, tcla
    real(kind=8) :: tcla1, tcla2, tcla3, g, k1, k2, k3
    real(kind=8) :: r8pre, sum_teth, sum_forc, r8bit2(2)
    real(kind=8) :: poids, norme, vf, rg, phig
    real(kind=8) :: fk(27,3,3)
    integer :: alp, jstno
    real(kind=8) :: e, nu, mu, depi, ka, coeff, coeff3
    real(kind=8) :: coorse(3*nno_se_max), geoloc(2*nno_par_max)
    real(kind=8) :: td1(3), td2(3), nd(3), xg(3), xg_loc(2), he(1), oprim(3)
    real(kind=8) :: depla(3), ff(nno_par_max), coorse_loc(2*nno_par_max)
    real(kind=8) :: dford1(3), dford2(3), forcg(3), dfor(3), forc
    real(kind=8) :: p(3, 3), invp(3, 3), valres(3), devres(3)
    real(kind=8) :: u1g(3), u2g(3), u3g(3), fkpo(3, 3)
    real(kind=8) :: dfdi_loc(nno_par_max,2)
    character(len=8) :: elrefp, elrese(4), elref, enr, noma
    character(len=16) :: nomres(3)
    aster_logical :: l_not_zero
    data          elrese /'SE2','TR3','SE3','TR6'/
!
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! - prealables
! ----------------------------------------------------------------------
!
! - element de reference parent
    call elref1(elrefp)
    call elrefe_info(fami='RIGI', ndim=ndime, nno=nnop, nnos=nnops)
    ASSERT(nnop .le. nno_par_max)
    ASSERT(ndime .eq. 2)
!
! - dimension de l'espace
    call tecael(iadzi, iazk24, noms=0)
    noma=zk24(iazk24)(1:8)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
    ASSERT(ndim .eq. 3)
!
! - sous-element de reference
    if (.not.iselli(elrefp)) then
        irese=2
    else
        irese=0
    endif
    elref=elrese(ndime+irese)
    call elrefe_info(elrefe=elref, fami='XCON', nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde)
    ASSERT(nno .le. nno_se_max)
!
! - on interdit le multi-heaviside
    call tecach('OOO', 'PHEAVTO', 'L', iret, nval=7, itab=jtab)
    ncomp = jtab(2)
    nfiss = jtab(7)
    ASSERT(nfiss .eq. 1)
!
! - initialisation des dimensions des ddls xfem
! - il ne faut pas appeler xteini car elle ne gere pas les elements de bord
    nfe = 0
    nfh = 0
    call teattr('S', 'XFEM', enr, ier)
    if (enr(1:2) .eq. 'XH') then
        nfh = 1
    endif
    if (enr(1:2) .eq. 'XT' .or. enr(3:3) .eq. 'T') then
        nfe = 1
    endif
    ASSERT((nfe .gt. 0) .or. (nfh .gt. 0))
!
! - on interdit tout ddl autre que le deplacement (contact...)
    call tecach('OOO', 'PDEPLAR', 'L', iret, nval=7, itab=jtab)
    ddld = ndim*(1+nfh+nfe)
    nlong_ddl = jtab(2)
    ASSERT(nlong_ddl .eq. nnop*ddld)
    ddls = ddld
    ddlm = ddld
!
! - initialisation terme classique
    tcla = 0.d0
    tcla1 = 0.d0
    tcla2 = 0.d0
    tcla3 = 0.d0
!
! - precision utilisee pour tester la nullite d'un reel
    r8pre = r8prem()
!
! - 2.*pi !
    depi = r8depi()
!
! - pas de calcul de G pour les elements ou la valeur de theta est nulle
    call jevech('PTHETAR', 'L', ithet)
    call jevech('PGTHETA', 'E', igthet)
    cpt = 0
    sum_teth = 0.d0
    do inop = 1, nnop
        do j = 1, ndim
            sum_teth = sum_teth + abs(zr(ithet-1 + ndim*(inop-1) + j))
        enddo
        if (abs(sum_teth).lt.r8pre) then
            cpt = cpt + 1
        endif
    enddo
    if (cpt .eq. nnop) then
        goto 999
    endif
!
! ----------------------------------------------------------------------
! - recuperation des entrees / sorties
! ----------------------------------------------------------------------
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PMATERC', 'L', imate)
!
! - si la pression n'est connue sur aucun noeud, on la choisit nulle
    call jevecd('PPRESSR', ipres, 0.d0)
!
! - la programmation relative aux chargements FORCE_FACE n'est pas
! - encore realisee dans ce te, on interdit donc la presence de ce
! - type de charge
    call jevech('PFR2D3D', 'L', iforc)
    cpt = 0
    sum_forc = 0.d0
    do inop = 1, nnop
        do j = 1, ndim
            sum_forc = sum_forc + abs(zr(iforc-1 + ndim*(inop-1) + j))
        enddo
        if (abs(sum_forc).lt.r8pre) then
            cpt = cpt + 1
        endif
    enddo
    if (cpt .ne. nnop) call utmess('F', 'XFEM_98')
!
! - parametres propres a xfem
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PBASLOR', 'L', ibalo)
    if (nfe.gt.0) call jevech('PSTANO', 'L', jstno)
!
! - donnees topologiques des fonction Heaviside
    if (enr(1:2).eq.'XH') then
        call jevech('PHEA_NO', 'L', jheavn)
        call tecach('OOO', 'PHEA_NO', 'L', iret, nval=7, itab=jtab)
        ncompn = jtab(2)/jtab(3)
    endif
!
! - parametres propres aux elements xfem quadratiques
    if (ier.eq.0 .and. ltequa(elref, enr)) then
        call jevech('PPMILTO', 'L', jpmilt)
    endif
!
! ----------------------------------------------------------------------
! - Definition du repere local de la face supposee plane (element lineaire)
! ----------------------------------------------------------------------
!
! - tangentes et normale a la face
    call vecini(ndim, 0.d0, td1)
    call vecini(ndim, 0.d0, td2)
    do j = 1, ndim
        td1(j) = zr(igeom+ndim*(2-1)+j-1)- zr(igeom+ndim*(1-1)+j-1)
        td2(j) = zr(igeom+ndim*(3-1)+j-1)- zr(igeom+ndim*(1-1)+j-1)
    enddo
!
! - calcul d'une base orthomormee 'Bprime' = (td1, td2, nd) 
!   rq : on norme td1 et td2 avant de faire les produits vectoriels
!        pour eviter les pb si on a des "petites" mailles
    call xnormv(ndim, td1, norme)
    ASSERT(norme .gt. r8pre)
    call xnormv(ndim, td2, norme)
    ASSERT(norme .gt. r8pre)
    call provec(td1, td2, nd)
    call xnormv(ndim, nd, norme)
    ASSERT(norme .gt. r8pre)
    call provec(nd, td1, td2)
    call xnormv(ndim, td2, norme)
    ASSERT(norme .gt. r8pre)
!
! - origine 'oprim' du repere local (1er noeud)
    do j = 1, ndim
        oprim(j) = zr(igeom+ndim*(1-1)+j-1)
    enddo
!
! - coordonnees des noeuds de l'element parent dans le 
! - repere 2D local (oprim, (td1, td2))
    call vecini(2*nno_par_max, 0.d0, geoloc)
    do inop = 1, nnop
        do j = 1, ndim
            geoloc(2*inop-1) = geoloc(2*inop-1)&
                               + (zr(igeom+ndim*(inop-1)+j-1) - oprim(j))&
                               * td1(j)
            geoloc(2*inop) = geoloc(2*inop)&
                             + (zr(igeom+ndim*(inop-1)+j-1) - oprim(j))&
                             * td2(j)
        enddo
    enddo
!
! ----------------------------------------------------------------------
! - recuperation des donnees materiau
! ----------------------------------------------------------------------
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'ALPHA'
!
! - attention : le materiau est suppose constant en espace, on le 
! - recupere donc sur le premier pg de la famille (bidon) RIGI
    call rcvad2('RIGI', 1, 1, '+', zi(imate), 'ELAS', 3, nomres,&
                valres, devres, icodre)
    if ((icodre(1).ne.0) .or. (icodre(2).ne.0)) then
        call utmess('F', 'RUPTURE1_25')
    endif
    if (icodre(3) .ne. 0) then
        valres(3) = 0.d0
        devres(3) = 0.d0
    endif
!
    e = valres(1)
    nu = valres(2)
!
    mu = e / (2.d0*(1.d0+nu))
    ka = 3.d0 - 4.d0*nu
    coeff = e / (1.d0-nu*nu)
    coeff3 = 2.d0 * mu
!
! ----------------------------------------------------------------------
! - Boucle d'integration sur les nse sous-elements 
! ----------------------------------------------------------------------
!
! - recuperation de la subdivision des elements en nse sous-elements
    nse = zi(jlonch-1+1)
!
    do ise = 1, nse
!
! ----- coordonnees des noeuds du sous-element dans le repere reel
        call vecini(3*nno_se_max, 0.d0, coorse)
        do in = 1, nno
            ino=zi(jcnset-1+nno*(ise-1)+in)
            do j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else if (ino.gt.2000 .and. ino.lt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-2000-&
                    1)+j)
                else if (ino.gt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-3000-&
                    1)+j)
                endif
            enddo
        enddo
!
! ----- coordonnees des noeuds du sous-element dans le repere 2D local
        call vecini(2*nno_par_max, 0.d0, coorse_loc)
        do in = 1, nno
            do j = 1, ndim
                coorse_loc(2*in-1) = coorse_loc(2*in-1)&
                                   + (coorse(ndim*(in-1)+j) - oprim(j))&
                                   * td1(j)
                coorse_loc(2*in) = coorse_loc(2*in)&
                                 + (coorse(ndim*(in-1)+j) - oprim(j))&
                                 * td2(j)
            enddo
        enddo
!
! ----- fonction heaviside constante sur le sous-element et par fissure
        do ifiss = 1, nfiss
            he(ifiss) = zi(jheavt-1+ncomp*(ifiss-1)+ise)
        end do
!
! ----- calcul de l'identifiant du sous-element
        hea_se = xcalc_code(nfiss, he_real=[he])
!
! ----------------------------------------------------------------------
! ----- Boucle sur les points de Gauss du sous-element 
! ----------------------------------------------------------------------
!
        do kpg = 1, npg
!
! --------- calcul du poids : poids = poids * jacobien
            call dfdm2d(nno, kpg, ipoids, idfde, coorse_loc, poids)
!
! --------- coordonnees du point de Gauss dans le repere global : xg
            call vecini(ndim, 0.d0, xg)
            do in = 1, nno
                vf = zr(ivf-1+nno*(kpg-1)+in)
                do j = 1, ndim
                    xg(j) = xg(j) + vf * coorse(ndim*(in-1)+j)
                enddo
            enddo
!
! --------- coordonnees du point de Gauss dans le repere 
! --------- 2D local (oprim, (td1, td2)) : xg_loc
            call vecini(2, 0.d0, xg_loc)
            do j = 1, ndim
                xg_loc(1) = xg_loc(1) + (xg(j) - oprim(j)) * td1(j)
                xg_loc(2) = xg_loc(2) + (xg(j) - oprim(j)) * td2(j)
            enddo
!
! --------- derivees des fonctions de formes dans le repere local
            call reeref(elrefp, nnop, geoloc, xg_loc, ndime,&
                        r8bit2, ff, dfdi=dfdi_loc(1:nnop,1:ndime))
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    CALCUL DES COOR. CYL.
!!!!!!!!!!!!!!!!!!!!!!!!!!!
             p(:,:)=0.d0
             invp(:,:)=0.d0
             call coor_cyl(ndim, nnop, zr(ibalo), zr(igeom), ff,&
                           p(1:ndim,1:ndim), invp(1:ndim,1:ndim), rg, phig,&
                           l_not_zero)
!
! --------- champs singuliers
             call xdeffk(ka, mu, rg, phig, 3, fkpo)
!
! --------- champs singuliers dans la base globale
            u1g(:)=0.
            u2g(:)=0.
            u3g(:)=0.
            do j = 1, ndim
                do k = 1, ndim
                    u1g(j) = u1g(j) + p(j,k)*fkpo(1,k)
                    u2g(j) = u2g(j) + p(j,k)*fkpo(2,k)
                    u3g(j) = u3g(j) + p(j,k)*fkpo(3,k)
                enddo
            enddo
!
! --------- calcul des fonctions d'enrichissement
            if (nfe .gt. 0) then
              call xcalfev_wrap(ndim, nnop, zr(ibalo), zi(jstno), he(1),&
                           zr(jlsn), zr(jlst), zr(igeom), ka, mu, ff, fk)
            endif
!
! --------- calcul de l'approximation du deplacement
            call vecini(ndim, 0.d0, depla)
            do inop = 1, nnop
                call indent(inop, ddls, ddlm, nnos, indeni)
                cpt = 0
!               ddls classiques
                do j = 1, ndim
                    cpt = cpt + 1
                    depla(j) = depla(j) + ff(inop)*zr(idepl-1+indeni+cpt)
                end do
!               ddls heaviside
                do ig = 1, nfh
                    do j = 1, ndim
                        cpt = cpt + 1
                        depla(j) = depla(j) + xcalc_heav(zi(jheavn-1+ncompn*(inop-1)+ig),&
                                                         hea_se,&
                                                         zi(jheavn-1+ncompn*(inop-1)+ncompn))&
                                            * ff(inop) * zr(idepl-1+indeni+cpt)
                    end do
                end do
!               ddls enrichis en fond de fissure
                do alp = 1, nfe*ndim
                    cpt = cpt + 1
                    do j = 1, ndim
                        depla(j) = depla(j) + fk(inop,alp,j) * zr(idepl-1+indeni+cpt)
                    end do
                end do
            end do
!
! --------- calcul de la pression
            pres = 0.d0 
            do inop = 1, nnop
                pres = pres + zr(ipres-1+inop) * ff(inop)
            enddo
!
! --------- calcul du terme surfacque
            th1 = 0.d0
            th2 = 0.d0
            dth1d1 = 0.d0
            dth2d2 = 0.d0
!
            do j = 1, ndim
                dford1(j) = 0.d0
                dford2(j) = 0.d0
                dfor(j) = 0.d0
                forcg(j) = 0.d0
            enddo
!
            do inop = 1, nnop
                do j = 1, ndim
                    th1 = th1 + ff(inop) * zr(ithet+3*(inop-1)+j-1) * td1(j)
                    th2 = th2 + ff(inop) * zr(ithet+3*(inop-1)+j-1) * td2(j)
                    dth1d1 = dth1d1 + zr(ithet-1+3*(inop-1)+j) * td1(j) * dfdi_loc(inop,1)
                    dth2d2 = dth2d2 + zr(ithet-1+3*(inop-1)+j) * td2(j) * dfdi_loc(inop,2)
                enddo
            enddo
!
            do j = 1, ndim
                dfor(j) = dfor(j) + dford1(j)*th1+dford2(j)*th2
            enddo
!
            divt = dth1d1+dth2d2
!
            do j = 1, ndim
                forc = forcg(j) - pres*nd(j)
                tcla = tcla + poids*(forc*divt+dfor(j))*depla(j)
                tcla1 = tcla1 + 0.5d0*poids*(forc*divt+dfor(j))*u1g(j)
                tcla2 = tcla2 + 0.5d0*poids*(forc*divt+dfor(j))*u2g(j)
                tcla3 = tcla3 + 0.5d0*poids*(forc*divt+dfor(j))*u3g(j)
            enddo
!
! ----------------------------------------------------------------------
! ----- Fin boucle sur les points de Gauss du sous-element 
! ----------------------------------------------------------------------
!
        enddo
!
! ----------------------------------------------------------------------
! - Fin boucle d'integration sur les nse sous-elements 
! ----------------------------------------------------------------------
!
    enddo
!
999 continue
!
    g = tcla
    k1 = tcla1 * coeff
    k2 = tcla2 * coeff
    k3 = tcla3 * coeff3
!
    zr(igthet) = g
    zr(igthet+1) = tcla1 * sqrt(coeff)
    zr(igthet+2) = tcla2 * sqrt(coeff)
    zr(igthet+3) = tcla3 * sqrt(coeff3)
    zr(igthet+4) = k1
    zr(igthet+5) = k2
    zr(igthet+6) = k3
!
end subroutine
