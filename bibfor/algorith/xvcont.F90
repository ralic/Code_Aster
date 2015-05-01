subroutine xvcont(algocr, cohes, jcohes, ncompv,&
                  coefcp, coefcr,&
                  ddlm, ddls, ffc, ffp, idepl,&
                  idepm, ifa, ifiss, imate, indco,&
                  ipgf, jac, jheavn, ncompn, jheafa, lact,&
                  ncomph, nd, nddl, ndim, nfh,&
                  nfiss, nno, nnol, nnos, nvit,&
                  pla, rela, reac, rr, singu,&
                  tau1, tau2, vtmp)
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! IN ALGOCR : ALGO CONTACT (1:LAG, 2:PENA, 3:COHESIF)
! IN CFACE  : CONNECTIVITE FACETTES DE CONTACT
! IN COEFCR : COEF AUGMENTATION CONTACT
! IN COEFCP : COEF PENALISATION CONTACT
! IN COHES  : VARIABLE INTERNE COHESIVE
! IN DDLM   : NOMBRE DE DDLS A CHAQUE NOEUD MILIEU
! IN DDLS   : NOMBRE DE DDLS A CHAQUE NOEUD SOMMET
! IN FFC    : FONCTIONS DE FORME DE CONTACT
! IN FFP    : FONCTIONS DE FORME ELEMENT PARENT
! IN IDEPL  : ADRESSE DEPLACEMENT COURANT
! IN IDEPM  : ADRESSE DEPLACEMENT INSTANT -
! IN IFA    : NUMERO FACETTE DE CONTACT
! IN IFISS  : NUMERO FISSURE
! IN IMATE  : ADRESSE MATERIAU
! IN INDCO  : ETAT DE CONTACT POINT DE GAUSS
! IN IPGF   : NUMERO POINT DE GAUSS DE CONTACT
! IN IVFF   : ADRESSE FONCTION DE FORME EL PARENT
! IN JAC    : PRODUIT JACOBIEN*POIDS
! IN JHEAFA
! IN NCOMPH
! IN ND     : NORMALE A LA SURFACE DE CONTACT AU PG
! IN NDDL   : NOMBRE TOTAL DDL DE L ELEMENT
! IN NDIM   : DIMENSION DU MODELE
! IN NFH    : NOMBRE DE DDL HEAVISIDE
! IN NFISS  : NOMBRE DE FISSURES
! IN NOEUD  : FORMULATION AUX NOEUDS OU NON
! IN NVIT   : ARETE VITALE OU NON
! IN PLA    : PLACE DES DDLS DE LAGRANGE
! IN RELA   : LOI DE COMPORTEMENT COHESIVE
! IN REAC   : REACTION DE CONTACT (LAMBDA)
! IN RR     : RACINE RAYON A LA POINTE DE FISSURE
! IN SINGU  : ELEMENT ENRICHI CTIP OU ON
! IN TAU1   : 1ERE TANGENTE SURFACE DE CONTACT
! IN TAU2   : 2EME TANGENTE (3D)
! OUT VTMP  : VECTEUR DE TRAVAIL SECOND MEMBRE
#include "asterfort/assert.h"
#include "asterfort/xmmsa2.h"
#include "asterfort/xmmsa3.h"
#include "asterfort/xmmsa5.h"
#include "asterfort/xmmsa6.h"
#include "asterfort/xmvco1.h"
#include "asterfort/xmvco2.h"
#include "asterfort/xmvco4.h"
#include "asterfort/xmvco5.h"
#include "asterfort/xmvec2.h"
#include "asterfort/xmvec3.h"
#include "asterfort/xmvep2.h"
#include "asterfort/xxlag2.h"
#include "asterfort/xxlag4.h"
#include "asterfort/xxlan5.h"
    integer :: jcohes, ncompv
    integer :: algocr, ibid
    integer :: ddlm, ddls, i, ino
    integer :: idepl, idepm, ifa, ifiss
    integer :: imate, indco, ipgf
    integer :: jheafa, lact(8), ncomph, jheavn, ncompn
    integer :: nddl, ndim, nfh, nfiss, nno
    integer :: nnol, nnos, nvec, nvit, pla(27)
    integer :: singu
    real(kind=8) :: alpha(3), am(3), dsidep(6, 6), cohes(3)
    real(kind=8) :: coefcr, coefcp, ffc(8), ffp(27), jac, raug
    real(kind=8) :: nd(3), p(3, 3), reac, rr, saut(3), mu(3)
    real(kind=8) :: sigma(6), tau1(3), tau2(3), vtmp(400), rela
    real(kind=8) :: delta(6), lamb(3), r, wsaut(3)
    real(kind=8) :: dtang(3), dnor(3), pp(3,3), un
    character(len=8) :: job, champ
!
! --- CAS COHESIF
!
    if (algocr .eq. 3) then
!
! --- SI LOI COHESIVE REGULARISEE CZM_XXX_REG
!
        if (rela .eq. 1.d0 .or. rela .eq. 2.d0) then
            un = 1.d0
            if (nvit .ne. 0) then
                call xmvec3(nnol, pla, ffc, reac, jac,&
                            un, vtmp)
            endif
!
! --- CALCUL DU SAUT DE DEPLACEMENT EQUIVALENT [[UEG]]
!
            nvec=2
            call xmmsa3(ndim, nno, nnos, ffp, nddl,&
                        nvec, zr(idepl), zr(idepm), zr(idepm), nfh,&
                        singu, rr, ddls, ddlm, jheavn, ncompn,&
                        nfiss, ifiss, jheafa, ncomph, ifa,&
                        saut)
!
            job='VECTEUR'
            call xmmsa2(ndim, ipgf, imate, saut, nd,&
                        tau1, tau2, cohes, job, rela,&
                        alpha, dsidep, sigma, pp, dnor,&
                        dtang, p, am)
!
! --- CALCUL DES SECONDS MEMBRES DE COHESION
!
            call xmvco1(ndim, nno, nnol, sigma, pla,&
                        lact, dtang, nfh, ddls, jac,&
                        ffc, ffp, singu, rr, un,&
                        nd, tau1, tau2, jheavn, ncompn,&
                        nfiss, ifiss, jheafa, ncomph, ifa,&
                        vtmp)
!
! --- SI FORMULATION "MORTAR" LOI CZM_LIN
!
        else if(rela.eq.5.d0) then
!
! --- CALCUL DU SAUT DE DEPLACEMENT [[U]]
!
            nvec=2
            call xmmsa3(ndim, nno, nnos, ffp, nddl,&
                        nvec, zr(idepl), zr(idepm), zr(idepm), nfh,&
                        singu, rr, ddls, ddlm, jheavn, ncompn,&
                        nfiss, ifiss, jheafa, ncomph, ifa,&
                        saut)
!
!           CALCUL W AU POINT DE GAUSS
            nvec = 2
            champ = 'W'
            call xxlag4(ffc, idepl, idepm, lact, ndim,&
                        nnol, pla, wsaut, nvec, champ)
!
!           CALCUL MU AU POINT DE GAUSS
            nvec = 2
            champ = 'MU'
            call xxlag4(ffc, idepl, idepm, lact, ndim,&
                        nnol, pla, mu, nvec, champ)
!
! --- CALCUL DES SECONDS MEMBRES DE COHESION
!
            call xmvco5(ndim, nno, nnol, pla, nd,&
                        tau1, tau2, mu, ddls, jac,&
                        ffc, ffp, nnos, ddlm, wsaut,&
                        saut, vtmp)
!
            do 10 ino = 1, nnol
                do 2 i = 1, ncompv
                    cohes(i) = zr(jcohes+ncompv*(ino-1)-1+i)
 2              continue
                nvec = 2
                champ = 'LAMBDA'
                call xxlan5(ino, idepl, idepm, ibid, lact, ndim,&
                            pla, lamb, nvec, champ)
                nvec = 2
                champ = 'W'
                call xxlan5(ino, idepl, idepm, ibid, lact, ndim,&
                            pla, wsaut, nvec, champ)
                job='VECTEUR'
                call xmmsa6(ndim, ipgf, imate, lamb, wsaut, nd,&
                            tau1, tau2, cohes, job, rela,&
                            alpha, dsidep, sigma, p, am, raug)
                call xmvco4(ino, ndim, nnol, sigma, lamb, pla,&
                            lact, jac, ffc, p, raug, vtmp)
10          continue
!
        else if (rela.eq.3.d0.or.rela.eq.4.d0) then
!
! SI LOI COHESIVE MIXTE CZM_XXX_MIX
! ON COMMENCE PAR CALCULER LE SAUT DE DEPLACEMENT
!
            nvec=2
            call xmmsa3(ndim, nno, nnos, ffp, nddl,&
                        nvec, zr(idepl), zr(idepm), zr(idepm), nfh,&
                        singu, rr, ddls, ddlm, jheavn, ncompn,&
                        nfiss, ifiss, jheafa, ncomph, ifa,&
                        saut)
!
! --- ON CALCULE LA CONTRAINTE
!
            nvec = 2
            call xxlag2(ffc, idepl, idepm, lact, ndim,&
                        nnol, pla, lamb, nvec)
!
!
! --- ON CALCULE ENSUITE DELTA AVEC XMMSA5
!
            job = 'VECTEUR'
            call xmmsa5(ndim, ipgf, imate, saut, lamb,&
                        nd, tau1, tau2, cohes, job,&
                        rela, alpha, dsidep, delta, p,&
                        am, r)
!
! --- CALCUL DES SECONDS MEMBRES
!
            call xmvco2(ndim, nno, nnol, nnos, lamb,&
                        am, delta, pla, lact, nfh,&
                        ddls, ddlm, nfiss, ifiss, jheafa,&
                        ifa, ncomph, jheavn, ncompn, jac, ffc,&
                        ffp, singu, r, rr, vtmp,&
                        p)
        endif
!
    else if (algocr.eq.1) then
!
! --- CAS LAGRANGIEN OU PENALISATION
!
        if (indco .eq. 0) then
            if (nvit .ne. 0) then
                call xmvec3(nnol, pla, ffc, reac, jac,&
                            coefcr, vtmp)
            endif
!
        else if (indco.eq.1) then
!
! --- CALCUL DU SAUT ET DE DN EN CE PG (DEPMOI + DEPDEL)
            nvec=2
            call xmmsa3(ndim, nno, nnos, ffp, nddl,&
                        nvec, zr(idepl), zr(idepm), zr(idepm), nfh,&
                        singu, rr, ddls, ddlm, jheavn, ncompn,&
                        nfiss, ifiss, jheafa, ncomph, ifa,&
                        saut)
!
! --- CALCUL DU VECTEUR LN1 & LN2
!
            call xmvec2(ndim, nno, nnos, nnol, pla,&
                        ffc, ffp, reac, jac, nfh,&
                        saut, singu, nd, rr, coefcr,&
                        ddls, ddlm, jheavn, ncompn, nfiss, ifiss,&
                        jheafa, ncomph, ifa, vtmp)
        endif
    else if (algocr.eq.2) then
        if (indco .eq. 0) then
            if (nvit .ne. 0) then
                call xmvec3(nnol, pla, ffc, reac, jac,&
                            coefcp, vtmp)
            endif
!
        else if (indco.eq.1) then
!
! --- CALCUL DU SAUT ET DE DN EN CE PG (DEPMOI + DEPDEL)
            nvec=2
            call xmmsa3(ndim, nno, nnos, ffp, nddl,&
                        nvec, zr(idepl), zr(idepm), zr(idepm), nfh,&
                        singu, rr, ddls, ddlm, jheavn, ncompn,&
                        nfiss, ifiss, jheafa, ncomph, ifa,&
                        saut)
            call xmvep2(ndim, nno, nnos, nnol, pla,&
                        ffc, ffp, reac, jac, nfh,&
                        saut, singu, nd, rr, coefcp,&
                        ddls, ddlm, jheavn, ncompn, nfiss, ifiss,&
                        jheafa, ncomph, ifa, vtmp)
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
end subroutine
