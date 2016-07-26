subroutine xmcont(algocr, coefcr, coefcp, cohes, coheo,&
                  jcohes, jcoheo, ncompv,&
                  ddlm, ddls, ffc, ffp, idepd,&
                  idepm, ifa, ifiss, jmate, indco,&
                  ipgf, jac, jheavn, ncompn, jheafa, mmat,&
                  lact, ncomph, nd, nddl, ndim,&
                  nfh, nfiss, nno, nnol, nnos,&
                  nvit, pla, rela, singu, fk,&
                  tau1, tau2)
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
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
! IN ALGOCR : ALGO CONTACT (1:LAG, 2:PENA, 3:COHESIF)
! IN COEFCR : COEF AUGMENTATION CONTACT
! IN COEFCP : COEF PENALISATION CONTACT
! IN COHES  : VARIABLE INTERNE COHESIVE
! IN DDLM   : NOMBRE DE DDLS A CHAQUE NOEUD MILIEU
! IN DDLS   : NOMBRE DE DDLS A CHAQUE NOEUD SOMMET
! IN FFC    : FONCTIONS DE FORME DE CONTACT
! IN FFP    : FONCTIONS DE FORME ELEMENT PARENT
! IN IDEPD  : ADRESSE INCREMENT DEPLACEMENT COURANT
! IN IDEPM  : ADRESSE DEPLACEMENT INSTANT -
! IN IFA    : NUMERO FACETTE DE CONTACT
! IN IFISS  : NUMERO FISSURE
! IN JMATE  : ADRESSE MATERIAU
! IN INDCO  : ETAT DE CONTACT POINT DE GAUSS
! IN IPGF   : NUMERO POINT DE GAUSS DE CONTACT
! IN IVFF   : ADRESSE FONCTION DE FORME EL PARENT
! IN JAC    : PRODUIT JACOBIEN*POIDS
! IN JHEAFA
! OUT MMAT  : MATRICE ELEMENTAIRE DE CONTACT
! IN NCOMPH
! IN ND     : NORMALE A LA SURFACE DE CONTACT AU PG
! IN NDDL   : NOMBRE TOTAL DDL DE L ELEMENT
! IN NDIM   : DIMENSION DU MODELE
! IN NFH    : NOMBRE DE DDL HEAVISIDE
! IN NFISS  : NOMBRE DE FISSURES
! IN NOEUD  : FORMULATION AUX NOEUDS
! IN PLA    : PLACE DES DDLS DE LAGRANGE
! IN RELA   : LOI DE COMPORTEMENT COHESIVE
! IN SINGU  : ELEMENT ENRICHI CTIP OU ON
! IN TAU1   : 1ERE TANGENTE SURFACE DE CONTACT
! IN TAU2   : 2EME TANGENTE (3D)
#include "asterfort/xmmaa3.h"
#include "asterfort/xmmaa4.h"
#include "asterfort/xmmco1.h"
#include "asterfort/xmmco2.h"
#include "asterfort/xmmpa3.h"
#include "asterfort/xmmsa2.h"
#include "asterfort/xmmsa3.h"
#include "asterfort/xmmsa5.h"
#include "asterfort/xmmsa6.h"
#include "asterfort/xxlag2.h"
#include "asterfort/xmmco3.h"
#include "asterfort/xmmco4.h"
#include "asterfort/xxlan5.h"
    integer :: i, ino, jcoheo, ncompv, jcohes
    integer :: algocr, ddlm, ddls
    integer :: idepd, idepm, ifa, ifiss, ibid
    integer :: jmate, indco, ipgf, jheafa, jheavn, ncompn
    integer :: ncomph, nddl, ndim, nfh, nfiss, lact(8)
    integer :: nno, nnol, nnos, nvec, nvit, pla(27)
    integer :: singu
    real(kind=8) :: alpha(3), am(3), cohes(3), coefcp, coefcr
    real(kind=8) :: dsidep(6, 6), ffc(8), ffp(27), raug, wsaut(3)
    real(kind=8) :: jac, mmat(216, 216), nd(3), p(3, 3)
    real(kind=8) :: sigma(6), saut(3), tau1(3), tau2(3), rela
    real(kind=8) :: lamb(3), delta(6), r, coheo(3)
    real(kind=8) :: dnor(3), dtang(3), pp(3,3), un
    real(kind=8) :: fk(27,3,3)
    character(len=8) :: job, champ
!
! CAS FORMULATION LAGRANGIEN AUGMENTE
!
    if (algocr .eq. 1) then
        if (indco .eq. 0) then
            if (nvit .ne. 0) then
                call xmmaa4(nnol, pla, ffc, jac, coefcr,&
                            mmat)
            endif
        else if (indco.eq.1) then
            call xmmaa3(ndim, nno, nnos, nnol, pla,&
                        ffc, ffp, jac, nfh, nd,&
                        coefcr, singu, fk, ddls, ddlm,&
                        jheavn, ncompn, nfiss, ifiss, jheafa, ncomph,&
                        ifa, mmat)
        endif
!
! CAS FORMULATION PENALISEE
!
    else if (algocr.eq.2) then
        if (indco .eq. 0) then
            if (nvit .ne. 0) then
                call xmmaa4(nnol, pla, ffc, jac, coefcp,&
                            mmat)
            endif
        else if (indco.eq.1) then
            call xmmpa3(ndim, nno, nnos, nnol, pla,&
                        ffc, ffp, jac, nfh, nd,&
                        coefcp, singu, fk, ddls, ddlm,&
                        jheavn, ncompn, nfiss, ifiss, jheafa, ncomph,&
                        ifa, mmat)
        endif
!
! CAS LOI COHESIVE
!
    else if (algocr.eq.3) then
!
!       CAS DES LOIS COHESIVES REGULARISEES
!
        if (rela .eq. 1.d0 .or. rela .eq. 2.d0) then
            nvec=2
            un = 1.d0
            if (nvit .ne. 0) then
                call xmmaa4(nnol, pla, ffc, jac, un,&
                            mmat)
            endif
            call xmmsa3(ndim, nno, nnos, ffp, nddl,&
                        nvec, zr(idepd), zr(idepm), zr(idepm), nfh,&
                        singu, fk, ddls, ddlm, jheavn, ncompn,&
                        nfiss, ifiss, jheafa, ncomph, ifa,&
                        saut)
            job='MATRICE'
            call xmmsa2(ndim, ipgf, zi(jmate), saut, nd,&
                        tau1, tau2, cohes, job, rela,&
                        alpha, dsidep, sigma, pp, dnor,&
                        dtang, p, am)
            call xmmco1(ndim, nno, dsidep, pp, p,&
                        nd, nfh, ddls, jac, ffp,&
                        singu, fk, tau1, tau2, mmat)
!
!           ACTUALISATION INDICATEUR PREDICTION / CORRECTION
            coheo(1)=cohes(1)
            coheo(2)=cohes(2)
            coheo(3)=2.d0
!
!       CAS DES LOIS MIXTES TYPE "MORTAR"
!
        else if(rela.eq.5.d0) then
!
! --- CALCUL DES MATRICES "MORTAR" QUI NE DEPENDENT
! --- PAS DE LA LOI DE COMPORTEMENT
!
            call xmmco4(ndim, nno, pla, nd, tau1,&
                        tau2, ffc, ddls, jac, ffp,&
                        nnol, ddlm, nnos, mmat)
            do 12 ino = 1, nnol
                nvec = 2
                champ = 'LAMBDA'
                call xxlan5(ino, idepd, idepm, ibid, lact, ndim,&
                            pla, lamb, nvec, champ )
!
                nvec = 2
                champ = 'W'
                call xxlan5(ino, idepd, idepm, ibid, lact, ndim,&
                            pla, wsaut, nvec, champ )
!
                do 2 i = 1, ncompv
                    cohes(i) = zr(jcohes+ncompv*(ino-1)-1+i)
 2              continue
                job='MATRICE'
                call xmmsa6(ndim, ipgf, zi(jmate), lamb, wsaut, nd,&
                            tau1, tau2, cohes, job, rela,&
                            alpha, dsidep, sigma, p, am, raug)
                call xmmco3(ino, ndim, dsidep, pla, p,&
                            ffc, jac, nnol, raug, mmat)
!
! --- ACTUALISATION INDICATEUR PREDICTION / CORRECTION
!
                coheo(1)=cohes(1)
                coheo(2)=cohes(2)
                coheo(3)=2.d0
!
                do 3 i = 1, ncompv
                    zr(jcoheo+ncompv*(ino-1)-1+i) = coheo(i)
 3              continue
12          continue
!
        else if (rela.eq.3.d0.or.rela.eq.4.d0) then
!
! CAS DES LOIS MIXTES CZM_TAC_MIX ET CZM_OUV_MIX
!
! --- ON COMMENCE EGALEMENT PAR CALCULER LE SAUT
!
            nvec = 2
            call xmmsa3(ndim, nno, nnos, ffp, nddl,&
                        nvec, zr(idepd), zr(idepm), zr(idepm), nfh,&
                        singu, fk, ddls, ddlm, jheavn, ncompn,&
                        nfiss, ifiss, jheafa, ncomph, ifa,&
                        saut)
!
! --- ON CALCULE ENSUITE LA VALEUR DE LA FORCE COHESIVE
!
            nvec = 2
            call xxlag2(ffc, idepd, idepm, lact, ndim,&
                        nnol, pla, lamb, nvec)
!
! --- ON VA ENSUITE ALLER CHERCHER LA MATRICE TGTE LOCALE
! --- ET LA MATRICE DE CHANGEMENT DE BASE
!
            job = 'MATRICE'
            call xmmsa5(ndim, ipgf, zi(jmate), saut, lamb,&
                        nd, tau1, tau2, cohes, job,&
                        rela, alpha, dsidep, delta, p,&
                        am, r)
!
! --- CALCUL DE LA MATRICE TANGENTE
!
            call xmmco2(ndim, nno, nnos, nnol, ddls,&
                        ddlm, dsidep, p, r, nfh,&
                        jac, ffp, ffc, pla, singu,&
                        nfiss, jheafa, jheavn, ncompn, ifa, ncomph,&
                        ifiss, fk, mmat)
!
! --- ACTUALISATION INDICATEUR PREDICTION / CORRECTION
!
            coheo(1)=cohes(1)
            coheo(2)=cohes(2)
            coheo(3)=2.d0
        endif
    endif
end subroutine
