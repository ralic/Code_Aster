subroutine xvfrot(algofr, coeffp, coeffr, ddlm, ddls,&
                  ffc, ffp, idepl, idepm, ifa,&
                  ifiss, indco, jac, jfisno, jheafa,&
                  lact, mu, ncomph, nd, nddl,&
                  ndim, nfh, nfiss, nno, nnol,&
                  nnos, nvit, pla, reac12, rr,&
                  seuil, singu, tau1, tau2, vtmp)
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! IN ALGOFR : ALGO FROTTEMENT (1:LAG, 2:PENA, 0:RIEN)
! IN CFACE  : CONNECTIVITE FACETTES DE CONTACT
! IN COEFFR : COEF AUGMENTATION FROT
! IN COEFFP : COEF PENALISATION FROT
! IN DDLM   : NOMBRE DE DDLS A CHAQUE NOEUD MILIEU
! IN DDLS   : NOMBRE DE DDLS A CHAQUE NOEUD SOMMET
! IN FFC    : FONCTIONS DE FORME DE CONTACT
! IN FFP    : FONCTIONS DE FORME ELEMENT PARENT
! IN IDEPL  : ADRESSE DEPLACEMENT COURANT
! IN IDEPM  : ADRESSE DEPLACEMENT INSTANT -
! IN IFA    : NUMERO FACETTE DE CONTACT
! IN INDCO  : ETAT DE CONTACT POINT DE GAUSS
! IN IPGF   : NUMERO POINT DE GAUSS DE CONTACT
! IN IVFF   : ADRESSE FONCTION DE FORME EL PARENT
! IN JAC    : PRODUIT JACOBIEN*POIDS
! IN LACT   : DDL DE LAGRANGE ACTIF OU NON
! IN MU     : COEFFICIENT DE COULOMB
! IN ND     : NORMALE A LA SURFACE DE CONTACT AU PG
! IN NDIM   : DIMENSION DU MODELE
! IN NFH    : NOMBRE DE DDL HEAVISIDE
! IN NFISS  : NOMBRE DE FISSURES
! IN NNO    : NOMBRE DE NOEUDS TOTAL ELEMENT PARENT
! IN NNOF   : NOMBRE DE NOEUDS D UNE FACETTE DE CONTACT
! IN NNOL   : NOMBRE DE NOEUDS EL PARENT PORTEURS DE DDL LAGRANGE
! IN NNOS   : NOMBRE DE NOEUDS SOMMET ELEMENT PARENT
! IN NOEUD  : FORMULATION AUX NOEUDS
! IN NVIT   : ARETE VITALE OU NON
! IN PLA    : PLACE DES DDLS DE LAGRANGE
! IN RR     : RACINE RAYON A LA POINTE DE FISSURE
! IN SINGU  : ELEMENT ENRICHI CTIP OU ON
! IN TAU1   : 1ERE TANGENTE SURFACE DE CONTACT
! IN TAU2   : 2EME TANGENTE (3D)
! OUT VTMP  : VECTEUR DE TRAVAIL SECOND MEMBRE
#include "asterfort/assert.h"
#include "asterfort/xmmsa3.h"
#include "asterfort/xmvef2.h"
#include "asterfort/xmvef3.h"
#include "asterfort/xmvef4.h"
    integer :: algofr, ddlm, ddls
    integer :: idepl, idepm, ifa, ifiss
    integer :: indco, jfisno
    integer :: jheafa, lact(8), ncomph
    integer :: nddl, ndim, nfh, nfiss, nno
    integer :: nnol, nnos, nvec, nvit
    integer :: pla(27), singu
    real(kind=8) :: coeffp, coeffr, ffc(8), ffp(27), jac
    real(kind=8) :: mu, nd(3), pb(3), reac12(3), rr, saut(3), seuil
    real(kind=8) :: tau1(3), tau2(3), vtmp(400)
!
    if (mu .eq. 0.d0) indco = 0
    if (algofr .ne. 0 .and. seuil .eq. 0.d0) indco = 0
    if (nfiss .gt. 1) indco = 0
!
    if (indco .eq. 0) then
        if (nvit .ne. 0) then
            nvec=2
            call xmmsa3(ndim, nno, nnos, ffp, nddl,&
                        nvec, zr(idepl), zr(idepm), zr(idepm), nfh,&
                        singu, rr, ddls, ddlm, jfisno,&
                        nfiss, ifiss, jheafa, ncomph, ifa,&
                        saut)
!
! --- CALCUL DU VECTEUR LN3
!
            call xmvef4(ndim, nnol, pla, ffc, reac12,&
                        jac, tau1, tau2, lact, vtmp)
!
! --- ACTIVATION DE LA LOI COHESIVE & RECUPERATION DES
! --- PARAMETRES MATERIAUX
!
        endif
!
    else if (indco.eq.1) then
!
! --- CALCUL DU VECTEUR LN1
!
        call xmvef2(ndim, nno, nnos, ffp, jac,&
                    seuil, reac12, singu, nfh, rr,&
                    coeffp, coeffr, mu, algofr, nd,&
                    ddls, ddlm, idepl, pb, vtmp)
!
! --- CAS LAGRANGIEN AUGMENTE
!
        if (algofr .eq. 1) then
!
!
! --- CALCUL DU VECTEUR LN3
!
            call xmvef3(ndim, nnol, pla, ffc, reac12,&
                        pb, jac, seuil, tau1, tau2,&
                        lact, coeffr, mu, vtmp)
!
        else if (algofr.eq.2) then
!
!
! --- CALCUL DU VECTEUR LN3
!
            call xmvef3(ndim, nnol, pla, ffc, reac12,&
                        pb, jac, seuil, tau1, tau2,&
                        lact, coeffp, mu, vtmp)
        endif
!
    else
        ASSERT(indco.eq.0 .or. indco.eq.1)
    endif
end subroutine
