subroutine xmfrot(algofr, coeffr, coeffp, ddlm, ddls,&
                  ffc, ffp, idepd, idepm, indco,&
                  jac, lact, mmat, mu, nd,&
                  ndim, nfh, nfiss, nno, nnol,&
                  nnos, nvit, pla, rr, seuil,&
                  singu, tau1, tau2)
    implicit none
    include 'jeveux.h'
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21
! IN ALGOFR : ALGO FROTTEMENT (1:LAG, 2:PENA, 0:RIEN)
! IN CFACE  : CONNECTIVITE FACETTES DE CONTACT
! IN COEFFR : COEF AUGMENTATION FROT
! IN COEFFP : COEF PENALISATION FROT
! IN DDLM   : NOMBRE DE DDLS A CHAQUE NOEUD MILIEU
! IN DDLS   : NOMBRE DE DDLS A CHAQUE NOEUD SOMMET
! IN FFC    : FONCTIONS DE FORME DE CONTACT
! IN FFP    : FONCTIONS DE FORME ELEMENT PARENT
! IN IDEPD  : ADRESSE INCREMENT DEPLACEMENT COURANT
! IN IDEPM  : ADRESSE DEPLACEMENT INSTANT -
! IN IFA    : NUMERO FACETTE DE CONTACT
! IN INDCO  : ETAT DE CONTACT POINT DE GAUSS
! IN IPGF   : NUMERO POINT DE GAUSS DE CONTACT
! IN IVFF   : ADRESSE FONCTION DE FORME EL PARENT
! IN JAC    : PRODUIT JACOBIEN*POIDS
! IN LACT   : DDL DE LAGRANGE ACTIF OU NON
! OUT MMAT  : MATRICE ELEMENTAIRE DE CONTACT
! IN MU     : COEFFICIENT DE COULOMB
! IN ND     : NORMALE A LA SURFACE DE CONTACT AU PG
! IN NDIM   : DIMENSION DU MODELE
! IN NFH    : NOMBRE DE DDL HEAVISIDE
! IN NFISS  : NOMBRE DE FISSURES
! IN NNO    : NOMBRE DE NOEUDS TOTAL ELEMENT PARENT
! IN NNOL   : NOMBRE DE NOEUDS EL PARENT PORTEURS DE DDL LAGRANGE
! IN NNOS   : NOMBRE DE NOEUDS SOMMET ELEMENT PARENT
! IN NOEUD  : FORMULATION AUX NOEUDS
! IN NVIT   : ARETE VITALE OU NON
! IN PLA    : PLACE DES DDLS DE LAGRANGE
! IN RR     : RACINE RAYON A LA POINTE DE FISSURE
! IN SINGU  : ELEMENT ENRICHI CTIP OU ON
! IN TAU1   : 1ERE TANGENTE SURFACE DE CONTACT
! IN TAU2   : 2EME TANGENTE (3D)
    include 'asterfort/assert.h'
    include 'asterfort/xmmab3.h'
    include 'asterfort/xmmab4.h'
    include 'asterfort/xmmab5.h'
    include 'asterfort/xmmab6.h'
    include 'asterfort/xmmbp3.h'
    include 'asterfort/xmmbp5.h'
    include 'asterfort/xmmsa1.h'
    integer :: algofr, ddlm, ddls
    integer :: idepd, idepm, indco
    integer :: lact(8), ndim
    integer :: nfh, nfiss, nno, nnol, nnos, nvit
    integer :: pla(27), singu
    real(kind=8) :: coeffp, coeffr, ik(3, 3), jac, knp(3, 3)
    real(kind=8) :: mmat(216, 216), mu, nd(3), p(3, 3), ffc(8), ffp(27)
    real(kind=8) :: ptknp(3, 3), rr, seuil, tau1(3), tau2(3)
    logical :: adher
!
    if (mu .eq. 0.d0 .or. seuil .eq. 0.d0) indco = 0
    if (nfiss .gt. 1) indco = 0
!
    if (indco .eq. 0) then
        if (nvit .ne. 0) then
!
! --- CALCUL DE LA MATRICE F - CAS SANS CONTACT
!
            call xmmab6(ndim, nnol, pla, ffc, jac,&
                        tau1, tau2, lact, mmat)
!
        endif
    else if (indco.eq.1) then
!
! --- CALCUL DES INCREMENTS - DÉPLACEMENTS&
! --- SEMI-MULTIPLICATEUR DE FROTTEMENT
!
        call xmmsa1(algofr, ndim, nno, nnos, nnol,&
                    pla, ffc, ffp, idepd, idepm,&
                    nfh, nd, tau1, tau2, singu,&
                    rr, lact, ddls, ddlm, coeffr,&
                    coeffp, p, adher, knp, ptknp,&
                    ik)
!
! --- CALCUL DE B, BT
!
        if (algofr .eq. 1) then
            call xmmab3(ndim, nno, nnos, nnol, pla,&
                        ffc, ffp, jac, knp, nfh,&
                        seuil, tau1, tau2, mu, singu,&
                        rr, lact, ddls, ddlm, mmat)
!
! --- CALCUL DE B_U
!
            call xmmab4(ndim, nno, nnos, ffp, jac,&
                        ptknp, nfh, seuil, mu, singu,&
                        rr, coeffr, ddls, ddlm, mmat)
!
! --- CALCUL DE F (NULLE EN ADHERENCE)
!
            if (.not.adher) then
                call xmmab5(ndim, nnol, pla, ffc, jac,&
                            coeffr, seuil, tau1, tau2, mu,&
                            ik, lact, mmat)
            endif
        else if (algofr.eq.2) then
            call xmmbp3(ndim, nno, nnos, nnol, pla,&
                        ffc, ffp, jac, knp, nfh,&
                        seuil, tau1, tau2, mu, singu,&
                        rr, lact, ddls, ddlm, mmat)
!
! --- CALCUL DE B_U
!
            call xmmab4(ndim, nno, nnos, ffp, jac,&
                        ptknp, nfh, seuil, mu, singu,&
                        rr, coeffp, ddls, ddlm, mmat)
!
! --- CALCUL DE F - CAS GLISSANT OU PENALISATION (NON SEULE)
!
            call xmmbp5(ndim, nnol, pla, ffc, jac,&
                        coeffp, seuil, tau1, tau2, mu,&
                        lact, mmat)
        else
            call assert(algofr.eq.0)
        endif
!
    else
        call assert(indco.eq.0 .or. indco.eq.1)
    endif
end subroutine
