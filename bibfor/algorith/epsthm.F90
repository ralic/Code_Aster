subroutine epsthm(nddls, nddlm, nno, nnos, nnom,&
                  nmec, dimdef, dimuel, ndim, npi,&
                  ipoids, ipoid2, ivf, ivf2, idfde,&
                  idfde2, dfdi, dfdi2, b, geom,&
                  depla, mecani, press1, press2, tempe,&
                  np1, np2, axi, epsm)
!
    implicit none
!
! DECLARATION PARAMETRES D'APPEL
!
    include 'asterfort/cabthm.h'
    integer :: nddls, nddlm, nno, nnos, nnom, nmec
    integer :: dimdef, dimuel, ndim, npi
    integer :: ipoids, ipoid2, ivf, ivf2
    integer :: idfde, idfde2
    integer :: np1, np2
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    real(kind=8) :: geom(ndim, nno), depla(dimuel), epsm(6, npi)
    logical :: axi
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE  CRP_21
! ======================================================================
!     BUT : CALCUL DES DEFORMATIONS MECANIQUES AU POINT DE GAUSS
!           EN MECANIQUE DES MILIEUX POREUX AVEC COUPLAGE THM
! ======================================================================
! IN  NDDL    : NOMBRE DE DEGRES DE LIBERTE PAR NOEUD DE L'ELEMENT
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NNOS    : NOMBRE DE NOEUDS SOMMET DE L'ELEMENT
! IN  DIMDEF  : DIMENSION DU TABLEAU DES DEFORMATIONS GENERALISEES
!               AU POINT DE GAUSS
! IN  NDIM    : DIMENSION DU PROBLEME
! IN  NPI     : NOMBRE DE POINTS DE GAUSS
! IN  IPOIDS  : ADRESSE DANS ZR DU TABLEAU POIDS(IPG)
! IN  IVF     : ADRESSE DANS ZR DU TABLEAU FF(INO,IPG)
! IN  IDFDE   : ADRESSE DANS ZR DU TABLEAU DFF(IDIM,INO,IPG)
! AUX DFDI    : DERIVEE DES FONCTIONS DE FORME POUR LES VARIABLES P2
! AUX DFDI2   : DERIVEE DES FONCTIONS DE FORME POUR LES VARIABLES P1
! AUX B       : MATRICE PERMETTANT LES OPERATIONS ELEMENTAIRES
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  DEPLA   : VALEURS DES DEPLACEMENTS AUX NOEUDS
! IN  NMEC    : VARIABLE CARACTERISANT LE MILIEU
! IN  MECANI  : VARIABLE CARACTERISANT LE MILIEU
! IN  PRESS1  : VARIABLE CARACTERISANT LE MILIEU
! IN  PRESS2  : VARIABLE CARACTERISANT LE MILIEU
! IN  TEMPE   : VARIABLE CARACTERISANT LE MILIEU
! IN  NP1     : VAUT 1 SI LA MODELISATION CONTIENT UNE PRESSION
!               COMME VARIABLE
! IN  NP2     : VAUT 1 SI LA MODELISATION CONTIENT UNE DEUXIEME
!               PRESSION COMME VARIABLE
! IN  AXI     : VRAI SI ELEMENT APPELANT AXI
! OUT EPSM    : DEFORMATIONS AUX POINTS DE GAUSS SUR L'ELEMENT COURANT
! ======================================================================
!
    integer :: ipi, kpi, iaux, j
    integer :: addeme, addep1, addep2, addete
    integer :: yamec, yate, yap1, yap2
    real(kind=8) :: poids, poids2, b(dimdef, dimuel)
    real(kind=8) :: dfdi(nno, 3), dfdi2(nnos, 3)
!
!====
! 1. PREALABLES
!====
!
    yamec = mecani(1)
    addeme = mecani(2)
!
    yap1 = press1(1)
    if (yap1 .eq. 1) then
        addep1 = press1(3)
    endif
!
    yap2 = press2(1)
    if (yap2 .eq. 1) then
        addep2 = press2(3)
    endif
!
    yate = tempe(1)
    if (yate .eq. 1) then
        addete = tempe(2)
    endif
!
!====
! 2. CALCUL SELON LES PHYSIQUES
!====
!
    do 20 , kpi = 1 , npi
!
! ======================================================================
! --- CALCUL DE LA MATRICE B AU POINT DE GAUSS -------------------------
! ======================================================================
    ipi = kpi
    call cabthm(nddls, nddlm, nno, nnos, nnom,&
                dimuel, dimdef, ndim, ipi, ipoids,&
                ipoid2, ivf, ivf2, idfde, idfde2,&
                dfdi, dfdi2, geom, poids, poids2,&
                b, nmec, yamec, addeme, yap1,&
                addep1, yap2, addep2, yate, addete,&
                np1, np2, axi)
!
! ======================================================================
! --- CALCUL DES DEFORMATIONS ------------------------------------------
! ======================================================================
!
    if (yamec .eq. 1) then
!
        do 210 , iaux = 1 , 6
!
        epsm(iaux,ipi) = 0.d0
!
        do 211 , j = 1,dimuel
        epsm(iaux,ipi) = epsm(iaux,ipi)+b(iaux+ndim,j)* depla(j)
211      continue
!
210      continue
!
    endif
!
! ======================================================================
!
    20 end do
!
! ======================================================================
!
end subroutine
