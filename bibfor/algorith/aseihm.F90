subroutine aseihm(option, axi, ndim, nno1, nno2,&
                  npi, npg, dimuel, dimdef, dimcon,&
                  nbvari, imate, iu, ip, ipf,&
                  iq, mecani, press1, press2, tempe,&
                  vff1, vff2, dffr2, instam, instap,&
                  deplm, deplp, sigm, sigp, varim,&
                  varip, nomail, wref, geom, ang,&
                  compor, perman, crit, vectu, matuu,&
                  retcom)
!
    implicit none
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! TOLE CRP_21 CRS_1404
! ======================================================================
! =====================================================================
!......................................................................
!
!     BUT:  CALCUL DU VECTEUR FORCES INTERNES ELEMENTAIRE, DES
!           CONTRAINTES GENERALISEES, DES VARIABLES INTERNES
!           ET/OU DE L'OPERATEUR TANGENT ELEMENTAIRE
!......................................................................
! =====================================================================
! IN OPTION  : OPTION DE CALCUL
! IN AXI     : AXISYMETRIQUE ?
! IN NDIM    : DIMENSION DE L'ESPACE
! IN NNO1    : NOMBRE DE NOEUDS DE LA FAMILLE 1
! IN NNO2    : NOMBRE DE NOEUDS DE LA FAMILLE 2
! IN NPI     : NOMBRE DE POINTS D'INTEGRATION
! IN NPG     : NOMBRE DE POINTS DE GAUSS
! IN DIMUEL  : NOMBRE DE DDL
! IN DIMDEF  : DIMENSION DU VECTEUR DEFORMATIONS GENERALISEES
! IN DIMCON  : DIMENSION DU VECTEUR CONTRAINTES GENERALISEES
! IN IMATE   : MATERIAU CODE
! IN IU      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
! IN IP      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION MILIEU
! IN IPF     : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION FACES
! IN IQ      : DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE HYDRO
! IN MECANI  : INFOS MECANIQUE
! IN PRESS1  : INFOS CONSTITUANT 1
! IN PRESS2  : INFOS CONSTITUANT 2
! IN TEMPE   : INFOS TEMPERATURE
! IN VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE 1)
! IN VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE 2)
! IN DFFR2   : DERIVEES DES FONCTIONS DE FORME (FAMILLE 2)
! IN INSTAM  : INSTANT PRECEDENT
! IN INSTAP  : INSTANT ACTUEL
! IN DEPLM   : DDL A L'INSTANT PRECEDENT
! IN DEPLP   : DDL A L'INSTANT ACTUEL
! IN SIGM    : CONTRAINTES GENERALISEES AUX POINTS D'INTEGRATION
! IN VARIM   : VARIABLES INTERNES AUX POINTS D'INTEGRATION
! IN NOMAIL  : NUMERO DE LA MAILLE
! IN WREF    : POIDS DE REFERENCE DES POINTS D'INTEGRATIONS
! IN GEOM    : COORDONNEES DES NOEUDS (FAMILLE 1)
! IN ANG     : ANGLES D'EULER NODAUX (FAMILLE 1)
! IN COMPOR  : COMPORTEMENT
! IN PERMAN  : REGIME PERMANENT ?
! IN CRIT    : CRITERES DE CONVERGENCE LOCAUX
! =====================================================================
! OUT RETCOM : CODE RETOUR LOI DE COMPORTEMENT
! OUT VECTU  : VECTEUR FORCE INTERNE ELEMENTAIRE
! OUT MATUU  : OPERATEUR TANGENT ELEMENTAIRE
! OUT SIGP   : CONTRAINTES GENERALISEES AU TEMPS PLUS AUX POINTS
!              D'INTEGRATION
! OUT VARIP  : VARIABLES INTERNES AU TEMPS PLUS AUX POINTS D'INTEGRATION
! --- POUR L'HYDRAULIQUE : VAR. INT. 1 : RHO_LIQUIDE - RHO_0
! --- POUR LE COUPLAGE   : VAR. INT. 1 : PHI - PHI_0
! ---                    : VAR. INT. 2 : PVP - PVP_0 SI VAPEUR
! ---                    : VAR. INT. 3 : SATURATION SI LOI NON SATUREE
!                        : VAR. INT. 4 : OUVH
! --- POUR LA MECANIQUE  : VAR. INT. 1 : TLINT
!
!......................................................................
!
!
!
! - VARIABLES ENTREE
    include 'asterfort/coeihm.h'
    include 'asterfort/matthm.h'
    integer :: ndim, nno1, nno2, npi, npg, dimuel, dimdef, dimcon, nbvari
    integer :: mecani(8), press1(9), press2(9), tempe(5)
    integer :: imate
    integer :: iu(3, 18), ip(2, 9), ipf(2, 2, 9), iq(2, 2, 9)
    real(kind=8) :: vff1(nno1, npi), vff2(nno2, npi), dffr2(ndim-1, nno2, npi)
    real(kind=8) :: wref(npi), ang(24), crit(*)
    real(kind=8) :: instam, instap, deplm(dimuel), deplp(dimuel)
    real(kind=8) :: geom(ndim, nno2)
    real(kind=8) :: sigm(dimcon, npi), varim(nbvari, npi)
    character(len=8) :: nomail
    character(len=16) :: option, compor(*)
    logical :: axi, perman
!
! - VARIABLES SORTIE
    integer :: retcom
    real(kind=8) :: vectu(dimuel), varip(nbvari, npi), sigp(dimcon, npi)
    real(kind=8) :: matuu(dimuel*dimuel)
!
! - VARIABLES LOCALES
    integer :: yamec, yap1, yap2, yate, addeme, adcome, addep1, addep2, addete
    integer :: adcp11, adcp12, adcp21, adcp22, adcote, adcop1, adcop2
    integer :: i, j, m, k, km, kpi, nbpha1, nbpha2, addlh1
    real(kind=8) :: q(dimdef, dimuel), res(dimdef), drde(dimdef, dimdef), wi
    real(kind=8) :: defgem(dimdef), defgep(dimdef), matri
    logical :: resi, rigi
!
!
! =====================================================================
! --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU ET OPTION ---
! =====================================================================
    yamec = mecani(1)
    addeme = mecani(2)
    adcome = mecani(3)
    yap1 = press1(1)
    nbpha1 = press1(2)
    addep1 = press1(3)
    addlh1 = press1(4)
    adcp11 = press1(5)
    adcp12 = press1(6)
    adcop1 = press1(7)
    yap2 = press2(1)
    nbpha2 = press2(2)
    addep2 = press2(3)
    adcp21 = press2(4)
    adcp22 = press2(5)
    adcop2 = press2(6)
    yate = tempe(1)
    addete = tempe(2)
    adcote = tempe(3)
!
    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    rigi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RIGI'
!
! ======================================================================
! --- INITIALISATION DE VECTU, MATUU A 0 SUIVANT OPTION ----------------
! ======================================================================
    if (resi) then
        do 1 i = 1, dimuel
            vectu(i)=0.d0
 1      continue
    endif
!
    if (rigi) then
        do 3 i = 1, dimuel*dimuel
            matuu(i)=0.d0
 3      continue
!
    endif
!
! =====================================================================
! --- BOUCLE SUR LES POINTS D'INTEGRATION -----------------------------
! =====================================================================
!
    do 10 kpi = 1, npi
!
! =====================================================================
! --- CALCUL DE LA MATRICE DE PASSAGE DDL -> DEFORMATIONS GENERALISEES
! =====================================================================
!
        call matthm(ndim, axi, nno1, nno2, dimuel,&
                    dimdef, iu, ip, ipf, iq,&
                    yap1, yap2, yate, addep1, addep2,&
                    addlh1, vff1(1, kpi), vff2(1, kpi), dffr2(1, 1, kpi), wref(kpi),&
                    geom, ang, wi, q)
!
! =====================================================================
! --- CALCUL DES DEFORMATIONS GENERALISEES E=QU -----------------------
! =====================================================================
        do 108 i = 1, dimdef
            defgem(i)=0.d0
            defgep(i)=0.d0
            do 109 j = 1, dimuel
                defgem(i)=defgem(i)+q(i,j)*deplm(j)
                defgep(i)=defgep(i)+q(i,j)*deplp(j)
109          continue
108      continue
!
!
! =====================================================================
! --- INTEGRATION DES LOIS DE COMPORTEMENT ----------------------------
! =====================================================================
!
        call coeihm(option, perman, resi, rigi, imate,&
                    compor, crit, instam, instap, nomail,&
                    ndim, dimdef, dimcon, nbvari, yamec,&
                    yap1, yap2, yate, nbpha1, nbpha2,&
                    addeme, adcome, addep1, adcp11, adcp12,&
                    addlh1, adcop1, addep2, adcp21, adcp22,&
                    adcop2, addete, adcote, defgem, defgep,&
                    kpi, npg, npi, sigm(1, kpi), sigp(1, kpi),&
                    varim(1, kpi), varip(1, kpi), res, drde, retcom)
!
! =====================================================================
! --- CALCUL DES FORCES INTERIEURES ET DE L'OPERATEUR TANGENT ---------
! =====================================================================
!
        if (resi) then
            do 699 k = 1, dimuel
                do 700 i = 1, dimdef
                    vectu(k)=vectu(k)+wi*q(i,k)*res(i)
700              continue
699          continue
        endif
!
        if (rigi) then
            km = 1
            do 702 k = 1, dimuel
                do 703 m = 1, dimuel
                    matri=0.d0
                    do 704 i = 1, dimdef
                        do 705 j = 1, dimdef
                            matri = matri + wi*q(i,k)*drde(i,j)*q(j,m)
705                      continue
704                  continue
                    matuu(km) = matuu(km) + matri
                    km = km + 1
703              continue
702          continue
        endif
!
10  end do
!
end subroutine
