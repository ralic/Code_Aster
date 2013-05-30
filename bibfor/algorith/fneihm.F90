subroutine fneihm(fnoevo, deltat, perman, nno1, nno2,&
                  npi, npg, wref, iu, ip,&
                  ipf, iq, vff1, vff2, dffr2,&
                  geom, ang, congem, r, vectu,&
                  mecani, press1, press2, tempe, dimdef,&
                  dimcon, dimuel, ndim, axi)
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
    implicit none
    include 'asterfort/fonoei.h'
    include 'asterfort/matthm.h'
    logical :: fnoevo, perman, axi
    integer :: dimdef, dimcon, nno1, nno2
    integer :: dimuel, ndim
    integer :: npi, npg, mecani(8), press1(9), press2(9)
    integer :: tempe(5), yamec, yap1, yap2, yate
    integer :: addeme, addep1, addep2, addete
    integer :: iu(3, 18), ip(2, 9), ipf(2, 2, 9), iq(2, 2, 9)
    real(kind=8) :: deltat, geom(ndim, nno2), dffr2(ndim-1, nno2, npi)
    real(kind=8) :: congem(dimcon, npi), vff1(nno1, npi), vff2(nno2, npi)
    real(kind=8) :: vectu(dimuel), r(dimdef), ang(24), wref(npg)
!
! ======================================================================
!     BUT:  CALCUL  DE L'OPTION FORC_NODA POUR JOINT AVEC COUPLAGE HM
!  SI FNOEVO = VRAI
!  C EST QUE L'ON APPELLE DEPUIS STAT NON LINE  :
!  ET ALORS LES TERMES DEPENDANT DE DELTAT SONT EVALUES
!
!  SI  FNOEVO = FAUX
!  C EST QUE L'ON APPELLE DEPUIS CALCNO  :
!  ET ALORS LES TERMES DEPENDANT DE DELTAT NE SONT PAS EVALUES
! ======================================================================
! IN
! ======================================================================
! AXI       AXISYMETRIQUE ?
! TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
! MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! NNO       NB DE NOEUDS DE L'ELEMENT
! NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
! NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
! NDDLS     NB DE DDL SUR LES SOMMETS
! NDDLM     NB DE DDL SUR LES MILIEUX
! NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
!
! NDIM      DIMENSION DE L'ESPACE
! DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
! DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
! DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
! IVF       FONCTIONS DE FORMES QUADRATIQUES
! IVF2      FONCTIONS DE FORMES LINEAIRES
! ======================================================================
! OUT
! ======================================================================
! OUT R       : TABLEAU DES RESIDUS
! OUT VECTU   : FORCES NODALES
! ======================================================================
    integer :: adcome, adcp11, adcp12, adcp21, adcp22, adcote
    integer :: addlh1
    integer :: adcop1, adcop2, nbpha1, nbpha2
    integer :: kpi, i, n
    real(kind=8) :: dt, wi, q(dimdef, dimuel)
!
! ======================================================================
! --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU --------------
! ======================================================================
!
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
    if (perman) then
        dt = 1.d0
    else
        dt = deltat
    endif
!
! ======================================================================
! --- INITIALISATION DE VECTU ------------------------------------------
! ======================================================================
    do 1 i = 1, dimuel
        vectu(i)=0.d0
 1  end do
! ======================================================================
! --- CALCUL POUR CHAQUE POINT DE GAUSS : BOUCLE SUR KPG ---------------
! ======================================================================
    do 10 kpi = 1, npg
!
! ======================================================================
! --- INITIALISATION DE R ----------------------------------------------
! ======================================================================
        do 22 i = 1, dimdef
            r(i)=0.d0
22      continue
!
! ======================================================================
! --- CALCUL DE LA MATRICE Q AU POINT DE GAUSS -------------------------
! ======================================================================
!
        call matthm(ndim, axi, nno1, nno2, dimuel,&
                    dimdef, iu, ip, ipf, iq,&
                    yap1, yap2, yate, addep1, addep2,&
                    addlh1, vff1(1, kpi), vff2(1, kpi), dffr2(1, 1, kpi), wref(kpi),&
                    geom, ang, wi, q)
!
! ======================================================================
        call fonoei(ndim, dt, fnoevo, dimdef, dimcon,&
                    yamec, yap1, yap2, yate, addeme,&
                    addep1, addep2, addete, addlh1, adcome,&
                    adcp11, adcp12, adcp21, adcp22, adcote,&
                    adcop1, adcop2, nbpha1, nbpha2, congem(1, kpi),&
                    r)
!
! ======================================================================
! --- CONTRIBUTION DU POINT D'INTEGRATION KPI AU RESIDU ----------------
! ======================================================================
!
        do 117 i = 1, dimuel
            do 118 n = 1, dimdef
                vectu(i)=vectu(i)+q(n,i)*r(n)*wi
118          continue
117      continue
!
! ======================================================================
10  end do
! ======================================================================
end subroutine
