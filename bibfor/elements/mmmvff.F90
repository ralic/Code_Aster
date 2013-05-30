subroutine mmmvff(phasep, ndim, nnl, nbcps, wpg,&
                  ffl, tau1, tau2, jacobi, coefaf,&
                  dlagrf, rese, lambda, coefff, dvite,&
                  mprojt, vectff)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/normev.h'
    character(len=9) :: phasep
    integer :: ndim, nnl, nbcps
    real(kind=8) :: wpg, ffl(9), jacobi, dlagrf(2)
    real(kind=8) :: tau1(3), tau2(3), rese(3)
    real(kind=8) :: coefaf
    real(kind=8) :: lambda, coefff
    real(kind=8) :: vectff(18)
    real(kind=8) :: dvite(3), mprojt(3, 3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DU VECTEUR LAGR_C
!
! ----------------------------------------------------------------------
!
!
! IN  PHASEP : PHASE DE CALCUL
!              'SANS' - PAS DE CONTACT
!              'ADHE' - CONTACT ADHERENT
!              'GLIS' - CONTACT GLISSANT
!              'SANS_PENA' - PENALISATION - PAS DE CONTACT
!              'ADHE_PENA' - PENALISATION - CONTACT ADHERENT
!              'GLIS_PENA' - PENALISATION - CONTACT GLISSANT
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNL    : NOMBRE DE NOEUDS LAGRANGE
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  NBCPS  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_C+LAGR_F
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  COEFAF : COEF_AUGM_FROT
! IN  DLAGRF : INCREMENT DEPDEL DU LAGRANGIEN DE FROTTEMENT
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  DVITE  : SAUT D'INCREMENT DES DEPLACEMENTS A L'INTERFACE
!               [[DELTA X]]
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE
! OUT VECTFF : VECTEUR ELEMENTAIRE LAGR_F
!
! ----------------------------------------------------------------------
!
    integer :: i, k, l, ii, nbcpf
    real(kind=8) :: tt(2)
    real(kind=8) :: nrese, inter(2)
    real(kind=8) :: dvitet(3)
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    do 306 i = 1, 3
        dvitet(i) = 0.0d0
306  end do
    do 305 i = 1, 2
        tt(i) = 0.d0
        inter(i) = 0.d0
305  end do
    nbcpf = nbcps-1
!
! --- MATRICE DE CHANGEMENT DE REPERE DES LAGR. DE FROTTEMENT
!
    if (ndim .eq. 2) then
        do 301 k = 1, ndim
            tt(1) = tau1(k)*tau1(k) +tt(1)
301      continue
        tt(1) = dlagrf(1)*tt(1)
        tt(2) = 0.d0
!
    else if (ndim.eq.3) then
        do 31 k = 1, ndim
            tt(1) = (dlagrf(1)*tau1(k)+dlagrf(2)*tau2(k))*tau1(k)+tt( 1)
31      continue
        do 32 k = 1, ndim
            tt(2) = (dlagrf(1)*tau1(k)+dlagrf(2)*tau2(k))*tau2(k)+tt( 2)
32      continue
    else
        call assert(.false.)
    endif
!
! --- QUANTITES POUR LE CAS ADHERENT
!
    if (phasep(1:4) .eq. 'ADHE') then
!
! --- PROJECTION DU SAUT SUR LE PLAN TANGENT
!
        do 21 i = 1, ndim
            do 22 k = 1, ndim
                dvitet(i) = mprojt(i,k)*dvite(k)+dvitet(i)
22          continue
21      continue
!
        if (ndim .eq. 2) then
            do 140 i = 1, 2
                inter(1)= dvitet(i)*tau1(i)+inter(1)
140          continue
        else if (ndim.eq.3) then
            do 150 i = 1, 3
                inter(1)= dvitet(i)*tau1(i)+inter(1)
                inter(2)= dvitet(i)*tau2(i)+inter(2)
150          continue
        endif
    endif
!
! --- QUANTITES POUR LE CAS GLISSANT
!
    if (phasep(1:4) .eq. 'GLIS') then
!
        call normev(rese, nrese)
        if (ndim .eq. 2) then
            do 228 i = 1, 2
                inter(1) = (dlagrf(1)*tau1(i)-rese(i))*tau1(i)+inter( 1)
228          continue
        else if (ndim.eq.3) then
            do 233 i = 1, 3
                inter(1)=(dlagrf(1)*tau1(i)+ dlagrf(2)*tau2(i)-rese(i)&
                )*tau1(i)+inter(1)
                inter(2)=(dlagrf(1)*tau1(i)+ dlagrf(2)*tau2(i)-rese(i)&
                )*tau2(i)+inter(2)
233          continue
        else
            call assert(.false.)
        endif
!
    endif
!
! --- CALCUL DU VECTEUR
!
    if (phasep .eq. 'SANS') then
        do 101 i = 1, nnl
            do 102 l = 1, nbcpf
                ii = (i-1)*nbcpf+l
                vectff(ii) = vectff(ii)+ wpg*ffl(i)*jacobi* tt(l)
102          continue
101      continue
    else if (phasep.eq.'SANS_PENA') then
        do 201 i = 1, nnl
            do 202 l = 1, nbcpf
                ii = (i-1)*nbcpf+l
                vectff(ii) = vectff(ii)- wpg*ffl(i)*jacobi* tt(l)/ coefaf
202          continue
201      continue
    else if (phasep.eq.'ADHE') then
        do 53 i = 1, nnl
            do 54 l = 1, nbcpf
                ii = (i-1)*nbcpf+l
                vectff(ii) = vectff(ii)- wpg*ffl(i)*jacobi* coefff* lambda*inter(l)
54          continue
53      continue
    else if (phasep.eq.'GLIS') then
        do 63 i = 1, nnl
            do 64 l = 1, nbcpf
                ii = (i-1)*nbcpf+l
                vectff(ii) = vectff(ii)+ wpg*ffl(i)*jacobi* coefff* lambda*inter(l)/coefaf
64          continue
63      continue
    else if (phasep.eq.'ADHE_PENA') then
        do 73 i = 1, nnl
            do 74 l = 1, nbcpf
                ii = (i-1)*nbcpf+l
                vectff(ii) = vectff(ii) + wpg*ffl(i)*jacobi*coefff* lambda*((tt(l)/coefaf)-inter(&
                             &l))
74          continue
73      continue
    else if (phasep.eq.'GLIS_PENA') then
        do 83 i = 1, nnl
            do 84 l = 1, nbcpf
                ii = (i-1)*nbcpf+l
                vectff(ii) = vectff(ii)+ wpg*ffl(i)*jacobi* coefff* lambda*inter(l)/coefaf
84          continue
83      continue
    else
        call assert(.false.)
    endif
!
end subroutine
