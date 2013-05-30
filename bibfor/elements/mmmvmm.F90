subroutine mmmvmm(phasez, ndim, nnm, norm, tau1,&
                  tau2, mprojt, wpg, ffm, jacobi,&
                  jeu, coefac, coefaf, lambda, coefff,&
                  dlagrc, dlagrf, dvite, rese, nrese,&
                  vectmm)
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
! TOLE CRP_21
!
    implicit none
    include 'asterfort/assert.h'
    character(len=*) :: phasez
    integer :: ndim, nnm
    real(kind=8) :: wpg, ffm(9), jacobi
    real(kind=8) :: dlagrc, dlagrf(2), dvite(3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: norm(3)
    real(kind=8) :: tau1(3), tau2(3), mprojt(3, 3)
    real(kind=8) :: coefac, coefaf, jeu
    real(kind=8) :: lambda, coefff
    real(kind=8) :: vectmm(27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DU VECTEUR DEPL_MAIT
!
! ----------------------------------------------------------------------
!
!
! IN  PHASEP : PHASE DE CALCUL
!              'CONT'      - CONTACT
!              'CONT_PENA' - CONTACT PENALISE
!              'ADHE'      - ADHERENCE
!              'ADHE_PENA' - ADHERENCE PENALISE
!              'GLIS'      - GLISSEMENT
!              'GLIS_PENA' - GLISSEMENT PENALISE
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNM    : NOMBRE DE NOEUDS MAITRES
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFM    : FONCTIONS DE FORMES DEPL_MAIT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  JEU    : VALEUR DU JEU
! IN  NORM   : NORMALE
! IN  COEFAC : COEF_AUGM_CONT
! IN  COEFAF : COEF_AUGM_FROT
! IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! IN  DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! IN  DVITE  : SAUT DE "VITESSE" [[DELTA X]]
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE
! OUT VECTMM : VECTEUR ELEMENTAIRE DEPL_MAIT
!
! ----------------------------------------------------------------------
!
    integer :: inom, idim, ii, i, j, k
    real(kind=8) :: dlagft(3), plagft(3), prese(3)
    real(kind=8) :: dvitet(3), pdvitt(3)
    character(len=9) :: phasep
!
! ----------------------------------------------------------------------
!
    phasep = phasez
    do 14 i = 1, 3
        plagft(i) = 0.d0
        dlagft(i) = 0.d0
        prese (i) = 0.d0
        dvitet(i) = 0.d0
        pdvitt(i) = 0.d0
14  end do
!
! --- PROJECTION DU LAGRANGE DE FROTTEMENT SUR LE PLAN TANGENT
!
    do 123 i = 1, ndim
        dlagft(i) = dlagrf(1)*tau1(i)+dlagrf(2)*tau2(i)
123  end do
!
! --- PRODUIT LAGR. FROTTEMENT. PAR MATRICE P
!
    do 221 i = 1, ndim
        do 222 j = 1, ndim
            plagft(i) = mprojt(i,j)*dlagft(j)+plagft(i)
222      continue
221  end do
!
! --- PRODUIT SEMI MULT. LAGR. FROTTEMENT. PAR MATRICE P
!
    if (phasep(1:4) .eq. 'GLIS') then
        do 228 i = 1, ndim
            do 229 j = 1, ndim
                prese(i) = mprojt(i,j)*rese(j)/nrese+prese(i)
229          continue
228      continue
    endif
!
! --- PROJECTION DU SAUT SUR LE PLAN TANGENT
!
    do 21 i = 1, ndim
        do 22 k = 1, ndim
            dvitet(i) = mprojt(i,k)*dvite(k)+dvitet(i)
22      continue
21  end do
!
! --- PRODUIT SAUT PAR MATRICE P
!
    do 721 i = 1, ndim
        do 722 j = 1, ndim
            pdvitt(i) = mprojt(i,j)*dvitet(j)+pdvitt(i)
722      continue
721  end do
!
! --- CALCUL DES TERMES
!
    if (phasep(1:4) .eq. 'SANS') then
! --- PAS DE CONTRIBUTION
    else if (phasep(1:4).eq.'CONT') then
        if (phasep(6:9) .eq. 'PENA') then
            do 75 inom = 1, nnm
                do 65 idim = 1, ndim
                    ii = ndim*(inom-1)+idim
                    vectmm(ii) = vectmm(ii)- wpg*ffm(inom)*jacobi* norm(idim)* jeu*coefac
65              continue
75          continue
        else
            do 70 inom = 1, nnm
                do 60 idim = 1, ndim
                    ii = ndim*(inom-1)+idim
                    vectmm(ii) = vectmm(ii)+ wpg*ffm(inom)*jacobi* norm(idim)* (dlagrc-jeu*coefac&
                                 &)
60              continue
70          continue
        endif
!
    else if (phasep(1:4).eq.'GLIS') then
        do 74 inom = 1, nnm
            do 64 idim = 1, ndim
                ii = ndim*(inom-1)+idim
                vectmm(ii) = vectmm(ii)+ wpg*ffm(inom)*jacobi*prese( idim)* lambda*coefff
64          continue
74      continue
!
    else if (phasep(1:4).eq.'ADHE') then
        if (phasep(6:9) .eq. 'PENA') then
            do 77 inom = 1, nnm
                do 67 idim = 1, ndim
                    ii = ndim*(inom-1)+idim
                    vectmm(ii) = vectmm(ii)+ wpg*ffm(inom)*jacobi* pdvitt(idim)* lambda*coefff*co&
                                 &efaf
67              continue
77          continue
        else
            do 73 inom = 1, nnm
                do 63 idim = 1, ndim
                    ii = ndim*(inom-1)+idim
                    vectmm(ii) = vectmm(ii)+ wpg*ffm(inom)*jacobi* lambda*coefff* (plagft(idim)+p&
                                 &dvitt(idim)*coefaf)
63              continue
73          continue
        endif
    else
        call assert(.false.)
    endif
!
end subroutine
