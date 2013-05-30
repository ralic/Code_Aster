subroutine mmmtff(phasep, ndim, nbcps, nnl, wpg,&
                  ffl, jacobi, tau1, tau2, rese,&
                  nrese, lambda, coefaf, coefff, matrff)
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
    include 'asterfort/matini.h'
    include 'asterfort/mmmmpb.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/vecini.h'
    character(len=9) :: phasep
    integer :: ndim, nnl, nbcps
    real(kind=8) :: wpg, ffl(9), jacobi
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: lambda
    real(kind=8) :: coefaf, coefff
    real(kind=8) :: matrff(18, 18)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE LAGR_F/LAGR_F
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
! IN  NBCPS  : NB DE DDL DE LAGRANGE
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : RACINE DE LA NORME DE RESE
! IN  LAMBDA : LAGRANGIEN DE CONTACT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  COEFAF : COEF_AUGM_FROT
! OUT MATRFF : MATRICE ELEMENTAIRE LAGR_F/LAGR_F
!
! ----------------------------------------------------------------------
!
    integer :: i, j, k, l, ii, jj, idim, nbcpf
    real(kind=8) :: tt(3, 3)
    real(kind=8) :: h1(3), h2(3), matprb(3, 3)
    real(kind=8) :: r(2, 2)
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    call matini(3, 3, 0.d0, tt)
    call matini(2, 2, 0.d0, r)
    call vecini(3, 0.d0, h1)
    call vecini(3, 0.d0, h2)
    nbcpf = nbcps - 1
!
! --- MATRICE DE CHANGEMENT DE REPERE [TT] = [T]t*[T]
!
    do 301 k = 1, ndim
        tt(1,1) = tau1(k)*tau1(k) + tt(1,1)
        tt(1,2) = tau1(k)*tau2(k) + tt(1,2)
        tt(2,1) = tau2(k)*tau1(k) + tt(2,1)
        tt(2,2) = tau2(k)*tau2(k) + tt(2,2)
301  end do
!
! --- MATRICE DE PROJECTION SUR LA BOULE UNITE
!
    if (phasep(1:4) .eq. 'GLIS') then
        call mmmmpb(rese, nrese, ndim, matprb)
    endif
!
! --- CALCUL DES TERMES
!
    if (phasep(1:4) .eq. 'SANS') then
        do 284 i = 1, nnl
            do 283 j = 1, nnl
                do 282 l = 1, nbcpf
                    do 281 k = 1, nbcpf
                        ii = (ndim-1)*(i-1)+l
                        jj = (ndim-1)*(j-1)+k
                        matrff(ii,jj) = matrff(ii,jj)+ wpg*ffl(i)*ffl( j)*jacobi*tt(l,k)
281                  continue
282              continue
283          continue
284      continue
    else if (phasep.eq.'GLIS') then
        call pmavec('ZERO', 3, matprb, tau1, h1)
        call pmavec('ZERO', 3, matprb, tau2, h2)
!      --- MATRICE [R] = [T]t*[T]-[T]t*[H]
        do 857 idim = 1, ndim
            r(1,1) = (tau1(idim)-h1(idim))*tau1(idim) + r(1,1)
            r(1,2) = (tau2(idim)-h2(idim))*tau1(idim) + r(1,2)
            r(2,1) = (tau1(idim)-h1(idim))*tau2(idim) + r(2,1)
            r(2,2) = (tau2(idim)-h2(idim))*tau2(idim) + r(2,2)
857      continue
!
        do 24 i = 1, nnl
            do 23 j = 1, nnl
                do 22 l = 1, nbcpf
                    do 21 k = 1, nbcpf
                        ii = (ndim-1)*(i-1)+l
                        jj = (ndim-1)*(j-1)+k
                        matrff(ii,jj) = matrff(ii,jj)+ wpg*ffl(i)*ffl( j)*jacobi* coefff*lambda*r&
                                        &(l,k) / coefaf
21                  continue
22              continue
23          continue
24      continue
    else if (phasep.eq.'GLIS_PENA') then
        do 384 i = 1, nnl
            do 383 j = 1, nnl
                do 382 l = 1, nbcpf
                    do 381 k = 1, nbcpf
                        ii = (ndim-1)*(i-1)+l
                        jj = (ndim-1)*(j-1)+k
                        matrff(ii,jj) = matrff(ii,jj)+ wpg*ffl(i)*ffl( j)*jacobi*tt(l,k) *coefff*&
                                        &lambda/coefaf
381                  continue
382              continue
383          continue
384      continue
!
    else if (phasep.eq.'ADHE_PENA') then
        do 484 i = 1, nnl
            do 483 j = 1, nnl
                do 482 l = 1, nbcpf
                    do 481 k = 1, nbcpf
                        ii = (ndim-1)*(i-1)+l
                        jj = (ndim-1)*(j-1)+k
                        matrff(ii,jj) = matrff(ii,jj)+ wpg*ffl(i)*ffl( j)*jacobi*tt(l,k) *coefff*&
                                        &lambda/coefaf
481                  continue
482              continue
483          continue
484      continue
    else
        call assert(.false.)
    endif
!
end subroutine
