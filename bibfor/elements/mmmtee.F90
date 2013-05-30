subroutine mmmtee(phasep, ndim, nne, mprojn, mprojt,&
                  wpg, ffe, jacobi, coefac, coefaf,&
                  coefff, rese, nrese, lambda, matree)
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
    include 'asterfort/matini.h'
    include 'asterfort/mmmmpb.h'
    include 'asterfort/pmat.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/vecini.h'
    character(len=9) :: phasep
    integer :: ndim, nne
    real(kind=8) :: mprojn(3, 3), mprojt(3, 3)
    real(kind=8) :: wpg, ffe(9), jacobi
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: coefac, coefaf
    real(kind=8) :: lambda, coefff
    real(kind=8) :: matree(27, 27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE DEPL_ESCL/DEPL_ESCL
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
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  MPROJN : MATRICE DE PROJECTION NORMALE [Pn]
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  COEFAC : COEF_AUGM_CONT
! IN  COEFAF : COEF_AUGM_FROT
! IN  LAMBDA : LAGRANGIEN DE CONTACT
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! OUT MATREE : MATRICE ELEMENTAIRE DEPL_E/DEPL_E
!
! ----------------------------------------------------------------------
!
    integer :: i, j, k, ii, jj, idim
    real(kind=8) :: g(3, 3), e(3, 3), d(3, 3), matprb(3, 3)
    real(kind=8) :: c1(3), c2(3), c3(3), d1(3), d2(3), d3(3)
    integer :: inoe1, inoe2, idim1, idim2
!
! ----------------------------------------------------------------------
!
    call matini(3, 3, 0.d0, e)
    call matini(3, 3, 0.d0, d)
    call matini(3, 3, 0.d0, g)
    call vecini(3, 0.d0, d1)
    call vecini(3, 0.d0, d2)
    call vecini(3, 0.d0, d3)
    call vecini(3, 0.d0, c1)
    call vecini(3, 0.d0, c2)
    call vecini(3, 0.d0, c3)
!
    do 3 k = 1, 3
        c1(k) = mprojt(k,1)
        c2(k) = mprojt(k,2)
        c3(k) = mprojt(k,3)
 3  end do
!
! --- PRODUIT [E] = [Pt]x[Pt]
!
    call pmat(3, mprojt, mprojt, e)
!
! --- MATRICE DE PROJECTION SUR LA BOULE UNITE
!
    if (phasep(1:4) .eq. 'GLIS') then
        call mmmmpb(rese, nrese, ndim, matprb)
    endif
!
! --- VECTEUR PROJ. BOULE SUR PLAN TGT
!
    if (phasep(1:4) .eq. 'GLIS') then
        call pmavec('ZERO', 3, matprb, c1, d1)
        call pmavec('ZERO', 3, matprb, c2, d2)
        call pmavec('ZERO', 3, matprb, c3, d3)
!
! ----- MATRICE [G] = [{D1}{D2}{D3}]
!
        do 416 idim = 1, 3
            g(idim,1) = d1(idim)
            g(idim,2) = d2(idim)
            g(idim,3) = d3(idim)
416      continue
!
! ----- MATRICE [D] = [Pt]*[G]t
!
        do 423 i = 1, ndim
            do 424 j = 1, ndim
                do 425 k = 1, ndim
                    d(i,j) = g(k,i)*mprojt(k,j) + d(i,j)
425              continue
424          continue
423      continue
    endif
!
! --- CALCUL DES TERMES
!
    if (phasep(1:4) .eq. 'CONT') then
        if (phasep(6:9) .eq. 'PENA') then
            do 160 inoe1 = 1, nne
                do 150 inoe2 = 1, nne
                    do 140 idim2 = 1, ndim
                        do 130 idim1 = 1, ndim
                            ii = ndim*(inoe1-1)+idim1
                            jj = ndim*(inoe2-1)+idim2
                            matree(ii,jj) = matree(ii,jj) + coefac* wpg*jacobi* ffe(inoe1)*mprojn&
                                            &(idim1,idim2) *ffe(inoe2)
130                      continue
140                  continue
150              continue
160          continue
        else
            do 161 inoe1 = 1, nne
                do 151 inoe2 = 1, nne
                    do 141 idim2 = 1, ndim
                        do 131 idim1 = 1, ndim
                            ii = ndim*(inoe1-1)+idim1
                            jj = ndim*(inoe2-1)+idim2
                            matree(ii,jj) = matree(ii,jj) + coefac* wpg*jacobi* ffe(inoe1)*mprojn&
                                            &(idim1,idim2) *ffe(inoe2)
131                      continue
141                  continue
151              continue
161          continue
        endif
    else if (phasep(1:4).eq.'ADHE') then
        if (phasep(6:9) .eq. 'PENA') then
            do 167 inoe1 = 1, nne
                do 157 inoe2 = 1, nne
                    do 147 idim1 = 1, ndim
                        do 137 idim2 = 1, ndim
                            ii = ndim*(inoe1-1)+idim1
                            jj = ndim*(inoe2-1)+idim2
                            matree(ii,jj) = matree(ii,jj) - coefaf* coefff*lambda* wpg*jacobi* ff&
                                            &e(inoe1)*e( idim1,idim2)*ffe(inoe2)
137                      continue
147                  continue
157              continue
167          continue
        else
            do 168 inoe1 = 1, nne
                do 158 inoe2 = 1, nne
                    do 148 idim1 = 1, ndim
                        do 138 idim2 = 1, ndim
                            ii = ndim*(inoe1-1)+idim1
                            jj = ndim*(inoe2-1)+idim2
                            matree(ii,jj) = matree(ii,jj) - coefaf* coefff*lambda* wpg*jacobi* ff&
                                            &e(inoe1)*e( idim1,idim2)*ffe(inoe2)
138                      continue
148                  continue
158              continue
168          continue
        endif
    else if (phasep(1:4).eq.'GLIS') then
        if (phasep(6:9) .eq. 'PENA') then
            do 462 inoe1 = 1, nne
                do 452 inoe2 = 1, nne
                    do 442 idim2 = 1, ndim
                        do 432 idim1 = 1, ndim
                            ii = ndim*(inoe1-1)+idim1
                            jj = ndim*(inoe2-1)+idim2
                            matree(ii,jj) = matree(ii,jj) - coefaf* coefff*lambda* wpg*jacobi* ff&
                                            &e(inoe1)*d( idim1,idim2)*ffe(inoe2)
432                      continue
442                  continue
452              continue
462          continue
        else
            do 362 inoe1 = 1, nne
                do 352 inoe2 = 1, nne
                    do 342 idim2 = 1, ndim
                        do 332 idim1 = 1, ndim
                            ii = ndim*(inoe1-1)+idim1
                            jj = ndim*(inoe2-1)+idim2
                            matree(ii,jj) = matree(ii,jj) - coefaf* coefff*lambda* wpg*jacobi* ff&
                                            &e(inoe1)*d( idim1,idim2)*ffe(inoe2)
332                      continue
342                  continue
352              continue
362          continue
        endif
    endif
!
end subroutine
