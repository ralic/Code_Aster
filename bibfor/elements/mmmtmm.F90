subroutine mmmtmm(phasep, lnewtg, ndim, nnm, mprojn,&
                  mprojt, wpg, ffm, dffm, jacobi,&
                  coefac, coefaf, coefff, rese, nrese,&
                  lambda, dlagrc, jeu, h11t1n, h12t2n,&
                  h21t1n, h22t2n, matrmm)
!
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
! person_in_charge: mickael.abbas at edf.fr
! TOLE CRP_21
!
    implicit     none
    include 'asterfort/matini.h'
    include 'asterfort/mmmmpb.h'
    include 'asterfort/pmat.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/vecini.h'
    character(len=9) :: phasep
    logical :: lnewtg
    integer :: ndim, nnm
    real(kind=8) :: mprojn(3, 3), mprojt(3, 3)
    real(kind=8) :: wpg, ffm(9), jacobi, dffm(2, 9)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: coefac, coefaf, jeu
    real(kind=8) :: lambda, coefff, dlagrc
    real(kind=8) :: h11t1n(3, 3), h12t2n(3, 3)
    real(kind=8) :: h21t1n(3, 3), h22t2n(3, 3)
    real(kind=8) :: matrmm(27, 27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE DEPL_MAIT/DEPL_MAIT
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
! IN  LNEWTG : .TRUE. SI CALCUL CONTRIBUTION GEOMETRIQUE EN NEWTON GENE.
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  MPROJN : MATRICE DE PROJECTION NORMALE [Pn]
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! IN  DFFM   : DERIVEES PREMIERES DES FONCTIONS DE FORME MAITRES
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  COEFAC : COEF_AUGM_CONT
! IN  COEFAF : COEF_AUGM_FROT
! IN  LAMBDA : LAGRANGIEN DE CONTACT
! IN  DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! IN  DJEU   : INCREMENT DEPDEL DU JEU
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  H11T1N : MATRICE
! IN  H21T1N : MATRICE
! IN  H12T2N : MATRICE
! IN  H22T2N : MATRICE
! OUT MATRMM : MATRICE ELEMENTAIRE DEPL_M/DEPL_M
!
! ----------------------------------------------------------------------
!
    integer :: i, j, k, l, ii, jj, idim
    real(kind=8) :: g(3, 3), e(3, 3), d(3, 3), matprb(3, 3), dffmt(9, 2)
    real(kind=8) :: c1(3), c2(3), c3(3), d1(3), d2(3), d3(3)
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
    do 3 idim = 1, 3
        c1(idim) = mprojt(idim,1)
        c2(idim) = mprojt(idim,2)
        c3(idim) = mprojt(idim,3)
 3  end do
!
    do 17 i = 1, 9
        do 18 j = 1, 2
            dffmt(i,j) = dffm(j,i)
18      continue
17  end do
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
            do 160 i = 1, nnm
                do 150 j = 1, nnm
                    do 140 k = 1, ndim
                        do 130 l = 1, ndim
                            ii = ndim*(i-1)+l
                            jj = ndim*(j-1)+k
                            matrmm(ii,jj) = matrmm(ii,jj) + coefac* wpg*jacobi*ffm(i)*mprojn(l,k)&
                                            &*ffm(j)
130                      continue
140                  continue
150              continue
160          continue
        else
            if (lnewtg) then
                do 761 i = 1, nnm
                    do 751 j = 1, nnm
                        do 741 k = 1, ndim
                            do 731 l = 1, ndim
                                ii = ndim*(i-1)+l
                                jj = ndim*(j-1)+k
                                matrmm(ii,jj) = matrmm(ii,jj) - wpg*jacobi*2.0d0* (dlagrc-coefac*&
                                                &jeu)* h11t1n(l,k)*ffm(i)*dffmt(j,1)- wpg*jacobi*&
                                                &2.0d0* (dlagrc-coefac*jeu)* h12t2n(l,k)*ffm(i)*d&
                                                &ffmt(j,1)- wpg*jacobi*2.0d0* (dlagrc-coefac*jeu)&
                                                &* h21t1n(l,k)*ffm(i)*dffmt(j,2)- wpg*jacobi*2.0d&
                                                &0* (dlagrc-coefac*jeu)* h22t2n(l,k)*ffm(i)*dffmt&
                                                &(j,2)
731                          continue
741                      continue
751                  continue
761              continue
            else
                do 361 i = 1, nnm
                    do 351 j = 1, nnm
                        do 341 k = 1, ndim
                            do 331 l = 1, ndim
                                ii = ndim*(i-1)+l
                                jj = ndim*(j-1)+k
                                matrmm(ii,jj) = matrmm(ii,jj) + coefac* wpg*jacobi*ffm(i)*mprojn(&
                                                &l,k)* ffm(j)
331                          continue
341                      continue
351                  continue
361              continue
            endif
        endif
    else if (phasep(1:4).eq.'ADHE') then
        if (phasep(6:9) .eq. 'PENA') then
            do 165 i = 1, nnm
                do 155 j = 1, nnm
                    do 145 k = 1, ndim
                        do 135 l = 1, ndim
                            ii = ndim*(i-1)+k
                            jj = ndim*(j-1)+l
                            matrmm(ii,jj) = matrmm(ii,jj) - coefaf* coefff*lambda* wpg*jacobi*ffm&
                                            &(i)*e(k,l)* ffm(j)
135                      continue
145                  continue
155              continue
165          continue
        else
            do 166 i = 1, nnm
                do 156 j = 1, nnm
                    do 146 k = 1, ndim
                        do 136 l = 1, ndim
                            ii = ndim*(i-1)+k
                            jj = ndim*(j-1)+l
                            matrmm(ii,jj) = matrmm(ii,jj) - coefaf* coefff*lambda* wpg*jacobi*ffm&
                                            &(i)*e(k,l)* ffm(j)
136                      continue
146                  continue
156              continue
166          continue
        endif
    else if (phasep(1:4).eq.'GLIS') then
        if (phasep(6:9) .eq. 'PENA') then
            do 465 i = 1, nnm
                do 455 j = 1, nnm
                    do 445 k = 1, ndim
                        do 435 l = 1, ndim
                            ii = ndim*(i-1)+l
                            jj = ndim*(j-1)+k
                            matrmm(ii,jj) = matrmm(ii,jj) - coefaf* coefff*lambda* wpg*jacobi*ffm&
                                            &(i)*d(l,k)* ffm(j)
435                      continue
445                  continue
455              continue
465          continue
        else
            do 365 i = 1, nnm
                do 355 j = 1, nnm
                    do 345 k = 1, ndim
                        do 335 l = 1, ndim
                            ii = ndim*(i-1)+l
                            jj = ndim*(j-1)+k
                            matrmm(ii,jj) = matrmm(ii,jj) - coefaf* coefff*lambda* wpg*jacobi*ffm&
                                            &(i)*d(l,k)* ffm(j)
335                      continue
345                  continue
355              continue
365          continue
        endif
    endif
!
end subroutine
