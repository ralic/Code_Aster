subroutine mmmtmc(phasep, ndim, nnl, nnm, norm,&
                  tau1, tau2, mprojt, wpg, ffl,&
                  ffm, jacobi, coefff, coefaf, dlagrf,&
                  djeut, rese, nrese, matrmc)
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
#include "asterfort/assert.h"
#include "asterfort/vecini.h"
    character(len=9) :: phasep
    integer :: ndim, nnm, nnl
    real(kind=8) :: ffm(9), ffl(9)
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: dlagrf(2)
    real(kind=8) :: djeut(3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: coefff, coefaf
    real(kind=8) :: matrmc(27, 9)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE DEPL_MAIT/LAGR_C
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
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
! IN  NORM   : NORMALE AU POINT DE CONTACT
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  COEFAF : COEF_AUGM_FROT
! IN  DJEUT  : INCREMENT DEPDEL DU JEU TANGENT
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! OUT MATRMC : MATRICE ELEMENTAIRE DEPL_M/LAGR_C
!
! ----------------------------------------------------------------------
!
    integer :: inoc, inom, idim, jj, i, j
    real(kind=8) :: dlagft(3), pdlaft(3), pdjeut(3), prese(3)
!
! ----------------------------------------------------------------------
!
    call vecini(3, 0.d0, dlagft)
    call vecini(3, 0.d0, pdlaft)
    call vecini(3, 0.d0, pdjeut)
    call vecini(3, 0.d0, prese)
!
! --- PROJECTION DU LAGRANGE DE FROTTEMENT SUR LE PLAN TANGENT
!
    if (phasep(1:4) .eq. 'ADHE') then
        do 10 idim = 1, ndim
            dlagft(idim) = dlagrf(1)*tau1(idim)+dlagrf(2)*tau2(idim)
10      continue
    endif
!
! --- PRODUIT LAGR. FROTTEMENT PAR MATRICE [Pt]
!
    if (phasep(1:4) .eq. 'ADHE') then
        do 20 i = 1, ndim
            do 25 j = 1, ndim
                pdlaft(i) = mprojt(i,j)*dlagft(j)+pdlaft(i)
25          continue
20      continue
    endif
!
! --- PRODUIT INCREMENT DEPDEL DU JEU TANGENT PAR MATRICE [Pt]
!
    if (phasep(1:4) .eq. 'ADHE') then
        do 30 i = 1, ndim
            do 35 j = 1, ndim
                pdjeut(i) = mprojt(i,j)*djeut(j)+pdjeut(i)
35          continue
30      continue
    endif
!
! --- PRODUIT SEMI MULT. LAGR. FROTTEMENT. PAR MATRICE P
!
    if (phasep(1:4) .eq. 'GLIS') then
        do 40 i = 1, ndim
            do 45 j = 1, ndim
                prese(i) = mprojt(i,j)*(rese(j)/nrese)+prese(i)
45          continue
40      continue
    endif
!
! --- CALCUL DES TERMES
!
    if (phasep(1:4) .eq. 'CONT') then
        if (phasep(6:9) .eq. 'PENA') then
!
! ----- ON NE FAIT RIEN / LA MATRICE EST NULLE
!
        else
            do 200 inoc = 1, nnl
                do 190 inom = 1, nnm
                    do 180 idim = 1, ndim
                        jj = ndim*(inom-1)+idim
                        matrmc(jj,inoc) = matrmc(jj,inoc) + wpg*ffl( inoc)*ffm(inom)*jacobi*norm(&
                                          &idim)
180                  continue
190              continue
200          continue
        endif
    else if (phasep(1:4).eq.'ADHE') then
        if (phasep(6:9) .eq. 'PENA') then
!
! ----- ON NE FAIT RIEN / LA MATRICE EST NULLE
!
        else
            do 207 inoc = 1, nnl
                do 197 inom = 1, nnm
                    do 187 idim = 1, ndim
                        jj = ndim*(inom-1)+idim
                        matrmc(jj,inoc) = matrmc(jj,inoc) + coefff* wpg*ffl(inoc)*ffm(inom)*jacob&
                                          &i* (pdlaft(idim)+ coefaf*pdjeut(idim))
187                  continue
197              continue
207          continue
        endif
    else if (phasep(1:4).eq.'GLIS') then
        if (phasep(6:9) .eq. 'PENA') then
!
! ----- ON NE FAIT RIEN / LA MATRICE EST NULLE
!
        else
            do 205 inoc = 1, nnl
                do 195 inom = 1, nnm
                    do 185 idim = 1, ndim
                        jj = ndim*(inom-1)+idim
                        matrmc(jj,inoc) = matrmc(jj,inoc) + coefff* wpg*ffl(inoc)*ffm(inom)*jacob&
                                          &i* prese(idim)
185                  continue
195              continue
205          continue
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
