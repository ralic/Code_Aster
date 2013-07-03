subroutine mmmtef(phasep, ndim, nne, nnl, nbcps,&
                  wpg, jacobi, ffe, ffl, tau1,&
                  tau2, mprojt, rese, nrese, lambda,&
                  coefff, matref)
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
#include "asterfort/matini.h"
#include "asterfort/mmmmpb.h"
#include "asterfort/pmavec.h"
#include "asterfort/vecini.h"
    character(len=9) :: phasep
    integer :: ndim, nne, nnl, nbcps
    real(kind=8) :: ffe(9), ffl(9)
    real(kind=8) :: wpg, jacobi
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: rese(3), nrese
    real(kind=8) :: mprojt(3, 3)
    real(kind=8) :: lambda
    real(kind=8) :: coefff
    real(kind=8) :: matref(27, 18)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE DEPL_ESCL/LAGR_F
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
! IN  NBCPS  : NB DE DDL DE LAGRANGE
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! IN  MPROJT : MATRICE DE PROJECTION TANGENTE [Pt]
! IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
! IN  FFL    : FONCTIONS DE FORMES LAGR.
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : RACINE DE LA NORME DE RESE
! IN  LAMBDA : LAGRANGIEN DE CONTACT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! OUT MATREF : MATRICE ELEMENTAIRE DEPL_E/LAGR_F
!
! ----------------------------------------------------------------------
!
    integer :: inof, inoe, icmp, idim, i, j, k, ii, jj, nbcpf
    real(kind=8) :: a(2, 3), b(2, 3), h(3, 2), matprb(3, 3)
    real(kind=8) :: h1(3), h2(3)
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    call matini(2, 3, 0.d0, a)
    call matini(2, 3, 0.d0, b)
    call matini(3, 2, 0.d0, h)
    call vecini(3, 0.d0, h1)
    call vecini(3, 0.d0, h2)
    nbcpf = nbcps - 1
!
! --- MATRICE [A] = [T]t*[Pt]
!
    if (phasep(1:4) .eq. 'ADHE') then
        do 4 i = 1, ndim
            do 5 k = 1, ndim
                a(1,i) = tau1(k)*mprojt(k,i) + a(1,i)
 5          continue
 4      continue
        do 6 i = 1, ndim
            do 7 k = 1, ndim
                a(2,i) = tau2(k)*mprojt(k,i) + a(2,i)
 7          continue
 6      continue
    endif
!
! --- MATRICE DE PROJECTION SUR LA BOULE UNITE
!
    if (phasep(1:4) .eq. 'GLIS') then
        call mmmmpb(rese, nrese, ndim, matprb)
    endif
!
! --- VECTEUR PROJ. BOULE SUR TANGENTES: {H1} = [K].{T1}
!
    if (phasep(1:4) .eq. 'GLIS') then
        call pmavec('ZERO', 3, matprb, tau1, h1)
        call pmavec('ZERO', 3, matprb, tau2, h2)
!
! ----- MATRICE [H] = [{H1}{H2}]
!
        do 16 idim = 1, 3
            h(idim,1) = h1(idim)
            h(idim,2) = h2(idim)
16      continue
!
! ----- MATRICE [B] = [Pt]*[H]t
!
        do 23 icmp = 1, nbcpf
            do 24 j = 1, ndim
                do 25 k = 1, ndim
                    b(icmp,j) = h(k,icmp)*mprojt(k,j)+b(icmp,j)
25              continue
24          continue
23      continue
    endif
!
! --- CALCUL DES TERMES
!
    if (phasep(1:4) .eq. 'ADHE') then
        if (phasep(6:9) .eq. 'PENA') then
!       ON NE FAIT RIEN / LA MATRICE EST NULLE
        else
            do 284 inof = 1, nnl
                do 283 inoe = 1, nne
                    do 282 icmp = 1, nbcpf
                        do 281 idim = 1, ndim
                            jj = nbcpf*(inof-1)+icmp
                            ii = ndim*(inoe-1)+idim
                            matref(ii,jj) = matref(ii,jj)- wpg*ffl( inof)*ffe(inoe)*jacobi* lambd&
                                            &a*coefff*a( icmp,idim)
!
281                      continue
282                  continue
283              continue
284          continue
        endif
    else if (phasep(1:4).eq.'GLIS') then
        if (phasep(6:9) .eq. 'PENA') then
!       ON NE FAIT RIEN / LA MATRICE EST NULLE
        else
            do 184 inof = 1, nnl
                do 183 inoe = 1, nne
                    do 182 icmp = 1, nbcpf
                        do 181 idim = 1, ndim
                            jj = nbcpf*(inof-1)+icmp
                            ii = ndim*(inoe-1)+idim
                            matref(ii,jj) = matref(ii,jj)- wpg*ffl( inof)*ffe(inoe)*jacobi* lambd&
                                            &a*coefff*b( icmp,idim)
181                      continue
182                  continue
183              continue
184          continue
        endif
    else
        call assert(.false.)
    endif
!
end subroutine
