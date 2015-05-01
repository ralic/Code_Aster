subroutine utpnlg(nno, ndim, pgl, matl,mate)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/r8inir.h"
    integer      :: nno, ndim
    real(kind=8) :: mate(1), pgl(ndim, ndim), matl(nno*ndim,nno*ndim)
! .....................................................................C
! .....................................................................C
!    - FONCTION REALISEE:  TRANSFORMATION DES MATRICES ELEMENTAIRES    C
!                          PASSAGE DU REPERE LOCAL AU REPERE GLOBAL    C
!    - ARGUMENTS:                                                      C
!        DONNEES:      MATE    -->  MATRICE ELEMENTAIRE                C
!                      PGL     -->  MATRICE DE PASSAGE L -> G          C
!                      NI      -->  DIMENTION DU PREMIER INDICE        C
!                      NJ      -->  DIMENTION DU DEUXIEME INDICE       C
!        SORTIE :      MATE    -->  MATRICE ELEMENTAIRE GLOBALE        C
! .....................................................................C
! .....................................................................
    integer :: i, j, k, ii,nj
    real(kind=8) :: mt(nno*ndim, nno*ndim), matg(nno*ndim, nno*ndim)
! .....................................................................
    nj=nno*ndim
! --- MATRICE DE TRANSFERT
    call r8inir(nno*nno*ndim*ndim,0.d0,mt,1)
    do 10 i = 1, ndim
        do 20 j = 1, ndim
           do 30 k = 0, nno-1
             mt(i ,j ) = pgl(i,j)
             mt(i+k*ndim,j+k*ndim) = pgl(i,j)
30         continue
20      continue
10  continue
!
! --- ON EFFECTUE : MATG() = MATE() * MT()
    do 40 k = 1, nno*ndim
        do 50 i = 1, nno*ndim
            matg(i,k) = 0.d0
            do 60 j = 1, nj
                matg(i,k) = matg(i,k) + matl(i,j) * mt(j,k)
60          continue
50      continue
40  continue
! --- MULTIPLICATION PAR LA MATRICE TRANSPOSEE DE "MT" LORSQUE
!           "MATE" EST RECTANGULAIRE DE DIMENSIONS 7X7
      do 70 i = 1, nno*ndim
          ii = nj * (i-1)
          do 80 k = 1, nno*ndim
              mate(ii+k) = 0.d0
              do 90 j = 1, nj
                  mate(ii+k) = mate(ii+k) + mt(j,i)*matg(j,k)
90            continue
80        continue
70    continue
! .....................................................................
end subroutine
