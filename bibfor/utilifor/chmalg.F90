subroutine chmalg(mate, pgl, ni, nj)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: ni, nj
    real(kind=8) :: mate(1), pgl(3, 3)
! .....................................................................C
! .....................................................................C
!    - FONCTION REALISEE:  TRANSFORMATION DES MATRICES ELEMENTAIRES    C
!                          PASSAGE DU REPERE LOCAL AU REPERE GLOBAL    C
!      ON TRAITE SEULEMENT LE CAS OU LES NOEUDS CONTIENNENT UN DES     C
!      DEUX GROUPES DE DDL SUIVANTS                                    C
!           1- DX DY DZ DRX DRY DRZ PHI                                C
!           2- PHI                                                     C
!      VOIR TE0470 OU TE0471                                           C
!                                                                      C
!    - ARGUMENTS:                                                      C
!        DONNEES:      MATE    -->  MATRICE ELEMENTAIRE                C
!                      PGL     -->  MATRICE DE PASSAGE L -> G          C
!                      NI      -->  DIMENTION DU PREMIER INDICE        C
!                      NJ      -->  DIMENTION DU DEUXIEME INDICE       C
!        SORTIE :      MATE    -->  MATRICE ELEMENTAIRE GLOBALE        C
! .....................................................................C
! .....................................................................
    integer :: i, j, k, ii
    real(kind=8) :: mt(7, 7), matg(7, 7)
! .....................................................................
    if (nj .gt. 7) then
        goto 9999
    endif
    do 1 i = 1, 7
        do 2 j = 1, 7
            mt(j,i) = 0.d0
 2      continue
 1  end do
! --- MATRICE DE TRANSFERT
    do 10 i = 1, 3
        do 20 j = 1, 3
            mt(i ,j ) = pgl(i,j)
            mt(i+3,j+3) = pgl(i,j)
20      continue
10  end do
!
    mt(7,7) = 1.d0
! --- ON EFFECTUE : MATG() = MATE() * MT()
    do 40 k = 1, nj
        do 50 i = 1, ni
            ii = nj * (i-1)
            matg(i,k) = 0.d0
            do 60 j = 1, nj
                matg(i,k) = matg(i,k) + mate(ii+j) * mt(j,k)
60          continue
50      continue
40  end do
! --- MULTIPLICATION PAR LA MATRICE TRANSPOSEE DE "MT" LORSQUE
!           "MATE" EST RECTANGULAIRE DE DIMENSIONS 7X7
    if (ni .ne. 1) then
        do 70 i = 1, ni
            ii = nj * (i-1)
            do 80 k = 1, nj
                mate(ii+k) = 0.d0
                do 90 j = 1, nj
                    mate(ii+k) = mate(ii+k) + mt(j,i)*matg(j,k)
90              continue
80          continue
70      continue
    else
        do 100 i = 1, ni
            ii = nj * (i-1)
            do 110 j = 1, nj
                mate(ii+j) = matg(i,j)
110          continue
100      continue
    endif
! .....................................................................
9999  continue
end subroutine
