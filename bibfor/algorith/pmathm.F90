subroutine pmathm(dimmat, dimdef, dimcon, dimuel, dsde,&
                  drds, ck, b, poids, matri)
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
! aslint: disable=W1306
    implicit   none
    integer :: dimdef, dimcon, dimuel, dimmat
    real(kind=8) :: dsde(dimcon, dimdef), drds(dimdef, dimcon), poids
    real(kind=8) :: ck(dimdef), b(dimdef, dimuel), matri(dimmat, dimmat)
! ======================================================================
! --- BUT : PRODUIT DES MATRICES BT,C,DRDS,D,DSDE,F,B*POIDS ------------
! ---       CONTRIBUTION DU POINT D'INTEGRATION A DF -------------------
! ---       C,F,D SONT DIAGONALES --------------------------------------
! ======================================================================
    integer :: i, j, k
    real(kind=8) :: g(dimcon, dimuel), h(dimdef, dimuel)
! ======================================================================
! --- ON FAIT LE CALCUL EN TROIS FOIS ----------------------------------
! ======================================================================
    do 10 i = 1, dimcon
        do 20 j = 1, dimuel
            g(i,j) = 0.d0
            do 30 k = 1, dimdef
                g(i,j) = g(i,j) + dsde(i,k)*b(k,j)
30          continue
20      continue
10  continue
    do 40 i = 1, dimdef
        do 50 j = 1, dimuel
            h(i,j)= 0.d0
            do 60 k = 1, dimcon
                h(i,j) = h(i,j) + ck(i)*drds(i,k)*g(k,j)
60          continue
50      continue
40  continue
    do 70 i = 1, dimuel
        do 80 j = 1, dimuel
            do 90 k = 1, dimdef
                matri(i,j) = matri(i,j) + b(k,i)*h(k,j)*poids
90          continue
80      continue
70  continue
! ======================================================================
end subroutine
