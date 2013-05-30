subroutine pgyty(nno, npg, dfde, yty)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    real(kind=8) :: dfde(*), yty(*)
    real(kind=8) :: enprim(3, 2)
! ----------------------------------------------------------------------
!     BUT:   POUR LES ELEMENTS DE CABLE, CALCUL DU PRODUIT DE MATRICES
!            YT * Y  Y ETANT LA MATRICES DES DERIVEES DES FONCTIONS DE
!            FORME ENPRIM.
!            LE PRODUIT EST CALCULE AUX POINTS DE GAUSS SUCCESSIFS
!            ET RANGE PAR LIGNES: 1ERE LIGNE, PUIS 2EME LIGNE...
!     IN: NNO  : NOMBRE DE NOEUDS
!         NPG  : NOMBRE DE POINTS DE GAUSS
!         DFDE : DERIVEES DES FONCTIONS DE FORME
!     OUT: YTY
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ii, j, jj, k, ki, ngaus
    integer :: nno, nordre, npg, numero
!-----------------------------------------------------------------------
    k = 0
    do 2 j = 1, npg
        do 1 i = 1, nno
            k = k + 1
            enprim(i,j) = dfde(k)
 1      continue
 2  end do
!
    nordre = 3 * nno
    numero = -nordre
    do 14 ngaus = 1, npg
        do 13 ii = 1, nno
            do 12 ki = 1, 3
                i = ki + 3*(ii-1)
                numero = numero + nordre
                do 11 jj = 1, nno
                    j = ki + 3*(jj-1)
                    yty(numero + j) = enprim(ii,ngaus) * enprim(jj, ngaus)
11              continue
12          continue
13      continue
14  end do
end subroutine
