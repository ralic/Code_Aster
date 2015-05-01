function biline(nordre, vect1, amat, vect2)
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
    real(kind=8) :: vect1(*), amat(*), vect2(*)
! ---------------------------------------------
!     BUT:   POUR LES ELEMENTS DE CABLE, CALCUL DE LA FORME BILINEAIRE:
!                         VECT1T * AMAT * VECT2
!            AMAT ETANT D'ORDRE NORDRE ET RANGEE PAR LIGNES DANS LE VEC-
!            TEUR AMAT: 1ERE LIGNE, PUIS 2EME LIGNE...
!     IN: NORDRE
!         VECT1
!         AMAT
!         VECT2
!     OUT: BILINE
! ---------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j, k, nordre
    real(kind=8) :: biline, produi, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    biline = zero
    k = 0
    do 2 i = 1, nordre
        produi = zero
        do 1 j = 1, nordre
            k = k + 1
            produi = produi + amat(k)*vect2(j)
 1      continue
        biline = biline + vect1(i)*produi
 2  end do
end function
