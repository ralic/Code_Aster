subroutine matvec(nordre, amat, nombv, v1, v2,&
                  vecres)
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
#include "asterfort/u2mess.h"
    real(kind=8) :: amat(*), v1(*), v2(*), vecres(*)
    real(kind=8) :: vsom(9)
! ---------------------------------------------
!     BUT:   POUR LES ELEMENTS DE CABLE, CALCUL DU PRODUIT DE LA MATRICE
!            AMAT D'ORDRE NORDRE PAR:
!            . SI NOMBV=1, LE VECTEUR V1;
!            . SI NOMBV=2, LA SOMME DES VECTEURS V1 ET V2.
!            LE VECTEUR RESULTAT EST MIS DANS VECRES.
!     IN: NORDRE
!         AMAT
!         NOMBV
!         V1
!         V2
!     OUT: VECRES
! ---------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j, k, nombv, nordre
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    zero = 0.d0
    if (nombv .eq. 1) then
        do 1 i = 1, nordre
            vsom(i) = v1(i)
 1      continue
    else if (nombv.eq.2) then
        do 2 i = 1, nordre
            vsom(i) = v1(i) + v2(i)
 2      continue
    else
        call u2mess('F', 'ELEMENTS2_34')
    endif
    k = 0
    do 12 i = 1, nordre
        vecres(i) = zero
        do 11 j = 1, nordre
            k = k + 1
            vecres(i) = vecres(i) + amat(k) * vsom(j)
11      continue
12  end do
end subroutine
