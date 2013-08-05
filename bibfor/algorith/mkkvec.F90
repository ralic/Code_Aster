subroutine mkkvec(rese, nrese, ndim, vec, resu)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/assert.h"
    real(kind=8) :: rese(3), nrese, resu(3), vec(3)
    integer :: ndim
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE DERIVEE DE LA BOULE UNITE
!
! ----------------------------------------------------------------------
!
!
! SUBROUTINE QUI CALCULE RESU = K(LAMBDA +RHO[[U]]_TAU)*VEC
!
! IN  RESE  : LAMBDA +RHO[[U]]_TAU
! IN  NRESE : RACINE DE LA NORME DE RESE
! IN  NDIM  : DIMENSION DU PROBLEME
! IN  VEC   : LE VECTEUR A MULTIPLIER
! OUT RESU  : LE RESULTAT  [K]*VEC
!                      K(x) = (Id-x*xt/!!x!!**)1/!!x!!
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: mat(3, 3)
    real(kind=8) :: norme, theta
    integer :: i, j
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    do 11 i = 1, 3
        do 12 j = 1, 3
            mat(i,j) = 0.d0
12      continue
        resu(i) = 0.d0
11  end do
    theta = 1.d0
!
! --- CALCUL DE LA NORME DE LAMBDA +RHO[[U]]_TAU
!
    norme = nrese*nrese
    if ((norme.eq.0.d0) .or. (nrese.eq.0.d0)) then
        ASSERT(.false.)
    endif
!
! --- CALCUL DU PRODUIT IK()VEC
!
    if (ndim .eq. 2) then
        do 13 i = 1, 2
            do 14 j = 1, 2
                mat(i,j) = -theta*rese(i)*rese(j)/norme
14          continue
13      continue
        do 15 j = 1, 2
            mat(j,j) = 1.d0+mat(j,j)
15      continue
        do 16 i = 1, 2
            do 17 j = 1, 2
                mat(i,j) = mat(i,j)/nrese
17          continue
16      continue
        do 18 i = 1, 2
            do 19 j = 1, 2
                resu(i) = mat(i,j)*vec(j)+resu(i)
                resu(3) = 0.d0
19          continue
18      continue
    else if (ndim.eq.3) then
        do 20 i = 1, 3
            do 21 j = 1, 3
                mat(i,j) = -theta*rese(i)*rese(j)/norme
21          continue
20      continue
        do 22 j = 1, 3
            mat(j,j) = 1.d0+mat(j,j)
22      continue
        do 23 i = 1, 3
            do 24 j = 1, 3
                mat(i,j) = mat(i,j)/nrese
24          continue
23      continue
        do 25 i = 1, 3
            do 26 j = 1, 3
                resu(i) = mat(i,j)*vec(j)+resu(i)
26          continue
25      continue
    endif
!
end subroutine
