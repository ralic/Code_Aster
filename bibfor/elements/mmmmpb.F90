subroutine mmmmpb(rese, nrese, ndim, matprb)
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
    include 'asterfort/assert.h'
    include 'asterfort/matini.h'
    real(kind=8) :: rese(3), nrese
    integer :: ndim
    real(kind=8) :: matprb(3, 3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA MATRICE DE PROJECTION SUR LA BOULE UNITE
!
! ----------------------------------------------------------------------
!
!
! IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT (x)
!               GTK = LAMBDAF + COEFAF*VITESSE
! IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  VEC    : LE VECTEUR A MULTIPLIER
! OUT MATPRB : MATRICE DE PROJECTION SUR LA BOULE UNITE
!                 K(x) = (Id-x*xt/!!x!!**)1/!!x!!
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: norme, theta
    integer :: i, j
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    call matini(3, 3, 0.d0, matprb)
    theta = 1.d0
    if (nrese .eq. 0.d0) call assert(.false.)
!
! --- CALCUL DE LA NORME DE LAMBDA +RHO[[U]]_TAU
!
    norme = nrese*nrese
!
! --- CALCUL DE LA MATRICE
!
    do 10 i = 1, ndim
        do 15 j = 1, ndim
            matprb(i,j) = -theta*rese(i)*rese(j)/norme
15      continue
10  end do
!
    do 20 j = 1, ndim
        matprb(j,j) = 1.d0+matprb(j,j)
20  end do
!
    do 30 i = 1, ndim
        do 35 j = 1, ndim
            matprb(i,j) = matprb(i,j)/nrese
35      continue
30  end do
!
end subroutine
