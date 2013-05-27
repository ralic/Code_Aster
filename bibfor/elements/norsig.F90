function norsig(sigma, nbsig)
    implicit none
    real(kind=8) :: norsig
    real(kind=8) :: sigma(nbsig)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!-----------------------------------------------------------------------
!     BUT           : CALCUL DE LA NORME DU TENSEUR DE CONTRAINTES
!                     SIGMA AU SENS SUIVANT :
!                     NORSIG = SIGMA(I,J)*SIGMA(I,J)
! IN  SIGMA(NBSIG)  : VECTEUR DES COMPOSANTES DU TENSEUR DE CONTRAINTES
! IN  NBSIG         : NOMBRE DE CONTRAINTES POUR UN TYPE D'ELEMENT
!                     DONNE
!-----------------------------------------------------------------------
    real(kind=8) :: norsi2
!
!-----------------------------------------------------------------------
    integer :: i, nbsig
    real(kind=8) :: deux, zero
!-----------------------------------------------------------------------
    zero = 0.0d0
    deux = 2.0d0
    norsig = zero
    norsi2 = zero
!
    do 10 i = 1, 3
        norsi2 = norsi2 + sigma(i)*sigma(i)
10  end do
!
    do 20 i = 4, nbsig
        norsi2 = norsi2 + deux*sigma(i)*sigma(i)
20  end do
!
    norsig = sqrt(norsi2)
!
end function
