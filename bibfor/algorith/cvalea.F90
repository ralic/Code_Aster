subroutine cvalea(ndim, cmod, ndimax, nbmod)
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
!
!***********************************************************************
!    B. GUIGON   P. RICHARD                    DATE 06/04/92
!-----------------------------------------------------------------------
!  BUT:  < COMPLEXE VECTEUR ALEATOIRE >
!
!   CETTE ROUTINE INITIALISE UN ENSEMBLE DE VECTEURS STOCKES EN COLONNE
!   DANS UNE MATRICE COMPLEXE A DES VALEUR ALEATOIRE COMPLEXE DE MODULE
!   UNITAIRE
!
!-----------------------------------------------------------------------
!
! NDIM     /I/: DIMENSION EFFICACE DES VECTEURS
! CMOD     /O/: MODES PROPRES COMPLEXES SOLUTIONS
! NDIM     /I/: DIMENSION MAX DES VECTEURS DES VECTEURS
! NBMOD    /I/: NOMBRE DE VECTEURS
!
!-----------------------------------------------------------------------
!
#include "asterfort/ggubs.h"
    integer :: ndim, nbmod
    complex(kind=8) :: cmod(ndimax, nbmod)
    real(kind=8) :: r(2), dseed
    integer :: i, k
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ndimax
!-----------------------------------------------------------------------
    dseed = 24331.d0
    do 10 k = 1, nbmod
        do 10 i = 1, ndim
            call ggubs(dseed, 2, r)
            cmod(i,k)=dcmplx(r(1),r(2))
10      continue
!
end subroutine
