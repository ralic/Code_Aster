subroutine calint(i, j, vect1, nbpts, vect2,&
                  long, tt)
    implicit none
#include "jeveux.h"
    integer :: i, j, nbpts
    real(kind=8) :: vect2(nbpts)
    complex(kind=8) :: vect1(long)
!     ----------------------------------------------------------------
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
!      A PARTIR DES VALEURS DE FONCTIONS CALCULE L'INTERSPECTRE  OU
!           L'AUTOSPECTRE
!     ----------------------------------------------------------------
!     IN  : VECT1 : VECTEUR DES VALEURS DES FONCTIONS DANS LE DOMAINE
!                   FREQUENTIEL
!     OUT : VECT2 : VALEURS DES AUTOSPECTRES ET INTERSPECTRES
!           NBPTS : NOMBRE DE POINTS DE LA DISCRETISATION FREQUENTIELLE
!           TT    : TEMPS TOTAL DE L'EVOLUTION TEMPORELLE
!
!-----------------------------------------------------------------------
    integer :: k, long, lvect1, lvect2, npt, npt2
    real(kind=8) :: tt
!-----------------------------------------------------------------------
    npt= nbpts
    npt2 = npt/2
    do 10 k = 1, npt2
        lvect1 = (i-1)*npt2+ k
        lvect2 = (j-1)*npt2+ k
        vect2(k) =(dble(vect1(lvect1)*dconjg(vect1(lvect2))))/tt
        vect2(npt2+k)=(dimag(vect1(lvect1)*dconjg(vect1(lvect2))))/tt
10  end do
end subroutine
