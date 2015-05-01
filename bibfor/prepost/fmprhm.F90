subroutine fmprhm(nbfonc, nbptot, sigm, rphmax)
    implicit none
!
#include "jeveux.h"
    integer :: nbfonc, nbptot
    real(kind=8) :: sigm(nbfonc*nbptot)
    real(kind=8) :: rphmax
!     ------------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     NBFONC  : IN  : NOMBRE DE FONCTIONS (6 EN 3D 4 EN 2D)
!     NBPTOT  : IN  : NOMBRE DE PAS DE TEMPS DE CALCUL
!     SIGM    : IN  : VECTEUR DES CONTRAINTES EN TOUS LES PAS DE TEMPS
!     RPHMAX  : OUT : VALEUR DU DE LA PRESSION HYDROSTATIQUE MAXIMALE
!
    real(kind=8) :: rph
!
! ------- CALCUL DE LA PRESSION HYDROSTATIQUE MAXIMALE---
!
!-----------------------------------------------------------------------
    integer :: i, ide
!-----------------------------------------------------------------------
    rphmax=(sigm(1)+sigm(2)+sigm(3))/3.d0
    do 10 i = 2, nbptot
        ide = (i-1)*nbfonc
        rph=(sigm(ide+1)+sigm(ide+2)+sigm(ide+3))/3.d0
        if (rph .gt. rphmax) rphmax = rph
10  end do
!
end subroutine
