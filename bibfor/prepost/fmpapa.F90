subroutine fmpapa(nbfonc, nbptot, sigm, rd0, rtau0,&
                  rcrit, rphmax, rayon)
    implicit none
!
#include "jeveux.h"
#include "asterfort/fmprhm.h"
#include "asterfort/fmrayo.h"
    integer :: nbfonc, nbptot
    real(kind=8) :: rphmax, rayon, sigm(nbfonc*nbptot)
    real(kind=8) :: rd0, rtau0, rcrit
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
!     RD0     : IN  : VALEUR DE D0
!     RTAU0   : IN  : VALEUR DE TAU0
!     RCRIT   : OUT : VALEUR DU CRITERE
!     RPHMAX  : OUT : VALEUR DE LA PRESSION HYDROSTATIQUE MAXIMALE
!     RAYON   : OUT : VALEUR DU RAYON DE LA SPHERE CIRCONSCRITE
!     ------------------------------------------------------------------
    real(kind=8) :: ra, rb
!
!------- CALCUL DU RAYON DE LA SPHERE CIRCONSCRITE ----
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call fmrayo(nbfonc, nbptot, sigm, rayon)
!
!------- CALCUL DE LA PRESSION HYDROSTATIQUE MAXIMALE -----
!
    call fmprhm(nbfonc, nbptot, sigm, rphmax)
!
!------- CALCUL DU CRITERE
!
    ra = (rtau0-rd0/sqrt(3.d0))/(rd0/3.d0)
    rb = rtau0
    rcrit = rayon + ra * rphmax - rb
!
end subroutine
