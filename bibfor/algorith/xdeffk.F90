subroutine xdeffk(kappa, mu, r, theta, ndim, fkpo)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterc/r8depi.h"
!
    integer :: ndim
    real(kind=8) :: r, theta, fkpo(ndim,ndim), kappa, mu
!
!
!     BUT:  FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE (R,THETA)
!
! IN  R      : PREMIERE COORDONNEE DANS LA BASE POLAIRE
! IN  THETA  : SECONDE COORDONNEE DANS LA BASE POLAIRE
! OUT FKPO     : VALEURS DES FONCTIONS D'ENRICHISSEMENT <VECTORIELLES>
!
!
    real(kind=8) :: rr
!---------------------------------------------------------------
!
    ASSERT(ndim.eq.2.or.ndim.eq.3)
!
    rr=sqrt(r)/(2.d0*mu*sqrt(r8depi()))
!
!     FONCTIONS D'ENRICHISSEMENT
    fkpo(1,1) = rr * ( kappa - cos(theta) ) * cos(theta/2.d0)
    fkpo(1,2) = rr * ( kappa - cos(theta) ) * sin(theta/2.d0)
    fkpo(2,1) = rr * ( kappa + 2.d0 + cos(theta) ) * sin(theta/2.d0)
    fkpo(2,2) = rr * ( 2.d0 - kappa - cos(theta) ) * cos(theta/2.d0)
    if (ndim.eq.3) then
      fkpo(1:2,3) = 0.d0
      fkpo(3,1:2) = 0.d0
      fkpo(3,3) = 4.d0*rr * sin(theta/2.d0)
    endif
!
end subroutine
