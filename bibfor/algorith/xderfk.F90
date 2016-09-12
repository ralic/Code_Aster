subroutine xderfk(kappa, mu, r, theta, ndim, dfkdpo)
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
!
#include "asterc/r8depi.h"
!
    integer :: ndim
    real(kind=8) :: r, theta, dfkdpo(ndim,ndim,2), kappa, mu
!
!
!     BUT : DERIVEES DES FONCTIONS D'ENRICHISSEMENT
!           DANS LA BASE POLAIRE (R,THETA)
!
! IN  R      : PREMIERE COORDONNEE DANS LA BASE POLAIRE
! IN  THETA  : SECONDE COORDONNEE DANS LA BASE POLAIRE
! OUT DFKDPO : DERIVEES DES FONCTIONS D'ENRICHISSEMENT
!   -- FORMAT DE STOCKAGE DES DERIVEES --
!       DFKDPO(i, j, l)
!            i <-> Ki
!            j <-> Kij=Ki.ej
!            l <-> [dKij/dr dKij/dtheta]
!----------------------------------------------------------------
!
    real(kind=8) :: rr, drr, s, c, s2, c2
!
!----------------------------------------------------------------
!
    rr=sqrt(r)/(2.d0*mu*sqrt(r8depi()))
    drr=1.d0/(4.d0*mu*sqrt(r)*sqrt(r8depi()))
    s = sin(theta)
    c = cos(theta)
    s2 = sin(theta/2.d0)
    c2 = cos(theta/2.d0)
!
!     DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE
    dfkdpo(1,1:2,1) = drr * [ (kappa-c)*c2 , (kappa-c)*s2 ]
    dfkdpo(1,1:2,2) = rr * [ s*c2-0.5d0*(kappa-c)*s2 , s*s2+0.5d0*(kappa-c)*c2 ]
!
    dfkdpo(2,1:2,1) = drr * [ (kappa+2+c)*s2 , (2-kappa-c)*c2 ]
    dfkdpo(2,1:2,2) = rr * [ -s*s2+0.5d0*(kappa+2+c)*c2 , s*c2-0.5d0*(2-kappa-c)*s2]
!
    if (ndim.eq.3) then
      dfkdpo(1:2,3,1:2) = 0.d0
      dfkdpo(3,1:2,1:2) = 0.d0
      dfkdpo(3,3,1:2) = 4.d0*[drr*s2 , 0.5d0*rr*c2 ]
    endif
!
end subroutine
