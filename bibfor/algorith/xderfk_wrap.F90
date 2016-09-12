subroutine xderfk_wrap(kappa, mu, r, theta, ndim, dfkdpo, option, istano)
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
#include "asterfort/assert.h"
#include "asterfort/xderfk.h"
#include "asterc/r8depi.h"
#include "asterc/r8prem.h"
!
    integer :: ndim, istano
    real(kind=8) :: r, theta, dfkdpo(ndim,ndim,2), kappa, mu
    character(len=*) :: option
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
    character(len=8) :: pref
!
    pref=option
!    if (istano.eq.-2) pref='SMOOTH'
!
    call xderfk(kappa, mu, r, theta, ndim, dfkdpo)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if     (pref.eq.'DEFAULT') then
      goto 999
!
    elseif (pref.eq.'BASIC') then
      dfkdpo(1:ndim,1:ndim,1:2)=0.
      dfkdpo(1,1,1:2)=[1/sqrt(r)*cos(theta/2.d0),-0.5*sqrt(r)*sin(theta/2.d0)]
      dfkdpo(1,2,1:2)=[1/sqrt(r)*sin(theta/2.d0),0.5*sqrt(r)*cos(theta/2.d0)]
      dfkdpo(2,1,1:2)=[1/sqrt(r)*sin(theta/2.d0),0.5*sqrt(r)*cos(theta/2.d0)]
      dfkdpo(2,2,1:2)=[1/sqrt(r)*cos(theta/2.d0),-0.5*sqrt(r)*sin(theta/2.d0)]
      if (ndim.eq.3) dfkdpo(3,3,1:2)=[1/sqrt(r)*sin(theta/2.d0),0.5*sqrt(r)*cos(theta/2.d0)]
!
    elseif (pref.eq.'SMOOTH') then
      dfkdpo(1:ndim,1:ndim,1)=0.
      if (r.gt.r8prem()) then
         dfkdpo(1:ndim,1:ndim,2)=dfkdpo(1:ndim,1:ndim,2)/sqrt(r)         
      endif
!
    elseif (pref.eq.'JUMP') then
      dfkdpo=2.d0*mu*sqrt(r8depi())*dfkdpo/(kappa+1)    
      !dfkdpo(1:2,1:2,1:2)=2.d0*mu*sqrt(r8depi())*dfkdpo(1:2,1:2,1:2)/(kappa+1)
    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
999 continue
!
end subroutine
