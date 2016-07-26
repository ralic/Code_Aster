subroutine xdeffk_wrap(kappa, mu, r, theta, ndim, fkpo, option, istano)
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
#include "asterfort/xdeffk.h"
#include "asterc/r8depi.h"
#include "asterc/r8prem.h"
!
    integer :: ndim, istano
    real(kind=8) :: r, theta, fkpo(ndim,ndim), kappa, mu
    character(len=*) :: option
!
!
!     BUT:  FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE (R,THETA)
!
! IN  R      : PREMIERE COORDONNEE DANS LA BASE POLAIRE
! IN  THETA  : SECONDE COORDONNEE DANS LA BASE POLAIRE
! OUT FKPO     : VALEURS DES FONCTIONS D'ENRICHISSEMENT <VECTORIELLES>
!
!---------------------------------------------------------------
!
    character(len=8) :: pref
!
    ASSERT(option.eq.'DEFAULT'.or.option.eq.'SMOOTH'.or.option.eq.'JUMP'.or.option.eq.'BASIC')
!
    pref=option
!    if (istano.eq.-2) pref='SMOOTH'
!
    call xdeffk(kappa, mu, r, theta, ndim, fkpo)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if     (pref.eq.'DEFAULT') then
      goto 999
!
    elseif (pref.eq.'BASIC') then
      fkpo(1:ndim,1:ndim)=0.
      fkpo(1,1:2)=[sqrt(r)*cos(theta/2.d0),sqrt(r)*sin(theta/2.d0)]
      fkpo(2,1:2)=[sqrt(r)*sin(theta/2.d0),sqrt(r)*cos(theta/2.d0)]
      if (ndim.eq.3) fkpo(3,3)=sqrt(r)*sin(theta/2.d0)
!
    elseif (pref.eq.'SMOOTH') then
      if (r.gt.r8prem()) then
         fkpo=fkpo/sqrt(r)
      endif
!
    elseif (pref.eq.'JUMP') then
      fkpo=2.d0*mu*sqrt(r8depi())*fkpo/(kappa+1)    
      !fkpo(1:2,1:2)=2.d0*mu*sqrt(r8depi())*fkpo(1:2,1:2)/(kappa+1)
!
    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
999 continue
!
end subroutine
