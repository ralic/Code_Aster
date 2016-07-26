subroutine xcoocy(ndim, xg, pfon, p, rg, tg, l_not_zero)
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
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/xnormv.h"
!
    integer :: ndim
    real(kind=8) :: rg, tg, xg(ndim), pfon(ndim)
    real(kind=8) :: p(ndim,ndim)
    aster_logical, optional :: l_not_zero
!
!
!
!     BUT:  CALCUL DES COORDONNEES CYLINDRIQUES EN FOND DE FISSURE
!
!----------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: cosi, og(ndim), sinu, tole
    parameter (tole=1.d-12)
!----------------------------------------------------------------
!
    ASSERT(ndim.eq.2.or.ndim.eq.3)
!
    do i = 1, ndim
      og(i)=xg(i)-pfon(i)
    enddo
    call xnormv(ndim, og, rg)
    cosi=0.d0
    sinu=0.d0
    do i = 1, ndim
      cosi=cosi+p(i,1)*og(i)
      sinu=sinu+p(i,2)*og(i)
    enddo
!    tg=he*abs(atan2(sinu,cosi))
    tg=atan2(sinu,cosi)
!  - CETTE COMPARAISON POURRIE, IL FAUT L AMALIORER:
!      * LA PROBABILITE QUE RG SOIT TRES PETIT EST QUASI NULLE
!      * LE TEST ICI N EST PAS MIS A L ECHELLE
    if (present(l_not_zero)) then
      l_not_zero=.true.
      if (rg .lt. tole) l_not_zero=.false.
    endif
!
end subroutine
