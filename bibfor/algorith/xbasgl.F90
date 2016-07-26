subroutine xbasgl(ndim, basloc, ipt, p, invp)
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
#include "asterfort/assert.h"
#include "asterfort/provec.h"
#include "asterfort/xnormv.h"
#include "asterc/r8prem.h"
#include "asterfort/matinv.h"
!
    integer :: ndim, ipt
    real(kind=8) :: basloc(*)
    real(kind=8) :: p(ndim,ndim), invp(ndim,ndim)
!
!
!
!     BUT:  CALCUL DE LA MATRICE DE PASSAGE BASE LOCALE => BASE GLOBALE
!
!----------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: e1(3), e2(3), e3(3), norme, det
!----------------------------------------------------------------
!
    ASSERT(ndim.eq.2.or.ndim.eq.3)
!
    e1(:)=0.d0
    e2(:)=0.d0
    e3(:)=0.d0
!
    do  i = 1, ndim
      e1(i) = basloc(3*ndim*(ipt-1)+i+ndim)
      e2(i) = basloc(3*ndim*(ipt-1)+i+2*ndim)
    end do
!
!  *  NORMALISATION DE LA BASE
    call xnormv(3, e1, norme)
    call xnormv(3, e2, norme)
    call provec(e1, e2, e3)
    call xnormv(3, e3, norme)
!    if (ndim.eq.3) call provec(e2, e3, e1)
!
!  *  CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
    p(1:ndim,1)=e1(1:ndim)
    p(1:ndim,2)=e2(1:ndim)
    if (ndim.eq.3) p(1:ndim,3)=e3(1:ndim)
!
!  *  VERIFICATION QUE LA BASE EST ORTHONORMEE (DETERMINANT DE P VAUT BIEN 1)
     call matinv('C', ndim, p, invp, det)
!    det = det_mat(ndim,p)
    if (abs((abs(det)-1.d0)).gt.1.d-5) then
!  *  SI LE DETERMINANT N EST PAS 1 XCALFEV A SANS DOUTE ETE APPELEE AVEC UNE BASE NULLE
!     POUR CALCULER ZERO
!      ASSERT(norme.lt.r8prem())
    endif
!
!    if (ndim.eq.2) then
!      p(1:2,2)=sign(1.,det)*[-e1(2),e1(1)]
!      if (e3(3).lt.0) then
!        p(1:2,2)=[-e1(2),e1(1)]
!      endif
!    endif
!
!  *  CALCUL DE L'INVERSE DE LA MATRICE DE PASSAGE : INV(P)=TRANSPOSE(P)
!    do i = 1, ndim
!      do  j = 1, ndim
!        invp(i,j)=p(j,i)
!      enddo
!    enddo
!
!
end subroutine
