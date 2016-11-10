subroutine xvechu(ndim, nnop, nnops, ddls, ddlm, pla,&
                  lamb, am, delta, r, p, ffp, jac, ffc, vect,&
                  ncompn, jheavn, ifiss, nfiss, nfh,&
                  ifa, jheafa, ncomph)
!
    implicit none  
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/hmdeca.h" 
#include "asterfort/vecini.h"
#include "asterfort/matini.h"
#include "asterfort/transp.h"   
#include "asterfort/prmave.h" 
#include "asterfort/xcalc_saut.h"
#include "asterfort/xcalc_code.h"
!
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
!
!
! ROUTINE MODELE HM-XFEM
! 
! CALCUL DES SECONDS MEMBRES VECT (EQUILIBRE MECANIQUE + LOI INTERFACE)
!
! ----------------------------------------------------------------------
!
    integer :: i, j, k, ndim, ier, nnop, ddls, ddlm, nnops, in, pla(27)
    integer :: pli, ncompn, jheavn, nfiss, hea_fa(2)
    integer :: ifiss, nfh, jheafa, ifa, ncomph, ifh, dec
    real(kind=8) :: h(3), hfix(3), ptr(3,3), lamb(3), am(3), delta(6)
    real(kind=8) :: r, p(3,3), ffp(27), jac, ffc(16), ffi
    real(kind=8) :: vect(560), coefi
    aster_logical :: lmultc
! 
!   INITIALISATIONS
    lmultc = nfiss.gt.1
    call vecini(3, 0.d0, h)
    call vecini(3, 0.d0, hfix)
    call matini(3, 3, 0.d0, ptr)
!
    do i = 1, ndim
       h(i) = - lamb(i) - r*am(i) + r*delta(i)
    end do
!
    if (.not.lmultc) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    else
      hea_fa(1) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+1)
      hea_fa(2) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+2)
    endif
! 
!   CONVERSION DE H EN BASE FIXE : {HFIX} = [P]T {H}
!
    call transp(p, 3, ndim, ndim, ptr, 3)
!
    call prmave(0, ptr, 3, ndim, ndim,&
                h, ndim, hfix, ndim, ier)
!
    coefi = xcalc_saut(1,0,1)
    do i = 1, nnop
       call hmdeca(i, ddls, ddlm, nnops, in, dec)
       do ifh = 1, nfh
          coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                             hea_fa(1), &
                             hea_fa(2),&
                             zi(jheavn-1+ncompn*(i-1)+ncompn))
          do j = 1, ndim
             vect(in+(ndim+dec)*ifh+j) = vect(in+(ndim+dec)*ifh+j)&
                                         - coefi*ffp(i)*hfix(j)*jac
          end do
       end do
    end do 
!
    do i = 1, nnops
       pli=pla(i)
       ffi=ffc(i)
       do k = 1, ndim
          vect(pli+2+k) = vect(pli+2+k) + (am(k)-delta(k))*ffi*jac
       end do
    end do
!
end subroutine
