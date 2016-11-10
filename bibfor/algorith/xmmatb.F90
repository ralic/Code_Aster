subroutine xmmatb(ndim, nnops, ddls, ddlm, ffc,&
                  pla, dt, ta, jac, ffp2, mmat,&
                  jheavn, ncompn, ifiss,&
                  nfiss, nfh, ifa, jheafa, ncomph)

    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/hmdeca.h"   
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_heav.h"
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
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
!
! ROUTINE MODELE HM-XFEM
!
! CALCUL DE LA MATRICE MMAT (CONSERVATION DE LA MASSE FLUIDE MASSIF)
!
! ----------------------------------------------------------------------

    integer :: i, j, ndim, nnops, in, ddls, ddlm, ncompn, ifh, nfh, ifiss
    integer :: plj, pla(27), jheavn, heavn(nnops, ncompn), nfiss, hea_fa(2)
    integer :: ifa, jheafa, ncomph, dec
    real(kind=8) :: ffj, ffc(16), dt, ta
    real(kind=8) :: jac, ffp2(27), mmat(560,560)
    aster_logical :: lmultc
!
    lmultc = nfiss.gt.1
    if (.not.lmultc) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    else
      hea_fa(1) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+1)
      hea_fa(2) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+2)
    endif
!
!     RECUPERATION DE LA DEFINITION DES DDLS HEAVISIDES
    do in = 1, nnops
      do i = 1 , ncompn
        heavn(in,i) = zi(jheavn-1+ncompn*(in-1)+i)
      enddo
    enddo
!
    do i = 1, nnops
       call hmdeca(i, ddls, ddlm, nnops, in, dec)
!
       do j = 1, nnops
          ffj = ffc(j)
          plj = pla(j)
!
          mmat(in+ndim+1, plj+1) = mmat(in+ndim+1, plj+1) + ffp2(i)*ffj*dt*ta*jac
!
          mmat(in+ndim+1, plj+2) = mmat(in+ndim+1, plj+2) + ffp2(i)*ffj*dt*ta*jac
!
          do ifh = 1, nfh
             mmat(in+(ndim+1)*(ifh+1), plj+1) = mmat(in+(ndim+1)*(ifh+1), plj+1) + ffp2(i)*&
                                      xcalc_heav(heavn(i,ifh),hea_fa(2),heavn(i,5))*ffj*dt*ta*jac
! 
             mmat(in+(ndim+1)*(ifh+1), plj+2) = mmat(in+(ndim+1)*(ifh+1), plj+2) + ffp2(i)*&
                                      xcalc_heav(heavn(i,ifh),hea_fa(1),heavn(i,5))*ffj*dt*ta*jac
          end do 
       end do 
    end do                                                     
end subroutine
