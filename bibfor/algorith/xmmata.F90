subroutine xmmata(ndim, nnops, nnop, ddls, ddlm, saut,&
                  nd, pla, ffc, dffc, mmat, rho11, mu,&
                  gradpf, ffp, dt, ta, jac,&
                  cliq, jheavn, ncompn, ifiss,&
                  nfiss, nfh, ifa, jheafa, ncomph)

    implicit none

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/vecini.h"
#include "asterfort/matini.h"
#include "asterfort/hmdeca.h"
#include "asterfort/xcalc_saut.h"
#include "asterfort/xcalc_code.h"
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
! ROUTINE MODELE HM-XFEM (CAS DE LA FRACTURE)
!
! CALCUL DE LA MATRICE MMAT
!
! ----------------------------------------------------------------------
!
    integer :: k, i, j, ndim, jheavn, ncompn, nfiss, nnop
    integer :: nnops, ddls, ddlm, in, pli, pla(27), plj, hea_fa(2)
    integer :: ifiss, nfh, ifa, jheafa, ncomph, ifh, dec
    real(kind=8) :: dffi(3), ps, saut(3), nd(3), cliq, dffj(3)
    real(kind=8) :: ffi, ffc(16), dffc(16,3), coefj
    real(kind=8) :: mmat(560,560), rho11, mu, gradpf(3)
    real(kind=8) :: dt, ta, jac, ffp(27), ffj
    aster_logical :: lmultc
!
    lmultc = nfiss.gt.1
    call vecini(3, 0.d0, dffi)
    call vecini(3, 0.d0, dffj)
!
    if (.not.lmultc) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    else
      hea_fa(1) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+1)
      hea_fa(2) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+2)
    endif
!
    ps = 0.d0
    do k = 1, ndim 
       ps = ps - saut(k)*nd(k)
    end do 
!
    do i = 1, nnops
       pli = pla(i)
       ffi = ffc(i)
       do k = 1, ndim
          dffi(k) = dffc(i,k)
       end do 
!
       do j = 1, nnop
          call hmdeca(j, ddls, ddlm, nnops, in, dec)
! 
          do ifh = 1, nfh
             coefj = xcalc_saut(zi(jheavn-1+ncompn*(j-1)+ifh),&
                                hea_fa(1), &
                                hea_fa(2),&
                                zi(jheavn-1+ncompn*(j-1)+ncompn))
             do k = 1, ndim
                mmat(pli,in+(ndim+dec)*ifh+k) = mmat(pli,in+(ndim+dec)*ifh+k) -&
                                       rho11*(3.d0*ps**2)/(12.d0*mu)*nd(k)*dffi(k)*gradpf(k)*&
                                       ffp(j)*coefj*dt*ta*jac
!
                mmat(pli,in+(ndim+dec)*ifh+k) = mmat(pli,in+(ndim+dec)*ifh+k) -&
                                       rho11*coefj*ffp(j)*ffi*nd(k)*jac
             end do 
          end do 
       end do
    end do
! 
    do i = 1, nnops
       pli = pla(i)
       ffi = ffc(i)
!
       do j = 1, nnops
!
          plj = pla(j)
          ffj = ffc(j)
!
          mmat(pli, plj+1) = mmat(pli, plj+1) - ffi*&
                                               ffj*dt*ta*jac
! 
          mmat(pli, plj+2) = mmat(pli, plj+2) - ffi*&
                                               ffj*dt*ta*jac
!
          mmat(pli, plj) = mmat(pli,plj) - rho11*(ps*cliq)*ffi*ffj*jac
!
       end do 
    end do  
!
    do i = 1, nnops
       pli = pla(i)
       do k = 1, ndim
          dffi(k) = dffc(i, k)
       end do
!
       do j = 1, nnops
          ffj = ffc(j)
          plj = pla(j)
          do k = 1, ndim
             dffj(k) = dffc(j, k)
          end do
!
          do k = 1, ndim
!
             mmat(pli, plj) = mmat(pli, plj) - (rho11*cliq)*(ps**3/(12.d0*mu))&
                                                *gradpf(k)*dffi(k)*ffj*dt*ta*jac
!
             mmat(pli, plj) = mmat(pli, plj) - (rho11*ps**3/(12.d0*mu))*&
                                                 dffi(k)*dffj(k)*dt*ta*jac
          end do
!
       end do
    end do
!
end subroutine
