subroutine xmmatu(ndim, nnop, nnops, ddls, ddlm, pla,&
                  dsidep, p, r, ffp, jac, ffc, nd, mmat,&
                  jheavn, ncompn, ifiss, nfiss, nfh, ifa,&
                  jheafa, ncomph)

    implicit none
 
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/matini.h"
#include "asterfort/hmdeca.h"
#include "asterfort/vecini.h"
#include "asterfort/promat.h"
#include "asterfort/transp.h"
#include "asterfort/prmave.h"
#include "asterfort/xcalc_saut.h"
#include "asterfort/xcalc_code.h"
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
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
!
! ROUTINE MODELE HM-XFEM (CAS DE LA FRACTURE)
!
! CALCUL DE LA MATRICE MMAT
!
! ----------------------------------------------------------------------
!
    integer :: i, j, ndim, ier1, ier2, nnop, ddls, ddlm, nnops, in, jn
    integer :: k, l, pla(27), pli, plj, jheavn, ncompn, nfiss, hea_fa(2)
    integer :: ifiss, nfh, ifa, ncomph, jheafa, ifh, jfh, dec, dej
    real(kind=8) :: unity(3,3), dside2(3,3), alocal(3,3), ptr(3,3), pdotal(3,3)
    real(kind=8) :: kdotal(3,3), au(3,3), dsidep(6,6), temp(3,3), knd(3), knloc(3)
    real(kind=8) :: r, p(3,3), ffp(27), jac, ffj, ffc(16), ffi, nd(3)
    real(kind=8) :: mmat(560,560), dside3(3,3), coefi, coefj
    aster_logical :: lmultc
! 
!   INITIALISATION
    lmultc = nfiss.gt.1
    call matini(3, 3, 0.d0, unity)
    call matini(3, 3, 0.d0, alocal)
    call matini(3, 3, 0.d0, ptr)
    call matini(3, 3, 0.d0, pdotal)
    call matini(3, 3, 0.d0, kdotal)
    call matini(3, 3, 0.d0, au)
    call matini(3, 3, 0.d0, dside2)
    call matini(3, 3, 0.d0, dside3)
    call matini(3, 3, 0.d0, temp)
    call vecini(3, 0.d0, knd)
    call vecini(3, 0.d0, knloc)
!
!   MATRICE -ID+R DSIDEP
!
    do i = 1, ndim
       unity(i,i) = 1.d0
    end do
!
    do i = 1, ndim
       do j = 1, ndim
          dside2(i,j) = dsidep(i,j)
          dside3(i,j) = -dsidep(i,j)
          alocal(i,j) = -unity(i,j) + r*dside2(i,j)
       end do
    end do 
!
!   MATRICE [P]T[ALOCAL]
!
    call transp(p, 3, ndim, ndim, ptr, 3)
!
    call promat(ptr, 3, ndim, ndim, alocal,&
                3, ndim, ndim, pdotal)
!
!   MATRICE [P]T[DSIDEP]
!
    call promat(ptr, 3, ndim, ndim, dside3,&
                3, ndim, ndim, temp)
!
    call promat(temp, 3, ndim, ndim, p,&
                3, ndim, ndim, kdotal)
!
    call prmave(0, kdotal, 3, ndim, ndim,&
                nd, ndim, knd, ndim, ier1)
!
!   VECTEUR [DSIDEP].ND
!
    call matini(3, 3, 0.d0, temp)
    call promat(dside2, 3, ndim, ndim, p,&
                3, ndim, ndim, temp)
!
    call prmave(0, temp, 3, ndim, ndim,&
                nd, ndim, knloc, ndim, ier2)
!
!   MATRICE TANGENTE EN BASE FIXE [P]T [DSIDEP] [P]
!
    call matini(3, 3, 0.d0, temp)
    call promat(ptr, 3, ndim, ndim, alocal,&
                3, ndim, ndim, temp)
!
    call promat(temp, 3, ndim, ndim, p,&
                3, ndim, ndim, au)
!
    if (.not.lmultc) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    else
      hea_fa(1) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+1)
      hea_fa(2) = zi(jheafa-1+ncomph*(ifiss-1)+2*(ifa-1)+2)
    endif
!
    do i = 1, nnops
       pli=pla(i)
       ffi=ffc(i)
       do k = 1, ndim
          do j = 1, nnop
             call hmdeca(j, ddls, ddlm, nnops, jn, dec)
             do ifh = 1, nfh
                coefj = xcalc_saut(zi(jheavn-1+ncompn*(j-1)+ifh),&
                                   hea_fa(1), &
                                   hea_fa(2),&
                                   zi(jheavn-1+ncompn*(j-1)+ncompn))
                do l = 1, ndim
! INDICES INVERSES MATRICE INTERFACE
                   mmat(pli+2+k,jn+(ndim+dec)*ifh+l) =&
                   mmat(pli+2+k,jn+(ndim+dec)*ifh+l) -&
                   coefj*ffp(j)*pdotal(l,k)*ffi*jac
! INDICES MEME ORDRE MATRICE EQUILIBRE
                   mmat(jn+(ndim+dec)*ifh+l,pli+2+k) =&
                   mmat(jn+(ndim+dec)*ifh+l,pli+2+k) -&
                   coefj*ffp(j)*pdotal(l,k)*ffi*jac
                end do 
             end do 
          end do 
       end do
    end do
!
    do i = 1, nnop
       call hmdeca(i, ddls, ddlm, nnops, in, dec)
       do ifh = 1, nfh
          coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                             hea_fa(1), &
                             hea_fa(2),&
                             zi(jheavn-1+ncompn*(i-1)+ncompn))
          do j = 1, nnop
             call hmdeca(j, ddls, ddlm, nnops, jn, dej)
             do jfh = 1, nfh
                coefj = xcalc_saut(zi(jheavn-1+ncompn*(j-1)+jfh),&
                               hea_fa(1), &
                               hea_fa(2),&
                               zi(jheavn-1+ncompn*(j-1)+ncompn))
                do k = 1, ndim
                   do l = 1, ndim
                      mmat(in+(ndim+dec)*ifh+k,jn+(ndim+dej)*jfh+l) =&
                      mmat(in+(ndim+dec)*ifh+k,jn+(ndim+dej)*jfh+l) -&
                      r*au(k,l)*coefi*ffp(i)*coefj*ffp(j)*jac
                   end do
                end do
             end do
          end do
       end do
    end do 
!
    do i = 1, nnop
       call hmdeca(i, ddls, ddlm, nnops, in, dec)
       do ifh = 1, nfh
          coefi = xcalc_saut(zi(jheavn-1+ncompn*(i-1)+ifh),&
                             hea_fa(1), &
                             hea_fa(2),&
                             zi(jheavn-1+ncompn*(i-1)+ncompn))
          do k = 1, ndim
             do j = 1, nnops
                plj=pla(j)
                ffj=ffc(j)
                mmat(in+(ndim+dec)*ifh+k,plj) =&
                mmat(in+(ndim+dec)*ifh+k,plj) + coefi*r*ffp(i)*knd(k)*ffj*jac
             end do 
          end do
       end do
    end do
!
    do i = 1, nnops
       pli=pla(i)
       ffi=ffc(i)
       do k = 1, ndim
          do j = 1, nnops
             plj=pla(j)
             ffj=ffc(j)
             do l = 1, ndim
                mmat(pli+2+k,plj+2+l) =&
                mmat(pli+2+k,plj+2+l) - ffi*dside2(k,l)*ffj*jac 
             end do 
          end do 
       end do
    end do
!
    do i = 1, nnops
       pli=pla(i)
       ffi=ffc(i)
       do k = 1, ndim
          do j = 1, nnops
             plj=pla(j)
             ffj=ffc(j)
             mmat(pli+2+k,plj) = mmat(pli+2+k,plj) -&
                                 ffi*knloc(k)*ffj*jac
          end do
       end do
    end do
!
end subroutine
