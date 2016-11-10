subroutine xvecha(ndim, pla, nnops, saut,&
                  sautm, nd, ffc, w11, w11m, jac,&
                  q1, q1m, q2, q2m, dt, ta, ta1,&
                  dffc, rho11, mu, gradpf, rho11m,&
                  gradpfm, vect)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/vecini.h"
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
! CALCUL DES SECONDS MEMBRES VECT (CONSERVATION DE LA MASSE FLUIDE FRACTURE)
!
! ----------------------------------------------------------------------
!
    integer :: k, i, ndim, pli, pla(27), nnops
    real(kind=8) :: ps, psm, saut(3), sautm(3)
    real(kind=8) :: nd(3), ffc(16), ffi, w11, w11m
    real(kind=8) :: jac, q1, dt, ta, q1m, ta1, q2, q2m
    real(kind=8) :: dffi(3), dffc(16,3), rho11
    real(kind=8) :: mu, gradpf(3), rho11m, gradpfm(3)
    real(kind=8) :: vect(560)
!
    call vecini(3, 0.d0, dffi)
!
    ps = 0.d0
    psm = 0.d0
    do k = 1, ndim 
       ps = ps - saut(k)*nd(k)
       psm = psm - sautm(k)*nd(k)
    end do 
!
    do i = 1, nnops
       ffi = ffc(i)
       pli = pla(i)
!
       vect(pli) = vect(pli) - ffi*(w11 - w11m)*jac
!
       vect(pli) = vect(pli) - ffi*q1*dt*ta*jac -&
                               ffi*q1m*dt*ta1*jac
!
       vect(pli) = vect(pli) - ffi*q2*dt*ta*jac -&
                               ffi*q2m*dt*ta1*jac
!
    end do  
!
    do i = 1, nnops
       pli = pla(i)
       do k = 1, ndim
          dffi(k) = dffc(i,k)
!
          vect(pli) = vect(pli) + dffi(k)*(-(rho11*ps**3/(12.d0*mu))*&
                                  gradpf(k))*dt*ta*jac
!
          vect(pli) = vect(pli) + dffi(k)*(-(rho11m*psm**3/(12.d0*mu))*&
                                  gradpfm(k))*dt*ta1*jac
       end do
    end do
end subroutine
