subroutine xsautl(ndim, nd, tau1, tau2, saut, sautm, p, am, ad)

    implicit none

#include "asterfort/matini.h"
#include "asterfort/vecini.h"
#include "asterfort/prmave.h"
   
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
! CALCUL DU CHANGEMENT DE BASE POUR LE SAUT DE DEPLACEMENT 
!
! ----------------------------------------------------------------------

    integer :: i, ndim, ier1, ier2
    real(kind=8) :: p(3,3), nd(3), tau1(3), tau2(3)
    real(kind=8) :: saut(3), sautm(3), am(3), ad(3)
    
    call matini(3, 3, 0.d0, p) 
    call vecini(3, 0.d0, am)
    call vecini(3, 0.d0, ad)
!
! --- ON CONSTRUIT P MATRICE DE PASSAGE BASE FIXE --> BASE COVARIANTE
!
    do i = 1, ndim
        p(1,i) = nd(i)
    end do
    do i = 1, ndim
        p(2,i) = tau1(i)
    end do
    if (ndim.eq.3) then
       do i = 1, ndim
            p(3,i) = tau2(i)
       end do
    endif    
!
! --- CALCUL SAUT DE DEPLACEMENT + EN BASE LOCALE {AM}=[P]{SAUT+}
!
    call prmave(0, p, 3, ndim, ndim,&
                saut, ndim, am, ndim, ier1)
!
! --- CALCUL SAUT DE DEPLACEMENT - EN BASE LOCALE {AD}=[P]{SAUT-}
!
    call prmave(0, p, 3, ndim, ndim,&
                sautm, ndim, ad, ndim, ier2)
end subroutine
