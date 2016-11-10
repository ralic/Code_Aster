subroutine xvechc(nnops, pla, ffc, pinf,&
                  pf, psup, jac, vect)
    implicit none   
!
#include "jeveux.h"
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
! CALCUL DES SECONDS MEMBRES VECT (CONTINUITE DE LA PRESSION)
!
! ----------------------------------------------------------------------
!
    integer :: i, nnops, pli, pla(27)
    real(kind=8) :: ffc(16), pinf, pf, psup, jac
    real(kind=8) :: vect(560), ffi
!   CALCUL DE LA CONTINUITE DE LA PRESSION
    do i = 1, nnops
       pli = pla(i)
       ffi = ffc(i)
!
       vect(pli+1) = vect(pli+1) + ffi*(psup-pf)*jac
!
       vect(pli+2) = vect(pli+2) + ffi*(pinf-pf)*jac
    end do
end subroutine
