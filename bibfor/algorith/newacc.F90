subroutine newacc(neq, c1, c2, c3, d0,&
                  v0, a0, d1, a1)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!**********************************************************************
    implicit none
!
!
!  INPUT:
!        NEQ   : NOMBRE D'EQUATIONS (D.D.L. ACTIFS)
!        C1,C2,C3 : CONSTANTES DE CALCUL
!        D0    : VECTEUR DEPLACEMENT  INITIAL   (NEQ)
!        V0    : VECTEUR VITESSE      INITIALE  (NEQ)
!        A0    : VECTEUR ACCELERATION INITIALE  (NEQ)
!        D1    : VECTEUR DEPLACEMENT SOLUTION   (NEQ)
!
!  OUTPUT:
!        A1    : VECTEUR ACCEL;ERATION SOLUTION (NEQ)
!
!  CALCUL DE L'ACCELERATION: ACCSOL = C1*( DEPSOL-D0) + C2*V0 + C3*A0
!
!
!----------------------------------------------------------------------
!   E.D.F DER   JACQUART G. 47-65-49-41      LE 19 JUILLET 1990
!**********************************************************************
!
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    real(kind=8) :: d0(*), d1(*), v0(*), a0(*), a1(*)
    real(kind=8) :: c1, c2, c3
    real(kind=8) :: scal
!
!-----------------------------------------------------------------------
    integer :: neq
!-----------------------------------------------------------------------
    scal = -1.d0
    call dcopy(neq, d1, 1, a1, 1)
    call daxpy(neq, scal, d0, 1, a1,&
               1)
    call dscal(neq, c1, a1, 1)
    call daxpy(neq, c2, v0, 1, a1,&
               1)
    call daxpy(neq, c3, a0, 1, a1,&
               1)
end subroutine
