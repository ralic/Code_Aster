subroutine rslpli(typ, mod, mater, hook, nmat,&
                  vin)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!       OPERATEUR DE RIGIDITE POUR COMPORTEMENT ELASTIQUE LINEAIRE
!       IN  TYP    :  TYPE OPERATEUR
!                     'ISOTROPE'
!                     'ORTHOTRO'
!                     'ANISOTRO'
!           MOD    :  MODELISATION
!           MATER  :  COEFFICIENTS MATERIAU ELASTIQUE
!       OUT HOOK   :  OPERATEUR RIGIDITE ELASTIQUE LINEAIRE
!       ----------------------------------------------------------------
!
#include "asterfort/lcopli.h"
#include "asterfort/lcprsm.h"
    integer :: nmat
!
    real(kind=8) :: un, rho, f, f0
    real(kind=8) :: hook(6, 6)
    real(kind=8) :: mater(nmat, 2), vin(*)
!
    parameter       ( un   = 1.d0   )
!
    character(len=8) :: mod, typ
!       ----------------------------------------------------------------
!
! --    CALCUL DE RHO
!
    f = vin(2)
    f0 = mater(3,2)
    rho = (un-f)/(un-f0)
!
    call lcopli(typ, mod, mater(1, 1), hook)
    call lcprsm(rho, hook, hook)
end subroutine
