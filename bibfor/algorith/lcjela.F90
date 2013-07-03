subroutine lcjela(loi, mod, nmat, mater, vin,&
                  dsde)
    implicit   none
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
!       MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT ELASTIQUE A T+DT OU T
!       IN  LOI    :  MODELE DE COMPORTEMENT
!           MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATER  :  COEFFICIENTS MATERIAU
!           VIN    :  VARIABLES INTERNES
!       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT
!       ----------------------------------------------------------------
#include "asterfort/lcopli.h"
#include "asterfort/rslpli.h"
    integer :: nmat
    real(kind=8) :: dsde(6, 6)
    real(kind=8) :: vin(*)
    real(kind=8) :: mater(nmat, 2)
    character(len=8) :: mod
    character(len=16) :: loi
!       ----------------------------------------------------------------
    if (loi(1:8) .eq. 'ROUSS_PR' .or. loi(1:10) .eq. 'ROUSS_VISC') then
        call rslpli('ISOTROPE', mod, mater, dsde, nmat,&
                    vin)
!
    else if (loi(1:8) .eq. 'MONOCRIS') then
!
        if (mater(nmat,1) .eq. 0) then
            call lcopli('ISOTROPE', mod, mater(1, 1), dsde)
        else if (mater(nmat,1).eq.1) then
            call lcopli('ORTHOTRO', mod, mater(1, 1), dsde)
        endif
!
!    CAS GENERAL : ELASTICITE LINEAIRE ISOTROPE OU ANISOTROPE
    else
!
        call lcopli('ISOTROPE', mod, mater(1, 1), dsde)
!
    endif
!
end subroutine
