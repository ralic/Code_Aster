subroutine lcopil(typ, mod, mater, kooh)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ----------------------------------------------------------------
!     OPERATEUR DE SOUPLESSE POUR UN COMPORTEMENT ELASTIQUE LINEAIRE
!     IN  TYP    :  TYPE OPERATEUR
!                   'ISOTROPE'
!                   'ORTHOTRO'
!                   'ANISOTRO'
!         MOD    :  MODELISATION
!         MATER  :  COEFFICIENTS MATERIAU ELASTIQUE
!     OUT KOOH   :  OPERATEUR DE SOUPLESSE ELASTIQUE LINEAIRE
!     ----------------------------------------------------------------
!
#include "asterfort/lcinma.h"
    integer :: ndt, ndi, i, j
    real(kind=8) :: un, zero
    parameter       ( un   = 1.d0   )
    parameter       ( zero = 0.d0   )
!
    real(kind=8) :: kooh(6, 6)
    real(kind=8) :: mater(*), e, nu, unpnue, unsure, mnuse
!
    character(len=8) :: mod, typ
!     ----------------------------------------------------------------
    common /tdim/   ndt  , ndi
!     ----------------------------------------------------------------
!
    call lcinma(zero, kooh)
!
    if (typ .eq. 'ISOTROPE') then
        e = mater(1)
        nu = mater(2)
        unpnue = ( un + nu ) / e
        unsure = un / e
        mnuse = - nu / e
!
! - 3D/DP/AX/CP
!
        if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:6) .eq. 'C_PLAN' .or.&
            mod(1:4) .eq. 'AXIS') then
            do 40 i = 1, ndi
                do 40 j = 1, ndi
                    if (i .eq. j) kooh(i,j) = unsure
                    if (i .ne. j) kooh(i,j) = mnuse
40              continue
            do 45 i = ndi+1, ndt
                do 45 j = ndi+1, ndt
                    if (i .eq. j) kooh(i,j) = unpnue
45              continue
!
! - 1D
!
        else if (mod(1:2) .eq. '1D') then
            kooh(1,1) = unsure
        endif
!
    else if (typ .eq. 'ORTHOTRO') then
!
        do 55 i = 1, 6
            do 56 j = 1, 6
                kooh(i,j)=mater(36+6*(j-1)+i)
56          continue
55      continue
!
    endif
end subroutine
