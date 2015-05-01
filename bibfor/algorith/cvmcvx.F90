subroutine cvmcvx(nmat, mater, sig, vin, seuil)
    implicit none
!       ================================================================
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
!       ----------------------------------------------------------------
!       VISCOCHABOCHE  :
!          CONVEXE VISCO PLASTIQUE POUR (MATER,SIG,X1,X2,R)  (OPTION 1 )
!                     SEUIL   F    = S   -  AR * R - K
!                                                T           1/2
!                       AVEC  S    = (3/2(D-X1-X2) (D-X1-X2))
!                       ET    D    = SIG - 1/3 TR(SIG) I
!       ----------------------------------------------------------------
!       IN  SIG    :  CONTRAINTE
!       IN  VIN    :  VARIABLES INTERNES = ( X1, X2, P, R, Q, XXI, E3 )
!       IN  NMAT   :  DIMENSION MATER
!       IN  MATER  :  COEFFICIENTS MATERIAU A TEMPERATURE
!       OUT SEUIL  :  SEUIL  ELASTICITE
!       ----------------------------------------------------------------
#include "asterfort/lcdevi.h"
#include "asterfort/lcdive.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcnrts.h"
#include "asterfort/lcprsv.h"
    integer :: ndt, ndi, nmat
    real(kind=8) :: sig(6), x1(6), x2(6), dev(6), vin(*)
    real(kind=8) :: difc1, difc2
    real(kind=8) :: ar, r, k, c1d, c2d
    real(kind=8) :: mater(nmat, 2), seuil
!       ----------------------------------------------------------------
    common /tdim/   ndt , ndi
    common /coed/   c1d , c2d
!       ----------------------------------------------------------------
!
! - CALCUL DU PREMIER SEUIL
!
!-----------------------------------------------------------------------
    real(kind=8) :: c1f, c2f
!-----------------------------------------------------------------------
    ar = mater(3,2)
    k = mater(4,2)
!        C1D      = MATERD(15,2)
!        C2D      = MATERD(20,2)
    c1f = mater(15,2)
    c2f = mater(20,2)
    call lceqvn(ndt, vin(1), x1)
    call lceqvn(ndt, vin(ndt+1), x2)
!
! --   CAS ANISOTHERME
!
    if (c1d .ne. 0.d0) then
        difc1 = c1f/c1d
        call lcprsv(difc1, x1, x1)
    endif
    if (c2d .ne. 0.d0) then
        difc2 = c2f/c2d
        call lcprsv(difc2, x2, x2)
    endif
!
    r = vin(2*ndt+2)
    call lcdevi(sig, dev)
    call lcdive(dev, x1, dev)
    call lcdive(dev, x2, dev)
    seuil = lcnrts( dev ) - ar * r - k
end subroutine
